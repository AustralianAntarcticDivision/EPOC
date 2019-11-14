################################################################################
# EPOC Period class
#
# Sub-component of EPOC Calendar class
#
# S4 Class
# Created 7/5/2009 Troy Robertson
# Modified 20/3/2012 - TR
################################################################################

# Create Period class
setClass("Period",
        representation(number			= "numeric",
					   day				= "numeric",			# Number of days in period
					   knifeEdge		= "logical",			# Are there any knife edge timesteps
					   yearPropn		= "numeric",			# Proportion of the year dedicated to period
					   periodStart		= "numeric",			# Julian start day of year for period
					   periodEnd		= "numeric",			# Julian end day of year for period
					   modules			= "list",				# Timestep data listed by [[module]][[element]]
					   periodActionMat	= "list"),				# The final period action matrix
        prototype(number			= 0,
				  day				= 0,	
				  knifeEdge			= FALSE,
				  yearPropn			= 0,
				  periodStart		= 0,
				  periodEnd			= 0,
				  modules			= NULL,
				  periodActionMat	= NULL)
)

setMethod("initialize", signature(.Object="Period"),
    function(.Object, universe, periodPropYear, periodKEData, previousDay, periodNumber) {
		modNames <- names(universe$modules)
		# build up a list of module matrices
		.Object@modules <- list()
		for (modNum in seq_along(universe$modules)) {
			.Object@modules[[modNames[[modNum]]]] <- if(!is.null(universe$modules[[modNum]]))
															matrix(NA,ncol=3,nrow=length(universe$modules[[modNum]]))
													 else NULL
		}

		.Object@number			<- periodNumber
		.Object@day   			<- periodPropYear[[1]]
		.Object@knifeEdge   	<- if(periodPropYear[[2]]==0) FALSE else TRUE
		.Object@yearPropn   	<- periodPropYear[[3]]
		.Object@periodStart 	<- previousDay / 365 	# proportion of year passed since 0 Jan to beginning of time period
		.Object@periodEnd   	<- previousDay / 365 + periodPropYear[[3]]
		
		.Object@periodActionMat <- list(Before = NULL, During = NULL, After = NULL)
		
		if (.Object@knifeEdge){ # fill in appropriate values for elements that
                                   #have knife-edge timesteps in this period
                 # periodKEData : col 1 = day in year of timestep,
                 #            	col 2 = is it knife-edge (0 = no, 1 = yes)
                 #            	col 3 = element class
                 #            	col 4 = element number
                 #            	col 5 = timestep number within element
                if (nrow(periodKEData) > 0) {
                    for (ke in 1:nrow(periodKEData)) {
						.Object@modules[[periodKEData[ke,3]]][periodKEData[ke,4],1] <- periodKEData[ke,5]	# Timestep number
						.Object@modules[[periodKEData[ke,3]]][periodKEData[ke,4],2] <- 1					# Timesteps % of period
						.Object@modules[[periodKEData[ke,3]]][periodKEData[ke,4],3] <- 1					# Periodstep number within timestep
                    }
                }
            }
			
		return(.Object)
	}
)

# Return a list containing period slot items
setGeneric("getPeriodInfo", function(.Object) standardGeneric("getPeriodInfo"))
setMethod("getPeriodInfo", signature(.Object="Period"),
    function(.Object) {
		return(list(Number=.Object@number, Day=.Object@day, KnifeEdge=.Object@knifeEdge, YearPropn=.Object@yearPropn, 
											PeriodStart=.Object@periodStart, PeriodEnd=.Object@periodEnd))  
	}
)

# Return the period action matrix
setGeneric("getPeriodActionMat", function(.Object) standardGeneric("getPeriodActionMat"))
setMethod("getPeriodActionMat", signature(.Object="Period"),
    function(.Object) return(.Object@periodActionMat)  
)

# Return the timestep data for a particular element 
setGeneric("getPeriodElementTSData", function(.Object, modnum, elemnum) standardGeneric("getPeriodElementTSData"))
setMethod("getPeriodElementTSData", signature(.Object="Period", modnum="numeric", elemnum="numeric"),
    function(.Object, modnum, elemnum) {
		return(.Object@modules[[modnum]][elemnum,]) 
	}
)