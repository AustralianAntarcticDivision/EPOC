################################################################################
# EPOC Calendar class
#
# Converted from C.Cal.01.01.R as created by A.Constable 14 July 2006
# Incorporates: C.Cal.01.01.R, C.Cal.output.01.R, C.Cal.01.addPeriodPropsToConfigParams.01.R,
#               C.Cal.ElementTimestepToPeriod.Prop.01.R, C.Cal.Tstep_to_period.01.R'
#               C.Cal.ActionMat.01.R
#
# S4 Class
# Created 4/2/2009 Troy Robertson
################################################################################
# Create Calendar class
setClass("Calendar",
        representation(periods		= "list",			# List of Period objects
					   periodN		= "numeric"),		# Number of periods in calendar
        prototype(periods			= NULL,
				  periodN			= 0),
		contains			= ".environment"
)

################################################################################
# Function:           C.Cal.01.01
# Description:        General controller for determining the calendar of events for a year
# Primary attributes: Determine calendar of events by comparing across all parts of the ecosystem
#                     Error check to ensure all actions can be accommodated
#
# Input parameters:   Universe - list including one or more of
#                               (Config, Biota, Environment, Activity, Management, Output, Presentation)
#                                Config elements are not examined for the calendar
# Returned            Calendar of events
#
########################################################
#
# Rules for each ecosystem component:
#   i)   The last time step must be 365 days - if not the first time step will use the remainder from the last timestep to 365
#   ii)  The actions nominated at a time step occur over the period from the previous time step to the current
#   iii) The actions are currently modelled as occurring at a constant rate over the time step
#   iv)  The actions are considered to appropriately act according to the state of the ecosystem at the beginning of the time step
#           unless changes to other components of the ecosystem have occurred that impact on the element,
#           such as the starting or stopping of predation by a competitor during a foraging timestep, or a predator during reproduction.
#
setGeneric("createCalendar", function(.Object, universe) standardGeneric("createCalendar"))
setMethod("createCalendar", signature(.Object="Calendar", universe="Universe"),
    function(.Object, universe) {
        epocMessage(universe, "Calendar setup...")
		
		# insert initial period data into calendar
		.Object <- .initialiseCalendar(.Object, universe)

		# fill Calendar periods with relevant TS data
        epocVerboseMessage(universe, "  set up timestep data...")
        .Object <- .setupPeriodTS(.Object, universe)
		
        # Convert timesteps to periods in all modules
        epocVerboseMessage(universe, "  convert timesteps to periods...")
		.convertTSToElementPeriods(.Object, universe)

		# Generate Action Matrice
        epocVerboseMessage(universe, "  generate action matrix for each period...")
		.Object <- .generateActionMatrix(.Object, universe)
     
        return(list(.Object, universe))
    }
)

# Accumulate time steps from all elements, noting when duplicate "calday"s within
# an element are indicative of knife-edge actions/updates - data are stored for
# specifying knife edge options later
#         
# Parameters:
#  	.Object    		Calendar 	this calendar
#	universe		Universe 	the entire universe
# Return: 	Calendar
setGeneric(".initialiseCalendar", function(.Object, universe) standardGeneric(".initialiseCalendar"))
setMethod(".initialiseCalendar", signature(.Object="Calendar", universe="Universe"),
    function(.Object, universe) {
	
		epocVerboseMessage(universe, "  accumulate time steps from all elements")
		
		# Create a vector of all the unique timesteps in calendar days - Day 1 is 1 January, Day 365 is 31 December
		# timeStepsDays is matrix - col 1 = day in year of timestep,
		#                           col 2 = is it knife-edge (0 = no, 1 = yes)
		#                           when it is knife edge the following information replaces NA values
		#                           col 3 = element class
		#                           col 4 = element number
		#                           col 5 = timestep number within element
        timeStepsDays <- NULL
		moduleNames <- names(universe$modules)
		# Loop through all element classes to accumulate time steps
        for (modulenum in seq_along(universe$modules)) {
			module <- universe$modules[[modulenum]]
			epocVerboseMessage(universe, "     Universe - Module ", as.character(modulenum), ": ", moduleNames[modulenum])
			epocVerboseMessage(universe, "                   elementsN = ",
					as.character(ifelse(!is.null(universe$modules[[modulenum]]), length(universe$modules[[modulenum]]), 0)))
			
			for (elementnum in seq_along(module)) {													# 3.02 secs
				element <- module[[elementnum]]
				
				for (tsnum in seq_along(element@timesteps)){
					tsCalDay <- element@timesteps[[tsnum]]$calday
					timeStepsDays <- rbind(timeStepsDays, c(tsCalDay, 0, NA, NA, NA))
					if (tsnum > 1) { # knife-edge activities can only be specified after a NULL or other action
						if (tsCalDay == lastCalDay) timeStepsDays[nrow(timeStepsDays),(2:5)] <- c(1, modulenum, elementnum, tsnum)
					}
					lastCalDay <- tsCalDay
				}
				
				# ######## TRY LAPPLY
				# #numTSs <- do.call("sum", lapply(module, FUN=function(ele) { return(length(ele@timesteps))}))
				# #modTSDays <- matrix(nrow=5, ncol=numTSs)
				# elementTSDays <- imapply(FUN=function(element, modnum, INDEX) {
					# #printSignature(element)
					# #tsDays <- NULL
					# tsDays <- matrix(ncol=5, nrow=length(element@timesteps), byrow=T)
					# #tsDays <- matrix(nrow=5, ncol=length(element@timesteps))
					# for (tsnum in seq_along(element@timesteps)) {
						# tsCalday <- element@timesteps[[tsnum]]$calday
						# tsDays[tsnum,] <- c(tsCalday, 0, NA, NA, NA)
						# #tsDays[,tsnum] <- c(tsCalday, 0, NA, NA, NA)
						# # knife-edge activities can only be specified after a NULL or other action
						# if (tsnum > 1 && tsCalday == lastCalDay) {
							# tsDays[tsnum,] <- c(tsCalday, 1, modnum, INDEX, tsnum)
							# #tsDays[,tsnum] <- c(tsCalday, 1, modnum, INDEX, tsnum)
						# }
						# lastCalDay <- tsCalday
					# }
					# #print(tsDays)
					# return(tsDays)
				# }, module, modnum=modulenum)
				# timeStepsDays <- rbind(timeStepsDays, do.call('rbind', elementTSDays))			# 3.39 secs
				# #modTSDays <- matrix(unlist(lapply(elementTSDays, "unmatrix")), ncol=5, byrow=T)  # 3.51 secs
				# #timeStepsDays <- rbind(timeStepsDays, modTSDays)									# 3.12 secs
			}
        }
		epocDebugMessage(universe, "Timestep Days matrix...")
		epocDebugMessage(universe, timeStepsDays)

        #  Establish time series of periods
        # subset the knifedge timesteps to retain which elements
        # and respective timesteps are knife-edge.
        # matrix declaration is to avoid a single row becoming a vector
        epocVerboseMessage(universe, "  establish time series of periods")
        tsKnifeEdges <- matrix(timeStepsDays[timeStepsDays[ ,2]==1, ], ncol=5)

        # determine unique timesteps (including knife-edge) and order them as periods
        timeStepsDays <- matrix(unique(timeStepsDays[ ,1:2]), ncol=2)
        if (nrow(timeStepsDays) > 1) timeStepsDays <- timeStepsDays[order(timeStepsDays[ ,1], timeStepsDays[ ,2]), ]

		# find number of periods in the year starting at 1 Jan and ending on 31 Dec
        .Object@periodN <- nrow(timeStepsDays)

        #  Calculate the proportion of the year taken up by each timestep, noting that the records in timeStepsDays reflect
        #   the end of the period at midnight of the nominated day prior to entering the next day
        # propYear : col 1 = day in year of timestep,
        #            col 2 = is it knife-edge (0 = no, 1 = yes) - will be 0 until knife edge added back
        #            col 3 = proportion of the year in that timestep
        propYear <- cbind(timeStepsDays,NA)
        if (.Object@periodN == 1) {
            propYear[1, 3] <- 1
        } else {
            pe <- c(2:.Object@periodN)
            propYear[1, 3] <- propYear[1, 1] / 365
            propYear[pe, 3] <- (propYear[pe, 1] - timeStepsDays[(pe - 1), 1]) / 365
        }

        # the first time step in each element may overlap the 1 January, requiring another period to finish off the year
        if (timeStepsDays[.Object@periodN, 1] != 365) {
            propYear <- rbind(propYear, c(365, 0, ((365 - timeStepsDays[.Object@periodN,1]) / 365)))
            .Object@periodN <- .Object@periodN + 1
        }

        # Loop through each ecosystem component to
        # create a list of each period in the calendar (between unique timesteps)
        #   In each period, have a matrix for each universe module (rows = elements
        #         col 1 = timestep for that component that is occurring in the calendar period
        #         col 2 = the proportion of the timestep that will be accounted for by the period.
        #         col 3 = the number (in sequence) of the period in the timestep
        # NB at present all components will be updated at each time step except for knife-edge timesteps
        # initialise calendar with periods
        epocVerboseMessage(universe, "  create periods")
        .Object@periods <- list()
        previousDay <- 0
        for (eTSD in 1:.Object@periodN) {
			.Object@periods[[eTSD]] <- new("Period", universe=universe, 
													 periodPropYear=propYear[eTSD, ],
													 periodKEData=matrix(tsKnifeEdges[tsKnifeEdges[ ,1]==propYear[eTSD, 1], ], ncol=5),
													 previousDay=previousDay,
													 periodNumber=eTSD)
            previousDay <- propYear[eTSD,1]
        }
		
		epocDebugMessage(universe, "Period list...")
		epocDebugMessage(universe, sapply(.Object@periods, "getPeriodInfo"))
		
		return(.Object)
	}
)
		
#C.Cal.elementTimeStepToPeriod.Prop.01<-function(ElementClass,ecData,Calendar){
# Method to estimate proportion of year carrying out functions of time step for each element
#         
# Parameters:
#  	.Object    		Calendar 	this calendar
#	universe		Universe 	the entire universe
# Return: 	Calendar
setGeneric(".setupPeriodTS", function(.Object, universe) standardGeneric(".setupPeriodTS"))
setMethod(".setupPeriodTS", signature(.Object="Calendar", universe="Universe"),
    function(.Object, universe) {
        if (is.null(.Object@periods)) return(invisible(.Object))
		
        # work through each module
		for (modulenum in seq_along(universe$modules)) {
			module <- universe$modules[[modulenum]]
            
			if(!is.null(module) && length(module) > 0) {  
				# and respective elements
				for (elementnum in seq_along(module)) {
					element <- module[[elementnum]]
					
					if (!is.null(element@timestepsN) && element@timestepsN > 0) {
						
						# Initialisation
						elementTSN <- element@timestepsN	# number of element time steps
						eTSDlast <- 0   					# the calendar day at the end of the last timestep
						currentTS <- 1 						# the current time step for the element
						currentTSLastDay <- element@timesteps[[currentTS]]$calday # the last day in the current time step
						periodStep <- 0 					# the number (in sequence) of the period within the timestep

						# estimate proportion of year carrying out functions of time step.  If the last time step is not on
						# the last day of the year (365) then time step corresponds to period to end the year plus the period
						# at the beginning
						currentTSYearProp <- (currentTSLastDay + 365 - element@timesteps[[elementTSN]]$calday)/365.0
						period1PropYear <- currentTSYearProp
						startFirstTSeTSD <- 0
						for (eTSD in seq_along(.Object@periods)) {
							period <- .Object@periods[[eTSD]]
							periodElemTSData <- period@modules[[modulenum]][elementnum, ]
							
							if (!period@knifeEdge) {

								Days <- period@day                	# PropYear[eTSD,1]
								propYear <- period@yearPropn     	#(Days-eTSDlast)/365
								periodStep <- periodStep + 1

								# if Days is greater than the current time step and there are more time steps
								if (Days > currentTSLastDay & currentTS < elementTSN) {
									  currentTS <- currentTS + 1
									  periodStep <- 1
									  tsOldDays <- currentTSLastDay
									  currentTSLastDay <- element@timesteps[[currentTS]]$calday
									  currentTSYearProp <- (element@timesteps[[currentTS]]$calday - tsOldDays)/365
								}

								# if Days is greater than the current time step but there are no more time steps
								# special case where the end of the year applies to the first time step
								if ((Days > currentTSLastDay)&(currentTS==elementTSN)) {
									currentTS <- 1
									currentTSLastDay <- 365.0
									currentTSYearProp <- period1PropYear
									startFirstTSeTSD <- eTSD
									periodStep <- 1
								}

								# Save the characteristics of the time step for that element
								periodElemTSData[1] <- currentTS                    # the number of the timestep for that element in the period
								periodElemTSData[2] <- propYear/currentTSYearProp  	# the proportion of the time step within the period
								periodElemTSData[3] <- periodStep                   # the number ofthe period in sequence within the timestep of that element
								eTSDlast <- Days
							} else {
								if(!is.na(periodElemTSData[1])) {
									currentTS <- periodElemTSData[1]
									periodStep <- 1
								} else {
									periodStep <- periodStep + 1
									periodElemTSData[1] <- currentTS    	# the number of the timestep for that element in the period
									periodElemTSData[2] <- NA            	# the proportion of the time step within the period
									periodElemTSData[3] <- periodStep   	# the number ofthe period in sequence within the timestep of that element
								}
							}
							
							# Update period
							.Object@periods[[eTSD]]@modules[[modulenum]][elementnum,] <- periodElemTSData
						}

						# in the case where the first timestep starts at the end of the year and is the first timestep to end in the new year
						# need to advance the Period.steps in the early part of the year to form a sequence from the latter part
						#  - add the last value used in "periodStep" to the values in the rest of the timestep in the early part of the year
						if (startFirstTSeTSD > 1) {
							for (eTSD in 1:(startFirstTSeTSD - 1)) {
								periodElemTSData <- .Object@periods[[eTSD]]@modules[[modulenum]][elementnum,]
								if (!is.na(periodElemTSData[1]) && periodElemTSData[1]==1) {
									.Object@periods[[eTSD]]@modules[[modulenum]][elementnum,3] <- periodElemTSData[3] + periodStep
								}
							}
						}
					}
				}
			}
		}
		
        return(invisible(.Object)) # return the calendar
    }
)

#C.Cal.tStep_to_period.01<-function (ElEnv,Module,Element,Universe,ElementID,ElementNames,Calendar)
# Function:           C.Cal.tStep_to_period.01
# Description:        Calendar - convert element time steps to periods
# Primary attributes:
#         
# Parameters:
#  	.Object    		Calendar 	this calendar
#	universe		Universe 	the entire universe
# Return: 	Universe
setGeneric(".convertTSToElementPeriods", function(.Object, universe) standardGeneric(".convertTSToElementPeriods"))
setMethod(".convertTSToElementPeriods", signature(.Object="Calendar", universe="Universe"),
    function(.Object, universe) {
        if (is.null(.Object@periods)) return(invisible(.Object))
		
		# work through each module
        moduleNames <- names(universe$modules)
        for (modulenum in seq_along(universe$modules)) {
			module <- universe$modules[[modulenum]]
            
			if(!is.null(module) && length(module) > 0) { 
				epocVerboseMessage(universe, "     Module ",as.character(modulenum),": ",moduleNames[[modulenum]])
				epocVerboseMessage(universe, "      elementsN = ",
                             as.character(ifelse(!is.null(universe$modules[[modulenum]])
                                                ,length(universe$modules[[modulenum]]),0)))
				elementNames <- names(module)
				# And respective elements
				for (elementnum in seq_along(module)) {
					element <- module[[elementnum]]
					
					if (!is.null(element@timestepsN) && element@timestepsN > 0) {
						epocVerboseMessage(universe, "      setting up element ", elementNames[[elementnum]])
						
						# Initialisation
						currentTS <- 0   		# current timestep number
						PtimeSteps <- NULL 		# list to replace timesteps in the [[Module]][[]] environment
						FirstPeriod <- TRUE  	# period 1 must be first period

						# loop through each period
						for (pe in seq_along(.Object@periods)) {
							period <- .Object@periods[[pe]]
							periodElemTSData <- period@modules[[modulenum]][elementnum,]
							
							#    accumulate actions for period in a list
							PtStepActions <- NULL
							PtStepActionN <- 0

							#    test whether this period has entered a new timestep
							#    if so change to timestep within period
							#    change flag to indicate first period in timestep
							if (periodElemTSData[1] != currentTS) {
								currentTS <- periodElemTSData[1]
								FirstPeriod <- TRUE
							}
							calday <- element@timesteps[[currentTS]]$calday
							
							#  if time in period for an action by this elementnum is NA then skip
							#  but include the period with 0 actions (a condition to be replaced when calendar modified)
							 ################################################
							if (!is.na(periodElemTSData[2])) {
								if (element@timesteps[[currentTS]]$actionsN > 0) {
									# test each action in active timestep and setup action in period according to the rules
									for (a in 1:element@timesteps[[currentTS]]$actionsN){

										action <- element@timesteps[[currentTS]]$actions[[a]]
										elementTSData <- period@modules[[modulenum]][elementnum,]
										# list within action
										#  list(fn               = E.O.Env.KPFM.annual_update.01
										#      ,tsType             = "AllPeriods","FirstPeriod","LastPeriod"
										#      ,tsTiming        = "Before","During","After"
										#      ,related.elemNums = NULL
										#      ,dSet             = NULL)

										# start series of action types - including looping through setup action types
										#if(sum(action$tsType==c("AllPeriods","FirstPeriod","LastPeriod"))>0){
										if (action$tsType %in% c("AllPeriods","FirstPeriod","LastPeriod")) {
											#########
											if (action$tsType == "AllPeriods"									# action occurs in every timestep
												|| (action$tsType == "FirstPeriod" & periodElemTSData[3] == 1) # action occurs in first period of timestep
												|| (action$tsType == "LastPeriod" & period@day == calday)) { 	# action occurs in last period of timestep

												PtStepActionN <- PtStepActionN + 1

												# copy all of timestep action to period action
												PtSA <- element@timesteps[[currentTS]]$actions[a]
												
												# transform if necessary
												if (!is.null(action$transAction)) {
													PtSA <- do.call(action$transAction$actionMethod, list(
																element,
																#.Object,
																period,
																PtSA,
																modulenum,
																elementnum,
																#currentTS,
																#a,
																#pe,
																FirstPeriod,
																action$transAction$dset)
															)
												}
												# add details to temporary list
												PtStepActions <- c(PtStepActions,PtSA)
											}
										} else { # type is non-standard
											if (sum(action$tsType == names(element$functions$setup))>0) {
											
												# Find out what sort of period timestep action this is
												PtSAtype <- element$functions$setup[[which(action$tsType==names(element$functions$setup))]]
												
												PtSA <- do.call(PtSAtype$actionMethod, list(
															element,
															#.Object,
															period,
															PtSA,
															modulenum,
															elementnum,
															#currentTS,
															#a,
															#pe,
															FirstPeriod,
															PtSAtype$dset)
														)
												PtStepActionN <- PtStepActionN + PtSA[[1]] # return 1 or 0 from Change
												PtStepActions <- c(PtStepActions, PtSA[2]) # list of list for action
											} else {
												epocErrorMessage(.Object, "Fatal error in setup - Action type is not defined", halt=TRUE)
											}
										}
									}
								}

								# reset main components of list including last day in period,
								# number of actions (set to 0 until actions added), list of actions (set to null until actions added)
								PtimeSteps <- c(PtimeSteps, list(list(calday = period@day,
																	actionsN = PtStepActionN,
																	 actions = PtStepActions)))
							}  else {
								PtimeSteps <- c(PtimeSteps, list(list(calday = period@day,
																	actionsN = 0,
																	 actions = NULL)))
							}
							 
							FirstPeriod <- FALSE
						}

						# add list back to element
						universe$modules[[modulenum]][[elementnum]]@timesteps <- PtimeSteps
						universe$modules[[modulenum]][[elementnum]]@timestepsN <- .Object@periodN
					}
				}
			}
		}
		
		return(universe)
    }
)

# Actions within period
# identify all actions within period for each element of each module
# sort into "Before", "During" and "After" period
#    These three divisions in the actions are for undertaking all Universe actions
#    required "Before" the period, then "During" the period, and then immediately after
#    the period.  Fore example, knife-edge actions can occur either at the end of a long period
#    or be specified to occur in a period of one day.
# Parameters:
#  	.Object    		Calendar 	this calendar
#	universe		Universe 	the entire universe
# Return: 	Calendar
setGeneric(".generateActionMatrix", function(.Object, universe) standardGeneric(".generateActionMatrix"))
setMethod(".generateActionMatrix", signature(.Object="Calendar", universe="Universe"),
    function(.Object, universe) {
		# first loop through all modules, elements and periods
        #   accumulate a full matrix - ActionMat
        #            Col  1  = module
        #            Col  2  = element
        #            Col  3  = period
        #            Col  4  = reference day in year
        #            Col  5  = action reference number in period (NA if no actions)
        #            Col  6  = number for "before =1", "during = 2", "after = 3" (NA if no actions
        actionMat <- NULL
		moduleNames <- names(universe$modules)
		
        for (modulenum in seq_along(universe$modules)) {
			module <- universe$modules[[modulenum]]
            epocVerboseMessage(universe, "     Module ",as.character(modulenum), ": ", moduleNames[modulenum])
            epocVerboseMessage(universe, "      elementsN = ", as.character(ifelse(!is.null(module), length(module), 0)))
			
			elementNames <- names(module)
			for (elementnum in seq_along(module)){
				element <- module[[elementnum]]
				epocVerboseMessage(universe, "          Element ",as.character(elementnum),": ",elementNames[elementnum])
				
				if (!is.null(element@timesteps)) {
					actionMat <- rbind(actionMat, .getElementActionMat(.Object, element, modulenum, elementnum))
				} else { # else if(!is.null(timesteps)
					actionMat <- rbind(actionMat,c(modulenum, elementnum, NA, NA, NA, NA))
				}
			}
        }

        # check to see if period has any actions remaining, if not then one or more elements
        # may have action set poorly specified either as the date required for action or
        # some timesteps with null steps
        # lastly, loop through calendar periods and add ActionMat for period
        twoPeriodsInOne <- FALSE # for testing the presence of a knife edge period in period
        for (periodnum in 1:.Object@periodN) {
			periodActionMat <- matrix(actionMat[actionMat[ ,4]==.Object@periods[[periodnum]]@day, ], ncol=6)

			# for knife edge periods (same day but knife edge is second period in day)
			if (!twoPeriodsInOne) {
				periodsInPeriod <- unique(periodActionMat[ ,3])
				if (length(periodsInPeriod) > 1) twoPeriodsInOne <- TRUE
				tpio <- 1
            } else {
				twoPeriodsInOne <- FALSE
				tpio <- 2
            }

			# now sort the actionMat according to the following sort
			#   4 - periods(caldays), 6 - timing, 1 - module, 2 - element, 3 - period in element, 5 - action
			if (nrow(periodActionMat) > 1) periodActionMat <- periodActionMat[order(periodActionMat[ ,4]
																				   ,periodActionMat[ ,6]
																				   ,periodActionMat[ ,1]
																				   ,periodActionMat[ ,2]
																				   ,periodActionMat[ ,3]
																				   ,periodActionMat[ ,5]), ]

			for (tm in 1:3) {
				pam <- matrix(periodActionMat[periodActionMat[ ,6] == tm & periodActionMat[ ,3] == periodsInPeriod[tpio], ],ncol=6)

				# during save, delete pam rows with action (col 5) = NA
				# matrix command ensures actionMat still is.matrix
				.Object@periods[[periodnum]]@periodActionMat[[tm]] <- matrix(pam[!is.na(pam[ ,5]), ],ncol=6)
            }
        }
		
		return(.Object)
	}
)

		
#C.Cal.ActionMat.01<-function (ElEnv,Module,Element)
# Function:           C.Cal.ActionMat.01
# Description:        generate action matrix
# Primary attributes:
#
# Parameters:    
#	.Object			Calendar	calendar object
#	element       	Element 	the specific element environment
#   modulelnum   	numeric		the number of the module
#   elementnum     	numeric 	the relative number of the taxon
#
# Return:  action matrix for element
#   accumulate a full matrix - ActionMat
#            Col  1  = module
#            Col  2  = element
#            Col  3  = period
#            Col  4  = reference day in year
#            Col  5  = action reference number in period (NA if no actions)
#            Col  6  = number for "before =1", "during = 2", "after = 3" (NA if no actions)
setGeneric(".getElementActionMat", function(.Object, element, modulenum, elementnum) standardGeneric(".getElementActionMat"))
setMethod(".getElementActionMat", signature(.Object="Calendar", element="Element", modulenum="numeric", elementnum="numeric"),
    function(.Object, element, modulenum, elementnum) {

        ElActionMat <- NULL
        if (!is.null(element@timestepsN) && element@timestepsN > 0) {
				
			# Preallocate matrix to speed up ?
			actionCnt <- sum(unlist(lapply(element@timesteps, function(x) length(x$actions))))	# total number of timestep actions
			emptyTimestepCnt <- sum(unlist(lapply(element@timesteps, function(x) length(x$actions)==0))==TRUE) # total number timesteps without actions
			ElActionMat <- matrix(ncol=6, nrow=emptyTimestepCnt + actionCnt, byrow=TRUE)
			
			rowNum <- 1
			for (tn in seq_along(element@timesteps)) {

				if(!is.null(element@timesteps[[tn]]$actions) && length(element@timesteps[[tn]]$actions) > 0) {
					for (a in seq_along(element@timesteps[[tn]]$actions)){
#cat("AM element:", element@signature@Name.short, names(element@timesteps)[[tn]], names(element@timesteps[[tn]]$actions)[[a]],"\n")
						timing <- which(element@timesteps[[tn]]$actions[[a]]$tsTiming == c("Before", "During", "After"))
						if (length(timing) <= 0) timing <- 2	# default
						ElActionMat[rowNum,] <- c(modulenum,elementnum,tn,element@timesteps[[tn]]$calday,a,timing)
						rowNum <- rowNum + 1
					}
				} else { 
					ElActionMat[rowNum,] <- c(modulenum,elementnum,tn,element@timesteps[[tn]]$calday,NA,NA)
					rowNum <- rowNum + 1
				} 
			}
		} else ElActionMat <- c(modulenum,elementnum,NA,NA,NA,NA)
		
        return(ElActionMat)
    }
)

################################################################################
# Calendar
#   It has the following format
#   list
#       Period 1  = see below
#       Period 2
#         .
#         .
#       Period n
#       Period.N = number of periods
#
#  In a period is the following list
#             Environment = NULL if no Environment in Universe
#                           else matrix - rows = 1 row per element
#                           col 1 = timestep for element that is occurring in the calendar period
#                           col 2 = the proportion of the timestep that will be accounted for by the period
#                           col 3 = the number (in sequence) of the period in the timestep
#             Biota       = as for Environment
#             Activity    = as for Environment
#             Management  = as for Environment
#             Output      = as for Environment
#             PeriodInfo  = list
#                           Number      = the number of the period
#                           Day         = the day in the year (as julian days)
#                           KnifeEdge   = Logical FALSE if not knife edge period (now redundant)
#                           YearPropn   = the proportion of the year taken up by period (0 if knife edge)
#                           PeriodStart = proportion of year passed since 0 Jan to beginning of time period
#                           PeriodEnd   = proportion of year passed since 0 Jan to end of period
#             PeriodActionMat   = list
#                           Before = matrix - rows of references to actions to be taken in period
#                                         ordered by Timing,Module,Element,ActionNo
#                                         Col  1  = module
#                                         Col  2  = element
#                                         Col  3  = period within Element
#                                         Col  4  = reference day in year
#                                         Col  5  = action reference number in period (NA if no actions)
#                                         Col  6  = number for "before =1", "during = 2", "after = 3" (NA if no actions)
#                           During = as Before
#                           After  = as Before (actions for after all element states have been updated)


# NOTE - if PeriodActionMat[[n]] has no values then it will be an empty matrix with nrow=0

# Output a textual representation of the calendar.  Defaults to file but with
# tofile=FALSE will output to stdout
# Parameters:
#	.Object		Calendar	calendar object
#	universe	Universe	Universe object to consult
#	tofile		logical		Print to file? (default TRUE after considering universe data input)
setGeneric("printCalendar", function(.Object, universe, ...) standardGeneric("printCalendar"))
setMethod("printCalendar", signature(.Object="Calendar", universe="Universe"),
    function(.Object, universe, tofile=NULL) {
        if (is.null(.Object@periods)) return(invisible())
		
		# Use tofile param, else universe input data, else false
        if (missing(tofile) || is.null(tofile)) {
			tofile <- universe@report$Diagnostics$Calendar$ToFile
			if (is.null(tofile)) tofile <- TRUE
		}
        
        # output Calendar to file
		universeModuleNames <- names(universe$modules)
		fileConn <- ""
		if (tofile) {
			fileName <- universe@report$Diagnostics$Calendar$Filename
			if (is.null(fileName)) fileName <- "Calendar.txt"
			filePath <- getRuntimePath(universe, fileName)
			fileConn <- file(description = filePath, open = "w")
		}
		
		# demarcation lines
		hl <- getReport(universe, "HeadingLines")
		Line_Heading1<-hl$Heading1
		Line_Heading2<-hl$Heading2
		Line_Heading3<-hl$Heading3

		# General output.  Calendar is based on first scenario (hardcoded)
		cat("Calendar - ", getSignature(universe@scenarios[[1]], "Name.full")
						,"\n",sep="",file=fileConn)
		cat("Number of Periods = ",.Object@periodN
						,"\n",sep="",file=fileConn)
		cat("\n"
						,"\n",sep="",file=fileConn)

		for (pe in 1:.Object@periodN) {
			cat(Line_Heading1, "\n",sep="",file=fileConn)
			cat("Period ", pe, "\n",sep="",file=fileConn)
			cat(Line_Heading1, "\n",sep="",file=fileConn)
			cat("", "\n",sep="",file=fileConn)
			cat("Period Number                   = ",.Object@periods[[pe]]@number
							,"\n",sep="",file=fileConn)
			cat("Last julian day of period       = ",.Object@periods[[pe]]@day
							,"\n",sep="",file=fileConn)
			cat("Knife-edge period (logical)     = ",.Object@periods[[pe]]@knifeEdge
							,"\n",sep="",file=fileConn)
			cat("Proportion of Year              = ",.Object@periods[[pe]]@yearPropn
							,"\n",sep="",file=fileConn)
			cat("Start of period (propn of year) = ",.Object@periods[[pe]]@periodStart
							,"\n",sep="",file=fileConn)
			cat("End of period (propn of year)   = ",.Object@periods[[pe]]@periodEnd
							,"\n",sep="",file=fileConn)
			cat(""
							,"\n",sep="",file=fileConn)

			cat(Line_Heading2
							,"\n",sep="",file=fileConn)
			cat("Check Timestep to Period conversion in each module"
							,"\n",sep="",file=fileConn)
			cat(Line_Heading2
							,"\n",sep="",file=fileConn)
			cat(""
							,"\n",sep="",file=fileConn)

			PeriodModuleNames <- names(.Object@periods[[pe]]@modules)

			for (ec in seq_along(universe$modules)) { # loop through modules
				if (ec <= length(.Object@periods[[pe]]@modules)) {
					if (ec>1) cat(Line_Heading3, "\n",sep="",file=fileConn)

					cat("Module ",PeriodModuleNames[ec]
								,"\n",sep="",file=fileConn)
					cat(""
								,"\n",sep="",file=fileConn)

					if(!is.null(.Object@periods[[pe]]@modules[[ec]]) && length(.Object@periods[[pe]]@modules[[ec]]) > 0) {
						ElementNames<-names(universe$modules[[ec]])
						cat("     Check - Module in Universe is  = ",universeModuleNames[ec]
								,"\n",sep="",file=fileConn)
						cat(""
								,"\n",sep="",file=fileConn)
						cat("     Element              Timestep  PerInTstep  PropOfTstep"
								,"\n",sep="",file=fileConn)
						cat(""
								,"\n",sep="",file=fileConn)

						DatMat<-.Object@periods[[pe]]@modules[[ec]]

						for (e in 1:nrow(.Object@periods[[pe]]@modules[[ec]])){
							cat("     "
								,fixedFieldLength(ElementNames[e],width=20)
								," "
								,fixedFieldLength(DatMat[e,1],width=5,sig=5)
								,"     "
								,fixedFieldLength(DatMat[e,3],width=6,sig=5)
								,"      "
								,fixedFieldLength(DatMat[e,2],width=9,sig=5,dec=3)
								,"\n",sep="",file=fileConn)
						}
					} else {
						cat("     Module not present in this period"
								,"\n",sep="",file=fileConn)
					}
					
					cat(""       ,"\n",sep="",file=fileConn)
				}
			}

			cat(Line_Heading2               ,"\n",sep="",file=fileConn)
			cat("Action matrix for period"  ,"\n",sep="",file=fileConn)
			cat(Line_Heading2               ,"\n",sep="",file=fileConn)
			cat(""                          ,"\n",sep="",file=fileConn)

			TimingName<-c("Before","During","After")
			cat("Module       ElementName          ElNum ElPer PerLastDay Action" # spacings of 2
							,"\n",sep="",file=fileConn)
			for (tm in 1:3){
				cat(""              ,"\n",sep="",file=fileConn)
				ActionMat <- .Object@periods[[pe]]@periodActionMat[[tm]]
				cat(TimingName[tm]  ,"\n",sep="",file=fileConn)
				cat(""              ,"\n",sep="",file=fileConn)

				if(nrow(ActionMat)>0){
					for (am in 1:nrow(ActionMat)){
						cat(
							fixedFieldLength(universeModuleNames[ActionMat[am,1]],width=12)
							," "
							,fixedFieldLength(names(universe$modules[[ActionMat[am,1]]])[ActionMat[am,2]],width=20)
							," "
							,fixedFieldLength(ActionMat[am,2],width=5,sig=5)
							," "
							,fixedFieldLength(ActionMat[am,3],width=5,sig=5)
							," "
							,fixedFieldLength(ActionMat[am,4],width=6,sig=3)
							,"     "
							,fixedFieldLength(names(universe$modules[[ActionMat[am,1]]][[ActionMat[am,2]]]@timesteps[[ActionMat[am,3]]]$actions)[ActionMat[am,5]],width=15)
									,"\n",sep="",file=fileConn)
					}
				} else { 
					cat(
					   fixedFieldLength("NA",width=12)
					   ," "
					   ,fixedFieldLength("NA",width=20)
					   ," "
					   ,fixedFieldLength(NA,width=5,sig=5)
					   ," "
					   ,fixedFieldLength(NA,width=5,sig=5)
					   ," "
					   ,fixedFieldLength(NA,width=6,sig=3)
					   ,"     "
					   ,fixedFieldLength("NA",width=15)
								,"\n",sep="",file=fileConn)
				}
			}

			cat("","\n",sep="",file=fileConn)
        }
		if (tofile) close(fileConn)
    }
)

# Return the calendar period object specified by periodNum
# Parameters:
#	periodNum	numeric		Number of the period to return
setGeneric("getPeriod", function(.Object, periodNum) standardGeneric("getPeriod"))
setMethod("getPeriod", signature(.Object="Calendar", periodNum="numeric"),
    function(.Object, periodNum) {
        if (is.null(.Object@periods[[periodNum]])) return(invisible())
        return(.Object@periods[[periodNum]])
    }
)

# Return a specified period information as a named list
# Parameters:
#	periodNum	numeric		Number of the period to return info on
setGeneric("getInfoForPeriod", function(.Object, periodNum) standardGeneric("getInfoForPeriod"))
setMethod("getInfoForPeriod", signature(.Object="Calendar", periodNum="numeric"),
    function(.Object, periodNum) {
		if (is.null(.Object@periods[[periodNum]])) {
			epocErrorMessage(.Object, "Trying to access period info for an unknown period = ", periodNum, halt=TRUE)
		} else {
			return(getPeriodInfo(.Object@periods[[periodNum]]))
		}
	}
)

# Return an action matrix for the period specified
# Parameters:
#	periodNum	numeric		Number of the period to return info on
setGeneric("getActionMatForPeriod", function(.Object, periodNum) standardGeneric("getActionMatForPeriod"))
setMethod("getActionMatForPeriod", signature(.Object="Calendar", periodNum="numeric"),
    function(.Object, periodNum) {
		if (is.null(.Object@periods[[periodNum]])) {
			epocErrorMessage(.Object, "Trying to access action matrix for an unknown period = ", periodNum, halt=TRUE)
		} else {
			return(getPeriodActionMat(.Object@periods[[periodNum]]))  
		}
	}
)

