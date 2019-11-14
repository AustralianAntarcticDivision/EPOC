################################################################################
# EPOC Controller class
#
# Main EPOC engine class used to run the simulation based on the universe passed in the
# constructor.  It builds a calendar from the universe data which is used to structure
# the action matrix for the Scenario simulation.
#
# S4
# Created 10/2/2009 Troy Robertson
################################################################################

# Define Controller class with data members (slots)
setClass("Controller",
    representation(
        universe    = "Universe",			# Current universe object
        calendar    = "Calendar",			# Current calendar built from universe
		epocdebug 	= "list"				# Debug break points
    ),
	prototype(
		universe	= NULL,
		calendar	= NULL,
		epocdebug 	= list()
	),
	contains		= "EPOCObject"
)

# Constructor for an EPOC Controller object
# This will automatically create a calendar from the universe data and then setup
# the universe.
# Parameters:
# 	universe		Universe		instantiated universe object upon which to act
# 	outputcalendar	logical			should calendar be output to screen/file (default=TRUE)
#	tofile			logical			if calendar to be output, screen or file (default=TRUE)
#	msglevel		character		notification/output level c("quiet", "normal", "verbose", "debug")
#	loglevel		character		log output level c("quiet", "normal", "verbose", "debug")
#	logfile			character		log file name (defaults to EPOC.log)
# 	logtrunc		logical			truncate file before starting log
setMethod("initialize", "Controller",
    function(.Object, universe=NULL, outputcalendar=TRUE, tofile=NULL, msglevel=getOption("epocmsglevel"), 
					loglevel=getOption("epocloglevel"), logfile=NULL, logtrunc=NULL, ..., .xData=new.env(parent=emptyenv())) {
		signature <- list(
			ClassName = "Controller",
			ID = 11001,
			ID.absolute = 11001,
			Name.full = "EPOC - Ecosystem Productivity Ocean and Climate - Coupled Ocean Ecology Model System",
			Name.short = "EPOC",
			Revision = "0.5.1",
			Authors = "A.Constable, T.Robertson",
			Last.edit = "25 May 2012"  #"27 September 2010"   #"2 June 2008"
		)
		.Object@signature <- setSignature(.Object@signature, signature)
		
		# Need to set any explicit msg/log settings on universe
		if (is.null(universe)) {
			.Object@universe <- new("Universe", msglevel, loglevel, logfile, logtrunc)
		} else {
			.Object@universe <- .resetReporting(universe, msglevel, loglevel, logfile, logtrunc)
		}
		
		epocMessage(.Object@universe, "Starting EPOC Controller...")
		epocMessage(.Object@universe, getSignatureMulti(.Object))
		
		# establish universe and scenario conditions - inputs
		if (!universe@created) .Object@universe <- createUniverse(.Object@universe)

		# establish an annual calendar of periods in which actions take place
		calendar <- new("Calendar")
		
		# hack to return two objects (as a list)
		combinedObjects <- createCalendar(calendar, .Object@universe)
		.Object@calendar <- combinedObjects[[1]]
		.Object@universe <- combinedObjects[[2]]
		epocVerboseMessage(.Object@universe, "Configured Calendar")
		
		# store period details in Universe$Config$Params
		.Object <- .addPeriodPropsToConfigParams(.Object)
		
		# setup the universe, including replacing ID numbers with references to elements in the universe
		.Object@universe <- .setupUniverse(.Object@universe)
		epocVerboseMessage(.Object@universe, "Configured relative universe according to calendar")
		
		callNextMethod(.Object)
		
		# output calendar
		if (outputcalendar) outputCalendar(.Object, tofile)
		
        return(.Object)
    }
)

# Display the calendar associated with this universe to file by default
# If tofile=TRUE then the calendar will be output to screen
# Parameter:
#	tofile		logical		Should the calendar be output to screen or file(default)
setGeneric("outputCalendar", function(.Object, ...) standardGeneric("outputCalendar"))
setMethod("outputCalendar", signature(.Object="Controller"),
    function(.Object, tofile=NULL) {
		if (class(.Object@universe) != "Universe" || class(.Object@calendar) != "Calendar") {
			epocMessage(.Object@universe, "A valid universe must be supplied to controller before a calendar can be built!")
			return()
		}
		
        epocVerboseMessage(.Object@universe, "Outputing Calendar...")
        printCalendar(.Object@calendar, .Object@universe, tofile)
    }
)

# Serialise the live controller and its associated universe and calendar objects to file
# Parameters:
#	scenarioName	character	name of serialised file
#	includeCM		logical		should source code be serialised with objects
setGeneric("serialiseSimulation", function(.Object, scenarioName, ...) standardGeneric("serialiseSimulation"))
setMethod("serialiseSimulation", signature(.Object="Controller", scenarioName="character"),
    function(.Object, scenarioName, includeCM=FALSE) {
		# Create scenarios directory
		if (!file.exists(file.path(getwd(), "scenarios"))) dir.create(file.path(getwd(), "scenarios"))
		
        conn <- file(file.path(getwd(), "scenarios", paste(scenarioName, ".epo", sep="")), "w")
		epocVerboseMessage(.Object@universe, "Serialising Universe...")
        serialize(.Object, conn)
        close(conn)
    }
)

# Unserialise the live controller and its associated universe and calendar (and possibly source code) from filename passed
# Parameters:
#	scenarioName	character	filename of serialised file (in getwd())
setGeneric("unserialiseSimulation", function(.Object, scenarioName, ...) standardGeneric("unserialiseSimulation"))
setMethod("unserialiseSimulation", signature(.Object="Controller", scenarioName="character"),
    function(.Object, scenarioName, resourceCM=FALSE) {
        conn <- file(file.path(getwd(), "scenarios", paste(scenarioName, ".epo", sep="")), "r")
      
		epocVerboseMessage(.Object@universe, "Unserialising Universe...")
		controller <- unserialize(conn)
		
		if (resourceCM) {
			# Source all missing classes
			reSourceElementClasses(.Object@universe)
			# Source all missing element methods
			reSourceElementMethods(.Object@universe)
		}
		close(conn)
		
        return(controller)
    }
)

# Return the calendar object held by the controller
setGeneric("getCalendar", function(.Object) standardGeneric("getCalendar"))
setMethod("getCalendar", signature(.Object="Controller"),
    function(.Object) return (.Object@calendar)
)

# Return the universe object held by the controller
setGeneric("getUniverse", function(.Object) standardGeneric("getUniverse"))
setMethod("getUniverse", signature(.Object="Controller"),
    function(.Object) return (.Object@universe)
)

# Close all open data files held by each element
# Each Element opens its own data file during initialisation.  Controller is responsible for closing them all
setGeneric("closeDatafiles", function(.Object) standardGeneric("closeDatafiles"))
setMethod("closeDatafiles", signature(.Object="Controller"),
    function(.Object) {
		epocVerboseMessage(.Object@universe, "Closing data files...")
		for (ec in seq_along(.Object@universe$modules)){
			for (e in seq_along(.Object@universe$modules[[ec]])){
				closeFileConnection(.Object@universe$modules[[ec]][[e]])
			}
		}
		closeFileConnection(.Object@universe)
		
		return(.Object)
	}
)

# Wrapper to run the simulation using the universe passed at instantiation, and show timing
# Parameters:
#	epocdebug	character	break simulation at the end of one of one or more of 
#							c("pre_action", "post_action", "period", "year", "scenario")
#							or the break at a particular value c(post_action="migrate", year=1950)
# 	timer		logical		should timing be printed to screen (default FALSE)
#	forceGC		logical		should a garbage collection event be forced before running the simulation
setGeneric("runSimulation", function(.Object, ...) standardGeneric("runSimulation"))
setMethod("runSimulation", signature(.Object="Controller"),
    function(.Object, epocdebug=NULL, forceGC=FALSE, timer=FALSE, ...) {
	
		# Restart logging
		.Object@universe <- .resetReporting(.Object@universe, ...)
		epocMessage(.Object@universe, "Starting simulation...")
		
		if (timer) {
			timing <- system.time(controller <- .runSimulation(.Object, epocdebug=epocdebug, forceGC=forceGC, ...), gcFirst=F)
			epocMessage(controller@universe, "Ending Simulation!")
			epocMessage(.Object@universe, "")
			epocMessage(.Object@universe, "   User: ", timing[1][[1]], " sec")
			epocMessage(.Object@universe, " System: ", timing[2][[1]], " sec")
			epocMessage(.Object@universe, "Elapsed: ", timing[3][[1]], " sec")
		} else {
			controller <- .runSimulation(.Object, epocdebug=epocdebug, forceGC=forceGC, ...)
			epocMessage(controller@universe, "Ending Simulation!")
		}
		epocMessage(.Object@universe, "")
		
		# Close all data file connections and log file connection
		controller <- closeDatafiles(controller)
		
		return(invisible(controller))
	}
)

# Private simulation method called by wrapper
setGeneric(".runSimulation", function(.Object, ...) standardGeneric(".runSimulation"))
setMethod(".runSimulation", signature(.Object="Controller"),
    function(.Object, epocdebug=NULL, forceGC=FALSE) {
		
		if (!missing(epocdebug) && !is.null(epocdebug)) {
			.Object@epocdebug <- as.list(epocdebug)
		} else {
			.Object@epocdebug <- .Object@universe@report$Diagnostics$Debug
		}
		
		# Force a garbage collection event before starting
		if (forceGC) gc()
		
        for (sc in seq_along(.Object@universe@scenarios)) {
			
			epocMessage(.Object@universe, "Initialising Scenario ", sc, "...") 
            .Object@universe$realtimeState$currentScenario <- sc
			.Object@universe$realtimeState$finalPeriod <- FALSE
            .Object@universe <- initialiseElementScenarios(.Object@universe@scenarios[[sc]], .Object@universe)
            .Object@universe <- initialiseElementTransitions(.Object@universe@scenarios[[sc]], .Object@universe)  # setup transition environments
			epocMessage(.Object@universe, "Commencing Scenario ", sc, "...")
			
			for (rp in 1:.Object@universe@scenarios[[sc]]@replicateCnt) {
				.Object@universe$realtimeState$currentReplicate <- rp
				.Object@universe <- initialiseElementReplicate(.Object@universe@scenarios[[sc]], .Object@universe)
				epocMessage(.Object@universe, "Replicate ", rp, "...\n")
				
				for (yr in 1:.Object@universe@scenarios[[sc]]@yearsN) {
					
					.Object@universe$realtimeState$relativeYear <- yr
					.Object@universe$realtimeState$currentYear <- .Object@universe@scenarios[[sc]]@yearStart + yr - 1
					
					for (pe in seq_along(.Object@calendar@periods)) {
						#if (recommence && epocdebug == "period" && sc <= .Object@universe$realtimeState$currentScenario) next
						epocMessage(.Object@universe, "Year ",as.character(.Object@universe$realtimeState$currentYear),
									" : Period ",as.character(pe))
						.Object@universe$realtimeState$currentPeriod <- pe
						.Object@universe$realtimeState$currentPeriodInfo <- getInfoForPeriod(.Object@calendar, pe)
						.Object@universe$realtimeState$finalPeriod <- (yr == .Object@universe@scenarios[[sc]]@yearsN && pe == length(.Object@calendar@periods))
						.Object <- .projectPeriod(.Object, pe)
						.breakout(.Object, "period")
					}
					.breakout(.Object, "year")
					.Object@universe$realtimeState["currentAction"] <- list(NULL)
					.Object@universe$realtimeState["currentPeriodInfo"] <- list(NULL) 
				}
				epocMessage(.Object@universe, "")
			}
            epocMessage(.Object@universe, "Completed Scenario ", sc)
			.breakout(.Object, "scenario")
        }
			
		return(invisible(.Object))
    }
)

# Run methods from the action matrix for the period
# C.PJ.Period.01<-function (Universe, PeriodInfo, PeriodActionMat)
# PRIVATE method
# Parameters:
#	periodNum	numeric		period number to project
setGeneric(".projectPeriod", function(.Object, periodNum) standardGeneric(".projectPeriod"))
setMethod(".projectPeriod", signature(.Object="Controller", periodNum="integer"),
    function(.Object, periodNum) {
		# Function:           C.PJ.Period.01
		# Description:        Projection over one period of the calendar
		# Primary attributes:

		# Input parameters:   Universe - list including one or more of
		#                               (Environment, Biota, Activity, Management, Output, Presentation)
		#                     PeriodInfo - the list of properties of the period in the Calendar
		#                           Number      = the number of the period
		#                           Day         = the day in the year (as julian days)
		#                           KnifeEdge   = Logical FALSE if not knife edge period (now redundant)
		#                           YearPropn   = the proportion of the year taken up by period (0 if knife edge)
		#                           PeriodStart = proportion of year passed since 0 Jan to beginning of time period
		#                           PeriodEnd   = proportion of year passed since 0 Jan to end of period

		#                      PeriodActionMat   = rows of references to actions to be taken in period
		#                                         ordered by Timing,Module,Element,ActionNo
		#                                         Col  1  = module
		#                                         Col  2  = element
		#                                         Col  3  = period within Element
		#                                         Col  4  = reference day in year
		#                                         Col  5  = action reference number in period (NA if no actions)
		#                                         Col  6  = number for "before =1", "during = 2", "after = 3" (NA if no actions)

		# Returned            List of "component$State" in each main category

		########################################################
		#      Signature <- list(
		#        ID           =  11010,
		#        Name.full    = "Projection over period",
		#        Name.short   = "PJ_Period01",
		#        Version      = "01",
		#        Authors      = "A.Constable",
		#        last.edit    = "15 April 2005"
		#        ) # end Signature
		########################################################

		# TR Some assignments to replace parameters to old function
		periodInfo <- getInfoForPeriod(.Object@calendar, periodNum)
		periodActionMat <- getActionMatForPeriod(.Object@calendar, periodNum)
        #  1  = module
        #  2  = element
        #  3  = period within Element
        #  4  = reference day in year (error checking only - refer to periodInfo)
        #  5  = action reference number in period
        #  6  = number for "before =1", "during = 2", "after = 3"  (error checking only - refer to periodInfo)
		
		############################################################################
		# Actions before period
		epocDebugMessage(.Object@universe, "   Before period...")
		.doCallTimingActions(.Object, periodActionMat$Before)

		############################################################################
		# Actions During period
		epocDebugMessage(.Object@universe, "   During period...")
		.doCallTimingActions(.Object, periodActionMat$During)

		############################################################################
		# Update all States in Universe
		# loop through all elements to update their states from $transition
		# check transition matrices and handle errors
		epocDebugMessage(.Object@universe, "   Updating States...")
		for (ec in seq_along(.Object@universe$modules)){
			for (e in seq_along(.Object@universe$modules[[ec]])){
				if (!is.null(.Object@universe$modules[[ec]][[e]]$transition)) {
					if (doUpdate(.Object@universe$modules[[ec]][[e]])) {
						# call element function and return
						if (hasMethod("updateState", signature(class(.Object@universe$modules[[ec]][[e]])[[1]], "Universe"))) {
							.Object@universe$realtimeState$currentMethod <- "updateState"
							.breakout(.Object, "pre_action")
							epocDebugMessage(.Object@universe, "\t--> ", .Object@universe$realtimeState$currentMethod, "\t(", getSignatureLine(.Object@universe$modules[[ec]][[e]]), ")")
							updateState(.Object@universe$modules[[ec]][[e]], .Object@universe)
							.breakout(.Object, "post_action")
						}
					}
				} 
			} 
		} 

		############################################################################
		# Actions After all states have been updated
		############################################################################
		epocDebugMessage(.Object@universe, "   After period...")
		.doCallTimingActions(.Object, periodActionMat$After)
		
		############################################################################
		# Print all States in Universe
		############################################################################
		# loop through all elements to print their states to file
		epocDebugMessage(.Object@universe, "   Printing States...")
		for (ec in seq_along(.Object@universe$modules)){
			for (e in seq_along(.Object@universe$modules[[ec]])){
				if (doPrint(.Object@universe$modules[[ec]][[e]]) ||
						(doPrintFinal(.Object@universe$modules[[ec]][[e]]) && .Object@universe$realtimeState$finalPeriod)) {
					# call printState method 
					if (hasMethod("printState", signature(class(.Object@universe$modules[[ec]][[e]])[[1]], "Universe"))) {
						.Object@universe$realtimeState$currentMethod <- "printState"
						.breakout(.Object, "pre_action")
						epocDebugMessage(.Object@universe, "\t--> ", .Object@universe$realtimeState$currentMethod, "\t(", getSignatureLine(.Object@universe$modules[[ec]][[e]]), ")")
						printState(.Object@universe$modules[[ec]][[e]], .Object@universe)
						.breakout(.Object, "post_action")
					}
				} 
			} 
		} 
		
		return(.Object)
	} 
)

# Show break messages and call recover
# PRIVATE method
# Parameters:
#	timeActionMat	matrix		period step timing action matrix
setGeneric(".doCallTimingActions", function(.Object, timeActionMat) standardGeneric(".doCallTimingActions"))
setMethod(".doCallTimingActions", signature(.Object="Controller", timeActionMat="matrix"),
    function(.Object, timeActionMat) {
		if (nrow(timeActionMat)>0) {
			for (am in 1:nrow(timeActionMat)){
				tam <- timeActionMat[am,]
				.Object@universe$realtimeState$currentAction <- timeActionMat[am,]
				.Object@universe$realtimeState$currentMethod <- .Object@universe$modules[[tam[1]]][[tam[2]]]@timesteps[[tam[3]]]$actions[[tam[5]]]$actionMethod
				filePath <- .Object@universe$modules[[tam[1]]][[tam[2]]]@timesteps[[tam[3]]]$actions[[tam[5]]]$actionFile
				
				.breakout(.Object, "pre_action")
				epocDebugMessage(.Object@universe, "\t--> ", .Object@universe$realtimeState$currentMethod, "\t(", getSignatureLine(.Object@universe$modules[[tam[1]]][[tam[2]]]), ")")
				# Check if R extension to path
				if (toupper(substring(filePath, nchar(filePath)-1) == ".R")) {
					# Local method call
					do.call(.Object@universe$realtimeState$currentMethod,
							list(.Object@universe$modules[[tam[1]]][[tam[2]]],# Element object
							.Object@universe)  								  # access to universe if needed
					)
				} else {
					# Assume external library call
					libName <- sub("\\.(dll|DLL|so|SO|sl|SL)$", "", basename(filePath))
					.Call(.Object@universe$realtimeState$currentMethod, package=libName)
				}
				.breakout(.Object, "post_action")
			} 
		}
	}
)
		
# Show break messages and call recover
# PRIVATE method
# Parameters:
#	steptype	character		type of debug break to check for
setGeneric(".breakout", function(controller, steptype) standardGeneric(".breakout"))
setMethod(".breakout", signature(controller="Controller", steptype="character"),
    function(controller, steptype) {
		stepvalue <- NULL
		# Check whether we need to break for this steptype
		if (steptype %in% attributes(controller@epocdebug)[["names"]]) {
			stepvalue <- controller@epocdebug[[steptype]]
		} else if (!(steptype %in% controller@epocdebug)) return()
		
		# Get the current rtstate values
		currentMethod <- controller@universe$realtimeState$currentMethod
		currentPeriod <- controller@universe$realtimeState$currentPeriod
		currentYear <- controller@universe$realtimeState$currentYear
		currentScenario <- controller@universe$realtimeState$currentScenario
		actionmat <- controller@universe$realtimeState$currentAction
		element <- controller@universe$modules[[actionmat[1]]][[actionmat[2]]]
		universe <- controller@universe
		
		if (length(stepvalue) > 0) {
			# Check whether we need to break for the epocdebug value(s) set
			baa1 <- ifelse("pre_action" %in% attributes(controller@epocdebug)[["names"]], controller@epocdebug[["pre_action"]], "")
			if (!is.null(baa1) && baa1 > 0 && baa1 != currentMethod) return()
			if (steptype == "pre_action" && stepvalue != currentMethod) return()
			baa2 <- ifelse("post_action" %in% attributes(controller@epocdebug)[["names"]], controller@epocdebug[["post_action"]], "")
			if (!is.null(baa2) && baa2 > 0 && baa2 != currentMethod) return()
			if (steptype == "post_action" && stepvalue != currentMethod) return()
			bap <- ifelse("period" %in% attributes(controller@epocdebug)[["names"]], controller@epocdebug[["period"]], "")
			if (!is.null(bap) && bap > 0 && bap != currentPeriod) return()
			if (steptype == "period" && stepvalue != currentPeriod) return()
			bay <- ifelse("year" %in% attributes(controller@epocdebug)[["names"]], controller@epocdebug[["year"]], "")
			if (!is.null(bay) && bay > 0 && bay != currentYear) return()
			if (steptype == "year" && stepvalue != currentYear) return()
			bat <- ifelse("scenario" %in% attributes(controller@epocdebug)[["names"]], controller@epocdebug[["scenario"]], "")
			if (!is.null(bat) && bat > 0 && bat != currentSenario) return()
			if (steptype == "scenario" && stepvalue != currentScenario) return()
		}
		
		rm(baa1)
		rm(baa2)
		rm(bap)
		rm(bay)
		rm(bat)
		msg <- "\nBreaking simulation"
		if (steptype != "pre_action") msg <- paste(msg, "after")
		if (steptype == "pre_action") msg <- paste(msg, "before Action:", currentMethod)
		if (steptype == "post_action") msg <- paste(msg, "Action:", currentMethod)
		if (steptype %in% c("pre_action", "post_action", "period")) msg <- paste(msg, "Period:", currentPeriod)
		if (steptype %in% c("pre_action", "post_action", "period", "year")) msg <- paste(msg, "Year:", currentYear) 
		if (steptype %in% c("pre_action", "post_action", "period", "year", "scenario")) msg <- paste(msg, "Scenario:", currentScenario)
		epocMessage(controller@universe, msg)
		epocMessage(controller@universe, "To enter interactive Browse mode, select the last frame number, otherwise 0 to continue!")
		epocMessage(controller@universe, "In Browse mode, type ls() to list objects available for investigation using EPOC API methods.")
		rm(msg)
		recover()
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
#  In a period is a list for each module, an action matrix and the period details, which has
#             PeriodInfo  = list
#                           Number      = the number of the period
#                           Day         = the day in the year (as julian days)
#                           KnifeEdge   = Logical FALSE if not knife edge period (now redundant)
#                           YearPropn   = the proportion of the year taken up by period (0 if knife edge)
#                           PeriodStart = proportion of year passed since 0 Jan to beginning of time period
#                           PeriodEnd   = proportion of year passed since 0 Jan to end of period
########################################################
# Create S4 method '.addPeriodPropsToConfigParams'
setGeneric(".addPeriodPropsToConfigParams", function(.Object) standardGeneric(".addPeriodPropsToConfigParams"))
setMethod(".addPeriodPropsToConfigParams", signature(.Object="Controller"),
    function(.Object) {
		if (class(.Object@universe) != "Universe" || class(.Object@calendar) != "Calendar") return(.Object)
        if (is.null(.Object@calendar@periods)) return(.Object)

        periodPropsInYear <- NULL
        periodStartPoints <- NULL
        periodEndPoints <- NULL
        for (pe in 1:.Object@calendar@periodN) {
            periodPropsInYear <- c(periodPropsInYear, .Object@calendar@periods[[pe]]@yearPropn)
            periodStartPoints <- c(periodStartPoints, .Object@calendar@periods[[pe]]@periodStart)
            periodEndPoints <- c(periodEndPoints, .Object@calendar@periods[[pe]]@periodEnd)
        }
        periodInfo <- list(PropInYear  = periodPropsInYear,
                         StartPoint  = periodStartPoints,
                         EndPoint    = periodEndPoints)
		epocDebugMessage(.Object, "Adding PeriodInfo to universe attributes...")
		epocDebugMessage(.Object, periodInfo)
		.Object@universe@epocAttributes$PeriodInfo <- periodInfo
		
        return(.Object)
    }
)
