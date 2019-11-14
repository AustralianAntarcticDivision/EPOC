################################################################################
# EPOC Universe class
#
# Representation of the universe with its associated module elements, scenario information,
# spatial/polygon information and reporting formatting.
# Universe input data is parsed from universe.data file found at the path passed to constructor.
#
# S4
# Created 5/2/2009 Troy Robertson
# Modified 20/3/2012 - TR
################################################################################

# Define Universe class with data members (slots)
setClass("Universe",
	# All these slots are unchanged by action methods during simulation
    representation(
		spatial			= "Spatial",	# Spatial object used to store polygon info
        scenarios		= "list",		# List of scenario objects
		report			= "list",		# Reporting format information
        inputPaths  	= "list",		# paths to all element class source files and input data files
        baseDirectory	= "character",	# Current working directory
		created			= "logical"),	# Has universe already been created (as opposed to instantiated)

    prototype(
		spatial			= new("Spatial"),
		scenarios		= list(),
		report			= list(),
        inputPaths  	= list(),
		baseDirectory	= getwd(),
		created			= FALSE),
	contains			= "EPOCObject"
)

# Private initialisation method called by constructor
# Parameters:
# 	dataPath		character	absolute path to universe data input file
setMethod("initialize", "Universe",
    function(.Object, dataPath=file.path(getwd(), "data", "Universe.data.R"), msglevel=getOption("epocmsglevel"),
								loglevel=getOption("epocloglevel"), logfile=NULL, logtrunc=TRUE, create=TRUE) {
		# Set working base directory
		.Object@baseDirectory <- dirname(dirname(dataPath))

		# Set up messaging and logging early from arguments
		logconn <- NULL
		if (!is.null(loglevel) && loglevel != "quiet") {
			openmode <- ifelse(is.null(logtrunc) || !logtrunc, "a", "w")
			logpath <- file.path(getwd(), ifelse(!is.null(logfile) && logfile != "", logfile, .Object@.logfile))
			logconn <- .Call("createRcppFileConn", logpath, openmode, PACKAGE="EPOC")
			.Call("writeRcppFileConn", logconn, paste(date(), ": Opening log file connection: ", logpath), TRUE, PACKAGE="EPOC")
		}

		# Suck in data.  logconn will start working after this
		.Object <- callNextMethod(.Object, dataPath, msglevel=msglevel, loglevel=loglevel, logconn=logconn)

		if (!file.exists(dataPath)) {
			epocErrorMessage(.Object, "Failed to find the Universe data input file at: ", dataPath, halt=TRUE)
		}

		# Initialise modules in environment .xData
		###########################################################
		.Object[['realtimeState']] <- list()	# Used to store real time information during simulation run
		.Object[['modules']] <- list()			# List of module which are in turn lists of elements
		###########################################################

		# Pull input data into class slots
		.Object <- parseInputData(.Object)

		# Set some Reporting defaults if they haven't already been set by the Universe data input
		# This may fire up logging if not explicitely specified by arguments to constructor
		.Object <- .resetReporting(.Object, msglevel, loglevel, logfile, logtrunc)

		invalidPaths <- testInputPaths(.Object)
        if (length(invalidPaths) > 0) {
			epocErrorMessage(.Object, "Failed to find the following input paths:")
			for (p in seq_along(invalidPaths)) epocErrorMessage(.Object, "  ",invalidPaths[[p]])
			epocErrorMessage(.Object, "Check all source paths!", halt=TRUE)
        }

		if (create) {
			.Object <- createUniverse(.Object)
			.Object@created = TRUE
        }

		return(.Object)
    }
)

# Test all paths to ensure that file exists at the location
# Uses Universe@inputPaths list as imported from universe data input file
# Return a vector of invalid paths
setGeneric("testInputPaths", function(.Object) standardGeneric("testInputPaths"))
setMethod("testInputPaths", signature(.Object="Universe"),
    function(.Object) {
		invalidPaths <- list()
		modNames <- names(.Object@inputPaths)
		for (m in seq_along(.Object@inputPaths)) {
			if (modNames[[m]] %in% c("Environment", "Biota", "Activity", "Management", "Output", "Presentation")) {
				elemNames <- names(.Object@inputPaths[[m]])
				if (!is.null(.Object@inputPaths[[m]])) {
					for (e in seq_along(.Object@inputPaths[[m]])) {
						inputPaths <- .Object@inputPaths[[m]][[e]]
						if (!is.null(inputPaths)) {
							if (is.null(inputPaths$className)) {
								invalidPaths <- c(invalidPaths, paste(modNames[[m]]," (",elemNames[[e]], ") Missing className input data!", sep=""))
							# Check if class is inbuilt/registered
							} else if (is.null(inputPaths$classFile) && !isClass(inputPaths$className)) {
								invalidPaths <- c(invalidPaths, paste(modNames[[m]]," (",elemNames[[e]], ") Missing classFile input data!", sep=""))
							} else if (!is.null(inputPaths$classFile) && !file.exists(getBasePath(.Object, inputPaths$classFile))) {
								invalidPaths <- c(invalidPaths, paste(modNames[[m]]," (",elemNames[[e]], ") Class path = ",
																		getBasePath(.Object, inputPaths$classFile), sep=""))
							}
							if (is.null(inputPaths$classData) || !file.exists(getBasePath(.Object, inputPaths$classData))) {
								invalidPaths <- c(invalidPaths, paste(modNames[[m]]," (",elemNames[[e]], ") Data path = ",
																		getBasePath(.Object, inputPaths$classData), sep=""))
							}
						}
					}
				}
			}
		}

		return(invalidPaths)
    }
)

# Build the universe object and its module elements based on the universe.data input file
# that was passed at instantiation
setGeneric("createUniverse", function(.Object) standardGeneric("createUniverse"))
setMethod("createUniverse", signature(.Object="Universe"),
    function(.Object) {
        # For each module (element class) develop a list of elements
        # if no elements then make the class equal to NULL e.g. Biota <- NULL
        epocMessage(.Object, "Loading universe...")

        # Load spatial (Polygon) information
		.Object@spatial <- new("Spatial", getBasePath(.Object, .Object@inputPaths$Config$Polygons))

		# Load and instantiate each scenario
		.Object@scenarios <- list()
		scenarioNames <- names(source(file=getBasePath(.Object, .Object@inputPaths$Config$Scenario))[[1]])
		scenarioN <- length(source(file=getBasePath(.Object, .Object@inputPaths$Config$Scenario))[[1]])
		for (i in 1:scenarioN) {
			.Object@scenarios[[scenarioNames[[i]]]] <- new("Scenario", universe=.Object,
													dataPath=getBasePath(.Object, .Object@inputPaths$Config$Scenarios), i)
		}
		epocVerboseMessage(.Object, "Loaded Scenarios - scenariosN = ",as.character(ifelse(!is.null(.Object@scenarios), length(.Object@scenarios),0)))

		# Report params now loaded from Universe data file and stored in slot report
		logconn <- getFileConnection(.Object, "logFile")

        # load each module in turn, instantiating an element object for each module element
		for (mod in c("Environment", "Biota", "Activity", "Management", "Output", "Presentation")) {
			elements <- NULL
			if (mod %in% names(.Object@inputPaths)) {
				elements <- list()

                # for each module element
				elemNames <- names(.Object@inputPaths[[mod]])
				for (ele in seq_along(.Object@inputPaths[[mod]])) {
					inputPaths <- .Object@inputPaths[[mod]][[ele]]
					if (!is.null(inputPaths)) {
						className <- inputPaths$className
						# testInputPaths() should already have weeded out any NULL classFile with class already available
						# So load source for class.  This will overload any existing class of same name!!
						if (!is.null(inputPaths$classFile)) source(file = getBasePath(.Object, inputPaths$classFile))
						# instaniate object
						elements[[elemNames[[ele]]]] = new(className, universe=.Object, dataPath=getBasePath(.Object, inputPaths$classData),
															msglevel=.Object@.msglevel, loglevel=.Object@.loglevel, logconn=logconn)
						# source any separate class methods
						sourceMethods(elements[[elemNames[[ele]]]], getBasePath(.Object))
					}
				}
                epocVerboseMessage(.Object, "Loaded ", mod, " - ElementsN = ",as.character(ifelse(!is.null(elements), length(elements),0)))
			}
			.Object$modules[[mod]] <- elements
		}

        epocVerboseMessage(.Object, "Finished loading universe")
        return(.Object)
    }
)

# Return the element in module as indicated by list index parameters
# NULL returned if element doesn't exist at indexes passed
# Parameters:
#	modID	character/numeric		module list index
#	elemID	character/numeric		element list index
setGeneric("getEPOCElement", function(.Object, modID, elemID) standardGeneric("getEPOCElement"))
setMethod("getEPOCElement", signature(.Object="Universe", modID="ANY", elemID="ANY"),
    function(.Object, modID, elemID) {
		modIndx <- 0
		elemIndx <- 0
		if (missing(modID) || missing(elemID)) return(NULL)
		if (is.character(modID) && modID %in% names(.Object$modules)) {
			modIndx <- which(names(.Object$modules) == modID)
		} else if (is.numeric(modID) && modID > 0 && modID <= length(.Object$modules)) {
			modIndx <- modID
		} else {
			return(NULL)
		}

		if (is.character(elemID) && elemID %in% names(.Object$modules[[modIndx]])) {
			elemIndx <- which(names(.Object$modules[[modIndx]]) == elemID)
		} else if (is.numeric(elemID) && elemID > 0 && elemID <= length(.Object$modules[[modIndx]])) {
			elemIndx <- elemID
		}

		if (modIndx > 0 && elemIndx > 0) return(.Object$modules[[modIndx]][[elemIndx]])
		return(NULL)
	}
)

# Set the element object at the parameter indexes
# Will overwrite any existing element object at that list position
# Parameters:
#	modID	character/numeric		module list index
#	elemID	character/numeric		element list index
#	element	Element					S4 Element object to be set
setGeneric("setEPOCElement", function(.Object, modID, elemID, element) standardGeneric("setEPOCElement"))
setMethod("setEPOCElement", signature(.Object="Universe", modID="ANY", elemID="ANY", element="Element"),
    function(.Object, modID, elemID, element) {
		if (missing(modID) || missing(elemID)) return(NULL)
		error <- try(.Object$modules[[modID]][elemID] <- list(element), silent=TRUE)
		if (exists("error") && class(error) == "try-error") {
			epocMessage(.Object, "Assignment of element ",element@signature@Name.short," into module '",
						as.character(modID), "' as element '", as.character(elemID), "' failed!")
			epocErrorMessage(.Object, trim(error[[1]]), halt=TRUE)
		}

		return(invisible(.Object))
	}
)

# Return the state list if it exists
# If item passed then value in state list is returned if available
# Parameters:
#	item		character	state list name to be returned (optional)
setGeneric("getRTState", function(.Object, item) standardGeneric("getRTState"))
setMethod("getRTState", signature(.Object="Universe", item="character"),
    function(.Object, item) {
        if (item %in% names(.Object$realtimeState)) return(.Object$realtimeState[[item]])
		epocErrorMessage(.Object, "Universe is missing requested Realtime State item: ", item, halt=TRUE)
    }
)
setMethod("getRTState", signature(.Object="Universe", item="missing"),
    function(.Object, item) return(.Object$realtimeState)
)

# NOTE: Not necessary to have available as an API method as all RTState values are set by
# controller or universe itself
# Set the state object for this object with that passed
# If listName passed then set value at slot in state if available
# setGeneric("setRTState", function(.Object, listName="character", value) standardGeneric("setRTState"))
# setMethod("setRTState", "Universe",
    # function(.Object, listName=NULL, value) {
        # if (is.null(listName) || listName == "") {
			# if (is.list(value)) .Object@realtimeState <- value
		# } else {
			# .Object@realtimeState[[listName]] <- value
		# }

		# return(.Object)
    # }
# )

# Return the base directory path
# If extPath is passed then full path to file is returned
# Parameters:
#	extPath		character	extension to base directory (optional)
setGeneric("getBasePath", function(.Object, extPath) standardGeneric("getBasePath"))
setMethod("getBasePath", signature(.Object="Universe", extPath="character"),
    function(.Object, extPath) {
        return(file.path(.Object@baseDirectory, extPath))
	}
)
setMethod("getBasePath", signature(.Object="Universe", extPath="missing"),
    function(.Object, extPath) return(.Object@baseDirectory)
)

# Return the runtime output directory path
# If extPath is passed then full path to file is returned
# Parameters:
#	extPath		character	extension to runtime directory (optional)
setGeneric("getRuntimePath", function(.Object, extPath) standardGeneric("getRuntimePath"))
setMethod("getRuntimePath", signature(.Object="Universe", extPath="character"),
    function(.Object, extPath) {
		scenarioDir <- getScenario(.Object, item="scenarioDir")
		if (is.null(scenarioDir)) scenarioDir <- file.path(getwd(), "runtime")
        return(file.path(scenarioDir, extPath))
	}
)
setMethod("getRuntimePath", signature(.Object="Universe", extPath="missing"),
    function(.Object, extPath) return(getScenario(.Object, item="scenarioDir"))
)

# Return the spatial object if it has been instantiated already
# If item passed then return value at slot in spatial if available
# Parameters:
#	item		character	spatial slot to be returned (optional)
setGeneric("getSpatial", function(.Object, item) standardGeneric("getSpatial"))
setMethod("getSpatial", signature(.Object="Universe", item="character"),
    function(.Object, item) {
        return(getSlot(getSlot(.Object, "spatial"), item))
	}
)
setMethod("getSpatial", signature(.Object="Universe", item="missing"),
    function(.Object, item)	return(getSlot(.Object, "spatial"))
)

# Return reporting list data
# If item passed then return that list item in report list if available
# Parameters:
#	item		character	state list name to be returned (optional)
setGeneric("getReport", function(.Object, item) standardGeneric("getReport"))
setMethod("getReport", signature(.Object="Universe", item="character"),
    function(.Object, item) {
        rep <- getSlot(.Object, "report")
		if (item %in% names(rep)) return(rep[[item]])
		epocErrorMessage(.Object, "Universe is missing requested Report item: ", item, halt=TRUE)
	}
)
setMethod("getReport", signature(.Object="Universe", item="missing"),
    function(.Object, item)	return(getSlot(.Object, "report"))
)

# Return scenario object if scenario is passed only.
# Return slot in scenario if item passed as well
# Uses CurrentScenario from realtimeState to specify scenario if not passed
# Parameters:
#	scenario	character	scenario name to retrieve
#	item		character	name of the scenario attribute to return
setGeneric("getScenario", function(.Object, scenario, item) standardGeneric("getScenario"))
setMethod("getScenario", signature(.Object="Universe", scenario="ANY", item="character"),
    function(.Object, scenario=0, item) {
        if (is.null(.Object@scenarios) || length(.Object@scenarios) <= 0) return(NULL)

		# Use currentscenario by default
		scenarioIdx <- .Object$realtimeState$currentScenario
		if (is.null(scenarioIdx) || scenarioIdx <= 0) scenarioIdx = 1
		if (!missing(scenario) && !is.null(scenario) && scenario > 0) scenarioIdx <- scenario

		# return slot detail from scenario if item specified
		if (item != "") return(getSlot(.Object@scenarios[[scenarioIdx]], item))
		return(.Object@scenarios[[scenarioIdx]])
    }
)
setMethod("getScenario", signature(.Object="Universe", scenario="ANY", item="missing"),
    function(.Object, scenario, item) return(getScenario(.Object, scenario, item=""))
)
setMethod("getScenario", signature(.Object="Universe", scenario="missing", item="missing"),
    function(.Object, scenario, item) return(getScenario(.Object, item=""))
)

# Returns a vector containing the module/element list indexes = c(moduleListIndex, elementListIndex)
# Returns an index of 0 for list names not found
# Parameters:
#	moduleName  	character	list name of module (optional)
#	element 		character	list name of element
setGeneric("getElementIndexes", function(.Object, moduleName, element) standardGeneric("getElementIndexes"))
setMethod("getElementIndexes", signature(.Object="Universe", moduleName="character", element="character"),
    function(.Object, moduleName, element) {
		mIdx <- 0
		eIdx <- 0
		msg <- paste("         Finding relative index of '", element, "' in module '", moduleName, "' = ", sep="")
		if (!is.null(.Object$modules) && length(.Object$modules) > 0) {
			if (!is.null(moduleName) && moduleName != "") {
				mIdx <- which(names(.Object$modules) == moduleName)
				if (length(mIdx) == 0) mIdx <- 0
				error <- try(eIdx <- which(names(.Object$modules[[mIdx]]) == element), silent=TRUE)
				if (!(exists("error") && class(error) == "try-error") && length(eIdx) > 0) {
					epocDebugMessage(.Object, msg, "(", mIdx, ",", eIdx, ")")
					return(c(mIdx,eIdx))
				}
			} else if (!is.null(element) && element != "") {
				# Still not found or module not specified, so check each module
				for (mIdx in 1:length(.Object$modules)) {
					error <- try(eIdx <- which(names(.Object$modules[[mIdx]]) == element), silent=TRUE)
					if (!(exists("error") && class(error) == "try-error") && length(eIdx) > 0) {
						epocDebugMessage(.Object, msg, "(", mIdx, ",", eIdx, ")")
						return(c(mIdx,eIdx))
					}
					remove(error)
				}
			}
		}

		return(c(0,0))
    }
)
setMethod("getElementIndexes", signature(.Object="Universe", moduleName="missing", element="character"),
    function(.Object, moduleName, element) return(getElementIndexes(.Object, "", element))
)

# Returns an vector containing the element c(moduleListIndex, elementListIndex)
# Search elements by EPOC signature ID
# Returns an index of 0 for list names not found
# Parameters:
#	moduleName 		character 	list name of module (optional)
#	element 		numeric		signature ID of element
setMethod("getElementIndexes", signature(.Object="Universe", moduleName="character", element="numeric"),
    function(.Object, moduleName, element) {
		mIdx <- 0
		eIdx <- 0
		msg <- paste("         Get relative index for ID '", element, "' in module '", moduleName, "' = ", sep="")
		if (!is.null(.Object$modules) && length(.Object$modules) > 0) {
			if (!is.null(moduleName) && moduleName != "") {
				mIdx <- which(names(.Object$modules) == moduleName)
				if (length(mIdx) == 0) mIdx <- 0
				error <- try(eIdx <- which(lapply(.Object$modules[[mIdx]], "getSignature", "ID") == element)[[1]], silent=TRUE)
				if (!(exists("error") && class(error) == "try-error") && length(eIdx) > 0) {
					epocDebugMessage(.Object, msg, "(", mIdx, ",", eIdx, ")")
					return(c(mIdx,eIdx))
				}
			} else if (element > 0) {
				# Still not found or module not specified, so check each module
				for (mIdx in 1:length(.Object$modules)) {
					error <- try(eIdx <- which(lapply(.Object$modules[[mIdx]], "getSignature", "ID") == element)[[1]], silent=TRUE)
					if (!(exists("error") && class(error) == "try-error") && length(eIdx) > 0) {
						epocDebugMessage(.Object, msg, "(", mIdx, ",", eIdx, ")")
						return(c(mIdx,eIdx))
					}
					remove(error)
				}
			}
		}

		epocDebugMessage(.Object, msg, "(Not found!)")
		return(c(0,0))
    }
)
setMethod("getElementIndexes", signature(.Object="Universe", moduleName="missing", element="numeric"),
    function(.Object, moduleName, element) return(getElementIndexes(.Object, "", element))
)

# Source all element class code files listed by universe
setGeneric("reSourceElementClasses", function(.Object) standardGeneric("reSourceElementClasses"))
setMethod("reSourceElementClasses", signature(.Object="Universe"),
    function(.Object) {
		modNames <- names(.Object$modules)
		for (ec in seq_along(.Object$modules)) {
			elemNames <- names(.Object$modules[[ec]])
			for (e in seq_along(.Object$modules[[ec]])) {
				# load source for class
				source(file = getBasePath(.Object, .Object@inputPaths[[modNames[[ec]]]][[elemNames[[e]]]]$classFile))
			}
		}
	}
)

# Source all element method code files listed by universe
setGeneric("reSourceElementMethods", function(.Object) standardGeneric("reSourceElementMethods"))
setMethod("reSourceElementMethods", signature(.Object="Universe"),
    function(.Object) {
		methodSources <- list()
		for (ec in seq_along(.Object$modules)) {
			elementNames <- names(.Object$modules[[ec]])
			for (e in seq_along(.Object$modules[[ec]])) {
				# load source for method
				methodSources[[elementNames[e]]] <- sourceMethods(.Object$modules[[ec]][[e]], getBasePath(.Object))
			}
		}

		return(methodSources)
	}
)

# Set up messaging, logging, debugging and heading line values
# These will be based, by order of preference as:
# values passed as parameters (eg when instantiating universe or controller)
# values set in universe data input file under report section
# lastly by a set of default values
setGeneric(".resetReporting", function(.Object, ...) standardGeneric(".resetReporting"))
setMethod(".resetReporting", signature(.Object="Universe"),
	function(.Object, msglevel=NULL, loglevel=NULL, logfile=NULL, logtrunc=NULL) {
		# create a report default structure
		repdef <- list(Diagnostics = list(
							Output = list(Level = "normal"),
							Log = list(Level = "quiet", Filename = "EPOC.log", Truncate=TRUE),
							Calendar = list(Print = TRUE, Filename = "Calendar.txt"),
							Debug = list()),
						HeadingLines = list(
							Heading1 = "##################################################################",
							Heading2 = "------------------------------------------------------------------",
							Heading3 = "+++++++++++++++++++++++++++++++++++++",
							Heading4 = "     ................................"))

		# then replace any NULL/missing settings with defaults
		if (is.null(.Object@report)) {
			.Object@report <- repdef
		} else {
			if (is.null(.Object@report$Diagnostics)) {
				.Object@report[["Diagnostics"]] <- repdef$Diagnostics
			} else {
				if (is.null(.Object@report$Diagnostics$Output)) .Object@report$Diagnostics[["Output"]] <- repdef$Diagnostics$Output
				if (is.null(.Object@report$Diagnostics$Output$Level)) .Object@report$Diagnostics$Output[["Level"]] <- repdef$Diagnostics$Output$Level
				if (is.null(.Object@report$Diagnostics$Log)) .Object@report$Diagnostics[["Log"]] <- repdef$Diagnostics$Log
				if (is.null(.Object@report$Diagnostics$Log$Level)) .Object@report$Diagnostics$Log[["Level"]] <- repdef$Diagnostics$Log$Level
				if (is.null(.Object@report$Diagnostics$Log$Filename)) .Object@report$Diagnostics$Log[["Filename"]] <- repdef$Diagnostics$Log$Filename
				if (is.null(.Object@report$Diagnostics$Log$Truncate)) .Object@report$Diagnostics$Log[["Truncate"]] <- repdef$Diagnostics$Log$Truncate
				if (is.null(.Object@report$Diagnostics$Calendar)) .Object@report$Diagnostics[["Calendar"]] <- repdef$Diagnostics$Calendar
				if (is.null(.Object@report$Diagnostics$Calendar$ToFile)) .Object@report$Diagnostics$Calendar[["ToFile"]] <- repdef$Diagnostics$Calendar$ToFile
				if (is.null(.Object@report$Diagnostics$Calendar$Filename)) .Object@report$Diagnostics$Calendar[["Filename"]] <- repdef$Diagnostics$Calendar$Filename
				if (is.null(.Object@report$Diagnostics$Debug)) .Object@report$Diagnostics[["Debug"]] <- repdef$Diagnostics$Debug
			}
			if (is.null(.Object@report$HeadLines)) {
				.Object@report[["HeadLines"]] <- repdef$HeadLines
			} else {
				if (is.null(.Object@report$HeadLines$Heading1)) .Object@report$HeadLines[["Heading1"]] <- repdef$HeadLines$Heading1
				if (is.null(.Object@report$HeadLines$Heading2)) .Object@report$HeadLines[["Heading2"]] <- repdef$HeadLines$Heading2
				if (is.null(.Object@report$HeadLines$Heading3)) .Object@report$HeadLines[["Heading3"]] <- repdef$HeadLines$Heading3
				if (is.null(.Object@report$HeadLines$Heading4)) .Object@report$HeadLines[["Heading4"]] <- repdef$HeadLines$Heading4
			}
		}

		# Prioritise by argument, then input file, thirdly the dafault
		if (!is.null(msglevel) && msglevel != "") .Object@report$Diagnostics$Output$Level <- msglevel
		if (!is.null(loglevel) && loglevel != "") .Object@report$Diagnostics$Log$Level <- loglevel
		if (!is.null(logfile) && logfile != "") .Object@report$Diagnostics$Log$Filename <- logfile
		if (!is.null(logtrunc)) .Object@report$Diagnostics$Log$Truncate <- logtrunc
		.Object@.msglevel <- .Object@report$Diagnostics$Output$Level
		.Object@.loglevel <- .Object@report$Diagnostics$Log$Level
		.Object@.logfile <- .Object@report$Diagnostics$Log$Filename
		.Object@.logtrunc <- .Object@report$Diagnostics$Log$Truncate

		logpath <- file.path(getwd(), .Object@.logfile)
		logconn <- getFileConnection(.Object, "logFile")
		# If log is open and path has changed then close so it can be reopened
		if (!is.null(logconn) && class(logconn) == "externalptr" && logpath != .Call("getpathRcppFileConn", logconn, PACKAGE="EPOC")) {
			oldlogpath <- .Call("getpathRcppFileConn", logconn, PACKAGE="EPOC")
			writeFileConnection(.Object, logconn, paste(date(), ": Closing log file connection: ", oldlogpath))
			closeFileConnection(.Object, logconn)
		}
		# Then open if not already opened by explicit arguments
		if ((is.null(logconn) || class(logconn) != "externalptr"
							|| !.Call("isopenRcppFileConn", logconn, PACKAGE="EPOC")) && .Object@.loglevel != "quiet") {
			openmode <- ifelse(.Object@.logtrunc, "w", "a")
			epocDebugMessage(.Object, date(), ": Opening log file connection: ", logpath)
			logconn <- getFileConnection(.Object, "logFile", logpath, openmode)
			writeFileConnection(.Object, logconn, paste(date(), ": Opening log file connection: ", logpath))
		}

		# Debug message the current settings
		epocDebugMessage(.Object, "Output and log settings:")
		epocDebugMessage(.Object, "Output - ", .Object@.msglevel, ", Logging - ", .Object@.loglevel,
								  ", Log file - ", .Object@.logfile, ", Truncate - ", .Object@.logtrunc)
		epocDebugMessage(.Object, "Calendar settings:")
		epocDebugMessage(.Object, "ToFile - ", .Object@report$Diagnostics$Calendar$ToFile, ", Calendar file - ", .Object@report$Diagnostics$Calendar$Filename)
		epocDebugMessage(.Object, "Debug settings:")
		epocDebugMessage(.Object, "Debug - ", .Object@report$Diagnostics$Debug)

		return(.Object)
	}
)

# Close log file if it is open and loglevel warrants it
setGeneric(".stopReporting", function(.Object, ...) standardGeneric(".stopReporting"))
setMethod(".stopReporting", signature(.Object="Universe"),
	function(.Object) {
		logconn <- getFileConnection(.Object, "logFile")
		if (!is.null(logconn) && class(logconn) == "externalptr") {
			logpath <- .Call("getpathFileConnection", .Object$.logconn, PACKAGE="EPOC")
			writeFileConnection(.Object, logconn, paste(date(), ": Closing log file connection: ", oldlogpath))
			closeFileConnection(.Object, logconn)
		}

		return(invisible(.Object))
	}
)
#C.SE.Setup.universe.01<-function (Universe, Calendar)
# Function:           C.SE.Setup.universe.01
# Description:        Setup the universe that may have generic functions but also need to invoke the setup functions
#                     in each component to facilitate establishing the component that may need relationships with other
#                     components
# Primary attributes: Establish the universe ready for simulations
#
# Input parameters:   Universe
#                     Calendar of events
# Returned            Linkages = the variety of linkage matrices that might be formed (for reporting)
# NB test to see that Universe is updated without having to return it.
########################################################
# Create S4 method '.setupUniverse'
# PRIVATE method
setGeneric(".setupUniverse", function(.Object) standardGeneric(".setupUniverse"))
setMethod(".setupUniverse", "Universe",
    function(.Object) {

        epocVerboseMessage(.Object, "Universe setup...")

        #  Universe[[1]] = Biota
        #          [[2]] = Environment
        #          [[3]] = Activity
        #          [[4]] = Management
        #          [[5]] = Output
        #          [[6]] = Config
        #          [[7]] = Presentation

        epocVerboseMessage(.Object, "  accumulate reference IDs for elements")

        elementIDs <- c()# col 1 = Module, 2 = ID, col 3 = reference number in module
        #elementNames <- c()


        for (ec in seq_along(.Object$modules)) {
			elementIDs <- matrix(ncol=3, nrow=length(.Object$modules[[ec]]), byrow=TRUE)
			for (e in seq_along(.Object$modules[[ec]])) {
				elementIDs[e,] <- c(ec, getSignature(.Object$modules[[ec]][[e]], "ID"), e)
				#elementNames<-c(elementNames, names(.Object$modules[[ec]])[[e]])
			}

			# check for duplicate IDs in module
			if(length(elementIDs[,2])!=length(unique(elementIDs[,2]))) {
				elementIDs
				epocErrorMessage(.Object, "Duplicate IDs in Element functions - review list above (col 1 = ID, col 2 = Reference in declaration)", halt=TRUE)
			}
        }

        ################################################################################
        # loop through modules to invoke the necessary setup functions should they exist
        moduleNames <- names(.Object$modules)
        for (ec in seq_along(.Object$modules)) {
			elementN <- ifelse(!is.null(.Object$modules[[ec]]), length(.Object$modules[[ec]]), 0)
            epocVerboseMessage(.Object, "   Setting up Module ", as.character(ec), ": ", moduleNames[ec], " (",
						elementN, " element", ifelse(elementN == 1, ")", "s)"))
			elementNames <- names(.Object$modules[[ec]])
			for (e in seq_along(.Object$modules[[ec]])) {
				epocVerboseMessage(.Object, "      Setting up element ", elementNames[e])
				# auto setup for all modules with time steps and convert Related ID numbers to relative
				.Object$modules[[ec]][[e]] <- .setupUniverseElement(.Object, .Object$modules[[ec]][[e]])
			}
        }

		return(.Object)
    }
)


#C.SE.Element.setup.01<-function (ElEnv, Module,Element,Universe,ElementID,ElementNames,Calendar)
# Function:           C.SE.Element.setup.01
# Description:        Run Time - setup element
# Primary attributes: Generic routine to setup elements with timesteps:
#                     i) change references to elements from absolute to relative ID
#                     ii) transform timesteps to periods
#
# Input parameters    ElEnv       = the specific element environment
#                     Module      = the number of the module
#                     Element     = the relative number of the taxon
#                     Universe    = the entire Universe, in case it is needed for specific Setup functions
#                     ElementID   = matrix of ID given in original signature (column 1) and relative ID within Universe (column 2)
#                     ElementNames = vector of Names for elements if needed.
#                     Calendar       = the Calendar of periods
################################################################################
#    ElEnv$Signature <- list(
#      ID           = ,
#      Name.full    = ,
#      Name.short   = ,
#      Morph        = ,
#      Version      = ,
#      Authors      = "A.Constable",
#      last.edit    = "10 April 2008"
#      ) # end Signature
#
# Create S4 method '.setupUniverseElement'
# PRIVATE method
setGeneric(".setupUniverseElement", function(.Object, element="Element") standardGeneric(".setupUniverseElement"))
setMethod(".setupUniverseElement", "Universe",
    function(.Object, element) {
        if (is.null(element)) return(invisible())

        # Change ID of related elements from absolute to a relative id, else NA
		for (tn in seq_along(element@timesteps)) {
			if(element@timesteps[[tn]]$actionsN > 0){
				for (a in seq_along(element@timesteps[[tn]]$actions)) {
					element@timesteps[[tn]]$actions[[a]]["relatedIndexes.N"] <- list(NULL)					# create new attributes
					element@timesteps[[tn]]$actions[[a]]["relatedIndexes"] <- list(NULL)

					RelEls <- element@timesteps[[tn]]$actions[[a]]$relatedElements
					if(!is.null(RelEls)){
						NrelatedElements<-nrow(RelEls)
						element@timesteps[[tn]]$actions[[a]]$relatedElements.N <- NrelatedElements
						element@timesteps[[tn]]$actions[[a]]$relatedIndexes.N <- NrelatedElements
						element@timesteps[[tn]]$actions[[a]]$relatedIndexes <- matrix(ncol=2, nrow=NrelatedElements ,byrow=TRUE)		# initialise new attributes

						for (e in 1:NrelatedElements) {
							element@timesteps[[tn]]$actions[[a]]$relatedIndexes[e,] <- getElementIndexes(.Object, RelEls[e,1], RelEls[e,2])
							# Show warning if element not found
							if (element@timesteps[[tn]]$actions[[a]]$relatedIndexes[e,1] == 0 || element@timesteps[[tn]]$actions[[a]]$relatedIndexes[e,2] == 0) {
								epocMessage(.Object, "Related element '", RelEls[e,2], "' in module '", RelEls[e,1],"' not found. Required by '", getSignature(element, "Name.short"), "' element")
							} else {
								epocDebugMessage(.Object, "         Assigning related element '", RelEls[e,2], "' in module '", RelEls[e,1],"'")
							}
						}
					}
				}
			}
		}

        # ############################################################################
        # 2. Change ID of Competitor elements from absolute to relative in Transition
        # ############################################################################
        if(!is.null(element$transition$CompetitorElements)){

        # change ID of Competitor elements from absolute to relative
        #   if related element not present in Universe then given value of NA

            RelEls <- element$transition$CompetitorElements

            NrelatedElements <- nrow(RelEls)
            element$transition$CompetitorElementsN <- NrelatedElements
			element$transition$CompetitorIndexesN <- NrelatedElements
			element$transition$CompetitorIndexes <- matrix(ncol=2, nrow=NrelatedElements, byrow=TRUE)		# initialise new attributes

            for (e in 1:NrelatedElements) {
				element$transition$CompetitorIndexes[e,] <- getElementIndexes(.Object, RelEls[e,1], RelEls[e,2])
				# Show warning if element not found
				if (element$transition$CompetitorIndexes[e,1] == 0 || element$transition$CompetitorIndexes[e,2] == 0) {
					epocMessage(.Object, "Competitor element '", RelEls[e,2], "' in module '", RelEls[e,1],"' not found. Required by '", getSignature(element, "Name.short"), "' element")
				} else {
					epocDebugMessage(.Object, "         Assigning competitor element '", RelEls[e,2], "' in module '", RelEls[e,1],"'")
				}
            }
        }

		return(element)
    }
)

