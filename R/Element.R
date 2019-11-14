################################################################################
# EPOC Element class
#
# S4 Base class for all EPOC Elements
# 
# VIRTUAL base class for all EPOC elements.  Inherits from EPOCObject.
# Contains data members and functionality common to all elements.
# Holds spatial, timestep, state and transition information.
# Its methods are available to all other derived element objects.
# NOTE: Some slots have now been replaced as objects in the environment list.  These all have accessor methods.
#
# Created 3/2/2009 Troy Robertson
# Modified 20/3/2012 - TR
################################################################################

# Define base Element class with data members (slots)
setClass("Element",
	# All these slots are unchanged during simulation
    representation(
        polygons    	= "vector",			# Element spatial/polygon information
        polygonsN   	= "numeric",		# Count of polygons
        birthday    	= "numeric",		# Element birthday
		timesteps   	= "list",			# Timestep information from input data file
        timestepsN  	= "numeric",		# Count of timesteps
		recordElements  = "numeric",		# 
		currentScenarioDir = "character",		# Directory path for current scenario
		"VIRTUAL"),
    prototype(
        polygons    	= 0,
        polygonsN   	= 0,
        birthday    	= 0,
		timesteps   	= list(),
        timestepsN  	= 0,
		recordElements  = 0,
		currentScenarioDir = ""
		),
	contains			= "EPOCObject"
)

setMethod("initialize", "Element",
    function(.Object, dataPath, sublist=0, ...) {
        
		.Object <- callNextMethod(.Object, dataPath, ...)
        epocVerboseMessage(.Object, getSignatureLine(.Object), " - create environment")
		
		# Pull input data into class slots
		.Object <- parseInputData(.Object)
		
		# Define and initialise xData environment slots
		# These are defined like this because they are transient data members 
		# and so are passed by reference to avoid excessive memcpy
		########################################################################
		.Object[['state']] <- list()			# Current object state
		.Object[['initialState']] <- list()		# Initiate object state before initialiseReplicate
		.Object[['functions']] <- list()		# Function data list
		.Object[['transition']] <- list()		# Transition data before state is updated
		.Object[['flags']] <- list()			# Runtime flags (eg doPrint, doUpdate etc)
		########################################################################
		
		# Find the number of polygons
        if (!is.null(.Object@polygons) && length(.Object@polygons) > 0) {
			.Object@polygonsN <- length(.Object@polygons)     
		} else {
			epocErrorMessage(.Object, "Element '", getSignature(.Object, "Name.short"), "' has no polygon extent!", halt=TRUE)
		}
		
		# Convert birthdate to day-of-year birthday
		birthdate <- getNoCase(.Object@epocAttributes, "birthdate")
		if (is.null(birthdate) || !is.list(birthdate) || length(birthdate) < 2 || !is.numeric(birthdate[[1]]) || !is.numeric(birthdate[[2]])) {
			epocErrorMessage(.Object, "Element '", getSignature(.Object, "Name.short"), "' does not contain a valid birthdate!", halt=TRUE)
		}
		.Object@birthday <- dayFromDate(birthdate[[1]], birthdate[[2]])/365
		if (is.na(.Object@birthday) || .Object@birthday <= 0) {
			epocErrorMessage(.Object, "Element '", getSignature(.Object, "Name.short"), "' does not contain a valid birthdate!", halt=TRUE)
		}
		
		# Process timesteps
		epocVerboseMessage(.Object, getSignatureLine(.Object), " - set TimeSteps")
		timesteps <- getTimestep(.Object)
        for (Tstep in seq_along(timesteps)){
			# number of actions in each timestep
            timesteps[[Tstep]]$actionsN <- length(timesteps[[Tstep]]$actions)
			# replace any named attributes with their param value
			for (act in seq_along(timesteps[[Tstep]]$actions)) {
				if ("namedAttributes" %in% names(act)) {
					for (natt in names(act$namedAttributes)) {
						# special case - functions
						if (natt == "actionMethod") {
							act[["actionMethod"]] <- getFunctionData(.Object, act$namedAttributes[[natt]])$actionMethod
							act[["actionFile"]] <- getFunctionData(.Object, act$namedAttributes[[natt]])$actionFile
						} else {
							act[[natt]] <- getAttribute(.Object, act$namedAttributes[[natt]])
						}
					}
				}
			}
        }
		.Object <- setTimestep(.Object, value=timesteps)
		
		# number of timesteps
		.Object <- setSlot(.Object, "timestepsN", value=length(getTimestep(.Object)))
		
		# Update functions
		epocVerboseMessage(.Object, getSignatureLine(.Object), " - set Ancillary functions")
		if ("functions" %in% names(.Object@epocAttributes)) {
			.Object <- setFunctionData(.Object, value=getAttribute(.Object, "functions"))
		} else if ("Functions" %in% names(.Object@epocAttributes)) {
			.Object <- setFunctionData(.Object, value=getAttribute(.Object, "Functions"))
		}
		
		# Initialise flags
		.Object$flags$doUpdate <- FALSE
		.Object$flags$doPrint <- FALSE
		.Object$flags$doPrintFinal <- FALSE
		
        return(.Object)
    }
)

# Method loader for all Element action methods
setGeneric("sourceMethods", function(.Object, basePath="character") standardGeneric("sourceMethods"))
setMethod("sourceMethods", "Element", 
    function(.Object, basePath=getwd()) {
		if (is.null(.Object@timesteps)) return(invisible())
		
		methodSources <- list()
		# First source all timestep actions
		for (ts in seq_along(.Object@timesteps)) {
			for (a in seq_along(.Object@timesteps[[ts]]$actions)) {
				actionMethod <- .Object@timesteps[[ts]]$actions[[a]]$actionMethod
				filePath <- file.path(basePath, .Object@timesteps[[ts]]$actions[[a]]$actionFile)
				
				if (is.null(actionMethod) || actionMethod == "") {
					epocErrorMessage(.Object, "Element: ", .Object@signature@Name.short, " has an action method declared without a name!", halt=TRUE)
				}
				if (actionMethod %in% c("initialize", "initialiseReplicate", "initialiseTransition")) {
					epocErrorMessage(.Object, "Element: ", .Object@signature@Name.short, 
							" has an action method declared that attempts to override default method '", actionMethod, "'!", halt=TRUE)
				}
				
				# Check if path is to a .R file or shared library
				if (toupper(substring(filePath, nchar(filePath)-1) == ".R")) {
					if (!(actionMethod %in% names(methodSources))) {
						epocDebugMessage(.Object, getSignatureLine(.Object), " - sourcing action method '", actionMethod, "' from file: ", filePath)
						methodSources[[actionMethod]] <- source(file=filePath)
					}
				} else {
					libname <- .loadDLLActionMethod(.Object, actionMethod, filePath)
				}
				
				# Check for any transAction functions for setups (NOT DEALING WITH DLLs ATM)
				if (!is.null(.Object@timesteps[[ts]]$actions[[a]]$transAction)) {
					actionMethod <- .Object@timesteps[[ts]]$actions[[a]]$transAction$actionMethod
					filePath <- file.path(basePath, .Object@timesteps[[ts]]$actions[[a]]$transAction$actionFile)
					if (!(actionMethod %in% names(methodSources))) {
						epocDebugMessage(.Object, getSignatureLine(.Object), " - sourcing transform method '", actionMethod, "' from file: ", filePath)
						methodSources[[actionMethod]] <- source(file=filePath)
					}
				}
			}
		}
		
		# Then do any function actions (some of these may be repeats of timestep actions)
		# First source all timestep actions
		for (f in seq_along(.Object$functions)) {
			if (tolower(names(.Object$functions)[[f]]) != "setup" && !is.null(.Object$functions[[f]]$actionFile)) {
				actionMethod <- .Object$functions[[f]]$actionMethod
				filePath <- file.path(basePath, .Object$functions[[f]]$actionFile)
				if (!(actionMethod %in% names(methodSources))) {
					epocDebugMessage(.Object, getSignatureLine(.Object), " - sourcing functions method '", actionMethod, "' from file: ", filePath)
					methodSources[[actionMethod]] <- source(file=filePath)
				}
			}
		}
		
		# Remove any with NULL actionFile
		
		return(methodSources)
	}
)

# Load shared library at dllPath and check that actionMethod is an available symbol in library
# Throw error if either load or symbol lookup fails
# Return name of library
setGeneric(".loadDLLActionMethod", function(.Object, actionMethod, dllPath) standardGeneric(".loadDLLActionMethod"))
setMethod(".loadDLLActionMethod", signature(.Object="Element", actionMethod="character", dllPath="character"),
    function(.Object, actionMethod, dllPath) {
		epocDebugMessage(.Object, getSignatureLine(.Object), " - sourcing action method '", actionMethod, "' from library: ", dllPath)
		
		# remove extension c(".dll", ".so", ",sl")
		dllPath <- sub("\\.(dll|DLL|so|SO|sl|SL)$", "", dllPath)
		dllName <- basename(dllPath)
		
		error <- try(dllinfo <- dyn.load(dllPath), silent=TRUE)
		if (exists("error") && class(error) == "try-error") {
			epocErrorMessage(.Object, "Element: ", .Object@signature@Name.short, " failed to find required action library at: ", dllPath, "!", halt=TRUE)
		}
		if (!is.loaded(actionMethod, PACKAGE=dllinfo[1]$name)) {
			epocErrorMessage(.Object, "Element: ", .Object@signature@Name.short, " failed to find required action function '", actionMethod, "'")
			epocErrorMessage(.Object, "in action library at: ", dllPath, "!", halt=TRUE)
		}
		
		return(dllinfo[1]$name)
	}
)

# Return the polygons slot data
setGeneric("getPolygons", function(.Object) standardGeneric("getPolygons"))
setMethod("getPolygons", "Element",
    function(.Object) {
		if (!is.null(.Object@polygons) && length(.Object@polygons) > 0) return(.Object@polygons)
		epocErrorMessage(.Object, "Element: ", .Object@signature@Name.short, " has no polygons extent!", halt=TRUE)
    }
)

# Return the elements Julian day of birth in any year.
setGeneric("getBirthday", function(.Object) standardGeneric("getBirthday"))
setMethod("getBirthday", signature(.Object="Element"),
    function(.Object) {
		if (!is.null(.Object@birthday) && is.numeric(.Object@birthday) && .Object@birthday > 0) {
			return(.Object@birthday)
		}
		epocErrorMessage(.Object, "Element: ", .Object@signature@Name.short, " has no birthday defined!", halt=TRUE)
    }
)

# Return the state object if it has been instantiated already
# If item passed then return value at list item in state if available
# Parameters:
#	item		character	state list member to be returned (optional)
setGeneric("getState", function(.Object, item) standardGeneric("getState"))
setMethod("getState", signature(.Object="Element", item="character"),
    function(.Object, item) {
		if (item %in% names(.Object$state)) return(.Object$state[[item]])
		epocErrorMessage(.Object, "Element: ", .Object@signature@Name.short, " is missing requested State item: ", item, halt=TRUE)
    }
)
setMethod("getState", signature(.Object="Element", item="missing"),
    function(.Object, item) return(.Object$state)
)

# Set the state list as the value passed
# If item passed then set value at name in state list 
# Parameters:
#	item		character	state list member at which to assign value
#	value		ANY			value to be assigned
setGeneric("setState", function(.Object, item, value) standardGeneric("setState"))
setMethod("setState", signature(.Object="Element", item="character", value="ANY"),
    function(.Object, item, value) {
        .Object$state[item] <- list(value)
		return(invisible(.Object))
    }
)
setMethod("setState", signature(.Object="Element", item="missing", value="list"),
    function(.Object, item, value) {
		.Object$state <- value
		return(invisible(.Object))
	}
)

# Return the transition list
# If item passed then return value at name in transition list if available
# Parameters:
#	item		character	transition list item to be returned
setGeneric("getTransition", function(.Object, item) standardGeneric("getTransition"))
setMethod("getTransition", signature(.Object="Element", item="character"),
    function(.Object, item) {
		if (item %in% names(.Object$transition)) return(.Object$transition[[item]])
		epocErrorMessage(.Object, "Element: ", .Object@signature@Name.short, " is missing requested Transition item: ", item, halt=TRUE)
    }
)
setMethod("getTransition", signature(.Object="Element", item="missing"),
    function(.Object, item) return(.Object$transition)
)

# Set the transition list as the value passed
# If item passed then set value at name in transition list
# Parameters:
#	item		character	transition list item to have value assigned (optional)
#	value		ANY			value to be assigned
setGeneric("setTransition", function(.Object, item, value) standardGeneric("setTransition"))
setMethod("setTransition", signature(.Object="Element", item="character", value="ANY"),
    function(.Object, item, value) {
        .Object$transition[item] <- list(value)
		return(invisible(.Object))
    }
)
setMethod("setTransition", signature(.Object="Element", item="missing", value="list"),
    function(.Object, item, value) {
		.Object$transition <- value
		return(invisible(.Object))
	}
)

# Return the functions list
# If item passed then return value at name in functions list if available
# Parameters:
#	item		character	functions list item to be returned
setGeneric("getFunctionData", function(.Object, item) standardGeneric("getFunctionData"))
setMethod("getFunctionData", signature(.Object="Element", item="character"),
    function(.Object, item) {
		if (item %in% names(.Object$functions)) return(.Object$functions[[item]])
		epocErrorMessage(.Object, "Element: ", .Object@signature@Name.short, " is missing requested Function Data: ", item, halt=TRUE)
    }
)
setMethod("getFunctionData", signature(.Object="Element", item="missing"),
    function(.Object, item) return(.Object$functions)
)

# Set the functions list as the value passed
# If item passed then set value at name in functions list
# Parameters:
#	item		character	functions list item to have value assigned (optional)
#	value		ANY			value to be assigned
setGeneric("setFunctionData", function(.Object, item, value) standardGeneric("setFunctionData"))
setMethod("setFunctionData", signature(.Object="Element", item="character", value="ANY"),
    function(.Object, item, value) {
        .Object$functions[item] <- list(value)
		return(invisible(.Object))
    }
)
setMethod("setFunctionData", signature(.Object="Element", item="missing", value="list"),
    function(.Object, item, value) {
		.Object$functions <- value
		return(invisible(.Object))
	}
)

# Return the timestep list
# If periodNum passed then return value at index in timesteps if available
# Parameters:
#	periodNum	numeric	timestep period to be returned (optional)
setGeneric("getTimestep", function(.Object, periodNum) standardGeneric("getTimestep"))
setMethod("getTimestep", signature(.Object="Element", periodNum="numeric"),
    function(.Object, periodNum) {
		if (periodNum > 0 && periodNum <= length(.Object@timesteps)) return(.Object@timesteps[[periodNum]])
		epocErrorMessage(.Object, "Element: ", .Object@signature@Name.short, " is missing requested Timestep period: ", periodNum, halt=TRUE)
    }
)
setMethod("getTimestep", signature(.Object="Element", periodNum="missing"),
    function(.Object, periodNum) return(.Object@timesteps)
)

# Set the timesteps list as the value passed
# If periodNum passed then set value at index in timesteps
# Parameters:
#	periodNum	numeric	timestep period to have value assigned (optional)
#	value		ANY		value to be assigned
setGeneric("setTimestep", function(.Object, periodNum, value) standardGeneric("setTimestep"))
setMethod("setTimestep", signature(.Object="Element", periodNum="numeric", value="ANY"),
    function(.Object, periodNum, value) {
        .Object@timesteps[periodNum] <- list(value)
		return(.Object)
    }
)
setMethod("setTimestep", signature(.Object="Element", periodNum="missing", value="list"),
    function(.Object, periodNum, value) {
		.Object@timesteps <- value
		return(.Object)
	}
)

# Check or set generic flag as specified
# Parameters:
#	flag		character	name of flag to set
#	do		logical		boolean value to set flag to (optional)
setGeneric("doFlag", function(.Object, flag, do) standardGeneric("doFlag"))
setMethod("doFlag", signature(.Object="Element", flag="character", do="logical"),
    function(.Object, flag, do) {
		.Object[["flags"]][[flag]] <- do
		return(.Object[["flags"]][[flag]])
	}
)
setMethod("doFlag", signature(.Object="Element", flag="character", do="missing"),
    function(.Object, flag, do) return(.Object[["flags"]][[flag]])
)

# Check or set doUpdate flag
# If a “updateState” method is available for this element and doUpdate==TRUE then execute
# method between “during” and “after” timestep/period timings
# Parameters:
#	do		logical 		boolean value to set flag to (optional)
setGeneric("doUpdate", function(.Object, do) standardGeneric("doUpdate"))
setMethod("doUpdate", signature(.Object="Element", do="logical"),
    function(.Object, do) return(invisible(doFlag(.Object, "doUpdate", do)))
)
setMethod("doUpdate", signature(.Object="Element", do="missing"),
    function(.Object, do) return(doFlag(.Object, "doUpdate"))
)

# Check or set doPrint flag
# If a “printState” method is available for this element and doPrint==TRUE then execute
# method as last step in each period
# Parameters:
#	do		logical		boolean value to set flag to (optional)
setGeneric("doPrint", function(.Object, do) standardGeneric("doPrint"))
setMethod("doPrint", signature(.Object="Element", do="logical"),
    function(.Object, do) return(invisible(doFlag(.Object, "doPrint", do)))
)
setMethod("doPrint", signature(.Object="Element", do="missing"),
    function(.Object, do) return(doFlag(.Object, "doPrint"))
)

# Check or set doPrintFinal flag
# If a “printState” method is available for this element and doPrintFinal==TRUE then execute
# method as last step before completing scenario simulation.
# Parameters:
#	do		logical		boolean value to set flag to (optional)
setGeneric("doPrintFinal", function(.Object, do) standardGeneric("doPrintFinal"))
setMethod("doPrintFinal", signature(.Object="Element", do="logical"),
    function(.Object, do) return(invisible(doFlag(.Object, "doPrintFinal", do)))
)
setMethod("doPrintFinal", signature(.Object="Element", do="missing"),
    function(.Object, do) return(doFlag(.Object, "doPrintFinal"))
)

# Specify generic methods
# These may be overloaded by specific element objects instantiated
############################################################################################################################
# Called by controller prior to execution of simulation for each scenario replicate specified
# Parameters:
#	universe		Universe		current universe object
#	scenario		Scenario		current scenario object
setGeneric("initialiseScenario", function(element, universe="Universe") standardGeneric("initialiseScenario"))
setGeneric("initialiseReplicate", function(element, universe="Universe") standardGeneric("initialiseReplicate"))

# Called by controller prior to execution of simulation for each scenario specified
setGeneric("initialiseTransition", function(element, universe="Universe") standardGeneric("initialiseTransition"))

# Called by controller after "During" action methods and before "After" action methods
# Can be used to update element state values based upon universe state or transition data
# Parameters:
#	universe		Universe		current universe object
setGeneric("updateState", function(element="Element", universe="Universe") standardGeneric("updateState"))

# Called as an action method in element timesteps if specified
# Parameters:
#	universe		Universe		current universe object
#	action		list			timestep action list
#	periodInfo	list			current period information
setGeneric("printState", function(element, universe="Universe") standardGeneric("printState"))

# All timestep action methods must conform to this method template with a unique method name 
# to replace ‘actionMethod’
# Parameters:
#	universe		Universe	current universe object
#	action		list		timestep action list
#	periodInfo	list		current period information
#setGeneric("actionMethod", function(.Object, universe="environment", action="list", periodInfo="list") standardGeneric("actionMethod"))