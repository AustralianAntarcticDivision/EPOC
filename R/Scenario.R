################################################################################
# EPOC Scenario class (S4)
#
# Scenario class inheriting from EPOCObject.  
# The scenario class holds data relating to a single scenario to be conducted during the simulation. 
# Scenarios will be conducted sequentially.
# 
# Created 13/2/2009 Troy Robertson
# Modified 20/3/2012 - TR
################################################################################

# Define Scenario class with data members (slots)
setClass("Scenario",
    representation(
		scenarioNum 		= "numeric",		# unique scenario number
		yearStart 			= "numeric",		# first year of scenario
		yearEnd				= "numeric",		# final year of scenario
		yearsN				= "numeric",		# number of years in scenario
		firstFishingYear	= "numeric",		# first year of fishing
		lastFishingYear		= "numeric",		# last year of fishing
		scenarioDir			= "character",		# scenario directory
		replicateCnt		= "numeric"			# number of replicates to process
	),
	prototype(),
	contains="EPOCObject"
)

setMethod("initialize", signature(.Object="Scenario"),
   function(.Object, universe, dataPath, scenarioNum) {

		# first call parents (Element) initialize method
        .Object <- callNextMethod(.Object, dataPath, scenarioNum)
	
		# Pull input data into class slots
		.Object <- parseInputData(.Object)

		.Object@scenarioNum <- scenarioNum
		
		# Create scenario directory
		if (!file.exists(.Object@scenarioDir)) dir.create(.Object@scenarioDir)
		
        return(.Object)
    }
)

# For each element, initialise scenario state files
# Parameters:
#	universe		Universe		current universe object
setGeneric("initialiseElementScenarios", function(.Object, universe) standardGeneric("initialiseElementScenarios"))
setMethod("initialiseElementScenarios", signature(.Object="Scenario", universe="Universe"),
    function(.Object, universe) {

		# Create scenario directory
		if (!file.exists(.Object@scenarioDir)) dir.create(.Object@scenarioDir)
		
		for (ec in seq_along(universe$modules)){
			for (e in seq_along(universe$modules[[ec]])){

				# Find the class of the element so that an initialiseReplicate method can be called if it exists
				if (hasMethod("initialiseScenario", signature(class(universe$modules[[ec]][[e]])[[1]], "Universe"))) {
					universe$modules[[ec]][[e]] <- initialiseScenario(universe$modules[[ec]][[e]], universe)
				}

				# Set the current scenario output directory for each element
				universe$modules[[ec]][[e]]@currentScenarioDir <- .Object@scenarioDir 
				
				# initialise files to be used output State if needed (files will be appended to)
				# use of these files will be nominated by an action in the element
				# or in an Output element for global State output
				element <- universe$modules[[ec]][[e]]
				outfiles <- element@epocAttributes$OutputFiles
				if (!is.null(outfiles)&& is.list(outfiles)) {
					for (p in seq_along(outfiles)) {
						if (element@epocAttributes$OutputFlags[[p]]) {
							tmpPath <- file(file.path(element@currentScenarioDir, outfiles[[p]]), "w")
							close(tmpPath)
						}
					}
				}
			} 
		} 
		
		return(universe)
	} 
)

# For each element, Call the elements initialiseReplicate method if available
# Parameters:
#	universe		Universe		current universe object
setGeneric("initialiseElementReplicate", function(.Object, universe) standardGeneric("initialiseElementReplicate"))
setMethod("initialiseElementReplicate", signature(.Object="Scenario", universe="Universe"),
    function(.Object, universe) {

		for (ec in seq_along(universe$modules)){
			for (e in seq_along(universe$modules[[ec]])){
				# Find the class of the element so that an initialiseReplicate method can be called if it exists
				if (hasMethod("initialiseReplicate", signature(class(universe$modules[[ec]][[e]])[[1]], "Universe"))) {
					universe$modules[[ec]][[e]] <- initialiseReplicate(universe$modules[[ec]][[e]], universe)
				}
			}
		}

		return(universe)
	}
)

# For each module element, call the initialiseTransition method if one exists
# Parameters:
#	universe		Universe		current universe object
setGeneric("initialiseElementTransitions", function(.Object, universe) standardGeneric("initialiseElementTransitions"))
setMethod("initialiseElementTransitions", signature(.Object="Scenario", universe="Universe"),
    function(.Object, universe) {

		for (ec in seq_along(universe$modules)){
			for (e in seq_along(universe$modules[[ec]])){
				if (hasMethod("initialiseTransition", signature(class(universe$modules[[ec]][[e]])[[1]], "Universe"))) {
					universe$modules[[ec]][[e]] <- initialiseTransition(universe$modules[[ec]][[e]], universe)
				}
			}
		}

		return(universe)
	}
)
