################################################################################
# EPOC Environment class
#
# Base class for all EPOC Environment Elements
# S4
# Created 3/2/2009 Troy Robertson
################################################################################

# Extend base class
setClass("Environment", 
	representation("VIRTUAL"),
    contains="Element"
)

setMethod("initialize", signature(.Object="Environment"),
    function(.Object, dataPath, ...) {
        # first call parents (Element) initialize method
        .Object <- callNextMethod(.Object, dataPath, ...)
		
		return(.Object)
	}
)