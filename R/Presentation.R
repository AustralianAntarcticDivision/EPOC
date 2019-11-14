################################################################################
# EPOC Presentation class
#
# Base class for all EPOC Presentation Elements
# S4
# Created 2/3/2012 Troy Robertson
################################################################################
# Extend base class
setClass("Presentation",
	representation("VIRTUAL"),
    contains="Element"
)

setMethod("initialize", signature(.Object="Presentation"),
    function(.Object, dataPath, ...) {
        # first call parents (Element) initialize method
        .Object <- callNextMethod(.Object, dataPath, ...)
		
		return(.Object)
	}
)