################################################################################
# EPOC Activity class
#
# Base class for all EPOC Activity Elements
# S4
# Created 28/5/2009 Troy Robertson
################################################################################
# Extend base class
setClass("Activity",
	representation("VIRTUAL"),
    contains="Element"
)

setMethod("initialize", signature(.Object="Activity"),
    function(.Object, dataPath, ...) {
        # first call parents (Element) initialize method
        .Object <- callNextMethod(.Object, dataPath, ...)
		
		return(.Object)
	}
)