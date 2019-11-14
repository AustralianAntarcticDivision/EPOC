################################################################################
# EPOC Output class
#
# Base class for all EPOC Output Elements
# S4
# Created 2/3/2012 Troy Robertson
################################################################################
# Extend base class
setClass("Output",
	representation("VIRTUAL"),
    contains="Element"
)

setMethod("initialize", signature(.Object="Output"),
    function(.Object, dataPath, ...) {
        # first call parents (Element) initialize method
        .Object <- callNextMethod(.Object, dataPath, ...)
		
		return(.Object)
	}
)