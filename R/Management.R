################################################################################
# EPOC Management class
#
# Base class for all EPOC Management Elements
# S4
# Created 2/6/2009 Troy Robertson
################################################################################
# Extend base class
setClass("Management",
	representation("VIRTUAL"),
    contains="Element"
)

setMethod("initialize", signature(.Object="Management"),
    function(.Object, dataPath, ...) {
        # first call parents (Element) initialize method
        .Object <- callNextMethod(.Object, dataPath, ...)
		
		return(.Object)
	}
)