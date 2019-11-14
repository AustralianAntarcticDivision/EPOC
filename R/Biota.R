################################################################################
# EPOC Biota class
#
# Base class for all EPOC Biota Elements
# S4
# Created 3/2/2009 Troy Robertson
################################################################################
# Extend base class
setClass("Biota",
    representation(
        nYrs        = "numeric",
		"VIRTUAL"
	),
    prototype(
        nYrs        = 0
    ),
    contains="Element"
)

setMethod("initialize", signature(.Object="Biota"),
    function(.Object, dataPath, ...) {
        # first call parents (Element) initialize method
		#if (!is.null(logconn)) print("Biota got logconn")
        .Object <- callNextMethod(.Object, dataPath, ...)
		
		return(.Object)
	}
)