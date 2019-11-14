################################################################################
# EPOC Signature class (S4)
#
# Class object used to store EPOC Object signature information.
# This class is one of few that do not inherit from EPOCObject.
# 
# Created 3/2/2009 Troy Robertson
################################################################################

setClass("Signature", 
	representation(
		ClassName    = "character",				# Name of class to instantiate this object as
		ID           = "numeric",				# Unique numeric identifier for object
		ID.absolute  = "numeric", 				# Same as ID, for output
		Name.full    = "character",				# Full name of object
		Name.short   = "character",				# Abbreviated name for this object
		Morph        = "character",
		Revision     = "character",				# Revision number for object
		Authors      = "character",				# Object authors
		Last.edit    = "character"),			# Date of last edit
	prototype(
		ClassName    = NULL,
		ID           = NULL,
		ID.absolute  = NULL,
		Name.full    = NULL,
		Name.short   = NULL,
		Morph        = NULL,
		Revision     = NULL,
		Authors      = NULL,
		Last.edit    = NULL)
)

# Set signature data member values using the named list passed as parameter
# Parameters:
#	signatureList		list		named list of signature values as read from input data files
setGeneric("setSignature", function(.Object, signatureList) standardGeneric("setSignature"))
setMethod("setSignature", signature(.Object="Signature", signatureList="list"),
    function(.Object, signatureList) {
		for (slotName in slotNames(.Object)) {
			value <- getNoCase(signatureList, slotName)
			if (!is.null(value)) slot(.Object, slotName) <- value
		}
		if (is.null(.Object@ID.absolute) && !is.null(.Object@ID)) .Object@ID.absolute <- .Object@ID
		
		return(.Object)
	}
)

# Return the value stored at the slot with name = item 
# Parameters:
#	item		character	slot name from which to return value
setGeneric("getSignatureItem", function(.Object, item="character") standardGeneric("getSignatureItem"))
setMethod("getSignatureItem", signature(.Object="Signature"),
    function(.Object, item) {
		if (!is.null(item) && item != "" && item %in% slotNames(.Object)) {
			return(slot(.Object, item))
		}
	}
)

# Return a one-line signature for object
setGeneric("getSimpleSignature", function(.Object) standardGeneric("getSimpleSignature"))
setMethod("getSimpleSignature", signature(.Object="Signature"),
    function(.Object) {
		return(paste(.Object@Morph, "-", .Object@ClassName, "-", .Object@Name.short))
	}
)

# Return a character vector containing multiline signature details
setGeneric("getFullSignature", function(.Object) standardGeneric("getFullSignature"))
setMethod("getFullSignature", signature(.Object="Signature"),
    function(.Object) {
		msg <- paste("        ID: ", as.character(.Object@ID), "\n",
					" Full Name: ", .Object@Name.full, "\n",
					"Short Name: ", .Object@Name.short, "\n", sep="")
		if (!is.null(.Object@Morph)) msg <- paste(msg, "     Morph: ", .Object@Morph, "\n", sep="")
		msg <- paste(msg, "  Revision: ", .Object@Revision, "\n",
						  "   Authors: ", .Object@Authors, "\n",
						  " Last Edit: ", .Object@Last.edit, sep="")
						  
		return(msg)
	}
)