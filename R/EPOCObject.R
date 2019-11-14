################################################################################
# EPOC Base Object class
#
# Base (virtual) class which all others inherit from.
# This includes the mechanisms for loading input data and parsing it into slots.
# All input data which does not match a slot is inserted into the params slot list.
# Its methods are available to all other class objects.
# This class inherits from .environment.
#
# S4 Class
# Created 14/5/2009 Troy Robertson
# Modified 20/3/2012 - TR
################################################################################

# Create EOPCObject class
setClass("EPOCObject",
		# All these slots are unchanged during simulation
        representation(inputData		= "list",			# All data from input data file inserted here until its parsed
					   dataPath			= "character",		# Path to input data file
					   signature		= "Signature",		# Signature object
					   epocAttributes	= "list",			# all additional parameters that do not fit into a slot are listed here
					   .msglevel 		= "character",		# EPOC messaging and logging arguments
					   .loglevel 		= "character",
					   .logfile 		= "character",
					   .logtrunc 		= "logical",
					   "VIRTUAL"
		),
        prototype(	   inputData		= list(),
					   dataPath			= "",
					   signature		= new("Signature"),
					   epocAttributes	= list(),
					   .msglevel 		= "normal",			# EPOC messaging and logging arguments
					   .loglevel 		= "quiet",
					   .logfile 		= "EPOC.log",
					   .logtrunc 		= FALSE
		),
		contains						= ".environment"
)

###########################################################################################
# Store data path and the input data sourced from that file
# Assign all signature parameters to a signature object.
# PRIVATE method
setMethod("initialize", signature(.Object="EPOCObject"),
    function(.Object, dataPath=NULL, sublist=NULL, msglevel=NULL, loglevel=NULL, logconn=NULL, ..., .xData=new.env(parent=emptyenv())) {
		if (!is.null(dataPath)) {				# EPOCObject does not need to have an input data file

			.Object@dataPath <- dataPath

			.Object <- callNextMethod(.Object, ..., .xData=.xData)

			# Define and initialise xData environment slots
			# These are defined like this because they are transient data members
			# and so are passed by reference to avoid excessive memcpy
			########################################################################
			.Object[['fileConnections']] <- list()
			#.Object[['connectionManager']] <- .Call("createConnectionManager", PACKAGE="EPOC")
			########################################################################

			# Set reporting args
			if (!is.null(msglevel) && msglevel != "") .Object@.msglevel <- msglevel
			if (!is.null(loglevel) && loglevel != "") .Object@.loglevel <- loglevel
			if (!is.null(logconn)) addFileConnection(.Object, logconn, "logFile")
			#if (!is.null(logconn)) .Call("addConnection", .Object$connectionManager, logconn, "logFile", PACKAGE="EPOC")

			# load input data source
			if (file.exists(dataPath)) {
				if (is.null(sublist) || sublist == 0) {
					.Object@inputData <- source(file=dataPath)[[1]]
				} else {
					.Object@inputData <- source(file=dataPath)[[1]][[sublist]]
				}

				# Set the signature if data is available (check for capitalized listname too)
				.Object@signature <- setSignature(.Object@signature, getNoCase(.Object@inputData, "signature"))
			} else {
				epocErrorMessage(.Object, "Object: ",class(.Object)," unable to find input data file at: ",dataPath, halt=TRUE)
			}
		}

		return(.Object)
	}
)

##
# Right...
# This little hack overrides the existing function for R (see getMethod("[[<-", ".environment")
# The reason it is included is that in R_2.11.1 this method was altered and started causing
# an Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
setMethod("[[<-", "EPOCObject",
	function (x, i, j, ..., value) {
		call <- sys.call()
		call[[2]] <- x@.Data
		eval.parent(call)
		x
	}
)

# Data will be extracted from inputData slot into slots with same names.
# Capitalised attributes will be matched to lowercase slots if available.
# All unmatched attributes will be added to the list in the epocAttributes slot.
# Default slot values should already have been set by this stage
setGeneric("parseInputData", function(.Object) standardGeneric("parseInputData"))
setMethod("parseInputData", signature(.Object="EPOCObject"),
    function(.Object) {

		.Object@epocAttributes <- list()
		if(!is.null(.Object@inputData) && length(.Object@inputData) > 0) {

			dataNames <- names(.Object@inputData)[tolower(names(.Object@inputData)) != "signature"]			# remove signature from vector
			slots <- slotNames(.Object)
			for (i in seq_along(dataNames)) {						# for each available object slot
				matchingSlots <- slots[tolower(dataNames[[i]]) == tolower(slots)]
				if (length(matchingSlots) > 0) {
					# At least one slot found matching dataName
					error <- try(slot(.Object, matchingSlots[[1]]) <- .Object@inputData[[dataNames[[i]]]], silent=TRUE)
				} else {
					# No matching slot found so add to epocAttributes list
					.Object@epocAttributes[dataNames[[i]]] <- list(.Object@inputData[[dataNames[[i]]]])
				}

				# check for errors occurring
				if (exists("error") && class(error) == "try-error") {
					epocMessage(universe, "Attribute '",dataNames[[i]],"' not loaded correctly for object ",.Object@signature@Name.short,". Datatype matching error occurred!")
					epocMessage(universe, trim(error[[1]]))
					rm(error)
				}
			}
		}

		# Now drop unneeded inputData structure
		if (!is.null(.Object@.msglevel) && .Object@.msglevel != "debug") .Object@inputData <- list(NULL)

		return(.Object)
	}
)

# Get the named file connection handle if it is listed else return NULL
# If listed but not open then open the connection first
# If a filepath is passed then open file if it is not listed, open or path/mode has changed
# and then store in named fileConnections list
# Return the connection or NULL if fails
# Parameters:
#	connname	character	name of connection
#	filepath	character 	path to file to open (optional)
#	openmode	character 	c(?a?, ?w?, ?r?) defaults to ?a?
setGeneric("getFileConnection", function(.Object, connname, ...) standardGeneric("getFileConnection"))
setMethod("getFileConnection", signature(.Object="EPOCObject", connname="character"),
    function(.Object, connname, filepath=missing, openmode="a") {
		if (connname == "") return()

		if (connname %in% names(.Object$fileConnections)) {
			if (!is.null(.Object$fileConnections[[connname]]) && class(.Object$fileConnections[[connname]]) == "externalptr") {
				if (!missing(filepath) && !is.null(filepath) && filepath != "") {
					# this call will only reopen if not open or path or mode has changed
					isopen <- .Call("openRcppFileConn", .Object$fileConnections[[connname]], filepath, openmode, PACKAGE="EPOC")
				} else {
					# this call will only reopen if it is closed
					isopen <- .Call("openRcppFileConn", .Object$fileConnections[[connname]], PACKAGE="EPOC")
				}
				if (isopen) return(.Object$fileConnections[[connname]])
			}
			.Object$fileConnections[[connname]] <- NULL
		}
		if (!missing(filepath) && filepath != "") {
			epocDebugMessage(.Object, class(.Object), ": ", .Object@signature@Name.short, " is opening connection '", connname, "' to: ", filepath)
			.Object$fileConnections[[connname]] <- .Call("createRcppFileConn", filepath, openmode, PACKAGE="EPOC")
			if (.Call("isopenRcppFileConn", .Object$fileConnections[[connname]], PACKAGE="EPOC")) return(.Object$fileConnections[[connname]])
		}

		return(NULL)
    }
)
# setGeneric("getFileConnection", function(.Object, conn, ...) standardGeneric("getFileConnection"))
# setMethod("getFileConnection", signature(.Object="EPOCObject", conn="character"),
    # function(.Object, conn, filepath=missing, openmode="a") {
		# if (conn == "") return()
		# #if (missing(filepath) || filepath == "") filepath=conn  # Test if path like then trim off direc as conn, else add getwd() prior
		# cat("getting conn manager to", conn, "\n")
		# if (is.null(.Object$connectionManager) || class(.Object$connectionManager) != "externalptr") {
			# .Object$connectionManager <- .Call("createConnectionManager", PACKAGE="EPOC")
		# }

		# cat("getting connection to", conn, "\n")
		# #if (.Object@signature@Name.short == "Es-KPFM" && conn == "abundNState") stop()
		# fc <- .Call("getConnection", .Object$connectionManager, conn, PACKAGE="EPOC")
		# if (is.null(fc) || class(fc) != "externalptr") {
			# if (missing(filepath) || filepath == "") return(NULL)
			# cat("failed get connection so open filepath", filepath, "for", conn, "\n")
			# fc <- .Call("openConnection", .Object$connectionManager, conn, filepath, openmode, PACKAGE="EPOC")
			# epocDebugMessage(.Object, "EPOCObject: ", .Object@signature@Name.short, " is opening connection '", conn, "' to: ", filepath)
		# }

		# return(fc)
    # }
# )

# Write a line to the specified file connection and return success
# Defaults to append mode, but will depend on the mode in which connectiion was opened.
# If a character name and filepath is passed the connection will be opened (if not open or path/mode differs)
# and the connection will be added to the fileConnections list.
# Parameters:
#	conn	externalptr/character	name of connection or pointer to connection
#	msg			ANY (hopefully)		First part of message to be written (uses toString() to convert)
#	...			ANY (hopefully)		Any further message parts to be pasted together using sep
#	filepath	character			Path for new connection to be made if necessary, only if conn = "character" (optional)
#	openmode	character			Open mode for new connection to be made if necessary, only if conn = "character" (default = "a")
#	sep			character			Separator character for multipart messages (default = "")
setGeneric("writeFileConnection", function(.Object, conn, ...) standardGeneric("writeFileConnection"))
setMethod("writeFileConnection", signature(.Object="EPOCObject", conn="externalptr"),
    function(.Object, conn, msg=NULL, ..., sep="", eol=TRUE) {
		if (is.null(msg)) return(FALSE)
		return(.Call("writeRcppFileConn", conn, msg=toString(paste(msg, ..., sep=sep)), eol=eol, PACKAGE="EPOC"))
	}
)
setMethod("writeFileConnection", signature(.Object="EPOCObject", conn="character"),
    function(.Object, conn, msg=NULL, ..., filepath=NULL, openmode="a", sep="", oel=TRUE) {
		if (is.null(msg)) return(FALSE)
		fc <- getFileConnection(.Object, conn, filepath, openmode)
		if (!is.null(fc)) return(writeFileConnection(.Object, conn=fc, msg=toString(paste(toString(msg), ..., sep=sep, eol=eol))))

		# Failed to open file
		return(FALSE)
	}
)

# Read from the specified file connection and return it
# Defaults to linenum=-1 which reads back complete file.
# If linenum=0 then next line is read back only else if linenum > 0 then that line only will be read back
# The next line as determined by C++ level pointer will be returned unless linenum is specified
# Parameters:
#	conn	externalptr/character	name of connection or pointer to connection
#	linenum		integer				Line number to read (starting at line 1 (defaults to current C++ filepointer)
# Returns:
#	character string if linenum >= 0 otherwise a list of character strings corresponding to lines
setGeneric("readFileConnection", function(.Object, conn, ...) standardGeneric("readFileConnection"))
setMethod("readFileConnection", signature(.Object="EPOCObject", conn="externalptr"),
    function(.Object, conn, linenum=-1) {
		if (linenum >= 0) {
			return(.Call("readlineRcppFileConn", conn, linenum, PACKAGE="EPOC"))
		} else {
			return(.Call("readRcppFileConn", conn, PACKAGE="EPOC"))
		}
	}
)
setMethod("readFileConnection", signature(.Object="EPOCObject", conn="character"),
    function(.Object, conn, linenum=-1, filepath=NULL, openmode="r") {
		fc <- getFileConnection(.Object, conn, filepath, openmode)
		if (!is.null(fc)) return(readFileConnection(.Object, conn=fc, linenum=linenum))

		return(NA)		# Failed to open file
	}
)

# Add the external pointer to file connection to the fileConnections named list using connname
# If connname already exists and is open then it will be closed and replaced.
# Parameters:
#	conn		externalptr		connection
#	connname	character		name of connection
setGeneric("addFileConnection", function(.Object, conn, connname) standardGeneric("addFileConnection"))
setMethod("addFileConnection", signature(.Object="EPOCObject", conn="externalptr", connname="character"),
    function(.Object, conn, connname) {
		if (connname == "") return()

		closeFileConnection(.Object, connname)
		epocDebugMessage(.Object, class(.Object), ": ", .Object@signature@Name.short, " is adding connection '", connname, "'")
		.Object$fileConnections[[connname]] <- conn

		return(.Object)
    }
)

# Close named file connection or external pointer to connection if it is in fileConnections list.
# If conn is not specified then close all listed file connections.
# externalptr to connection still remains after being closed and can be reopened
# Returns whether connection existed and was open and therefore able to be closed
# Parameters:
#	conn	externalptr/character	name of connection or pointer to connection (optional)
setGeneric("closeFileConnection", function(.Object, conn) standardGeneric("closeFileConnection"))
setMethod("closeFileConnection", signature(.Object="EPOCObject", conn="externalptr"),
    function(.Object, conn) {
		success <- .Call("closeRcppFileConn", conn, PACKAGE="EPOC")
		epocDebugMessage(.Object, class(.Object), ": ", .Object@signature@Name.short, " is closing connection")
		return(success)
	}
)
setMethod("closeFileConnection", signature(.Object="EPOCObject", conn="character"),
    function(.Object, conn) {
		if (conn == "") return(FALSE);
		if (!(conn %in% names(.Object$fileConnections))) return(FALSE)
		success <- .Call("closeRcppFileConn", .Object$fileConnections[[conn]], PACKAGE="EPOC")
		epocDebugMessage(.Object, class(.Object), ": ", .Object@signature@Name.short, " is closing connection '", conn, "'")
		return(success)
	}
)
setMethod("closeFileConnection", signature(.Object="EPOCObject", conn="missing"),
    function(.Object, conn) {
		# close all connections
		closing <- FALSE
		allclosed <- TRUE
		for (fc in seq_along(.Object$fileConnections)) {
			closing <- TRUE
			if (!is.null(fc) && class(fc) == "externalptr") {
				if (!.Call("closeRcppFileConn", fc, PACKAGE="EPOC")) allclosed <- FALSE
			}
		}
		if (closing) epocDebugMessage(.Object, class(.Object), ": ", .Object@signature@Name.short, " is closing all connections")
		return(allclosed)
	}
)

# Return the elements signature object
# If item is passed then return the value at that slot instead
# Parameters:
#	item		character		name of signature slot to return (optional)
setGeneric("getSignature", function(.Object, item) standardGeneric("getSignature"))
setMethod("getSignature", signature(.Object="EPOCObject", item="character"),
    function(.Object, item) return(getSignatureItem(.Object@signature, item))
)
setMethod("getSignature", signature(.Object="EPOCObject", item="missing"),
    function(.Object, item) return(.Object@signature)
)

# Return a list of EPOC attribute names held by this object
setGeneric("getAttributeNames", function(.Object) standardGeneric("getAttributeNames"))
setMethod("getAttributeNames", signature(.Object="EPOCObject"),
    function(.Object) return(names(.Object@epocAttributes))
)

# Return the objects epocAttributes list
# If list item name is passed then return the value at that list psoition instead
# Parameters:
#	item		character		name of list item to return
setGeneric("getAttribute", function(.Object, item) standardGeneric("getAttribute"))
setMethod("getAttribute", signature(.Object="EPOCObject", item="character"),
    function(.Object, item) {
		if (item != "") {
			if (item %in% names(.Object@epocAttributes)) return(.Object@epocAttributes[[item]])
			# Just check if it is a slot
			if (item %in% slotNames(.Object)) {
				epocErrorMessage(.Object, "Object: ", .Object@signature@Name.short, " only contains requested Attribute: ", item, " as a Slot", halt=TRUE)
			}
		}
		epocErrorMessage(.Object, "Object: ", .Object@signature@Name.short, " is missing requested Attribute: ", item, halt=TRUE)
	}
)
setMethod("getAttribute", signature(.Object="EPOCObject", item="missing"),
    function(.Object, item) {
		return(.Object@epocAttributes)
	}
)

# Set the value of item in epocAttributes list
# Will append to epocAttributes list if not already in existence
# Parameters:
#	item		character	name of list item to insert value as
#	value		R			value to assign to the list
setGeneric("setAttribute", function(.Object, item, value) standardGeneric("setAttribute"))
setMethod("setAttribute", signature(.Object="EPOCObject", item="character", value="ANY"),
    function(.Object, item, value) {
		if (item != "") .Object@epocAttributes[item] <- list(value)
		return(.Object)
	}
)

# Return a list of available slot names held by this object
setGeneric("getSlotNames", function(.Object) standardGeneric("getSlotNames"))
setMethod("getSlotNames", signature(.Object="EPOCObject"),
    function(.Object) return(slotNames(.Object))
)

# Return value at item passed if item exists as a slot
# Note that some data members are now stored in .xData environment
# Parameters:
#	item		character	slot from which to return data
setGeneric("getSlot", function(.Object, item) standardGeneric("getSlot"))
setMethod("getSlot", signature(.Object="EPOCObject", item="character"),
    function(.Object, item) {
		if (item != "") {
			if (item %in% slotNames(.Object)) {
				return(slot(.Object, item))
			} else if (item %in% ls(.Object)) {
				return(.Object[[item]])
			}
			# TR Could have an else clause which checks epocAttributes list if item doesn't exist
			epocErrorMessage(.Object, "Object: ", .Object@signature@Name.short, " is missing requested Slot : ", item, halt=TRUE)
		}

		return(NA)
	}
)

# Set value at item passed if item exists as a slot
# Parameters:
#	item		character	slot at which to assign value
#	value		R			value to be assigned
setGeneric("setSlot", function(.Object, item, value) standardGeneric("setSlot"))
setMethod("setSlot", signature(.Object="EPOCObject", item="character", value="ANY"),
    function(.Object, item, value) {
		if (item != "" && item %in% slotNames(.Object)) {
			slot(.Object, item) <- value
		} else if (item != "" && item %in% ls(.Object)) {
			.Object[[item]] <- value
		} else {
			# TR Could have an else clause which appends it to epocAttributes list if item not found
			epocErrorMessage(.Object, "Object: ", .Object@signature@Name.short, " is missing assigned Slot : ", item, halt=TRUE)
		}

		return(.Object)
	}
)

# Print verbose messages to stdout and log file as dictated by .msglevel="quiet"
# and .loglevel="quiet"
setGeneric("epocMessage", function(.Object, msg, ...) standardGeneric("epocMessage"))
setMethod("epocMessage", signature(.Object="EPOCObject", msg="ANY"),
	function(.Object, msg="", ..., sep="") {
		if (!is.null(.Object@.loglevel) && .Object@.loglevel != "quiet") {
			.logEPOCMessage(.Object, msg, ..., sep=sep)
		}
		if (is.null(.Object@.msglevel) || .Object@.msglevel != "quiet") {
			message(paste(msg, ..., sep=sep))
			flush.console()
		}
	}
)

# Print verbose messages to stdout and log file as dictated by .msglevel="verbose" or "debug"
# and .loglevel="verbose" or "debug"
setGeneric("epocVerboseMessage", function(.Object, msg, ...) standardGeneric("epocVerboseMessage"))
setMethod("epocVerboseMessage", signature(.Object="EPOCObject", msg="ANY"),
	function(.Object, msg="", ..., sep="") {
		if (!is.null(.Object@.loglevel) && (.Object@.loglevel == "verbose" || .Object@.loglevel == "debug")) {
			.logEPOCMessage(.Object, msg, ..., sep=sep)
		}
		if (!is.null(.Object@.msglevel) && (.Object@.msglevel == "verbose" || .Object@.msglevel == "debug")) {
			message(paste(msg, ..., sep=sep))
			flush.console()
		}
	}
)

# Print debug messages to stdout and log file as dictated by .msglevel="debug"
# and .loglevel="debug"
setGeneric("epocDebugMessage", function(.Object, msg, ...) standardGeneric("epocDebugMessage"))
setMethod("epocDebugMessage", signature(.Object="EPOCObject", msg="ANY"),
	function(.Object, msg="", ..., sep="") {
		if (!is.null(.Object@.loglevel) && .Object@.loglevel == "debug") {
			.logEPOCMessage(.Object, msg, ..., sep=sep)
		}
		if (!is.null(.Object@.msglevel) && .Object@.msglevel == "debug") {
			message(paste(msg, ..., sep=sep))
			flush.console()
		}
	}
)

# Print error message without consulting reporting params
setGeneric("epocErrorMessage", function(.Object, msg, ...) standardGeneric("epocErrorMessage"))
setMethod("epocErrorMessage", signature(.Object="EPOCObject", msg="ANY"),
	function(.Object, msg="", ..., sep="", halt=FALSE) {
		.logEPOCMessage(.Object, msg, ..., sep=sep, flag="ERROR: ")
		if (halt) {
			stop(paste(msg, ..., sep=sep))
		} else {
			warning(paste("ERROR: ", msg, ..., sep=sep))
			flush.console()
		}
	}
)

# Helper funtion to do the call to Rcpp to write to the logfile
setGeneric(".logEPOCMessage", function(.Object, msg, ...) standardGeneric(".logEPOCMessage"))
setMethod(".logEPOCMessage", signature(.Object="EPOCObject", msg="ANY"),
	function(.Object, msg="", ..., sep="", flag="") {
		logconn <- .Object$fileConnections$logFile
		if (!is.null(logconn) && class(logconn) == "externalptr" && .Call("isopenRcppFileConn", logconn, PACKAGE="EPOC")) {
			.Call("writeRcppFileConn", logconn, toString(paste(flag, toString(msg), ..., sep=sep)), TRUE, PACKAGE="EPOC")
		}
	}
)

# Return the objects simple one-line signature
setGeneric("getSignatureLine", function(.Object, ...) standardGeneric("getSignatureLine"))
setMethod("getSignatureLine", signature(.Object="EPOCObject"),
    function(.Object, display=FALSE) {
		sig <- getSimpleSignature(.Object@signature)
		if (display) epocMessage(.Object, sig)
		return(sig)
	}
)

# Return a multiline string object signature
setGeneric("getSignatureMulti", function(.Object, ...) standardGeneric("getSignatureMulti"))
setMethod("getSignatureMulti", signature(.Object="EPOCObject"),
    function(.Object, display=FALSE) {
		sig <- getFullSignature(.Object@signature)
		if (display) epocMessage(.Object, sig)
		return(sig)
	}
)
