################################################################################
# EPOC Support Methods
#
# This file contains a number of support methods which are not related to EPOC classes 
# but provide support functionality during the course of their business. 
#
# Created 8/7/2009 Troy Robertson
# Modified 20/3/2012 - TR
################################################################################

# Convert from day and month integers to a single day in year integer
# Parameters:
#	day		numeric		day in month
#	month	numeric		month in year
setGeneric("dayFromDate", function(day, month) standardGeneric("dayFromDate"))
setMethod("dayFromDate", signature(day="numeric", month="numeric"),
    function(day, month) {
		calDay <- 0
		daysInMonths <- c(31,28,31,30,31,30,31,31,30,31,30,31)
		if (month < 1 || month > 12) return(NA)
		if (day < 1 || day > daysInMonths[month]) return(NA)
		if (month > 1) calDay <- sum(daysInMonths[1:(month-1)])
		calDay <- calDay + day
		
		return(calDay)
	}
)

# Return a fixed field character value
# parameters: 
#	value	numeric/logical/character 		value
#    width  	integer		desired length of string
#    sig    	integer		number of decimal places
#    dec    	integer		if NULL then unlimited otherwise limit to number
setGeneric("fixedFieldLength", function(value, width="integer", sig="integer", dec="integer") standardGeneric("fixedFieldLength"))
setMethod("fixedFieldLength", "numeric",
    function(value, width=10, sig=5, dec=NULL) {
		
		if (is.na(value)) return(fixedFieldLength("NA", width))
			
		# if limit decimal places then round
		if(!is.null(dec)) value <- round(value, dec)

		textOut <- ""
		
		# is negative
		if (value < 0) {
			width <- width - 1
			textOut <- paste(textOut, "-", sep="")
			value <- abs(value)
        }

		e <- trunc(log10(value))
		sNum <- 10^(log10(value) - e)

		if (e < 0) { # if dealing only with fraction therefore only scientific
			if (sig <= (width - 2 + e)) {
				textOut <- paste(textOut, as.character(round(sNum,(width - 2 + e)) * 10^e), sep="")
            } else {
				chE <- paste(ifelse(e > -10, "E-0", "E-"), as.character(abs(e)), sep="")
				textOut <- paste(textOut, as.character(round(sNum, (width - 6))), chE, sep="")
            }
        } else { # greater than 1
			if (e <= (width - 2)) {
                textOut <- paste(textOut, as.character(round(sNum, width - 2) * 10^e), sep="")
            } else { # else E>W-1
				chE <- paste(ifelse(e < 10,"E0","E"), as.character(e), sep="")
				textOut <- paste(textOut, as.character(round(sNum,(width - 5))), chE, sep="")
            }
        }

		# now return Width to original value if Value was negative
        if (value < 0) width <- width + 1
	
		return(format(textOut, justify = "right", width = width))
	}
)
setMethod("fixedFieldLength", "logical",
    function(value, width=10, sig, dec) {
		if (is.na(value)) {
			textOut <- "NA"
		} else if (value == TRUE) {
			textOut <- "TRUE"
		} else if (value == FALSE) {
			textOut <- "FALSE"
		}
		
		return(format(textOut, justify = "right", width = width))
	}
)
setMethod("fixedFieldLength", "character",
    function(value, width=10, sig, dec) {
		textOut <- ifelse(nchar(value) > width, substr(value, 1, width), value)
		
		return(format(textOut, justify = "left", width = width))
	}
)

# Return the list element from the named list based on a non-case sensitive name
# Returns NULL if not in list
# parameters:
#	x			list		named list
#	element		character	case insensitive name
setGeneric("getNoCase", function(x, element) standardGeneric("getNoCase"))
setMethod("getNoCase", signature(x="list",element="character"),
	function(x, element=NULL) {
		if (is.null(x)) return(NULL)
		if (is.null(element) || element == "") return(x)
		if (length(x[tolower(names(x))==tolower(element)]) == 0) return(NULL)
		return (x[tolower(names(x))==tolower(element)][[1]])
	}
)

# Return a character string with values passed separated by sep
# Parameters:
#	values	vector/list/matrix		named list
#	...		vector/list/matrix		more values
#	sep			character			character separator (default = ",")
setGeneric("asCSVCharacter", function(values, ...) standardGeneric("asCSVCharacter"))
setMethod("asCSVCharacter", signature(values="vector"),
	function(values, ..., sep=",") {
		return(do.call("paste", c(as.list(c(values, ...)), sep=sep)))
	}
)
setMethod("asCSVCharacter", signature(values="list"),
	function(values, ..., sep=",") {
		return(do.call("paste", c(c(values, ...), sep=sep)))
	}
)
setMethod("asCSVCharacter", signature(values="matrix"),
	function(values, ..., sep=",") {
		return(do.call("paste", c(c(values, ...), sep=sep)))
	}
)
# Return a character(default) vector from values passed split by sep
# Treats a list as separate items to be dealt with separately and a list returned
# Parameters:
#	values		character/list		named list
#	type		character			type of vector to return (default = "character")
#	sep			character			character separator (default = ",")
setGeneric("fromCSVCharacter", function(values, ...) standardGeneric("fromCSVCharacter"))
setMethod("fromCSVCharacter", signature(values="character"),
	function(values, type="character", sep=",") {
		resultvec <- unlist(strsplit(values, split=sep))
		if (type == "integer") return(as.integer(resultvec))
		if (type == "double" || type == "numeric") return(as.double(resultvec))
		return(resultvec)
	}
)
setMethod("fromCSVCharacter", signature(values="list"),
	function(values, type="character", sep=",") {
		results <- list()
		for (val in seq_along(values)) {
			resultvec <- unlist(strsplit(values[[val]], split=sep))
			if (type == "numeric") {
				results[[length(results)+1]] <- as.numeric(resultvec)
			} else if (type == "double" || type == "numeric") {
				results[[length(results)+1]] <- as.double(resultvec)
			} else {
				results[[length(results)+1]] <- resultvec
			}
		}
		
		return(results)
	}
)
	
imapply <- function(FUN, X, ...) {
 	index <- seq(length.out = length(X))
    return(mapply(FUN, X, INDEX=index, SIMPLIFY=FALSE, MoreArgs=list(...)))
}
		
# Compiles body using package 'inline'
# Sets a generic method (if needed) and an EPOC Action method for class element
# Rip offs of inline functions setCMethod() which was failing to pass on ... args
# in its internal cfunction() call, and also adds default compiler flags for EPOC 
# inc <- c("#include <EPOC.h>")
# cppa <- c(paste("-I", system.file("include", package="EPOC"), sep=""))
# liba <- c("-lEPOC", paste("-L", system.file("libs", "i386", package="EPOC"), sep=""))
# sig <- signature(sexpObj="Krill", universe="Universe")
setGeneric( "setEPOCCPPMethod", function(method, element, body, ...) standardGeneric("setEPOCCPPMethod") )
setMethod( "setEPOCCPPMethod", signature(method="character", element="character", body="character"),
	function(method, element, body, 
			includes=c("#include <EPOC.h>"),
			otherdefs="",
			language=c("C++", "C", "Fortran", "F95", "ObjectiveC", "ObjectiveC++"),
			verbose=NULL, 
			convention=c(".Call", ".C", ".Fortran"),
			Rcpp=TRUE,
			cppargs=c(paste("-I", system.file("include", package="EPOC"), sep="")),
			libargs=c("-lEPOC", paste("-L", system.file("libs", "i386", package="EPOC"), sep="")),
			where=topenv(.GlobalEnv),
			...)
	{
		   
		sig <- signature(element=element, universe="Universe")
		
		# Set EPOC verbosity
		msglevel <- getOption("epocmsglevel")
		if (is.null(verbose)) verbose <- (!is.null(msglevel) && (msglevel == "verbose" || msglevel == "debug"))
		if (!verbose) Sys.setenv(CYGWIN="nodosfilewarning")
		
		if (!require("inline")) stop("Compilation of C++ methods for EPOC requires inclusion of 'inline' package!")
		if (is.null(msglevel) || msglevel != "quiet") message("Compiling C++ method '", method, "' for Element '", element, "'...")
		
		#fns <- cfunction(sig, body, includes, otherdefs, language, verbose, convention)
		fn <- cfunction(sig, body, includes, otherdefs, language, verbose, convention, Rcpp, cppargs, libargs=libargs, ...)

		# Let's try to create generic
		generic <- paste( "if (!isGeneric(\"", method, "\"))\n",
						  "setGeneric(\"", method, "\", function(element, universe) ",
						  "standardGeneric(\"", method, "\"),where=where)", sep="")
		eval(parse(text=generic))
		# Now the method
		setMethod(method, sig, fn, where=where)
		if ( verbose ) {
			message("\nThe following method is now defined:")
			showMethods(method)
		}
	}
)

# Used to create an EPOC Action wrapper method around a .Call to libfn in shared library at libpath
# Sets a generic (if needed) and method for class element
setGeneric( "setEPOCLibMethod", function(method, element, libpath, ...) standardGeneric("setEPOCLibMethod") )
setMethod( "setEPOCLibMethod", signature(method="character", element="character", libpath="character"),
	function(method, element, libpath, libfn=method) {
		where=topenv(.GlobalEnv)
		# remove extension c(".dll", ".so", ",sl")
		libpath <- sub("\\.(dll|DLL|so|SO|sl|SL)$", "", libpath)
		libname <- basename(libpath)
		
		error <- try(libinfo <- dyn.load(libpath), silent=TRUE)
		if (exists("error") && class(error) == "try-error") {
			stop("Element '", element, "' failed to find required action library at: ", libpath, "!\n", sep="")
		}
		if (!is.loaded(libfn, PACKAGE=libinfo[1]$name)) {
			stop("Element '", element, "' failed to find required action function '", libfn, "'\n",
				 "in action library at: ", libpath, "!\n", sep="")
		}
		
		sig <- signature(element=element, universe="Universe")
		fn <- function(element, universe) {}
		body(fn) <-	parse(text=paste("return(.Call(\"", libfn, "\", element, PACKAGE=\"", libname, "\"))", sep=""))
		
		# Set generic method
		generic <- paste( "if (!isGeneric(\"", method, "\"))\n",
						  "setGeneric(\"", method, "\", function(element, universe) ",
						  "standardGeneric(\"", method, "\"), where=where)", sep="")
		eval(parse(text=generic))
		# Set method wrapper
		setMethod(method, sig, fn, where=where)
		
		return(invisible(libinfo[1]$name))
	}
)