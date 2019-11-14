/*********************************************************************************
 * Copyright (C) 2012	Australian Antarctic Division
 *
 * Author: Troy Robertson
 *
 * EPOCObject.cpp
 * This file forms part of the API library for EPOC and is included in the 
 * EPOC R package along with:
 * EPOC.h
 * EPOC.cpp
 * EPOCSignature.cpp
 * EPOCObject.cpp
 * EPOCElement.cpp
 * EPOCUniverse.cpp
 * EPOCPeriod.cpp
 * EPOCController.cpp
 *
 * EPOC is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *********************************************************************************/
//#include <Rcpp.h>
#include <string>

#ifndef EPOC_hpp
#include <EPOC.h>
#endif

using namespace Rcpp;

/********* EPOCObject class functions *********/
/**
 * Return a list of all EPOC attribute list names
 **/
RcppExport SEXP getAttributeNames(SEXP epocObj) {
BEGIN_RCPP
	Rcpp::S4 s4Obj = asS4EO(epocObj);
	Rcpp::List attrList = s4Obj.slot("epocAttributes");
	Rcpp::List namesList = attrList.names();
	
	return namesList;
END_RCPP
}

/**
 * Return the elements epocAttributes list
 * If list element name is passed then return the value at that slot instead
 * Parameters:
 *	item		character		name of list item to return
 * setGeneric("getAttribute", function(.Object, item="character") standardGeneric("getAttribute"))
 **/
RcppExport SEXP getAttribute(SEXP epocObj, SEXP item) {
BEGIN_RCPP
	std::string itemName = toStr(item);
	Rcpp::S4 s4Obj = asS4EO(epocObj);
	Rcpp::List attrList = s4Obj.slot("epocAttributes");
	SEXP result = R_NilValue;
	//Rcpp::List attrNames = attrList.names();
	
	if (itemName == "") return attrList;
	try {
		result = attrList[itemName.c_str()];
	} catch(...) {
		if ( s4Obj.hasSlot(itemName.c_str()) ) {
			throw std::invalid_argument((getClassName(s4Obj)+": "+getSigName(s4Obj)
					+" only contains requested Attribute: "+itemName+" as a Slot").c_str());
		} else {
			throw std::invalid_argument((getClassName(s4Obj)+": "+getSigName(s4Obj)
					+" is missing requested Attribute: "+itemName).c_str());
		}
	}

	return result;
END_RCPP
}
/** Wrapper function to allow string specification of attribute to be returned **/
SEXP getAttribute(SEXP epocObj, const char* item = "") {
	return getAttribute(epocObj, Rcpp::wrap(item));
}

/**
 * Set the value of item in epocAttributes list
 * Will append to epocAttributes list if not already in existence
 * Parameters:
 *	item		character	name of list item to insert value as
 *	val			R			value to assign to the list
 * setGeneric("setAttribute", function(.Object, item="character", val) standardGeneric("setAttribute"))
 **/
RcppExport SEXP setAttribute(SEXP epocObj, SEXP item, SEXP val) {
BEGIN_RCPP
	std::string itemName = toStr(item);
	Rcpp::S4 s4Obj = asS4EO(epocObj);
	Rcpp::List attrList = s4Obj.slot("epocAttributes");

	if (itemName == "") throw std::invalid_argument((getClassName(s4Obj)+": "+getSigName(s4Obj)
								+" had data assigned without specifying Attribute").c_str());
	if (val == NULL) throw std::invalid_argument((getClassName(s4Obj)+": "+getSigName(s4Obj)
								+" had no data assigned to Attribute: "+itemName).c_str());
	attrList[itemName.c_str()] = val;
	
	return s4Obj;
END_RCPP
}
/** Wrapper function to allow string specification of attribute to be set **/
SEXP setAttribute(SEXP epocObj, const char* item, SEXP val) {
	return setAttribute(epocObj, Rcpp::wrap(item), val);
}
/** Wrapper function to allow string specification of attribute and value **/
SEXP setAttribute(SEXP epocObj, const char* item, const char* val) {
	return setAttribute(epocObj, Rcpp::wrap(item), Rcpp::wrap(val));
}

/**
 * Return a list of all slots available in object
 **/
RcppExport SEXP getSlotNames(SEXP epocObj) {
BEGIN_RCPP
	Rcpp::S4 s4Obj = asS4EO(epocObj);
	//TODO
	return Rcpp::List::create();
END_RCPP
}
	
/**
 * Return value at slt passed if slt exists as a slot
 * Note that some data members are now stored in environment
 * Parameters:
 *	slt		character	slot from which to return data
 * setGeneric("getSlot", function(.Object, slt="character") standardGeneric("getSlot"))
 **/
RcppExport SEXP getSlot(SEXP epocObj, SEXP slt) {
BEGIN_RCPP
	std::string slotName = toStr(slt);
	Rcpp::S4 s4Obj = asS4EO(epocObj);
	SEXP result = R_NilValue;
	
	if ( slotName == "" ) {
		throw std::invalid_argument((getClassName(s4Obj)+": "+getSigName(s4Obj)+" had data requested without specifying Slot").c_str());
	}
	if ( s4Obj.hasSlot(slotName.c_str()) ) {
		result = s4Obj.slot(slotName.c_str());
	} else if (s4Obj.is("Element")) {
		try {	// obtain from .xData list instead
			result = getXData(epocObj, "Element", slotName.c_str());
		} catch(...) {
			throw std::invalid_argument((getClassName(s4Obj)+": "+getSigName(s4Obj)+" is missing requested Slot: "+slotName).c_str());
		}
	}
	
	return result;
END_RCPP
}
/** Wrapper function to allow string specification of slot to return **/
SEXP getSlot(SEXP epocObj, const char* slt) {
	return getSlot(epocObj, Rcpp::wrap(slt));
}

/**
 * Set value at slt passed if slt exists as a slot
 * Parameters:
 *	slt		character	slot at which to assign value
 *	val		R			value to be assigned
 * setGeneric("setSlot", function(.Object, slt="character", val) standardGeneric("setSlot"))
 **/
RcppExport SEXP setSlot(SEXP epocObj, SEXP slt, SEXP val) {
BEGIN_RCPP
	std::string slotName = toStr(slt);
	Rcpp::S4 s4Obj = asS4EO(epocObj);
	
	if ( slotName == "" ) {
		throw std::invalid_argument((getClassName(s4Obj)+": "+getSigName(s4Obj)+" had data assigned without specifying Slot").c_str());
	}
	if (val == NULL) {
		throw std::invalid_argument((getClassName(s4Obj)+": "+getSigName(s4Obj)+" had no data assigned to Slot: "+slotName).c_str());
	}
	if ( s4Obj.hasSlot(slotName.c_str()) ) {
		s4Obj.slot(slotName.c_str()) = val;
	} else if (s4Obj.is("Element")) {
		try {	// set to .xData list instead
			setXData(s4Obj, "Element", slotName.c_str(), R_NilValue, val);
		} catch(...) {
			throw std::invalid_argument((getClassName(s4Obj)+": "+getSigName(s4Obj)+" is missing assigned Slot: "+slotName).c_str());
		}
	}
	
	return s4Obj;
END_RCPP
}

/** Wrapper function allowing string specification of slot to be set **/
SEXP setSlot(SEXP epocObj, const char* slt, SEXP val) {
	return setSlot(epocObj, Rcpp::wrap(slt), val);
}
/** Wrapper function allowing string specification of slot and value **/
SEXP setSlot(SEXP epocObj, const char* slt, const char* val) {
	return setSlot(epocObj, Rcpp::wrap(slt), Rcpp::wrap(val));
}

/**
 * Get the named file connection handle if it is listed else return NULL
 * If listed but not open then open the connection first
 * If a filepath is passed then open file if it is not listed, open or path/mode has changed 
 * and then store in named fileConnections list
 * Return the connection or NULL if fails
 * Parameters:
 *	connname	character	name of connection
 *	filepath	character 	path to file to open (optional)
 *	openmode	character 	c(“a”, “w”, “r”) defaults to “a”
 **/
RcppExport SEXP getFileConnection(SEXP epocObj, SEXP connname, SEXP filepath, SEXP openmode) {
BEGIN_RCPP
	Rcpp::S4 s4Obj = asS4EO(epocObj);
	if ( connname == NULL || Rf_isNull(connname) ) Rf_error("Connection name required.");
	SEXP fileConn;
	
	try {
		fileConn = getXData(epocObj, "EPOCObject", "fileConnections", connname);
	} catch(std::range_error re) {
		// need to add
		epocDebugMessage(epocObj, (getClassName(epocObj)+": "+getSigName(s4Obj)
									+" is opening connection '"+toStr(connname)+"' to: "+toStr(filepath)).c_str());
		fileConn = createRcppFileConn(filepath, openmode);
		setXData(epocObj, "EPOCObject", "fileConnections", connname, fileConn);
	}
	openRcppFileConn(fileConn, filepath, openmode);
	return fileConn;
END_RCPP
}
/** Wrappers to allow char specification of arguments **/
SEXP getFileConnection(SEXP epocObj, SEXP connname, const char* filepath, const char* openmode) {
	return getFileConnection(epocObj, connname, Rcpp::wrap(filepath), Rcpp::wrap(openmode));
}
SEXP getFileConnection(SEXP epocObj, const char* connname, const char* filepath, const char* openmode) {
	return getFileConnection(epocObj, Rcpp::wrap(connname), Rcpp::wrap(filepath), Rcpp::wrap(openmode));
}

/**
 * Add the external pointer to file connection to the fileConnections named list using connname
 * If connname already exists and is open then it will be closed and replaced.
 * Parameters:
 *	conn		externalptr		connection
 *	connname	character		name of connection
 **/
RcppExport SEXP addFileConnection(SEXP epocObj, SEXP conn, SEXP connname) {
BEGIN_RCPP
	Rcpp::S4 s4Obj = asS4EO(epocObj);
	Rcpp::XPtr<FileConnection> fc(conn);
	
	if ( connname == NULL || Rf_isNull(connname) || toStr(connname) == "" ) Rf_error("Connection name required.");
	closeFileConnection(s4Obj, connname);
	epocDebugMessage(s4Obj, (getClassName(s4Obj)+": "+getSigName(s4Obj)+" is adding connection '"+toStr(connname)+"'").c_str());
	setXData(s4Obj, "EPOCObject", "fileConnections", connname, fc);
	
	return epocObj;
END_RCPP
}
/** Wrapper to allow char specification of arguments **/
SEXP addFileConnection(SEXP epocObj, SEXP conn, const char* connname) {
	return addFileConnection(epocObj, conn, Rcpp::wrap(connname));
}

/**
 * Close named file connection or external pointer to connection if it is in fileConnections list.
 * If conn is not specified then close all listed file connections.
 * externalptr to connection still remains after being closed and can be reopened
 * Returns whether connection existed and was open and therefore able to be closed
 * Parameters:
 *	conn	externalptr/character	name of connection or pointer to connection (optional)
 **/
RcppExport SEXP closeFileConnection(SEXP epocObj, SEXP conn) {
BEGIN_RCPP
	SEXP fileConn = R_NilValue;
	bool allclosed = true;
	std::string msg = getClassName(epocObj)+": "+getSigName(epocObj)+" is closing ";
	
	if ( conn == NULL || Rf_isNull(conn) ) {	// close all connections
		Rcpp::List connList = getXData(epocObj, "EPOCObject", "fileConnections");
		for ( int i = 1 ; i <= connList.size() ; i++ ) {
			if ( !Rf_isNull(connList[i]) ) {
				if ( !Rcpp::as<bool>(closeRcppFileConn(connList[i])) ) allclosed = false;
			}
		}
		if ( connList.size() > 0 ) epocDebugMessage(epocObj, (msg+"all connections").c_str());
	} else {
		if ( TYPEOF(conn) == EXTPTRSXP ) {
			fileConn = conn;
			msg += "connection";
		} else if ( Rf_isString(conn) && toStr(conn) != "" ) {
			try {
				fileConn = getXData(epocObj, "EPOCObject", "fileConnections", conn);
				msg += "connection "+Rcpp::as<std::string>(conn);
			} catch(std::range_error re) {
				return Rcpp::wrap(false);
			}
		}
		if ( !Rf_isNull(fileConn) ) {
			allclosed = Rcpp::as<bool>(closeRcppFileConn(fileConn));
			epocDebugMessage(epocObj, msg.c_str());
		}
	}
	
	return Rcpp::wrap(allclosed);
END_RCPP
}
/** Wrapper to allow char specification of arguments **/
SEXP closeFileConnection(SEXP epocObj, const char* conn) {
	return closeFileConnection(epocObj, Rcpp::wrap(conn));
}

/**
 * Write a line to the specified file connection and return success
 * Defaults to append mode, but will depend on the mode in which connectiion was opened.
 * If a character name and filepath is passed the connection will be opened (if not open or path/mode differs)
 * and the connection will be added to the fileConnections list.
 * Parameters:
 *	conn	externalptr/character	name of connection or pointer to connection
 *	msg			ANY (hopefully)		First part of message to be written (uses toString() to convert)
 *	filepath	character			Path for new connection to be made if necessary, only if conn = "character" (optional)
 *	openmode	character			Open mode for new connection to be made if necessary, only if conn = "character" (default = "a")
 *	sep			character			Separator character for multipart messages (default = "")
 **/
RcppExport SEXP writeFileConnection(SEXP epocObj, SEXP conn, SEXP msg, SEXP filepath, SEXP openmode, SEXP eol) {
BEGIN_RCPP
	if ( msg == NULL || Rf_isNull(msg) ) return Rcpp::wrap(false);
	
	if ( conn != NULL && TYPEOF(conn) == EXTPTRSXP ) {
		return writeRcppFileConn(conn, msg, eol);
	} else if ( Rf_isString(conn) ) {
		SEXP fileConn = getFileConnection(epocObj, conn, filepath, openmode);
		if ( !Rf_isNull(fileConn) ) return writeRcppFileConn(fileConn, msg, eol);
	}
	return Rcpp::wrap(false);
END_RCPP
}
/** Wrappers to allow char specification of arguments **/
SEXP writeFileConnection(SEXP epocObj, SEXP conn, const char* msg, const char* filepath, const char* openmode, bool eol) {
	return writeFileConnection(epocObj, Rcpp::wrap(conn), Rcpp::wrap(msg), Rcpp::wrap(filepath), Rcpp::wrap(openmode), Rcpp::wrap(eol));
}
SEXP writeFileConnection(SEXP epocObj, const char* conn, const char* msg, const char* filepath, const char* openmode, bool eol) {
	return writeFileConnection(epocObj, Rcpp::wrap(conn), Rcpp::wrap(msg), Rcpp::wrap(filepath), Rcpp::wrap(openmode), Rcpp::wrap(eol));
}

/**
 * Read from the specified file connection and return it
 * Defaults to linenum=-1 which reads back complete file.
 * If linenum=0 then next line is read back only else if linenum > 0 then that line only will be read back
 * The next line as determined by C++ level pointer will be returned unless linenum is specified
 * Parameters:
 *	conn	externalptr/character	name of connection or pointer to connection
 *	linenum		integer				Line number to read (starting at line 1 (defaults to current C++ filepointer)
 **/
RcppExport SEXP readFileConnection(SEXP epocObj, SEXP conn, SEXP linenum, SEXP filepath, SEXP openmode) {
BEGIN_RCPP
	int lno = -1;
	if ( linenum != NULL || !Rf_isNull(linenum) ) lno = Rcpp::as<int>(linenum);
	
	if ( conn != NULL && TYPEOF(conn) == EXTPTRSXP ) {
		if ( lno >= 0 ) return readlineRcppFileConn(conn, lno);
		return readRcppFileConn(conn);
	} else if ( Rf_isString(conn) ) {
		SEXP fileConn = getFileConnection(epocObj, conn, filepath, openmode);
		if ( !Rf_isNull(fileConn) ) {
			if ( lno >= 0 ) return readlineRcppFileConn(fileConn, lno);
			return readRcppFileConn(fileConn);
		}
	}
	return Rcpp::wrap(false);
END_RCPP
}
/** Wrappers to allow char specification of arguments **/
SEXP readFileConnection(SEXP epocObj, SEXP conn, int linenum, const char* filepath, const char* openmode) {
	return readFileConnection(epocObj, Rcpp::wrap(conn), Rcpp::wrap(linenum), Rcpp::wrap(filepath), Rcpp::wrap(openmode));
}
SEXP readFileConnection(SEXP epocObj, const char* conn, int linenum, const char* filepath, const char* openmode) {
	return readFileConnection(epocObj, Rcpp::wrap(conn), Rcpp::wrap(linenum), Rcpp::wrap(filepath), Rcpp::wrap(openmode));
}

/**
 * Generic function providing return of the .xData list object specified by xdName
 * If item passed then return value at list item in state if available
 * Parameters:
 *	xdName			character	xData list member
 *	item			character	named list member to be returned (optional)
 * setGeneric("getState", function(.Object, xdName="character", item="character") standardGeneric("getState"))
 **/
SEXP getXData(SEXP epocObj, const char* className, const char* xdName, SEXP item) throw (std::range_error) {
	std::string itemName = toStr(item);
	Rcpp::S4 s4Obj = asS4EO(epocObj, className);
	Rcpp::Environment xdEnv = s4Obj.attr(".xData");
	Rcpp::List dataList;
	SEXP result = R_NilValue;
	
	try {
		if (itemName == "") return xdEnv[xdName];
		dataList = xdEnv[xdName];
	} catch(...) {
		return result;
	}

	try {
		result = dataList[itemName];
	} catch(...) {
		throw std::range_error((std::string(className)+": "+getSigName(s4Obj)
				+" is missing requested "+std::string(xdName)+" item: "+itemName).c_str());
	}
	
	return result;
}
/** Wrappers to allow char specification of arguments **/
SEXP getXData(SEXP epocObj, const char* className, const char* xdName, const char* item) throw (std::range_error) {
	return getXData(epocObj, className, xdName, Rcpp::wrap(item));
}
SEXP getXData(SEXP epocObj, const char* className, const char* xdName) throw (std::range_error) {
	return getXData(epocObj, className, xdName, R_NilValue);
}

/**
 * Generic function providing setting of the .xData list object specified by xdName
 * If item passed then set value at name in state list 
 * Parameters:
 *	item		character	state list member at which to assign value
 *	val			R			value to be assigned
 * setGeneric("setState", function(.Object, listName="character", value) standardGeneric("setState"))
 **/
SEXP setXData(SEXP epocObj, const char* className, const char* xdName, SEXP item, SEXP val) throw (std::invalid_argument) {
	std::string itemName = toStr(item);
	Rcpp::S4 s4Obj = asS4EO(epocObj, className);
	Rcpp::Environment xdEnv = s4Obj.attr(".xData");
	
	if ( val == NULL || Rf_isNull(val) ) {
		throw std::invalid_argument((std::string(className)+": "+getSigName(s4Obj)
				+" had no data assigned to "+std::string(xdName)+" item: "+itemName).c_str());
	}
	if ( itemName == "" ) {
		// No item named but a list passed as val
		if (TYPEOF(val) == VECSXP || TYPEOF(val) == LISTSXP) xdEnv.assign(xdName, val);
	} else {
		if (Rf_isNull(xdEnv[xdName]) || (TYPEOF(xdEnv[xdName]) != VECSXP && TYPEOF(xdEnv[xdName]) != LISTSXP)) {
			// Create a new list
			xdEnv.assign(xdName, Rcpp::List::create(Rcpp::Named(itemName)=val));
		} else {
			// Add item to existing list
			Rcpp::List dataList = xdEnv[xdName];
			dataList[itemName.c_str()] = val;
			xdEnv.assign(xdName, dataList);
		}
	}
	
	return s4Obj;
}
/** Wrapper to allow char specification of arguments **/
SEXP setXData(SEXP epocObj, const char* className, const char* xdName, const char* item, SEXP val) throw (std::invalid_argument) {
	return setXData(epocObj, className, xdName, Rcpp::wrap(item), val);
}

/**
 * NOTE Doesn't work if void and NULL is not returned
 * Print messages as dictated by epocnotify option
 * msg can be a character vector or a list of character vectors.
 * Print standard messages as dictated by .msglevel!="quiet"
 **/
RcppExport SEXP epocMessage(SEXP epocObj, SEXP msg) {
BEGIN_RCPP
	std::string msglevel = toStr(getSlot(epocObj, ".msglevel"));
	std::string loglevel = toStr(getSlot(epocObj, ".loglevel"));
	SEXP logconn = R_NilValue;
	
	try {
		logconn = getXData(epocObj, "EPOCObject", "fileConnections", "logFile");
	} catch(std::range_error re) { /* do nothing */ }
	if (msglevel != "quiet") printToR(msg);
	if (loglevel != "" && loglevel != "quiet") logToR(logconn, msg);
	
	return R_NilValue;
END_RCPP
}
/** Wrapper to allow character specification of message **/
void epocMessage(SEXP epocObj, const char* msg) {
	epocMessage(epocObj, Rcpp::wrap(msg));
}

/**
 * Print messages as dictated by epocnotify option
 * ... MUST be terminated by an empty string
 **/
// void epocMessage(const char* msg ...) {
	// std::string epocnotify = toStr(getOption("epocnotify"));
	// if (epocnotify != "quiet") {
		// va_list args;
		// va_start(args, msg);
		// printToR(msg, args);
		// va_end(args);
	// }
// }

/**
 * Print verbose messages as dictated by .msglevel="verbose" or "debug"
 * setGeneric("epocVerboseMessage", function(.Object, msg, ...) standardGeneric("epocVerboseMessage"))
 **/
 RcppExport SEXP epocVerboseMessage(SEXP epocObj, SEXP msg) {
 BEGIN_RCPP
	std::string msglevel = toStr(getSlot(epocObj, ".msglevel"));
	std::string loglevel = toStr(getSlot(epocObj, ".loglevel"));
	SEXP logconn = R_NilValue;
	
	try {
		logconn = getXData(epocObj, "EPOCObject", "fileConnections", "logFile");
	} catch(std::range_error re) { /* do nothing */ }
	if (msglevel == "verbose" || msglevel == "debug") printToR(msg);
	if (loglevel == "verbose" || loglevel == "debug") logToR(logconn, msg);
	
	return R_NilValue;
END_RCPP
}
/** Wrapper to allow character specification of message **/
void epocVerboseMessage(SEXP epocObj, const char* msg) {
	epocVerboseMessage(epocObj, Rcpp::wrap(msg));
}

/**
 * Print debug messages as dictated by .msglevel="debug"
 * setGeneric("epocDebugMessage", function(.Object, msg, ...) standardGeneric("epocDebugMessage"))
 **/
 RcppExport SEXP epocDebugMessage(SEXP epocObj, SEXP msg) {
 BEGIN_RCPP
	std::string msglevel = toStr(getSlot(epocObj, ".msglevel"));
	std::string loglevel = toStr(getSlot(epocObj, ".loglevel"));
	SEXP logconn = R_NilValue;
	
	try {
		logconn = getXData(epocObj, "EPOCObject", "fileConnections", "logFile");
	} catch(std::range_error re) { /* do nothing */ }
	if ( msglevel == "debug" ) printToR(msg);
	if ( loglevel == "debug" ) logToR(logconn, msg);
	
	return R_NilValue;
END_RCPP
}
/** Wrapper to allow character specification of message **/
void epocDebugMessage(SEXP epocObj, const char* msg) {
	epocDebugMessage(epocObj, Rcpp::wrap(msg));
}

/**
 * Print error message without consulting reporting params
 * setGeneric("epocErrorMessage", function(.Object, msg, ...) standardGeneric("epocErrorMessage"))
 **/
RcppExport SEXP epocErrorMessage(SEXP epocObj, SEXP msg, SEXP halt) {
BEGIN_RCPP
	if ( halt == NULL || Rf_isNull(halt) ) halt = Rcpp::wrap(false);
	SEXP logconn = R_NilValue;
	
	try {
		logconn = getXData(epocObj, "EPOCObject", "fileConnections", "logFile");
	} catch(std::range_error re) { /* do nothing */ }
	printToR(msg);
	logToR(logconn, msg);
	
	if (halt) Rf_error("");
	return R_NilValue;
END_RCPP
}
/** Wrapper to allow character specification of message **/
void epocErrorMessage(SEXP epocObj, const char* msg, bool halt) {
	epocErrorMessage(epocObj, Rcpp::wrap(msg), Rcpp::wrap(halt));
}

/**
 * Return the elements signature object
 * If slot name is passed then return the value at that slot instead
 * Parameters:
 *	item		character		name of signature slot to return (optional)
 * setGeneric("getSignature", function(.Object, item="character") standardGeneric("getSignature"))
 **/
 RcppExport SEXP getSignature(SEXP epocObj, SEXP item) {
 BEGIN_RCPP
	std::string itemName = toStr(item);
	SEXP sig = getSlot(epocObj, "signature");
	if ( itemName == "" ) return sig;
	return getSignatureItem(sig, item);
END_RCPP
 }
/** Wrapper function to allow string specification of signature item to return **/
SEXP getSignature(SEXP epocObj, const char* item) {
	return getSignature(epocObj, Rcpp::wrap(item));
} 

/**
 * Return a simple one line signature
 * setGeneric("getSignatureLine", function(.Object) standardGeneric("getSignatureLine"))
 **/
RcppExport SEXP getSignatureLine(SEXP epocObj, SEXP display) {
BEGIN_RCPP
	if ( display == NULL || Rf_isNull(display) ) display = Rcpp::wrap(false);
	bool disp = Rcpp::as<bool>(display);
	SEXP sigstr = getSimpleSignature(getSlot(epocObj, "signature"));
	if ( disp ) epocMessage(epocObj, sigstr); 
	return sigstr;
END_RCPP
}
SEXP getSignatureLine(SEXP epocObj, bool display) { return getSignatureLine(epocObj, Rcpp::wrap(display)); }

/**
 * Return multiline signature
 * setGeneric("printSignature", function(.Object) standardGeneric("printSignature"))
 **/
RcppExport SEXP getSignatureMulti(SEXP epocObj, SEXP display) {
BEGIN_RCPP
	if ( display == NULL || Rf_isNull(display) ) display = Rcpp::wrap(false);
	bool disp = Rcpp::as<bool>(display);
	SEXP sigstr = getFullSignature(getSlot(epocObj, "signature"));
	if ( disp ) epocMessage(epocObj, sigstr); 
	return sigstr;
END_RCPP
}
SEXP getSignatureMulti(SEXP epocObj, bool display) { return getSignatureMulti(epocObj, Rcpp::wrap(display)); }