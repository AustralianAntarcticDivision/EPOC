/*********************************************************************************
 * Copyright (C) 2012	Australian Antarctic Division
 *
 * Author: Troy Robertson
 *
 * EPOC.cpp
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

/*
#include "EPOCSignature.cpp"
#include "EPOCFileConnection.cpp"
#include "EPOCObject.cpp"
#include "EPOCElement.cpp"
#include "EPOCUniverse.cpp"
#include "EPOCPeriod.cpp"
#include "EPOCCalendar.cpp"
*/

using namespace Rcpp;

/********* Helper functions *********/
/**
 * Return an S4 object, checking at the same time that it inherits from class cName passed
 **/
Rcpp::S4 asS4EO(SEXP sexpObj, const char* cName) {
	if ( !Rcpp::RObject(sexpObj).isS4() ) Rf_error((std::string(cName)+" object is not an S4 object").c_str());
	Rcpp::S4 s4Obj( sexpObj );
	if ( !s4Obj.is(cName) ) Rf_error( ("S4 object does not inherit from "+std::string(cName)).c_str() );

	return s4Obj;
}
/** Wrapper function with cName defaulting to "EPOCObject" **/
Rcpp::S4 asS4EO(SEXP sexpObj) {
	return asS4EO(sexpObj, "EPOCObject");
}

/**
 * Return a new target object which only contains items intersected by source vector indexes
 **/
Rcpp::List intersect(Rcpp::List target, Rcpp::NumericVector source) {
	Rcpp::List resultList = Rcpp::List::create();
	Rcpp::List namesList = target.names();

	for ( int i = 0 ; i < source.size() ; i++ ) {
		if ( source[i] < target.size() ) resultList.push_back(target[i], namesList[i]);
	}
	return resultList;
}
Rcpp::CharacterVector intersect(Rcpp::CharacterVector target, Rcpp::NumericVector source) {
	Rcpp::CharacterVector resultVec = Rcpp::CharacterVector::create();
	for ( int i = 0 ; i < source.size() ; i++ ) {
		if ( source[i] < target.size() ) resultVec.push_back(target[i]);
	}
	return resultVec;
}
Rcpp::NumericVector intersect(Rcpp::NumericVector target, Rcpp::NumericVector source) {
	Rcpp::NumericVector resultVec = Rcpp::NumericVector::create();
	for ( int i = 0 ; i < source.size() ; i++ ) {
		if ( source[i] < target.size() ) resultVec.push_back(target[i]);
	}
	return resultVec;
}


/** Return the direct S4 class name of the object passed **/
std::string getClassName(Rcpp::S4 s4Obj) {
	Rcpp::List classList = s4Obj.attr("class");
	return Rcpp::as<std::string>(classList[0]);
}
/** Helper to return an S4 EPOC objects signature Name.short value **/
std::string getSigName(Rcpp::S4 epocObj) {
	if ( !epocObj.is("EPOCObject") ) Rf_error("R Object is not an S4 object inheriting from EPOCObject");
	SEXP sname = Rcpp::S4(epocObj.slot("signature")).slot("Name.short");
	return (Rf_isNull(sname) ? "" : Rcpp::as<std::string>(sname));
}

/** Helper function to convert simple SEXP to a std::string **/
std::string toStr(SEXP sexpObj) {

	if (sexpObj == NULL || Rf_isNull(sexpObj)) return "";
	return std::string(CHAR(Rf_asChar(sexpObj)));
}

/** Return the option value from the R options list, or R_NilValue if not found **/
SEXP getOption(SEXP option) {
	std::string optionName = toStr(option);
	Environment base = Environment::base_env();
    Rcpp::List optionList = base[".Options"];

	if ( optionName == "" ) return optionList;
	try {
		return optionList[optionName.c_str()];
	} catch (...) {
	}

	return R_NilValue;
}
/** Wrapper to getOption taking char parameter **/
SEXP getOption(const char* option) {
	return getOption(Rcpp::wrap(option));
}

/**
 * Write msg out to R stdout using Rprintf
 * If msg is a list then concatenate it all together
 **/
void printToR(SEXP msg) {
	std::string message;

	if ( msg == NULL || Rf_isNull(msg) ) return;
	if (TYPEOF(msg) == VECSXP || TYPEOF(msg) == LISTSXP) {
		Rcpp::List msgList = msg;
		for (int i = 0 ; i < msgList.size() ; i++ ) message += toStr(msgList[i]);
	} else {
		message = toStr(msg);
	}

	Rprintf("%s\n", message.c_str());
}
/** Wrapper to Rprintf which first concatenates args until a NULL is found **/
void printToR(const char* msg, va_list args) {
	std::stringstream outsstr;
	const char* nextmsg;

	outsstr << msg;
	while ( true ) {
		try {
			nextmsg = va_arg( args, const char* );
			if (nextmsg == NULL || std::string(nextmsg) == "") break;
			outsstr << nextmsg;
		} catch (...) {
			break;
		}
	}

	Rprintf("%s\n", outsstr.str().c_str());
	/** INVESTIGATE THESE
	The function REprintf is similar but writes on the error stream (stderr) which may or may not be different from the standard output stream.
	Functions Rvprintf and REvprintf are analogues using the vprintf interface. Because that is a C99 interface, they are only defined
	by R_ext/Print.h in C++ code if the macro R_USE_C99_IN_CXX is defined when it is included.
	**/
}

/**
 * Write msg out to the current logFile connection if one exists
 * If msg is a list then concatenate it all together
 **/
void logToR(SEXP logconn, SEXP msg) {
	std::string message;

	if ( logconn == NULL || Rf_isNull(logconn) || TYPEOF(logconn) != EXTPTRSXP || !isopenRcppFileConn(logconn) ) return;
	if ( msg == NULL || Rf_isNull(msg)) return;
	if (TYPEOF(msg) == VECSXP || TYPEOF(msg) == LISTSXP) {
		Rcpp::List msgList = msg;
		for (int i = 0 ; i < msgList.size() ; i++ ) message += toStr(msgList[i]);
	} else {
		message = toStr(msg);
	}

	writeRcppFileConn(logconn, message.c_str(), true);
}

/** Return a character vector with values pass separated by sep **/
RcppExport SEXP asCSVCharacter(SEXP rObj, SEXP sep) {
BEGIN_RCPP
	if ( rObj == NULL || Rf_isNull(rObj) ) return Rcpp::wrap("");
	if ( toStr(sep) == "" || !Rf_isString(sep) ) sep = Rcpp::wrap(",");  // defaults to ","
	std::string charstr = "", separator = toStr(sep);

	if ( Rf_isVectorList(rObj) ) {
		Rcpp::List rList(rObj);
		for ( int i = 0 ; i < rList.size() ; i++ ) {
			if ( charstr != "" ) charstr += separator;
			charstr += toStr(rList[i]);
		}
	} else if ( Rf_isVector(rObj) ) {
		Rcpp::CharacterVector rVec(rObj);
		for ( int i = 0 ; i < rVec.size() ; i++ ) {
			if ( charstr != "" ) charstr += separator;
			charstr += toStr(rVec[i]);
		}
	}

	return Rcpp::wrap(charstr);
END_RCPP
}
SEXP asCSVCharacter(SEXP rObj, const char* sep) { return asCSVCharacter(rObj, Rcpp::wrap(sep)); }

/** Return a character(default) vector from values passed split by sep **/
RcppExport SEXP fromCSVCharacter(SEXP rObj, SEXP type, SEXP sep) {
BEGIN_RCPP
	if ( rObj == NULL || Rf_isNull(rObj) ) return Rcpp::wrap("");
	if ( type == NULL || Rf_isNull(type) || !Rf_isString(type) ) type = Rcpp::wrap("character");
	if ( toStr(sep) == "" || !Rf_isString(sep) ) sep = Rcpp::wrap(",");		// defaults to ","
	char separator = (toStr(sep))[0];

	if ( Rf_isVectorList(rObj) ) {
		Rcpp::List rList(rObj);
		Rcpp::List resultList;
		for ( int i = 0 ; i < rList.size() ; i++ ) {
			resultList.push_back(fromCSVCharacter(rList[i], type, sep));
		}
		return resultList;
	} else if ( Rf_isVector(rObj) ) {
		Rcpp::CharacterVector rVec(rObj);
		std::string chunk;
		if ( toStr(type) == "integer" ) {
			Rcpp::IntegerVector intVec;
			for ( int i = 0 ; i < rVec.size() ; i++ ) {
				std::istringstream iss(toStr(rVec[i]));
				while ( getline(iss, chunk, separator) ) { intVec.push_back(atoi(chunk.c_str())); }
			}
			return intVec;
		} else if ( toStr(type) == "double" || toStr(type) == "numeric" ) {
			Rcpp::NumericVector fltVec;
			for ( int i = 0 ; i < rVec.size() ; i++ ) {
				std::istringstream iss(toStr(rVec[i]));
				while ( getline(iss, chunk, separator) ) { fltVec.push_back(atof(chunk.c_str())); }
			}
			return fltVec;
		} else {
			Rcpp::CharacterVector charVec;
			for ( int i = 0 ; i < rVec.size() ; i++ ) {
				std::istringstream iss(toStr(rVec[i]));
				while ( getline(iss, chunk, separator) ) { charVec.push_back(chunk); }
			}
			return charVec;
		}
	}

	return R_NilValue;
END_RCPP
}
SEXP fromCSVCharacter(SEXP rObj, const char* type, const char* sep) { return fromCSVCharacter(rObj, Rcpp::wrap(type), Rcpp::wrap(sep)); }

/********** Unit testing ************************/
RcppExport SEXP testAction(SEXP eo) {
	SEXP retData = R_NilValue;
	//epocMessage(eo, "Testing Rcpp...");
	//ConnectionManager* conn = new ConnectionManager("Test3", "Testing3", "w");
	//conn->write("Test3", "BLDDFDL");
	//conn->close("Test3");
	//SEXP connmgr = getXData(eo, "EPOCObject", "connectionManager", NULL);
	//logToR(connmgr, Rcpp::wrap("BLAH"));
	//setSlot(eo, "inputData", Rcpp::wrap("stuff"));

	//retData = getSignatureLine(eo);
	//retData = getSlot(eo, "inputData");
	//retData = getAttribute(eo, "dataPath");
	//retData = Rcpp::wrap(getClassName(asS4EO(eo)));
	//retData = getOption("width");

	retData = getFileConnection(eo, "testrcpp", "TestRcpp.log");
	writeFileConnection(eo, retData, "Line 1");
	writeFileConnection(eo, retData, "Line 2");
	writeFileConnection(eo, "testrcpp", "Line 3");
	SEXP ret1 = readFileConnection(eo, retData);
	SEXP ret2 = readFileConnection(eo, "testrcpp", 3);
	closeFileConnection(eo, "testrcpp");

	SEXP retData2 = getFileConnection(eo, "logFile", "", "a");
	writeFileConnection(eo, retData2, "TESTING IN THE LOG FILE");
	closeFileConnection(eo, retData2);

	return Rcpp::wrap((Rcpp::as<std::string>(ret1)+"\n\n"+Rcpp::as<std::string>(ret2)).c_str());
}
