/*********************************************************************************
 * Copyright (C) 2012	Australian Antarctic Division
 *
 * Author: Troy Robertson
 *
 * EPOCElement.cpp
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

/**
 * Return the polygons slot data
 * setGeneric("getPolygons", function(.Object) standardGeneric("getPolygons"))
 **/
RcppExport SEXP getPolygons(SEXP elemObj) {
BEGIN_RCPP
	Rcpp::S4 s4Obj = asS4EO(elemObj, "Element");
	
	if ( !s4Obj.hasSlot("polygons") || Rf_isNull(s4Obj.slot("polygons")) || Rf_length(s4Obj.slot("polygons")) <= 0 ) {
		throw std::range_error(("Element: "+getSigName(s4Obj)+" has no polygons extent!").c_str());
	}
	
	return s4Obj.slot("polygons");
END_RCPP
}

/**
 * Return the element birthday
 * setGeneric("getBirthday", function(.Object) standardGeneric("getBirthday"))
 **/
RcppExport SEXP getBirthday(SEXP elemObj) {
BEGIN_RCPP
	Rcpp::S4 s4Obj = asS4EO(elemObj, "Element");
	
	if ( !s4Obj.hasSlot("birthday") || Rf_isNull(s4Obj.slot("birthday")) 
			|| !Rf_isNumeric(s4Obj.slot("birthday")) || Rcpp::as<int>(s4Obj.slot("birthday")) <= 0 ) {
		throw std::range_error(("Element: "+getSigName(s4Obj)+" has no birthday defined!").c_str());
	}
	
	return s4Obj.slot("birthday");
END_RCPP
}
 
/**
 * Return the state object if it has been instantiated already
 * If item passed then return value at list item in state if available
 * Parameters:
 *	item		character	state list member to be returned (optional)
 * setGeneric("getState", function(.Object, item="character") standardGeneric("getState"))
 **/
RcppExport SEXP getState(SEXP elemObj, SEXP item) {
BEGIN_RCPP
	return getXData(elemObj, "Element", "state", item);
END_RCPP
}

/**
 * Wrapper function to allow string specification of state item to return
 **/
SEXP getState(SEXP elemObj, const char* item) {
	return getXData(elemObj, "Element", "state", item);
}

/**
 * Set the state list as the value passed
 * If item passed then set value at name in state list 
 * Parameters:
 *	listName		character	state list member at which to assign value
 *	value		R			value to be assigned
 * setGeneric("setState", function(.Object, listName="character", value) standardGeneric("setState"))
 **/
RcppExport SEXP setState(SEXP elemObj, SEXP item, SEXP val) {
BEGIN_RCPP
	return setXData(elemObj, "Element", "state", item, val);
END_RCPP
}

/**
 * Wrapper function to allow string specification of state item to set
 **/
SEXP setState(SEXP elemObj, const char* item, SEXP val) {
	return setXData(elemObj, "Element", "state", Rcpp::wrap(item), val);
}

/**
 * Wrapper function to allow string specification of state item and value
 **/
SEXP setState(SEXP elemObj, const char* item, const char* val) {
	return setXData(elemObj, "Element", "state", Rcpp::wrap(item), Rcpp::wrap(val));
}

/**
 * Return the transition list
 * If item passed then return value at name in transition list if available
 * Parameters:
 *	item		character	transition list item to be returned
 * setGeneric("getTransition", function(.Object, item="character") standardGeneric("getTransition"))
 **/
RcppExport SEXP getTransition(SEXP elemObj, SEXP item) {
BEGIN_RCPP
	return getXData(elemObj, "Element", "transition", item);
END_RCPP
}

/**
 * Wrapper function to allow string specification of transition item to return
 **/
SEXP getTransition(SEXP elemObj, const char* item) {
	return getXData(elemObj, "Element", "transition", Rcpp::wrap(item));
}

/**
 * Set the transition list as the value passed
 * If item passed then set value at name in transition list
 * Parameters:
 *	item		character	transition list item to have value assigned (optional)
 *	value		R			value to be assigned
 * setGeneric("setTransition", function(.Object, item="character", value) standardGeneric("setTransition"))
 **/
RcppExport SEXP setTransition(SEXP elemObj, SEXP item, SEXP val) {
BEGIN_RCPP
	return setXData(elemObj, "Element", "transition", item, val);
END_RCPP
}

/**
 * Wrapper function to allow string specification of transition item to set
 **/
SEXP setTransition(SEXP elemObj, const char* item, SEXP val) {
	return setXData(elemObj, "Element", "transition", Rcpp::wrap(item), val);
}

/**
 * Wrapper function to allow string specification of transition item and value
 **/
SEXP setTransition(SEXP elemObj, const char* item, const char* val) {
	return setXData(elemObj, "Element", "transition", Rcpp::wrap(item), Rcpp::wrap(val));
}

/**
 * Return the functions list
 * If item passed then return value at name in functions list if available
 * Parameters:
 *	item		character	functions list item to be returned
 * setGeneric("getFunctionData", function(.Object, item="character") standardGeneric("getFunctionData"))
 **/
RcppExport SEXP getFunctionData(SEXP elemObj, SEXP item) {
BEGIN_RCPP
	return getXData(elemObj, "Element", "functions", item);
END_RCPP
}

/**
 * Wrapper function to allow string specification of function data item to return
 **/
SEXP getFunctionData(SEXP elemObj, const char* item) {
	return getXData(elemObj, "Element", "functions", Rcpp::wrap(item));
}

/**
 * Set the functions list as the value passed
 * If item passed then set value at name in functions list
 * Parameters:
 *	item		character	functions list item to have value assigned (optional)
 *	value		R			value to be assigned
 * setGeneric("setFunctionData", function(.Object, item="character", value) standardGeneric("setFunctionData"))
 **/
RcppExport SEXP setFunctionData(SEXP elemObj, SEXP item, SEXP val) {
BEGIN_RCPP
	return setXData(elemObj, "Element", "functions", item, val);
END_RCPP
}

/**
 * Wrapper function to allow string specification of function data item to set
 **/
SEXP setFunctionData(SEXP elemObj, const char* item, SEXP val) {
	return setXData(elemObj, "Element", "functions", Rcpp::wrap(item), val);
}

/**
 * Wrapper function to allow string specification of function data item and value
 **/
SEXP setFunctionData(SEXP elemObj, const char* item, const char* val) {
	return setXData(elemObj, "Element", "functions", Rcpp::wrap(item), Rcpp::wrap(val));
}

/**
 * Return the timestep list
 * If periodNum passed then return value at index in timesteps if available
 * Parameters:
 *	periodNum	numeric	timestep period to be returned (optional)
 * setGeneric("getTimestep", function(.Object, periodNum="numeric") standardGeneric("getTimestep"))
 **/
RcppExport SEXP getTimestep(SEXP elemObj, SEXP periodNum) {
BEGIN_RCPP
	std::string periodName = toStr(periodNum);
	Rcpp::S4 s4Obj = asS4EO( elemObj, "Element" );
	Rcpp::List tsList = s4Obj.slot("timesteps");

	if ( periodName == "" ) return tsList;
	try {
		if ( Rf_isInteger(periodNum) ) {
			int pNum = Rcpp::as<int>(periodNum) - 1;
			if ( pNum >= 0 && pNum < tsList.size() ) return tsList[pNum];
		}
	} catch (...) {
		throw std::range_error(("Element: "+getSigName(s4Obj)
				+" is missing requested Timestep period: "+periodName).c_str());
	}
	
	return R_NilValue;
END_RCPP
}

/**
 * Wrapper function to allow int specification of timestep period number
 **/
SEXP getTimestep(SEXP elemObj, int periodNum) {
	SEXP pNum = ( periodNum == 0 ? R_NilValue : Rcpp::wrap(periodNum) ); 
	return getTimestep(elemObj, pNum);
}

/**
 * Set the timesteps list as the value passed
 * If periodNum passed then set val at index in timesteps
 * Parameters:
 *	periodNum	numeric	timestep period to have value assigned (optional)
 *	val			R		value to be assigned
 * setGeneric("setTimestep", function(.Object, periodNum="numeric", val) standardGeneric("setTimestep"))
 **/
RcppExport SEXP setTimestep(SEXP elemObj, SEXP periodNum, SEXP val) {
BEGIN_RCPP
	std::string periodName = toStr(periodNum);
	Rcpp::S4 s4Obj = asS4EO(elemObj, "Element");
	Rcpp::List tsList = s4Obj.slot("timesteps");
	
	if ( val == NULL ) {
		throw std::invalid_argument(("Element: "+getSigName(s4Obj)
				+" had no data assigned to timestep period: "+toStr(periodNum)).c_str());
	}
	if ( periodName == "" ) {
		// No period named but a list passed as val
		if (TYPEOF(val) == VECSXP || TYPEOF(val) == LISTSXP) s4Obj.slot("timesteps") = val;
	} else if ( Rf_isNumeric(periodNum) ) {
		int pNum = Rcpp::as<int>(periodNum) - 1;
		if ( pNum >= 0 ) { // && pNum <= tsList.size()) {
			for ( int i=tsList.size() ; i <= pNum ; i++ ) tsList.push_back(R_NilValue);
			tsList[pNum] = val;
			s4Obj.slot("timesteps") = tsList;
		}
	}
	
	return s4Obj;
END_RCPP
}

/**
 * Wrapper function to allow int specification of timestep period number
 **/
SEXP setTimestep(SEXP elemObj, int periodNum, SEXP val) {
	SEXP pNum = ( periodNum == 0 ? R_NilValue : Rcpp::wrap(periodNum) ); 
	return setTimestep(elemObj, pNum, Rcpp::wrap(val));
}

/**
 * Get the named file connection handle if it is listed else return NULL
 * If listed but not open then open the connection first
 * If named is not listed but a filepath is passed then open file, store in named list
 * and return the connection
 * Parameters:
 *	connName		character     name of file connection
 * setGeneric("getFileConnection", function(.Object, connName="character", filepath="character", openmode="character") standardGeneric("getFileConnection"))
 **/

/**
 * Close named file connection if its in list
 * If not named then close all listed file connections
 * setGeneric("closeFileConnection", function(.Object, connName="character") standardGeneric("closeFileConnection"))
 */

/**
 * Generic function to deal with flags.  If doFlag is logical then the specified flag
 * is set to that value.  Returns the value of the flag.
 * setGeneric("doFlag", function(.Object, flag="character", state="logical") standardGeneric("doFlag"))
 **/
RcppExport SEXP doFlag(SEXP elemObj, const char* flag, SEXP state) {
BEGIN_RCPP
	Rcpp::S4 s4Obj = asS4EO( elemObj, "Element" );
	Rcpp::List xdList = s4Obj.attr(".xData");
	Rcpp::List flagList;
	
	try {
		flagList = xdList["flags"];
	} catch(...) {
		flagList = Rcpp::List::create();
	}
	
	if (state != NULL && !Rf_isNull(state) && Rf_isLogical(state)) { 
		flagList[flag] = state;
		xdList["flags"] = flagList;
		s4Obj.attr(".xData") = xdList;
	} 
	
	try {
		return flagList[flag];
	} catch(...) {
		return Rcpp::wrap(NA_LOGICAL);
	}
END_RCPP
}

/**
 * setGeneric("doUpdate", function(.Object, state="logical") standardGeneric("doUpdate"))
 **/
RcppExport SEXP doUpdate(SEXP elemObj, SEXP state) { return doFlag(elemObj, "doUpdate", state); }
/** Wrapper function to allow bool specification of flag state **/
SEXP doUpdate(SEXP elemObj, bool state) { return doFlag(elemObj, "doUpdate", Rcpp::wrap(state)); }

/**
 * setGeneric("doPrint", function(.Object, state="logical") standardGeneric("doPrint"))
 **/
RcppExport SEXP doPrint(SEXP elemObj, SEXP state) { return doFlag(elemObj, "doPrint", state); }
/** Wrapper function to allow bool specification of flag state **/
SEXP doPrint(SEXP elemObj, bool state) { return doFlag(elemObj, "doPrint", Rcpp::wrap(state)); }

/**
 * setGeneric("doPrintFinal", function(.Object, state="logical") standardGeneric("doPrintFinal"))
 **/
RcppExport SEXP doPrintFinal(SEXP elemObj, SEXP state) { return doFlag(elemObj, "doPrintFinal", state); }
/** Wrapper function to allow bool specification of flag state **/
SEXP doPrintFinal(SEXP elemObj, bool state) { return doFlag(elemObj, "doPrintFinal", Rcpp::wrap(state)); }