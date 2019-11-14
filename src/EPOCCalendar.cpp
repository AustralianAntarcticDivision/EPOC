/*********************************************************************************
 * Copyright (C) 2012	Australian Antarctic Division
 *
 * Author: Troy Robertson
 *
 * EPOCController.cpp
 * This file forms part of the API library for EPOC and is included in the 
 * EPOC R package along with:
 * EPOC.h
 * EPOC.cpp
 * EPOCSignature.cpp
 * EPOCObject.cpp
 * EPOCElement.cpp
 * EPOCUniverse.cpp
 * EPOCPeriod.cpp
 * EPOCCalendar.cpp
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

/********* Calendar class functions *********/
/**
 * Output a textual representation of the calendar.  Defaults to file but with
 * toScreen=TRUE will output to stdout
 * Parameters:
 *	universe	Universe	Universe object to consult
 *	toScreen	logical		Print to screen? (default FALSE)
 * setGeneric("outputCalendar", function(.Object, universe="Universe", toScreen="logical") standardGeneric("outputCalendar"))
 **/
 
/**
 * Return the calendar period object specified by periodNum
 * Parameters:
 *	periodNum	numeric	Number of the period to return
 * setGeneric("getPeriod", function(.Object, periodNum) standardGeneric("getPeriod"))
 **/
RcppExport SEXP getPeriod(SEXP calObj, SEXP periodNum) {
BEGIN_RCPP
	Rcpp::S4 s4CalObj = asS4EO( calObj, "Calendar" );
	Rcpp::List periodList = s4CalObj.slot("periods");
	
	if ( !Rf_isNull(periodList) && periodNum != NULL && Rf_isNumeric(periodNum) ) {
		try {
			return periodList[Rcpp::as<int>(periodNum)];
		} catch ( ... ) {
			// Do nothing
		}
	}
	
	return R_NilValue;
END_RCPP
}
 
/**
 * Wrapper function to allow integer specification of periodNum
 **/
SEXP getPeriod(SEXP calObj, int periodNum) {
	return getPeriod(calObj, Rcpp::wrap(periodNum));
}

/** 
 * Return a specified period information as a named list
 * Parameters:
 *	periodNum	numeric		Number of the period to return info on
 * setGeneric("getInfoForPeriod", function(.Object, periodNum) standardGeneric("getInfoForPeriod"))
 **/
RcppExport SEXP getInfoForPeriod(SEXP calObj, SEXP periodNum) {
BEGIN_RCPP
	return getPeriodInfo(getPeriod(calObj, periodNum));
END_RCPP
}

/**
 * Wrapper function to allow integer specification of periodNum
 **/
SEXP getInfoForPeriod(SEXP calObj, int periodNum) {
	return getInfoForPeriod(calObj, Rcpp::wrap(periodNum));
}

/**
 * Return an action matrix for the period specified
 * Parameters:
 *	periodNum	numeric		Number of the period to return info on
 * setGeneric("getActionMatForPeriod", function(.Object, periodNum) standardGeneric("getActionMatForPeriod"))
 **/
RcppExport SEXP getActionMatForPeriod(SEXP calObj, SEXP periodNum) {
BEGIN_RCPP
	return getPeriodActionMat(getPeriod(calObj, periodNum));
END_RCPP
}

/**
 * Wrapper function to allow integer specification of periodNum
 **/
SEXP getActionMatForPeriod(SEXP calObj, int periodNum) {
	return getActionMatForPeriod(calObj, Rcpp::wrap(periodNum));
}

