/*********************************************************************************
 * Copyright (C) 2012	Australian Antarctic Division
 *
 * Author: Troy Robertson
 *
 * EPOCPeriod.cpp
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

/********* Period class functions *********/
/**
 * Return a list containing period slot items
 * setGeneric("getPeriodInfo", function(.Object) standardGeneric("getPeriodInfo"))
 **/
RcppExport SEXP getPeriodInfo(SEXP periodObj) {
BEGIN_RCPP
	Rcpp::S4 s4Obj = asS4EO( periodObj, "Period" );
	
	return Rcpp::List::create(Rcpp::Named("Number")=s4Obj.slot("number"),
							  Rcpp::Named("Day")=s4Obj.slot("day"),
							  Rcpp::Named("KnifeEdge")=s4Obj.slot("knifeEdge"),
							  Rcpp::Named("YearPropn")=s4Obj.slot("yearPropn"),
							  Rcpp::Named("PeriodStart")=s4Obj.slot("periodStart"),
							  Rcpp::Named("PeriodEnd")=s4Obj.slot("periodEnd"));
END_RCPP
}

/**
 * Return the period action matrix
 * setGeneric("getPeriodActionMat", function(.Object) standardGeneric("getPeriodActionMat"))
 **/
RcppExport SEXP getPeriodActionMat(SEXP periodObj) {
BEGIN_RCPP
	Rcpp::S4 s4Obj = asS4EO( periodObj, "Period" );
	
	return s4Obj.slot("periodActionMat");
END_RCPP
}
 
/**
 * Return the period element timestep data
 * setGeneric("getPeriodElementTSData", function(.Object) standardGeneric("getPeriodActionMat"))
 **/
RcppExport SEXP getPeriodElementTSData(SEXP periodObj, SEXP modnum, SEXP elemnum) {
BEGIN_RCPP
	Rcpp::S4 s4Obj = asS4EO( periodObj, "Period" );
	Rcpp::List modList = s4Obj.slot("modules");
	
	if ( modnum == NULL || !Rf_isNumeric(modnum) || elemnum == NULL || !Rf_isNumeric(elemnum) ) return R_NilValue;
	int mNum = Rcpp::as<int>(modnum) - 1;
	int eNum = Rcpp::as<int>(elemnum) - 1;
	
	if ( !Rf_isNull(modList) && mNum >= 0 && modList.size() > mNum ) {
		try {
			Rcpp::NumericMatrix elemMat = modList[mNum];
			if ( !Rf_isNull(elemMat) && eNum >= 0 && elemMat.nrow() > eNum ) return Rcpp::NumericVector(elemMat.row(eNum));
		} catch ( Rcpp::index_out_of_bounds& e ) {
			// Do nothing
		}
	}

	return R_NilValue;
END_RCPP
}

/**
 * Wrapper function to allow int specification module and element indexes
 **/
SEXP getPeriodElementTSData(SEXP periodObj, int modnum, int elemnum) {
	return getPeriodElementTSData(periodObj, Rcpp::wrap(modnum), Rcpp::wrap(elemnum));
}