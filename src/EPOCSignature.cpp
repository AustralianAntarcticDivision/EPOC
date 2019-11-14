/*********************************************************************************
 * Copyright (C) 2012	Australian Antarctic Division
 *
 * Author: Troy Robertson
 *
 * EPOCSignature.cpp
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

/********* Signature class functions **********/
/**
 * Return the value stored at the slot with name = item 
 * Parameters:
 *	item		character	slot name from which to return value
 * setGeneric("getSignatureItem", function(.Object, item="character") standardGeneric("getSignatureItem"))
 **/
RcppExport SEXP getSignatureItem(SEXP sigObj, SEXP item) {
	std::string slotName = toStr(item);
	Rcpp::S4 s4Obj = asS4EO( sigObj, "Signature" );
	SEXP result = R_NilValue;
	
	if ( slotName != "" && s4Obj.hasSlot(slotName.c_str()) ) result = s4Obj.slot(slotName.c_str());
	
	return result;
}

/**
 * Wrapper function to allow string specification of signature item to return
 **/
SEXP getSignatureItem(SEXP sigObj, const char* item) {
	return getSignatureItem(sigObj, Rcpp::wrap(item));
}

/**
 * Return object signature to screen
 * setGeneric("getSignatureSimple", function(.Object) standardGeneric("getSignatureSimple"))
 **/
RcppExport SEXP getSimpleSignature(SEXP sigObj) {
	Rcpp::S4 s4Obj = asS4EO( sigObj, "Signature" );

	std::string signame = toStr(s4Obj.slot("Morph"));
	signame += "-"+toStr(s4Obj.slot("ClassName"));
	signame += "-"+toStr(s4Obj.slot("Name.short"));
	
	return Rcpp::wrap(signame);
}

/**
 * Return a multiline string containing all signature details
 * setGeneric("getFullSignature", function(.Object) standardGeneric("displaySignature"))
 **/
RcppExport SEXP getFullSignature(SEXP sigObj) {
	Rcpp::S4 s4Obj = asS4EO( sigObj, "Signature" );
	
	std::string signame  = "        ID: " + toStr(s4Obj.slot("ID")) + "\n";
				signame += " Full Name: " + toStr(s4Obj.slot("Name.full")) + "\n";
				signame += "Short Name: " + toStr(s4Obj.slot("Name.short")) + "\n";
	if (!Rf_isNull(s4Obj.slot("Morph"))) signame += "     Morph: " + toStr(s4Obj.slot("Morph")) + "\n";
				signame += "  Revision: " + toStr(s4Obj.slot("Revision")) + "\n";
				signame += "   Authors: " + toStr(s4Obj.slot("Authors")) + "\n";
				signame += " Last Edit: " + toStr(s4Obj.slot("Last.edit"));
	
	return Rcpp::wrap(signame);
}
