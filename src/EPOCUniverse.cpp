/*********************************************************************************
 * Copyright (C) 2012	Australian Antarctic Division
 *
 * Author: Troy Robertson
 *
 * EPOCUniverse.cpp
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
 * Return the element in module as indicated by list index parameters
 * NULL returned if element doesn't exist at indexes passed
 * Parameters:
 *	modID	character/numeric		module list index
 *	elemID	character/numeric		element list index
 * setGeneric("getElement", function(.Object, modID, elemID) standardGeneric("getElement"))
 **/
RcppExport SEXP getEPOCElement(SEXP uniObj, SEXP modID, SEXP elemID) {
BEGIN_RCPP
	std::string modName = toStr(modID);
	std::string elemName = toStr(elemID);
	Rcpp::S4 s4UniObj = asS4EO( uniObj, "Universe" );
	Rcpp::List modList = getXData(s4UniObj, "Universe", "modules");
	
	if ( modName == "" || modName == "0" || elemName == "" || elemName == "0" ) return R_NilValue;
	try{
		if ( Rf_isNumeric(modID) ) {
			if ( Rcpp::as<int>(modID) <= modList.size() ) {
				Rcpp::List elemList = modList[Rcpp::as<int>(modID) - 1];
				if ( !Rf_isNumeric(elemID) ) return elemList[elemName.c_str()];
				if ( Rcpp::as<int>(elemID) <= elemList.size() ) return elemList[Rcpp::as<int>(elemID) - 1];
			}
		} else {
			Rcpp::List elemList = modList[modName.c_str()];
			if ( !Rf_isNumeric(elemID) ) return elemList[elemName.c_str()];
			if ( Rcpp::as<int>(elemID) <= elemList.size() ) return elemList[Rcpp::as<int>(elemID) - 1];
		}
	//} catch( std::exception& __ex__ ) {
	//	forward_exception_to_r( __ex__ );
	} catch (...) {
		//Rf_error(("Universe: "+getSigName(s4UniObj)+" is missing requested Element: "+elemName+" in Module: "+modName).c_str());
	}

	return R_NilValue;
END_RCPP
}

SEXP getEPOCElement(SEXP uniObj, const char* modID, const char* elemID) {
	return getEPOCElement(uniObj, Rcpp::wrap(modID), Rcpp::wrap(elemID));
}
 
SEXP getEPOCElement(SEXP uniObj, int modID, int elemID) {
	return getEPOCElement(uniObj, Rcpp::wrap(modID), Rcpp::wrap(elemID));
}

/**
 * Set the element object at the parameter indexes
 * Will overwrite any existing element object at that list position
 * Parameters:
 *	modID	character/numeric		module list index
 *	elemID	character/numeric		element list index
 *	element	Element				S4 Element object to be set
 * setGeneric("setElement", function(.Object, modID, elemID, element) standardGeneric("setElement"))
 **/
RcppExport SEXP setEPOCElement(SEXP uniObj, SEXP modID, SEXP elemID, SEXP element) {
BEGIN_RCPP
	std::string modName = toStr(modID);
	std::string elemName = toStr(elemID);
	Rcpp::S4 s4UniObj = asS4EO( uniObj, "Universe" );
	Rcpp::List modList = getXData(s4UniObj, "Universe", "modules");
	
	if ( element == NULL || Rf_isNull(element)) {
		throw std::invalid_argument(("Universe: "+getSigName(s4UniObj)+" had no data assigned to Module: "+modName+" as Element: "+elemName).c_str());
	}
	Rcpp::S4 s4Ele = asS4EO( element, "Element" );
	try {
		if ( modName == "" || modName == "0" || elemName == "" || elemName == "0" ) throw std::range_error("");
		if ( Rf_isNumeric(modID) ) {
			Rcpp::List elemList = modList[Rcpp::as<int>(modID) - 1];
			if ( !Rf_isNumeric(elemID) ) {
				elemList[elemName.c_str()] = s4Ele;
			} else {
				int eNum = Rcpp::as<int>(elemID) - 1;
				for ( int i=elemList.size() ; i <= eNum ; i++ ) elemList.push_back(R_NilValue);
				elemList[eNum] = s4Ele;	
			}
			int mNum = Rcpp::as<int>(modID) - 1;
			for ( int i=modList.size() ; i <= mNum ; i++ ) modList.push_back(R_NilValue);
			modList[mNum] = elemList;
		} else {
			Rcpp::List elemList = modList[modName.c_str()];
			if ( !Rf_isNumeric(elemID) ) {
				elemList[elemName.c_str()] = s4Ele;
			} else {
				int eNum = Rcpp::as<int>(elemID) - 1;
				for ( int i=elemList.size() ; i <= eNum ; i++ ) elemList.push_back(R_NilValue);
				elemList[eNum] = s4Ele;	
			}
			modList[modName.c_str()] = elemList;
		}
		setXData(s4UniObj, "Universe", "modules", R_NilValue, modList);
	} catch (...) {
		throw std::range_error(("Assignment of Element: "+getSigName(s4Ele)+" into Module: "+modName+" as Element: "+elemName+" failed!").c_str());
	}
	
	return s4UniObj;
END_RCPP
}

SEXP setEPOCElement(SEXP uniObj, const char* modID, const char* elemID, SEXP element) {
	return setEPOCElement(uniObj, Rcpp::wrap(modID), Rcpp::wrap(elemID), element);
}

SEXP setEPOCElement(SEXP uniObj, int modID, int elemID, SEXP element) {
	return setEPOCElement(uniObj, Rcpp::wrap(modID), Rcpp::wrap(elemID), element);
}

/** 
 * Return the state list if it exists
 * If item passed then value in state list is returned if available
 * Parameters:
 *	item		character	state list name to be returned (optional)
 * setGeneric("getRTState", function(.Object, item="character") standardGeneric("getRTState"))
 **/
RcppExport SEXP getRTState(SEXP uniObj, SEXP item) {
BEGIN_RCPP
	return getXData(uniObj, "Universe", "realtimeState", item);
END_RCPP
}

/**
 * Wrapper function to allow string specification of realtimeState item to return
 **/
SEXP getRTState(SEXP uniObj, const char* item) { return getXData(uniObj, "Universe", "realtimeState", Rcpp::wrap(item)); }
SEXP getRTState(SEXP uniObj) { return getXData(uniObj, "Universe", "realtimeState", Rcpp::wrap("")); }

/**
 * Return the input directory path
 * If extPath is passed then full path to file is returned 
 * Parameters:
 *	extPath		character	extension to base directory
 * setGeneric("getBasePath", function(.Object, extPath="character") standardGeneric("getBasePath"))
 **/
RcppExport SEXP getBasePath(SEXP uniObj, SEXP extPath) {
BEGIN_RCPP
	std::string extPathName = toStr(extPath);
	SEXP bDir = getSlot(uniObj, "baseDirectory");
	std::string path = ( Rf_isNull(bDir) ? "" : Rcpp::as<std::string>(bDir) );
	
	if( extPathName != "" ) {
		std::string sep = "\\";
		// if forward slash then *nix
		if ( path.find_first_of("/") != std::string::npos ) sep = "/";
		path += sep+extPathName;
	}
	
	return Rcpp::wrap(path);	
END_RCPP
}

/**
 * Wrapper function to allow string specification of extPath
 **/
SEXP getBasePath(SEXP uniObj, const char* extPath) { return getBasePath(uniObj, Rcpp::wrap(extPath)); }
SEXP getBasePath(SEXP uniObj) { return getBasePath(uniObj, Rcpp::wrap("")); }

/**
 * Return the input directory path
 * If extPath is passed then full path to file is returned 
 * Parameters:
 *	extPath		character	extension to base directory
 * setGeneric("getRuntimePath", function(.Object, extPath="character") standardGeneric("getRuntimePath"))
 **/
RcppExport SEXP getRuntimePath(SEXP uniObj, SEXP extPath) {
BEGIN_RCPP
	std::string extPathName = toStr(extPath);
	SEXP bDir = getSlot(uniObj, "baseDirectory");
	std::string path = ( Rf_isNull(bDir) ? "" : Rcpp::as<std::string>(bDir) );
	
	if( extPathName != "" ) {
		std::string sep = "\\";
		// if forward slash then *nix
		if ( path.find_first_of("/") != std::string::npos ) sep = "/";
		path += sep+extPathName;
	}
	
	return Rcpp::wrap(path);	
END_RCPP
}

/**
 * Wrapper function to allow string specification of extPath
 **/
SEXP getRuntimePath(SEXP uniObj, const char* extPath) { return getRuntimePath(uniObj, Rcpp::wrap(extPath)); }
SEXP getRuntimePath(SEXP uniObj) { return getRuntimePath(uniObj, Rcpp::wrap("")); }

/** 
 * Return the spatial object if it has been instantiated already
 * If slt passed then return value at slot in spatial if available
 * Parameters:
 *	slt		character	spatial slot to be returned (optional)
 * setGeneric("getSpatial", function(.Object, slt="character") standardGeneric("getSpatial"))
 **/
RcppExport SEXP getSpatial(SEXP uniObj, SEXP slt) {
BEGIN_RCPP
	std::string sltName = toStr(slt);
	Rcpp::S4 s4UniObj = asS4EO( uniObj, "Universe" );
	SEXP spatial = getSlot(uniObj, "spatial");

	if (sltName == "") return spatial;
	return getSlot(spatial, sltName.c_str());
END_RCPP
}

/**
 * Wrapper function to allow string specification of spatial item
 **/
SEXP getSpatial(SEXP uniObj, const char* item) { return getSpatial(uniObj, Rcpp::wrap(item)); }
SEXP getSpatial(SEXP uniObj) { return getSpatial(uniObj, Rcpp::wrap("")); }

/**
 * Return reporting list data
 * If item passed then return that list item in report list if available
 * Parameters:
 *	item		character	state list name to be returned (optional)
 * setGeneric("getReport", function(.Object, item="character") standardGeneric("getReport"))
 **/
RcppExport SEXP getReport(SEXP uniObj, SEXP item) {
BEGIN_RCPP
	std::string itemName = toStr(item);
	Rcpp::S4 s4UniObj = asS4EO( uniObj, "Universe" );
	Rcpp::List repList = s4UniObj.slot("report");

	if (itemName == "") return repList;
	try {
		return repList[itemName];
	} catch (...) {
		//throw std::invalid_argument(("Universe: "+getSigName(s4UniObj)
		//		+" is missing requested Report item: "+itemName).c_str());
	}
	
	return R_NilValue;
END_RCPP
}

/**
 * Wrapper function to allow string specification of report item
 **/
SEXP getReport(SEXP uniObj, const char* item) { return getReport(uniObj, Rcpp::wrap(item)); }
SEXP getReport(SEXP uniObj) { return getReport(uniObj, Rcpp::wrap("")); }

/** 
 * Return scenario object if scenarioNum or scenarioName is passed only.
 * Return slot in scenario if item passed as well
 * Uses CurrentScenario from realtimeState to specify scenario if not passed
 * Parameters:
 *	scenarioName	character	scenario name to retrieve
 *	scenarioNum		numeric		scenario number to retrieve
 *	item		character	name of the scenario attribute to return
 * setGeneric("getScenario", function(.Object, scenarioName="character", scenarioNum="numeric", item="character") standardGeneric("getScenario"))
 **/
RcppExport SEXP getScenario(SEXP uniObj, SEXP scenario, SEXP item) {
BEGIN_RCPP
	std::string scenarioName = toStr(scenario);
	std::string itemName = toStr(item);
	Rcpp::S4 s4UniObj = asS4EO( uniObj, "Universe" );
	Rcpp::List trList = s4UniObj.slot("scenarios");
	
	if ( scenarioName == "" ) scenario = getRTState(uniObj, "currentScenario");
	try {
		if (Rf_isNumeric(scenario)) {
			int tNum = Rcpp::as<int>(scenario) - 1;
			if (tNum >= 0 && tNum < trList.size()) {
				if ( itemName != "" ) return getSlot(trList[tNum], item);
				return trList[tNum];
			}
		} else {
			if ( itemName != "" ) return getSlot(trList[scenarioName], item);
			return trList[scenarioName];
		}
	} catch (...) {
		//throw std::invalid_argument(("Universe: "+getSigName(s4UniObj)
		//		+" is missing requested Scenario: "+scenarioName).c_str());
	}

	return R_NilValue;
END_RCPP
}

/**
 * Wrapper function to allow string specification of scenario item
 **/
SEXP getScenario(SEXP uniObj, const char* scenario, const char* item) { return getScenario(uniObj, Rcpp::wrap(scenario), Rcpp::wrap(item)); }
SEXP getScenario(SEXP uniObj, const char* scenario) { return getScenario(uniObj, Rcpp::wrap(scenario), Rcpp::wrap("")); }

/**
 * Wrapper function to allow string specification of scenario item
 **/
SEXP getScenario(SEXP uniObj, int scenario, const char* item) { return getScenario(uniObj, Rcpp::wrap(scenario), Rcpp::wrap(item)); }
SEXP getScenario(SEXP uniObj, int scenario) { return getScenario(uniObj, Rcpp::wrap(scenario), Rcpp::wrap("")); }

/** 
 * Returns an vector containing the element c(moduleListIndex, elementListIndex)
 * Returns an index of 0 for list names not found
 * Parameters: 
 *	moduleName as character
 *	elementName as character	(required)
 * setGeneric("getElementIndexes", function(.Object, moduleName=moduleName, moduleName=moduleName) standardGeneric("getElementIndexes"))
 **/
RcppExport SEXP getElementIndexes(SEXP uniObj, SEXP moduleName, SEXP element) {
	if ( element != NULL && !Rf_isNull(element) ) {
		if ( Rf_isNumeric(element) ) {
			return getElementIndexes(uniObj, moduleName, Rcpp::as<int>(element));
		} else {
			return getElementIndexes(uniObj, moduleName, Rcpp::as<const char*>(element));
		}
	}
	
	return Rcpp::List::create(0, 0);
}
SEXP getElementIndexes(SEXP uniObj, SEXP moduleName, const char* elementName) {
BEGIN_RCPP
	std::string modName = toStr(moduleName);
	std::string elemName = std::string(elementName);
	Rcpp::S4 s4UniObj = asS4EO( uniObj, "Universe" );
	Rcpp::List modList = getXData(s4UniObj, "Universe", "modules");
	int m = 0, e = 0;
	
	if ( !Rf_isNull(modList) && modList.size() > 0 && elemName != ""  ) {
		if ( modName != "" ) {
			try {
				m = modList.offset(modName);
				if ( m >= 0 ) {
					Rcpp::List elemList = modList[m];
					e = elemList.offset(elemName);
					if ( e >= 0 ) return Rcpp::List::create(m + 1, e + 1);
				}
			} catch ( Rcpp::index_out_of_bounds& e ) {
				// Do nothing
			}
		} else {
			// Search all module lists and return first found
			for ( m = 0 ; m < modList.size() ; m++ ) {
				try {
					Rcpp::List elemList = modList[m];
					e = elemList.offset(elemName);
					if ( e >= 0 ) return Rcpp::List::create(m + 1, e + 1);
				} catch ( Rcpp::index_out_of_bounds& e ) {
					// Do nothing
				}
			}
			m = 0; e = 0;
		}
	}
	
	return Rcpp::List::create(0, 0); 
END_RCPP
}

/**
 * Wrapper function to allow string specification of module and element names
 **/
SEXP getElementIndexes(SEXP uniObj, const char* moduleName, SEXP elementName) {
	return getElementIndexes(uniObj, Rcpp::wrap(moduleName), elementName);
}

/**
 * Wrapper function to allow string specification of module and element names
 **/
SEXP getElementIndexes(SEXP uniObj, const char* moduleName, const char* elementName) {
	return getElementIndexes(uniObj, Rcpp::wrap(moduleName), Rcpp::wrap(elementName));
}

/** 
 * Searches element modules to find element with a signature ID matching that passed
 * Returns an vector containing the element c(moduleListName, elementListName)
 * Parameters:		
 *	moduleName as character
 *	elementID as numeric	(required)
 * setGeneric("getElementIDIndexes", function(.Object, moduleName=moduleName, elementID=elementID) standardGeneric("getElementIDIndexes"))
 **/
SEXP getElementIndexes(SEXP uniObj, SEXP moduleName, int elementID) {
BEGIN_RCPP
	std::string modName = toStr(moduleName);
	Rcpp::S4 s4UniObj = asS4EO( uniObj, "Universe" );
	Rcpp::List modList = getXData(s4UniObj, "Universe", "modules");
	int m = 0, e = 0;
	
	if ( !Rf_isNull(modList) && modList.size() > 0 ) { //&& elementID != NULL ) {
		if ( moduleName != NULL && modName != "" ) {
			try {
				m = modList.offset(modName);
				if ( m >= 0 ) {
					// Search element list and return first found
					Rcpp::List elemList = modList[m];
					for ( e = 0 ; e < elemList.size() ; e++ ) {
						SEXP id = getSignature(elemList[e], "ID");
						if (!Rf_isNull(id) && Rcpp::as<int>(id) == elementID) return Rcpp::List::create(m + 1, e + 1);
					}
				}
			} catch ( Rcpp::index_out_of_bounds& e ) {
				// Do nothing
			}
		} else {
			// Search all module lists and return first found
			for ( m = 0 ; m < modList.size() ; m++ ) {
				// Search element list and return first found
				Rcpp::List elemList = modList[m];
				for ( e = 0 ; e < elemList.size() ; e++ ) {
					SEXP id = getSignature(elemList[e], "ID");
					if (!Rf_isNull(id) && Rcpp::as<int>(id) == elementID) return Rcpp::List::create(m + 1, e + 1);
				}
			}
		}
	}
	
	return Rcpp::List::create(0, 0); 
END_RCPP
}

/**
 * Wrapper function to allow string specification of moduleName and integer specification of elementID
 **/
SEXP getElementIndexes(SEXP uniObj, const char* moduleName, int elementID) {
	return getElementIndexes(uniObj, Rcpp::wrap(moduleName), Rcpp::wrap(elementID));
}
