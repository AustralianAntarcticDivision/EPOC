/*********************************************************************************
 * Copyright (C) 2012	Australian Antarctic Division
 *
 * Author: Troy Robertson
 *
 * EPOC.h
 * This file forms part of the API library for EPOC and is included in the 
 * EPOC R package along with:
 * EPOC.h
 * EPOC.cpp
 * EPOCFileConnection.cpp
 * EPOCObject.cpp
 * EPOCSignature.cpp
 * EPOCPeriod.cpp
 * EPOCElement.cpp
 * EPOCUniverse.cpp
 * EPOCCalendar.cpp
 *
 * EPOC is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *********************************************************************************/
#ifndef EPOC_hpp
#define EPOC_hpp
#include <string>
#include <iostream>
#include <fstream>
#include <map>
#include <Rcpp.h>

class FileConnection {
	public:
		FileConnection ( std::string connpath, std::string openmode );
		~FileConnection ();
		bool open(std::string connpath, std::string openmode);
		bool openconn();
		bool isopen();
		const char* getpath();
		const char* getmode();
		bool write ( std::string msg, bool eol );
		std::string readline (int linenum = 0 );
		std::map<int, std::string> readfile ();
		bool close ();
	
	private:
		std::string path, mode;
		std::fstream iofile;
};

/********* EPOCObject class functions *********/
RcppExport SEXP getAttributeNames(SEXP epocObj);
RcppExport SEXP getAttribute(SEXP epocObj, SEXP item);
SEXP getAttribute(SEXP epocObj, const char* item);
RcppExport SEXP setAttribute(SEXP epocObj, SEXP item, SEXP val);
SEXP setAttribute(SEXP epocObj, const char* item, SEXP val);
SEXP setAttribute(SEXP epocObj, const char* item, const char* val);
RcppExport SEXP getSlotNames(SEXP epocObj);
RcppExport SEXP getSlot(SEXP epocObj, SEXP slt);
SEXP getSlot(SEXP epocObj, const char* slt);
RcppExport SEXP setSlot(SEXP epocObj, SEXP slt, SEXP val);
SEXP setSlot(SEXP epocObj, const char* slt, SEXP val);
SEXP setSlot(SEXP epocObj, const char* slt, const char* val);
RcppExport SEXP getFileConnection(SEXP epocObj, SEXP connname, SEXP filepath=R_NilValue, SEXP openmode=R_NilValue);
SEXP getFileConnection(SEXP epocObj, const char* connname, const char* filepath="", const char* openmode="");
RcppExport SEXP addFileConnection(SEXP epocObj, SEXP conn, SEXP connname);
SEXP addFileConnection(SEXP epocObj, SEXP conn, const char* connname);
RcppExport SEXP closeFileConnection(SEXP epocObj, SEXP conn);
SEXP closeFileConnection(SEXP epocObj, const char* conn);
RcppExport SEXP writeFileConnection(SEXP epocObj, SEXP conn, SEXP msg, SEXP filepath=R_NilValue, SEXP openmode=R_NilValue, SEXP eol=R_NilValue);
SEXP writeFileConnection(SEXP epocObj, SEXP conn, const char* msg, const char* filepath="", const char* openmode="", bool eol=true);
SEXP writeFileConnection(SEXP epocObj, const char* conn, const char* msg, const char* filepath="", const char* openmode="", bool eol=true);
RcppExport SEXP readFileConnection(SEXP epocObj, SEXP conn, SEXP linenum, SEXP filepath=R_NilValue, SEXP openmode=R_NilValue);
SEXP readFileConnection(SEXP epocObj, SEXP conn, int linenum=-1, const char* filepath="", const char* openmode="");
SEXP readFileConnection(SEXP epocObj, const char* conn, int linenum=-1, const char* filepath="", const char* openmode="");
SEXP getXData(SEXP epocObj, const char* className, const char* xdName, SEXP item) throw (std::range_error);
SEXP getXData(SEXP epocObj, const char* className, const char* xdName, const char* item) throw (std::range_error);
SEXP getXData(SEXP epocObj, const char* className, const char* xdName) throw (std::range_error);
SEXP setXData(SEXP epocObj, const char* className, const char* xdName, SEXP item=R_NilValue, SEXP val=R_NilValue) throw (std::invalid_argument);
SEXP setXData(SEXP epocObj, const char* className, const char* xdName, const char* item="", SEXP val=R_NilValue) throw (std::invalid_argument);
RcppExport SEXP epocMessage(SEXP epocObj, SEXP msg);
void epocMessage(SEXP epocObj, const char* msg);
RcppExport SEXP epocVerboseMessage(SEXP epocObj, SEXP msg);
void epocVerboseMessage(SEXP epocObj, const char* msg);
RcppExport SEXP epocDebugMessage(SEXP epocObj, SEXP msg);
void epocDebugMessage(SEXP epocObj, const char* msg);
RcppExport SEXP epocErrorMessage(SEXP epocObj, SEXP msg, SEXP halt);
void epocErrorMessage(SEXP epocObj, const char* msg, bool halt);
RcppExport SEXP getSignature(SEXP epocObj, SEXP item);
SEXP getSignature(SEXP epocObj, const char* item);
RcppExport SEXP getSignatureLine(SEXP epocObj, SEXP display=R_NilValue);
SEXP getSignatureLine(SEXP epocObj, bool display=FALSE);
RcppExport SEXP getSignatureMulti(SEXP epocObj, SEXP display=R_NilValue);
SEXP getSignatureMulti(SEXP epocObj, bool display=FALSE);

/********* Signature class functions **********/
RcppExport SEXP getSignatureItem(SEXP sigObj, SEXP item);
SEXP getSignatureItem(SEXP sigObj, const char* item);
RcppExport SEXP getSimpleSignature(SEXP sigObj);
RcppExport SEXP getFullSignature(SEXP sigObj);

/********* Element class functions ************/
RcppExport SEXP getPolygons(SEXP elemObj);
RcppExport SEXP getBirthday(SEXP elemObj);
RcppExport SEXP getState(SEXP elemObj, SEXP item);
SEXP getState(SEXP elemObj, const char* item="");
RcppExport SEXP setState(SEXP elemObj, SEXP item, SEXP val);
SEXP setState(SEXP elemObj, const char* item="", SEXP val=R_NilValue);
SEXP setState(SEXP elemObj, const char* item, const char* val);
RcppExport SEXP getTransition(SEXP elemObj, SEXP item);
SEXP getTransition(SEXP elemObj, const char* item="");
RcppExport SEXP setTransition(SEXP elemObj, SEXP item, SEXP val);
SEXP setTransition(SEXP elemObj, const char* item="", SEXP val=R_NilValue);
SEXP setTransition(SEXP elemObj, const char* item, const char* val);
RcppExport SEXP getFunctionData(SEXP elemObj, SEXP item);
SEXP getFunctionData(SEXP elemObj, const char* item="");
RcppExport SEXP setFunctionData(SEXP elemObj, SEXP item, SEXP val);
SEXP setFunctionData(SEXP elemObj, const char* item="", SEXP val=R_NilValue);
SEXP setFunctionData(SEXP elemObj, const char* item, const char* val);
RcppExport SEXP getTimestep(SEXP elemObj, SEXP periodNum);
SEXP getTimestep(SEXP elemObj, int periodNum=0);
RcppExport SEXP setTimestep(SEXP elemObj, SEXP periodNum, SEXP val);
SEXP setTimestep(SEXP elemObj, int periodNum=0, SEXP val=R_NilValue);
RcppExport SEXP doFlag(SEXP elemObj, const char* flag, SEXP doFlag);
RcppExport SEXP doUpdate(SEXP elemObj, SEXP doFlag);
SEXP doUpdate(SEXP elemObj, bool doFlag);
RcppExport SEXP doPrint(SEXP elemObj, SEXP doFlag);
SEXP doPrint(SEXP elemObj, bool doFlag);
RcppExport SEXP doPrintFinal(SEXP elemObj, SEXP doFlag);
SEXP doPrintFinal(SEXP elemObj, bool doFlag);

/********* Universe class functions ***********/
RcppExport SEXP getEPOCElement(SEXP uniObj, SEXP modID, SEXP elemID);
SEXP getEPOCElement(SEXP uniObj, const char* modID, const char* elemID);
SEXP getEPOCElement(SEXP uniObj, int modID, int elemID);
RcppExport SEXP setEPOCElement(SEXP uniObj, SEXP modID, SEXP elemID, SEXP element);
SEXP setEPOCElement(SEXP uniObj, const char* modID, const char* elemID, SEXP element);
SEXP setEPOCElement(SEXP uniObj, int modID, int elemID, SEXP element);
RcppExport SEXP getRTState(SEXP uniObj, SEXP item);
SEXP getRTState(SEXP uniObj, const char* item);
SEXP getRTState(SEXP uniObj);
RcppExport SEXP getBasePath(SEXP uniObj, SEXP extPath);
SEXP getBasePath(SEXP uniObj, const char* extPath);
SEXP getBasePath(SEXP uniObj);
RcppExport SEXP getRuntimePath(SEXP uniObj, SEXP extPath);
SEXP getRuntimePath(SEXP uniObj, const char* extPath);
SEXP getRuntimePath(SEXP uniObj);
RcppExport SEXP getSpatial(SEXP uniObj, SEXP item);
SEXP getSpatial(SEXP uniObj, const char* item);
SEXP getSpatial(SEXP uniObj);
RcppExport SEXP getReport(SEXP uniObj, SEXP item);
SEXP getReport(SEXP uniObj, const char* item);
SEXP getReport(SEXP uniObj);
RcppExport SEXP getScenario(SEXP uniObj, SEXP scenario, SEXP item);
SEXP getScenario(SEXP uniObj, const char* scenario, const char* item);
SEXP getScenario(SEXP uniObj, const char* scenario);
SEXP getScenario(SEXP uniObj, int scenario, const char* item);
SEXP getScenario(SEXP uniObj, int scenario);
RcppExport SEXP getElementIndexes(SEXP uniObj, SEXP moduleName, SEXP element);
SEXP getElementIndexes(SEXP uniObj, SEXP moduleName, const char* elementName);
SEXP getElementIndexes(SEXP uniObj, const char* moduleName, SEXP elementName);
SEXP getElementIndexes(SEXP uniObj, const char* moduleName, const char* elementName);
SEXP getElementIndexes(SEXP uniObj, SEXP moduleName, int elementID);
SEXP getElementIndexes(SEXP uniObj, const char* moduleName, int elementID);

/********* Period class functions *************/
RcppExport SEXP getPeriodInfo(SEXP periodObj);
RcppExport SEXP getPeriodActionMat(SEXP periodObj);
RcppExport SEXP getPeriodElementTSData(SEXP periodObj, SEXP modnum, SEXP elemnum);
SEXP getPeriodElementTSData(SEXP periodObj, int modnum, int elemnum);

/********* Calendar class functions ***********/
RcppExport SEXP getPeriod(SEXP calObj, SEXP periodNum);
SEXP getPeriod(SEXP calObj, int periodNum);
RcppExport SEXP getInfoForPeriod(SEXP calObj, SEXP periodNum);
SEXP getInfoForPeriod(SEXP calObj, int periodNum);
RcppExport SEXP getActionMatForPeriod(SEXP calObj, SEXP periodNum);
SEXP getActionMatForPeriod(SEXP calObj, int periodNum);

/********* File Connection functions **********/
RcppExport SEXP createRcppFileConn(SEXP connpath, SEXP openmode);
SEXP createRcppFileConn(const char* connpath, const char* openmode="");
RcppExport SEXP openRcppFileConn(SEXP fcObj, SEXP connpath, SEXP openmode);
SEXP openRcppFileConn(SEXP fcObj, const char* connpath="", const char* openmode="");
RcppExport SEXP isopenRcppFileConn(SEXP fcObj);
RcppExport SEXP getpathRcppFileConn(SEXP fcObj);
RcppExport SEXP getmodeRcppFileConn(SEXP fcObj);
RcppExport SEXP writeRcppFileConn(SEXP fcobj, SEXP msg, SEXP eol);
SEXP writeRcppFileConn(SEXP fcObj, const char* msg, bool eol=true);
RcppExport SEXP readlineRcppFileConn(SEXP fcObj, SEXP linenum);
SEXP readlineRcppFileConn(SEXP fcObj, int linenum=0);
RcppExport SEXP readRcppFileConn(SEXP fcObj);
RcppExport SEXP closeRcppFileConn(SEXP fcObj);
/** NOTE: There is a problem with this class and it's associated Rcpp exported functions
 * For some reason FileConnection objects instantiated by this class are not PROTECTED and
 * memory segmentation faults occur.
 * Instead, EPOC R code implements the functionality of the ConnectionManager.
 **/
 /**
RcppExport SEXP createConnectionManager();
//RcppExport SEXP createConnectionManager(SEXP connname, SEXP connpath, SEXP openmode);
RcppExport SEXP getConnection(SEXP cmObj, SEXP connname);
RcppExport SEXP addConnection(SEXP cmObj, SEXP fcObj, SEXP connname);
RcppExport SEXP openConnection(SEXP cmObj, SEXP connname, SEXP connpath, SEXP openmode);
RcppExport SEXP isopenConnection(SEXP cmObj, SEXP connname);
RcppExport SEXP getpathConnection(SEXP cmObj, SEXP connname);
RcppExport SEXP getmodeConnection(SEXP cmObj, SEXP connname);
RcppExport SEXP writeConnection(SEXP cmObj, SEXP connname, SEXP msg);
SEXP writeConnection(SEXP cmObj, const char* connname, const char* msg);
RcppExport SEXP readlineConnection(SEXP cmObj, SEXP connname, SEXP linenum);
RcppExport SEXP readConnection(SEXP cmObj, SEXP connname);
RcppExport SEXP closeConnection(SEXP cmObj, SEXP connname);
**/

/********* Helper functions *******************/
Rcpp::S4 asS4EO(SEXP sexpObj, const char* cName);
Rcpp::S4 asS4EO(SEXP sexpObj);
Rcpp::List intersect(Rcpp::List target, Rcpp::NumericVector source);
Rcpp::CharacterVector intersect(Rcpp::CharacterVector target, Rcpp::NumericVector source);
Rcpp::NumericVector intersect(Rcpp::NumericVector target, Rcpp::NumericVector source);
std::string getClassName(Rcpp::S4 epocObj);
std::string getSigName(Rcpp::S4 epocObj);
std::string toStr(SEXP sexpObj);
SEXP getOption(SEXP option);
SEXP getOption(const char* option);
RcppExport SEXP asCSVCharacter(SEXP rObj, SEXP sep=R_NilValue);
SEXP asCSVCharacter(SEXP rObj, const char* sep=",");
RcppExport SEXP fromCSVCharacter(SEXP rObj, SEXP type=R_NilValue, SEXP sep=R_NilValue);
SEXP fromCSVCharacter(SEXP rObj, const char* type="character", const char* sep=",");
void printToR(SEXP msg);
void printToR(const char* msg, va_list args);
void logToR(SEXP logconn, SEXP msg);
#endif