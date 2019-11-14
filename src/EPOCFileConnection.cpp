/*********************************************************************************
 * Copyright (C) 2012	Australian Antarctic Division
 *
 * Author: Troy Robertson
 *
 * EPOCFileConnection.cpp
 * This file forms part of the API library for EPOC and is included in the 
 * EPOC R package along with:
 * EPOC.h
 * EPOC.cpp
 * EPOCFileConnection.cpp
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
#include <iostream>
#include <fstream>
#include <map>

#ifndef EPOC_hpp
#include <EPOC.h>
#endif

using namespace std;
using namespace Rcpp;

FileConnection::FileConnection ( std::string connpath, std::string openmode ) : path(connpath), mode(openmode) {
	if ( path == "" ) return;
	if ( mode.find("r") != string::npos ) {
		iofile.open(path.c_str(), ios::in);
		if (!iofile) std::cerr << "Can't open input file: " << path << endl;
	} else if ( mode.find("w") != string::npos ) {
		iofile.open(path.c_str(), ios::out);
	} else {
		// open in append mode
		mode = "a";
		iofile.open(path.c_str(), ios::out | std::ios::app);
	}
}
FileConnection::~FileConnection () {
	if ( iofile.is_open() ) iofile.close();
}
bool FileConnection::open(string connpath, string openmode) {
	if ( connpath == "" ) return false;
	// reopen in new mode if necessary
	if ( (connpath == path && openmode == mode) && iofile.is_open() ) return true; 
	path = connpath;
	mode = openmode;
	close();
	return openconn();
}
bool FileConnection::openconn() {
	if ( iofile.is_open() ) return true;
	if ( mode.find("r") != string::npos ) {
		iofile.open(path.c_str(), ios::in);
		if (!iofile) {
			cerr << "Can't open input file: " << path << endl;
			return false;
		}
	} else if ( mode.find("w") != string::npos ) {
		iofile.open(path.c_str(), ios::out);
	} else {
		// open in append mode
		iofile.open(path.c_str(), ios::out | std::ios::app);
	}
	return true;
}
bool FileConnection::isopen() { return iofile.is_open(); }
const char* FileConnection::getpath() { return path.c_str(); }
const char* FileConnection::getmode() { return mode.c_str(); }
bool FileConnection::write(std::string msg, bool eol) {
	// Check mode
	if ( mode.find("w") == string::npos && mode.find("a") == string::npos ) open(path.c_str(), "a");
	if ( openconn() ) {
		iofile << msg;
		if ( eol ) iofile << endl;
		iofile.flush();
		return true;
	}
	return false;
}
string FileConnection::readline(int linenum) {
	string msg;
	// Check mode and reopen if necessary
	if ( mode.find("r") == string::npos ) open(path.c_str(), "r");
	if ( openconn() ) {
		int lineidx = linenum - 1;
		if ( lineidx >= 0 ) {
			iofile.clear();
			iofile.seekg(ios::beg);
			for ( int i = 0 ; i < lineidx && getline(iofile, msg) ; i++ );
		}
		if ( !iofile.eof() ) getline(iofile, msg);
	}
	return msg;
}
map<int, string> FileConnection::readfile() {
	string line;
	int i = 0;
	map<int, string> msg;
	
	// Check mode and reopen if necessary, reset to start file
	if ( mode.find("r") == string::npos ) open(path.c_str(), "r");
	if ( openconn() ) {
		iofile.clear();
		iofile.seekg(ios::beg);
		while ( getline(iofile, line) ) {
			msg.insert(std::pair<int, string>(i, line));
			i += 1;
			//msg.push_back(line);
		}
	}
	return msg;
}
bool FileConnection::close() {
	if ( iofile.is_open() ) {
		iofile.close();
		// If file was write and is later reopened it will then be in append
		if ( mode.find("w") != string::npos ) mode = "a"; 
		return true;
	}
	return false;
}

RCPP_MODULE(FileConn_Module) {
	class_<FileConnection>( "FileConnection" )
	.constructor<std::string, std::string>()
	//.field( "connpath", &FileConnection::path )
	//.field( "openmode", &FileConnection::mode )
	.method( "open", &FileConnection::open )
	.method( "isopen", &FileConnection::isopen )
	.method( "getpath", &FileConnection::getpath )
	.method( "getmode", &FileConnection::getmode )
	.method( "write", &FileConnection::write )
	.method( "readline", &FileConnection::readline )
//	.method( "readfile", &FileConnection::readfile )
	.method( "close", &FileConnection::close )
	//.finalizer( &FileConnection_finalizer ) ;
	;
}

/** Create an external pointer to a FileConnection object */
RcppExport SEXP createRcppFileConn(SEXP connpath, SEXP openmode) {
BEGIN_RCPP
	if ( toStr(connpath) == "" ) Rf_error("Connection path required.");
	if ( toStr(openmode) == "" ) openmode = Rcpp::wrap("a");
	return Rcpp::XPtr<FileConnection>( new FileConnection(Rcpp::as<std::string>(connpath), 
														Rcpp::as<std::string>(openmode)), true );
END_RCPP
}
SEXP createRcppFileConn(const char* connpath, const char* openmode) {
	return createRcppFileConn(Rcpp::wrap(connpath), Rcpp::wrap(openmode));
}
RcppExport SEXP openRcppFileConn(SEXP fcObj, SEXP connpath, SEXP openmode) {
BEGIN_RCPP
	Rcpp::XPtr<FileConnection> fc(fcObj);
	if ( toStr(openmode) == "" ) openmode = Rcpp::wrap("a");
	if ( toStr(connpath) == "" ) return Rcpp::wrap(fc->openconn());
	return Rcpp::wrap(fc->open(Rcpp::as<std::string>(connpath), Rcpp::as<std::string>(openmode)));
END_RCPP
}
SEXP openRcppFileConn(SEXP fcObj, const char* connpath, const char* openmode) {
	return openRcppFileConn(fcObj, Rcpp::wrap(connpath), Rcpp::wrap(openmode));
}
RcppExport SEXP isopenRcppFileConn(SEXP fcObj) {
BEGIN_RCPP
	Rcpp::XPtr<FileConnection> fc(fcObj);
	return Rcpp::wrap(fc->isopen());
END_RCPP
}
RcppExport SEXP getpathRcppFileConn(SEXP fcObj) {
BEGIN_RCPP
	Rcpp::XPtr<FileConnection> fc(fcObj);
	return Rcpp::wrap(fc->getpath());
END_RCPP
}
RcppExport SEXP getmodeRcppFileConn(SEXP fcObj) {
BEGIN_RCPP
	Rcpp::XPtr<FileConnection> fc(fcObj);
	return Rcpp::wrap(fc->getmode());
END_RCPP
}
RcppExport SEXP writeRcppFileConn(SEXP fcObj, SEXP msg, SEXP eol) {
BEGIN_RCPP
	if ( eol == NULL || Rf_isNull(eol) ) eol = Rcpp::wrap(true);
	Rcpp::XPtr<FileConnection> fc(fcObj);
	return Rcpp::wrap(fc->write(Rcpp::as<std::string>(msg), Rcpp::as<bool>(eol)));
END_RCPP
}
SEXP writeRcppFileConn(SEXP fcObj, const char* msg, bool eol) {
	return writeRcppFileConn(fcObj, Rcpp::wrap(string(msg)), Rcpp::wrap(eol));
}
RcppExport SEXP readlineRcppFileConn(SEXP fcObj, SEXP linenum) {
BEGIN_RCPP
	if ( linenum == NULL || Rf_isNull(linenum) ) linenum = Rcpp::wrap(0);
	Rcpp::XPtr<FileConnection> fc(fcObj);
	return Rcpp::wrap(fc->readline(Rcpp::as<int>(linenum)));
END_RCPP
}
SEXP readlineRcppFileConn(SEXP fcObj, int linenum) {
	return readlineRcppFileConn(fcObj, Rcpp::wrap(linenum));
}
RcppExport SEXP readRcppFileConn(SEXP fcObj) {
BEGIN_RCPP
	Rcpp::XPtr<FileConnection> fc(fcObj);
	std::map<int, string> linemap = fc->readfile();
	std::map<int, string>::iterator mapiter;
	Rcpp::List linelist(linemap.size());
	for ( mapiter = linemap.begin() ; mapiter != linemap.end() ; ++mapiter ) {
		linelist[mapiter->first] = mapiter->second;
	}
	return linelist;
END_RCPP
}
RcppExport SEXP closeRcppFileConn(SEXP fcObj) {
BEGIN_RCPP
	Rcpp::XPtr<FileConnection> fc(fcObj);
	return Rcpp::wrap(fc->close());
END_RCPP
}

/** NOTE: There is a problem with this class and it's associated Rcpp exported functions
 * For some reason FileConnection objects instantiated by this class are not PROTECTED and
 * memory segmentation faults occur.
 * Instead, EPOC R code implements the functionality of the ConnectionManager.
 **/
 /**
class ConnectionManager {
	public:
		ConnectionManager () { connections = new ConnMap; }
		ConnectionManager ( std::string name, std::string connpath, std::string openmode ) {
			connections = new ConnMap;
			// open and add to list if not already there
			FileConnection *conn = new FileConnection(connpath, openmode);
			(*connections)[name] = conn;
		}
		~ConnectionManager () {
			for ( ConnMap::iterator i = connections->begin(); i != connections->end(); ++i ) {
				(*i->second).close();
			}
			delete connections;
		}
		FileConnection* get(std::string name) {
			if ( connections->find(name) != connections->end() ) return (*connections)[name];
			return NULL;
		}
		bool add(FileConnection* conn, std::string name) {
			(*connections)[name] = conn;
			return true;
		}
		FileConnection* open(std::string name, std::string connpath, std::string openmode) {
			if ( connections->find(name) == connections->end() ) {
				//if not in conn map create and add to map first
				FileConnection *conn = new FileConnection(connpath.c_str(), openmode.c_str());
				connections->insert(std::pair<std::string, FileConnection*>(name, conn));
			} else {
				// otherwise call reopen in case path/mode has changed
				(*connections)[name]->open(connpath, openmode);
			}
			
			return (*connections)[name];
		}
		bool close() {
			bool retval = false;
			for ( ConnMap::iterator i = connections->begin(); i != connections->end(); ++i ) {
				(*i->second).close();
				retval = true;
			}
			return retval;
		}
		bool close(std::string name) {
			if ( connections->find(name) != connections->end() ) return (*connections)[name]->close();
			return false;
		}
		bool write(std::string name, std::string msg) {
			if ( connections->find(name) != connections->end() ) return (*connections)[name]->write(msg);
			return false;
		}
		std::string readline(std::string name, int linenum = -1) {
			if ( connections->find(name) != connections->end() ) return (*connections)[name]->readline(linenum);
			return "";
		}
		std::string read(std::string name) {
			if ( connections->find(name) != connections->end() ) return (*connections)[name]->readfile();
			return "";
		}
		
	private:
		typedef std::map<std::string, FileConnection*> ConnMap;
		ConnMap *connections;
};

RCPP_MODULE(ConnManager_Module) {
	class_<ConnectionManager>( "ConnectionManager" )
	.constructor()
	.constructor<std::string, std::string, std::string>()
	//.field( "connections", &ConnectionManager::mode )
	//.method( "getconn", &ConnectionManager::get )
	//.method( "openconn", &ConnectionManager::open )
	.method( "readlineconn", &ConnectionManager::readline )
	.method( "readconn", &ConnectionManager::read )
	.method( "writeconn", &ConnectionManager::write )	
	//.method( "closeconn", &ConnectionManager::close )
	;
}

RcppExport SEXP createConnectionManager() {
BEGIN_RCPP
	return Rcpp::XPtr<ConnectionManager>( new ConnectionManager(), true );
END_RCPP
}
RcppExport SEXP getConnection(SEXP cmObj, SEXP connname) {
BEGIN_RCPP
	if ( connname == NULL || Rf_isNull(connname) || !Rf_isString(connname) ) Rf_error("Connection string required.");
	Rcpp::XPtr<ConnectionManager> cm(cmObj);
	FileConnection* fc = cm->get(Rcpp::as<std::string>(connname));
	if (fc != NULL) return Rcpp::XPtr<FileConnection>(fc);
	return R_NilValue;
END_RCPP
}
RcppExport SEXP addConnection(SEXP cmObj, SEXP fcObj, SEXP connname) {
BEGIN_RCPP
	if ( connname == NULL || Rf_isNull(connname) || !Rf_isString(connname) ) Rf_error("Connection string required.");
	Rcpp::XPtr<ConnectionManager> cm(cmObj);
	Rcpp::XPtr<FileConnection> fc(fcObj);
	return Rcpp::wrap(cm->add(fc, Rcpp::as<std::string>(connname)));
END_RCPP
}
RcppExport SEXP openConnection(SEXP cmObj, SEXP connname, SEXP connpath, SEXP openmode) {
BEGIN_RCPP
	if ( connname == NULL || Rf_isNull(connname) || !Rf_isString(connname) ) Rf_error("Connection string required.");
	if ( connpath == NULL || Rf_isNull(connpath) || !Rf_isString(connpath) ) connpath = connname;
	if ( openmode == NULL || Rf_isNull(openmode) || !Rf_isString(openmode) ) openmode = Rcpp::wrap("a");
	Rcpp::XPtr<ConnectionManager> cm(cmObj);
	FileConnection* fc = cm->open(Rcpp::as<std::string>(connname), Rcpp::as<std::string>(connpath), 
																		Rcpp::as<std::string>(openmode));
	if (fc != NULL) return Rcpp::XPtr<FileConnection>(fc);
	return R_NilValue;
END_RCPP
}
RcppExport SEXP isopenConnection(SEXP cmObj, SEXP connname) {
BEGIN_RCPP
	if ( connname == NULL || Rf_isNull(connname) || !Rf_isString(connname) ) Rf_error("Connection string required.");
	Rcpp::XPtr<ConnectionManager> cm(cmObj);
	FileConnection* fc = cm->get(Rcpp::as<std::string>(connname));
	if (fc != NULL) return Rcpp::wrap(fc->isopen());
	return R_NilValue;
END_RCPP
}
RcppExport SEXP getpathConnection(SEXP cmObj, SEXP connname) {
BEGIN_RCPP
	if ( connname == NULL || Rf_isNull(connname) || !Rf_isString(connname) ) Rf_error("Connection string required.");
	Rcpp::XPtr<ConnectionManager> cm(cmObj);
	FileConnection* fc = cm->get(Rcpp::as<std::string>(connname));
	if (fc != NULL) return Rcpp::wrap(fc->getpath());
	return R_NilValue;
END_RCPP
}
RcppExport SEXP getmodeConnection(SEXP cmObj, SEXP connname) {
BEGIN_RCPP
	if ( connname == NULL || Rf_isNull(connname) || !Rf_isString(connname) ) Rf_error("Connection string required.");
	Rcpp::XPtr<ConnectionManager> cm(cmObj);
	FileConnection* fc = cm->get(Rcpp::as<std::string>(connname));
	if (fc != NULL) return Rcpp::wrap(fc->getmode());
	return R_NilValue;
END_RCPP
}
RcppExport SEXP writeConnection(SEXP cmObj, SEXP connname, SEXP msg) {
BEGIN_RCPP
	if ( connname == NULL || Rf_isNull(connname) || !Rf_isString(connname) ) Rf_error("Connection string required.");
	if ( msg == NULL || Rf_isNull(msg) ) msg = Rcpp::wrap("");
	Rcpp::XPtr<ConnectionManager> cm(cmObj);
	return Rcpp::wrap(cm->write(Rcpp::as<std::string>(connname), toStr(msg)));
END_RCPP
}
SEXP writeConnection(SEXP cmObj, const char* connname, const char* msg) {
	return writeConnection(cmObj, Rcpp::wrap(string(connname)), Rcpp::wrap(string(msg)));
}
RcppExport SEXP readlineConnection(SEXP cmObj, SEXP connname, SEXP linenum) {
BEGIN_RCPP
	if ( connname == NULL || Rf_isNull(connname) || !Rf_isString(connname) ) Rf_error("Connection string required.");
	if ( linenum == NULL || Rf_isNull(linenum) || !Rf_isNumeric(linenum) ) linenum = Rcpp::wrap(-1);
	Rcpp::XPtr<ConnectionManager> cm(cmObj);
	return Rcpp::wrap(cm->readline(Rcpp::as<std::string>(connname), Rcpp::as<int>(linenum)));
END_RCPP
}
RcppExport SEXP readConnection(SEXP cmObj, SEXP connname) {
BEGIN_RCPP
	if ( connname == NULL || Rf_isNull(connname) || !Rf_isString(connname) ) Rf_error("Connection string required.");
	Rcpp::XPtr<ConnectionManager> cm(cmObj);
	return Rcpp::wrap(cm->read(Rcpp::as<std::string>(connname)));
END_RCPP
}
RcppExport SEXP closeConnection(SEXP cmObj, SEXP connname) {
BEGIN_RCPP
	Rcpp::XPtr<ConnectionManager> cm(cmObj);
	if (connname == NULL || Rf_isNull(connname) ) {
		return Rcpp::wrap(cm->close());
	} else {
		return Rcpp::wrap(cm->close(Rcpp::as<std::string>(connname)));
	}
END_RCPP
}
**/