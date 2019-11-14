
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EPOC

<!-- badges: start -->

<!-- badges: end -->

Migrating EPOC, MDSumner 2019-11-14

From:
<https://github.com/AndrewJConstable/EPOCbase/tree/6b387ba39a312df5d674592acb123ae7dc6bf92a/Packaging/EPOC>

Status of that was committed with tag v0.5.3 at
<https://github.com/AustralianAntarcticDivision/EPOC/releases/tag/v0.5.3>

## Fixes / TODO

  - `openRcppFileConn` had incompatible signatures
  - `writeRcppFileCon` had incompatible signatures
  - there’s no src for getpathFileConnection, so I’ve commented out the
    S4 method:

<!-- end list -->

``` r
# setMethod(".stopReporting", signature(.Object="Universe"),
#   function(.Object) {
#       logconn <- getFileConnection(.Object, "logFile")
#       if (!is.null(logconn) && class(logconn) == "externalptr") {
#           logpath <- .Call("getpathFileConnection", .Object$.logconn, PACKAGE="EPOC")
#           writeFileConnection(.Object, logconn, paste(date(), ": Closing log file connection: ", oldlogpath))
#           closeFileConnection(.Object, logconn)
#       }
#
#       return(invisible(.Object))
#   }
# )
```
