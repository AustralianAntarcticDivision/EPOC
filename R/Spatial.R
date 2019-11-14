################################################################################
# EPOC Spatial class
#
# EC.Polygons.KPFM.01<-function (Config, InputData)

# Function:           KPFM.Polygons.01.R
# Description:        KPFM configuration element for KPFM model - polygons
# Primary attributes: Details of polygons including grid coordinates,
#                     relative areas of each grid cell contributing to polygon
#                     total area of polygons

# S4 Class
# Created 14/5/2009 Troy Robertson
################################################################################

# Create Spatial class
setClass("Spatial",
	representation(polygonNames		= "character",
				   polygonAreas		= "numeric",	
				   coords			= "list",
				   coordAreas		= "list",
				   coordProportions	= "list",
				   overlap			= "matrix",
				   polygonN			= "numeric"),
				   
	# prototype(	   polygonNames		= NULL,
				   # polygonAreas		= NULL,	
				   # coords			= NULL,
				   # coordAreas		= NULL,
				   # coordProportions	= NULL,
				   # overlap			= NULL,
				   # polygonN			= NULL),
	contains						= "EPOCObject"
)

setMethod("initialize", signature(.Object="Spatial"),
    function(.Object, dataPath=NULL) {
		
		.Object <- callNextMethod(.Object, dataPath)
		
		######### Set any default slots
		.Object@polygonNames = c(
			"Entirety"
		)
	
		# square km (original in m2 from Watters data file - precision kept the same for comparisons)
		.Object@polygonAreas = c(1)

		.Object@coords = list(
			Entirety = c(1)
		)
		
		.Object@coordAreas = list(
			Entirety = 1
		)

		.Object@coordProportions = list(
			Entirety = 1
		)

		#  number of polygons
		.Object@polygonN = length(.Object@coords)
		
		# additional amalgamated polygons
		#    GeneralPredator<-NULL
		#    GenPredGridAreas<-NULL
		#    GenPredPolygonArea<-0
		#    for (pn in 1:Polygon$Polygon.N){
		#       GeneralPredator<-c(GeneralPredator,Polygon$Coords[[pn]])
		#       GenPredGridAreas<-c(GenPredGridAreas,Polygon$Coord.Areas[[pn]])
		#       GenPredPolygonArea<-GenPredPolygonArea+Polygon$Polygon.Areas[pn]
		#      }
		#    Polygon$Polygon.Names<-c(Polygon$Polygon.Names,"GeneralPredator")
		#    Polygon$Polygon.Areas<-c(Polygon$Polygon.Areas,GenPredPolygonArea)
		#    Polygon$Coords<-c(Polygon$Coords,list(GenPredator = GeneralPredator))
		#    Polygon$Coord.Areas<-c(Polygon$Coord.Areas,list(GenPredator = GenPredGridAreas))
		#    Polygon$Polygon.N = length(Polygon$Coords)

		#   Polygon overlap - matrix of proportions of overlap -
		#        proportion of subject polygon (rows) in other polygon (cols)
		#     ????need to write a routine that automatically determines overlap
		#       (need to avoid partial coordinates that are non-overlapping but may
		#        appear overlapping when sorted solely by coordinates.)
		#
		.Object@overlap <- matrix(c(1),nrow=.Object@polygonN,byrow=TRUE)
     
		# Overwrite default data with any available in input data file
		.Object <- parseInputData(.Object)
      
		.Object@polygonN <- length(.Object@coords)
		
		#   return Polygon environment
		return(.Object)
	}
)

## Accessor methods
# Available via setSlot and getSlot in EPOCObject