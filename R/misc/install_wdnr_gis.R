
# Paul Fater and Zac Driscol split wdnr.gis into two packages:
#  + General ArcGIS Rest API package
#  + Specific DNR APR packages

# Must install sf first (from CRAN), then arcgis.rest, and then wdnr.gis.

# wdnr.gis is contains functions to pull spatial data layers from DNR. 
#   + list_sections(), list_services, list_layers(), and 
#   + match_sections(), match_services(), match_layers() 

# The main functions are wrapper functions to pull specific layers.
#  + get_watershed_layer()
#  + get_hydro_layer( 
#  + get_fmdb_sites_layer()
#  + get_roads_layer()

# These wrap around functions from arcgis.rest:
#  + get_spatial_layer()
#  + get_layer_by_<poly, point, line, etc.>()

# We can add wrapper functions for layers that we will need. 

# Install arcgis rest from file:
install.packages("C:/Users/maitlb/Documents/WI-Trout/Packages/arcgis.rest_0.0.0.9999.tar.gz", 
                 type = "source",
                 repos = NULL)


# Install wdnr.gis from file:
install.packages("C:/Users/maitlb/Documents/WI-Trout/Packages/wdnr.gis_0.0.0.9999.tar.gz", 
                 type = "source",
                 repos = NULL)
