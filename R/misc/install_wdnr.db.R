# to install wdnr.db, download the wdnr.db_0.x.x.tar.gz file from sharepoint
# and run the following
# Please, please, please reach out to Paul Frater <paul.frater@wisconsin.gov>
# if you run into any issues!

install.packages(file.choose(), type = "source", repos = NULL)

library(wdnr.db)


?query_db


con <- wdnr_connect("dnr_sde.world")
huc8 <- query_db(con, "select huc8_name from SDEDNR.EN_WBD_HUC8_AR_VAR")
