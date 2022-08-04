# =============================================================

# code to access WHD 24k hydro data and save to rds files
# Bryan M Maitland, PhD
# 25 Nov 2020

# ============================================================


# Workflow ----------------------------------------------
#  1) switch to 32-bit R (global options, then restart R)
#  1) pull data
#  2) save data to Rdata image
#  4) switch back to 64-bit R


# Switch to 32-bit R (MS Access is 32-bit) =========

# packages
library(RODBC)  

# Open DB connection
con <- odbcConnectAccess(
  here::here("data","spatial","whd_24k","24K_Hydro_VA_data.mdb"))

# Check available tables in WHD Plus
sqlTables(con)

# Pull tables
whd_base_atribs <- sqlFetch(con, "BaseAttributes_24k")
whd_flow_temp <- sqlFetch(con, "flow_temperature")
whd_watershed <- sqlFetch(con, "watershed_24K")

# Close DB connection
odbcClose(con)


# Open DB connection
con <- odbcConnectAccess(
  "./data/databases/Hydro24k_topology.mdb"
)

# Check available tables in WHD Plus
sqlTables(con)

# Pull tables
topology <- sqlFetch(con, "topology")

# Close DB connection
odbcClose(con)

# 3) Save Rdata ============================================

# Save tables into Rdata image to load into 64-bit R
save(
  whd_base_atribs, 
  whd_flow_temp,
  whd_watershed, 
  file = "./data/data-raw/hydro24k_data.Rdata"
)

saveRDS(whd_base_atribs, "./data/data-raw/whd_base_atribs.rds")
saveRDS(whd_flow_temp, "./data/data-raw/whd_flow_temp.rds")
saveRDS(whd_watershed, "./data/data-raw/whd_watersheds.rds")
saveRDS(topology, "./data/data-raw/whd_topology.rds")


### END 32-bit R session ###

# Switch to 64-bit R ===================================