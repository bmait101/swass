

con <- 
  wdnr.db::wdnr_connect(
    "dnr_secprd.world",
    user = "maitlb", 
    password = "BrookFish2112"
  )

wdnr.db::wdnr_connect(db = "dnr_sde.world", schema = "SDEDNR", user = "maitlb", 
                      password = "BrookFish2112")


list_schemas(con)
# sort(list_tables(con, schema = "W07510"))
# list_fields(con, schema = "W07510", table = "WT_SWIMS_MONIT_STATION_LOC_V") %>% sort()
# list_fields(con, schema = "W07510", table = "WT_SWIMS_RESULT_FACT_V") %>% sort()

# first need xwalk table to get data from the results table

# create a special SQL string to pass to the W07510.WT_SWIMS_MONIT_STATION_LOC_V
ids <- df_fishraw_cln %>% distinct(swims.station.id) %>% pull(swims.station.id)
id_string <- toString(sprintf("'%s'", ids))
sql_fmt <- "select STATION_ID, MONIT_STATION_SEQ_NO from W07510.WT_SWIMS_MONIT_STATION_LOC_V where STATION_ID in (%s)"
sql <- sprintf(sql_fmt, id_string)
xwalk <- query_db(con, sql)

# create a special SQL string to pass to the W07510.WT_SWIMS_RESULT_FACT_V
ids <- xwalk %>% pull(MONIT_STATION_SEQ_NO)
id_string <- toString(sprintf("'%s'", ids))
sql_fmt <- "select * from W07510.WT_SWIMS_RESULT_FACT_V where MONIT_STATION_SEQ_NO in (%s)"
sql <- sprintf(sql_fmt, id_string)
df_swims <- query_db(con, sql)

# check it
df_swims %>% glimpse()

# write_rds()
# read_rds()


wdnr_disconnect(con)



# 56 = flow rate g/d
# 58
# 61 - stream flow cfs
# 2000-2003
# 4226 - calc corrected flow
# 91870 - measured field discharge - cfs

df_swims_temp <- 
  df_swims %>% 
  as_tibble() %>% 
  filter(DNR_PARAMETER_CODE %in% c("10"), 
         QC_FLAG == "1") %>% 
  select(
    swims_station_id = MONIT_STATION_SEQ_NO, 
    sample_id = SAMPLE_RESULT_SEQ_NO,
    temp = RESULT_AMT, 
    units = RESULT_UNITS_TEXT, 
    date_time = RESULT_DATE_TIME
  )


df_swims_flow <- 
  df_swims %>% 
  as_tibble() %>% 
  filter(DNR_PARAMETER_CODE %in% c("61"), 
         QC_FLAG == "1") %>% 
  select(
    swims_station_id = MONIT_STATION_SEQ_NO, 
    sample_id = SAMPLE_RESULT_SEQ_NO,
    temp = RESULT_AMT, 
    units = RESULT_UNITS_TEXT, 
    date_time = RESULT_DATE_TIME
  )