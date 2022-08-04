
library(tidyverse) 
library(readxl)

data_dir <- here("data","mitro")

data_mitro <- 
  data_dir %>% 
  dir_ls(regexp = "\\.xlsx$") %>% 
  map_dfr(read_excel, skip = 2, .id = "source")

data_mitro


data_dir <- here("data","mitro")

files <- data_dir %>% dir_ls(regexp = "\\.xlsx$")

files <- list.files(path = here("data","mitro"),
                            pattern = "\\.xlsx$",
                            full.names = TRUE)

# nested map_df call
data_xlsx_df <- map_df(set_names(files), function(file) {
  file %>% 
    excel_sheets() %>% 
    set_names() %>% 
    map_df(
      ~ read_xlsx(path = file, sheet = .x),
      .id = "sheet")
}, .id = "file")
