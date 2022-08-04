
# Install wdnr.fmdb from file:
install.packages(here::here("wdnr.fmdb_0.3.2.tar.gz"), 
                 type = "source",
                 repos = NULL)

# Update package
wdnr.fmdb::update_fmdb()
