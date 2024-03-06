download.file("http://data.cquest.org/dgfip_dfi/dfi.sql.gz",
              "dfi.sql.gz")


library(DBI)
# Connect to the MySQL database: con
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "defaultdb", 
                 host = "postgresql-810211", 
                 port = 5432,
                 user = "user-aubinpoissonnier",
                 password = "4ldcu5218n7y6twxk3eg")
# # Get table names
tables <- dbListTables(con)
# Display structure of tables
str(tables)

dfi <- tbl(con, "dgfip_dfi")
head(dfi)
