download.file("http://data.cquest.org/dgfip_dfi/dfi.sql.gz",
              "dfi.sql.gz")

library(tidyverse)
library(DBI)
# Connect to the MySQL database: con
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "defaultdb", 
                 host = "postgresql-728589", 
                 port = 5432,
                 user = "user-aubinpoissonnier",
                 password = "4ldcu5218n7y6twxk3eg")
# # Get table names
tables <- dbListTables(con)
# Display structure of tables
str(tables)

dfi <- tbl(con, "dgfip_dfi")
head(dfi)

colnames(dfi)

filter_condition <- "depcom IN ('69381', '69382', '69383', '69384', '69385', '69386', '69387', '69388', '69389')"

# Construct the SQL query with the filter
sql_query <- paste("SELECT * FROM dgfip_dfi WHERE", filter_condition)

filtered_table_data <- dbGetQuery(con, sql_query)

# Write filtered data to CSV
write.csv(filtered_table_data, "dfi_lyon.csv", row.names = FALSE)
