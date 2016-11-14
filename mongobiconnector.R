library(RODBC)

odbc.database = "biuser"
odbc.user = "biuser"
odbc.password = "test77"

bi_odbc <- odbcConnect("MongoBI", uid = odbc.user, pwd = odbc.password)

odbcGetInfo(bi_odbc)

bi_tables <- sqlTables(bi_odbc)

record_actual <- sqlQuery(bi_odbc, query = "select * from record_actual")

postal_address <- sqlQuery(bi_odbc,
                           query = "select * from record_actual_merged_postal_address")

odbcClose(bi_odbc)



record_actual_cols <- sqlColumns(bi_odbc, "record_actual")


foo <- sqlQuery(bi_odbc, 
                as.is = TRUE, 
                query = "select * from foo")


sqlQuery(bi_odbc, "SELECT * FROM record_actual")


record_actual <- sqlQuery(bi_odbc, query = "select * from record_actual")

record_actual_merged_party <- sqlQuery(bi_odbc, as.is = TRUE, 
                                       query = "select * from \
                                       record_actual_merged_party")


# more stuff to come...

