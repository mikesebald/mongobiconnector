library(RODBC)

odbc.database = "biuser"
odbc.user = "biuser"
odbc.password = "test123Abc"

bi_odbc <- odbcConnect("MongoBI", uid = odbc.user, pwd = odbc.password)

foo <- sqlQuery(bi_odbc, 
                as.is = TRUE, 
                query = "select * from foo")

odbcGetInfo(bi_odbc)

bi_tables <- sqlTables(bi_odbc)

odbcClose(bi_odbc)



record_actual_cols <- sqlColumns(bi_odbc, "record_actual")

record_actual <- sqlQuery(bi_odbc, as.is = TRUE, 
                          query = "select * from record_actual")

record_actual_merged_party <- sqlQuery(bi_odbc, as.is = TRUE, 
                                       query = "select * from \
                                       record_actual_merged_party")



