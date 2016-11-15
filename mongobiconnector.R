library(RODBC)
library(ggplot2)
library(microbenchmark)

# odbc.database = "biuser"
# odbc.user = "biuser"
# odbc.database = "winbiuser"
# odbc.user = "winbiuser"
odbc.database = "replbiuser"
odbc.user = "replbiuser"
odbc.password = "test77"

# bi_odbc <- odbcConnect("MongoBI", uid = odbc.user, pwd = odbc.password)
# bi_odbc <- odbcConnect("WinMongoBI", uid = odbc.user, pwd = odbc.password)
bi_odbc <- odbcConnect("ReplMongoBI", uid = odbc.user, pwd = odbc.password)

odbcGetInfo(bi_odbc)

bi_tables <- sqlTables(bi_odbc)

sqlQuery(bi_odbc, query = "select * from record_actual limit 1")

microbenchmark({
  postal_address <- sqlQuery(bi_odbc,
                             query = "select * from record_actual_validated_postal_address")
}, times = 5)

nrow(postal_address)
countries <- table(postal_address$validated.postal_address.country_code)
pie(countries)

odbcClose(bi_odbc)






ggplot(data = postal_address,
       mapping = aes(validated.postal_address.country_code))

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

