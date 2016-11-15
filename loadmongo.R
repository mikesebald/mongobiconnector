library(mongolite)

args <- commandArgs(TRUE)
if (length(args) != 5) {
  message("------------------------------------------------------------------------------")
  message("Usage: rscript loadmongo.R <database> <collection> <source file> <url> <sleep>")
  message("e.g.: rscript loadmongo.R cdh record_actual e:\ubuntu_transfer\record_actual.json mongodb://localhost:27019 1")
  message("Exiting...")
  quit()
}

mdbname <- args[1]
colname <- args[2]
file.name <- args[3]
mongo.url <- args[4]
sleep.time <- args[5]

message(paste0("Trying to write into collection ", colname, " in database ", 
               mdbname, " from file ", file.name, " and waiting for ", 
               sleep.time, " second(s) after every insert.\n"))

mdb <- mongo(collection = colname, db = mdbname, url = mongo.url)

message("Reading file...")
con <- file(file.name)
json.file <- readLines(con)
close(con)
file.length <- length(json.file)
message(paste0("done. Read ", file.length, " lines."))
        

if (file.length == 0) {
  message("Input file is empty, exiting.")
  quit()
}

message("Purging all records from collection...")
mdb$remove(query = "{}", multiple = TRUE)
message("done.")

for (i in 1:file.length) {
  mdb$insert(json.file[i])
  message(paste0("Processed line ", i, " of ", length(json.file), ", sleeping..."))
  Sys.sleep(sleep.time)
}

rm(mdb)
gc()
