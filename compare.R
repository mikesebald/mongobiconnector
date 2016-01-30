library(data.table)
library(stringdist)

setwd("~/R/compare_data/")

# assumption: merged and raw record files have the same number of columns
cols <- read.table("fieldfile_merged.txt", stringsAsFactors = FALSE)
cols_merged <- gsub("person.0.", "", as.vector(cols$V1))
cols <- read.table("fieldfile_raw.txt", stringsAsFactors = FALSE)
cols_raw <- gsub("person.0.", "", as.vector(cols$V1))

ncols <- nrow(cols)

# the data used here is a mongoexport from t02
system.time(dt_merged <- fread("record_actual_merged_subs.csv", sep = ",", header = TRUE, na.strings = NULL, encoding = "UTF-8", colClasses = rep("character", ncols)))
system.time(dt_raw <- fread("record_actual_raw_subs.csv", sep = ",", header = TRUE, na.strings = NULL, encoding = "UTF-8", colClasses = rep("character", ncols)))

kpi1 <- nrow(dt_merged)
kpi2 <- nrow(dt_raw)

system.time(df_join <- as.data.frame(merge(dt_merged, dt_raw, by.x = "merged.key", by.y = "raw.key")))
colnames(df_join)[1:ncols] <- cols_merged
colnames(df_join)[(ncols + 1):(ncols * 2 - 1)] <- cols_raw[c(1, 3:ncols)]

paste(kpi3 <- nrow(df_join))


# the following creates a boolean vector per field comparison. Each vector indicates if there 
# is a difference in the respective field, so each vector identifies the records with differences in given name, 
# gender, salutation and so on. Each vector is cbound to the data frame, starting at 2*ncols
# columns 1 to 3 just contain source, key and type so nothing to compare here
system.time(for (j in 4:ncols) {
  isEqual <- apply(df_join[, c(j, j + ncols - 1)], 1, function(x) {x[1] == x[2]} )
  df_join <- cbind(df_join, isEqual)
})

# This is the distance between a merged column and its corresponding same/different vector
col_dist <- 2 * ncols - 4

# assign proper column names
system.time(for (j in 4:ncols) {
  colnames(df_join)[j + col_dist] <- paste("equal_", gsub("raw.", "", cols_raw[j]), sep = "")
})

# print number of differences
for (j in 4:ncols) {
  num_diff <- sum(!df_join[j + col_dist])
  print(paste("Number of differences for column", j, gsub("merged.", "", colnames(df_join)[j]), ":", num_diff))
}


# to get the keys of records with differences in e.g. given_names_full (column 8) we can call
# df_join[!df_join[8 + col_dist], 1]

# to display all records with changes in surname first
View(df_join[!df_join[10 + col_dist], c(1, 7:10, 24:27)])


# this one is probably right. HOWEVER: this just creates a new vector which is unrelated to the original data frame. On the other side,
# it doesn't make sense to calculate the distance for equal strings
system.time(d <- apply(df_join[!df_join[,], c(10, 27)], 1, function(x) { adist(x[1], x[2]) } ))


# the following is a list of both raw and validated records, ordered by source system key
dt_all <- rbindlist(list(dt_merged, dt_raw))
setorder(dt_all, merged.key)



print(paste("Number of validated records", kpi1))
print(paste("Number of raw records", kpi2))
print(paste("Number of records joining validated and raw (sanity check only)", kpi1))
