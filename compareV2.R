library(data.table)
library(ggplot2)
library(plotly)

# ----------------------------------------------------------------------------
# we assume that we compare valid records from both data sets only. Invalid 
# records haven't been touched by the system, so there is no point in 
# comparing them

# ----------------------------------------------------------------------------
# reading both files and compare column names to identify differences in the
# CSV export structure. So we are creating a data frame with the header names
# from the CSV exports. The data frame helps us to identify differences and
# to rearrange if necessary
 
setwd("e:/R/compare_data/")
file.a <- "valid-15.4.csv"
file.b <- "valid-16.1.csv"

# determine the number of columns per file using
# head -n 1 <filename> |grep -o "\," |wc -l
ncol.a <- 113
ncol.b <- 116

system.time(dt.a <- fread(file.a, sep = ",", header = TRUE, na.strings = NULL, 
                          encoding = "UTF-8", colClasses = rep("character", 
                          ncol.a)))
system.time(dt.b <- fread(file.b, sep = ",", header = TRUE, na.strings = NULL,
                          encoding = "UTF-8", colClasses = rep("character", 
                          ncol.b)))

colnames.a <- colnames(dt.a)
colnames.b <- colnames(dt.b)

length(colnames.a) <- max(length(colnames.a), length(colnames.b))
length(colnames.b) <- max(length(colnames.a), length(colnames.b))

column.names <- data.frame(colnames.a, colnames.b, colnames.a == colnames.b,
                           stringsAsFactors = FALSE)
View(column.names)

# ----------------------------------------------------------------------------
# lets check for the number of records and compare them

nrow.a <- nrow(dt.a)
nrow.b <- nrow(dt.b)
max.rows <- max(nrow.a, nrow.b)
total.rows <- data.frame(c("CDH 15.4", "CDH 16.1"), 
                         c(nrow.a, nrow.b),
                         c((max.rows - nrow.a), (max.rows - nrow.b)),
                         stringsAsFactors = FALSE)
colnames(total.rows) <- c("system", "records", "difference")

plot.rows <- plot_ly(total.rows, x = system, y = records, 
                     name = "Valid Records", type = "bar") %>%
  add_trace(x = system, y = difference, name = "Difference")
layout(plot.rows, barmode = "stack")

# ----------------------------------------------------------------------------
# so now we know if there is a difference in the number of valid records. It is
# just the total number. There might be different records validated in system A 
# and system B so there might be different sets of data valid.
# So which records are valid in BOTH systems and how many of them are there?
# However: a record can be valid in both system A and B but with a different 
# validation result.
# Before we start joining, we have to keep in mind that each party can have 
# multiple addresses, phone numbers and other entities. So the records get 
# "exploded" i.e. we have a lot of redundant data per party.
#
# TODO: If we have become very clever with R and JSON we may switch from the 
#       CSV/line based apporoach to an unstructured/document style.
#
# Also we want to measure the difference in address validation and name 
# validation, so it might be the right time now to split the data sets. In order
# todo so, we review the column.names data frame created above and select the 
# relevant columns for subsetting
#
# TODO: Investigate how the selection of the relevant columns to subset can be 
# automated

# ----------------------------------------------------------------------------
# let's start with addresses and exclude the rows without an address type and 
# then let's get rid of duplicates

dt.a[, c(1, 2, 63:82), with = FALSE] %>%
  subset(postal_address.type != "") %>%
  unique() -> address.a

dt.b[, c(1, 2, 63:82), with = FALSE] %>%
  subset(postal_address.type != "") %>%
  unique() -> address.b

nrow.address.a <- nrow(address.a)
nrow.address.b <- nrow(address.b)

max.rows <- max(nrow.address.a, nrow.address.b)
address.rows <- data.frame(c("CDH 15.4", "CDH 16.1"), 
                         c(nrow.address.a, nrow.address.b),
                         c((max.rows - nrow.address.a), 
                           (max.rows - nrow.address.b)),
                         stringsAsFactors = FALSE)
colnames(address.rows) <- c("system", "records", "difference")

plot.address.rows <- plot_ly(address.rows, x = system, y = records, 
                             name = "Records with Addresses", type = "bar") %>%
  add_trace(x = system, y = difference, name = "Difference")
layout(plot.address.rows, barmode = "stack")

# ----------------------------------------------------------------------------
# now we have deduplicated unique addresses per source system and are ready to 
# (INNER) join both data sets

setkey(address.a, record.source, record.key, postal_address.type)
setkey(address.b, record.source, record.key, postal_address.type)

system.time(address.ab <- merge(address.a, address.b))

nrow.address.ab <- nrow(address.ab)

# ----------------------------------------------------------------------------
# it would also be interesting to identify the records which are either valid in 
# system A or B, but not in both systems
# LEFT OUTER join excluding everything on the right. Adding column with X to 
# have something to filter on
#
# TODO: can we visualize that somehow?

temp <- cbind(address.b, x = "X")
merge(address.a, temp, all.x = TRUE) %>%
  subset(is.na(x)) -> uniques.a

temp <- cbind(address.a, x = "X")
merge(address.b, temp, all.x = TRUE) %>%
  subset(is.na(x)) -> uniques.b

rm(temp)

#sanity.check <- merge(uniques.a, uniques.b)
#nrow(sanity.check) # -> must return 0

# ----------------------------------------------------------------------------
# If we compare these IDs we see that records fail validation because of 
# different reasons. Record X fails phone validation in system A but passes it
# in system B - regardless of the address.
#
# TODO: what about valid addresses in invalid records? Should we also export
#       invalid records and compare them? How can we export all records with a
#       validated address, regardless of the other entities? Of course, we would
#       have to do the same for names.
#
# For the time being, let's continue with addresses in valid records
#
# Okay, now we want to find out, if we have different address validation results
# in A and B
#
# The following creates a boolean vector per field comparison. Each vector 
# indicates if there is a difference in the respective field, so each vector 
# identifies the records with differences in given name, gender, salutation and 
# so on. Each vector is cbound to the right of the data frame
# columns 1 to 3 just contain source, key and type so nothing to compare here

View(data.table(colnames(address.ab)))

ncol.address.ab <- ncol(address.a) - 3
df.address.ab <- as.data.frame(address.ab)
is.equal <- vector(length = nrow(address.ab))
system.time(
  for (j in 4:ncol.address.ab) {
    is.equal <- df.address.ab[j] == df.address.ab[j + ncol.address.ab - 1]
    df.address.ab <- cbind(df.address.ab, is.equal)
  }
)











x <- data.table(tier = c("Katze","Hund", "Adler", "Papagei", "Hai", "Forelle", "Frosch"),
               id = c("1", "1", "2", "2", "3", "3", "5"), key = "id")
y <- data.table(art = c("Säugetier", "Vogel", "Vogel2", "Vogel3", "Fisch", "Reptil"),
               id = c("1", "2", "2", "2", "3", "4"), key = "id")
z <- merge(x, y, all.x = TRUE)

w <- subset(z, is.na(art))









# the following creates a boolean vector per field comparison. Each vector indicates if there 
# is a difference in the respective field, so each vector identifies the records with differences in given name, 
# gender, salutation and so on. Each vector is cbound to the data frame, starting at 2*ncols.
# columns 1 to 3 just contain source, key and type so nothing to compare here
# thanks to the post in http://datascienceplus.com/strategies-to-speedup-r-code/ this piece is now 150x faster than 
# using apply which was unnecessary anyway

isEqual <- vector(length = nrow(df_join))
system.time(
for (j in 4:ncols) {
  isEqual <- df_join[j] == df_join[j + ncols - 1]
  df_join <- cbind(df_join, isEqual)
})


# This is the distance between a merged column and its corresponding same/different vector
col_dist <- 2 * ncols - 4

# assign proper column names
system.time(for (j in 4:ncols) {
  colnames(df_join)[j + col_dist] <- paste("equal_", gsub("raw.", "", cols_raw[j]), sep = "")
})

# print number of differences
# calculation can be done simpler, see below!
for (j in 4:ncols) {
  num_diff <- sum(!df_join[j + col_dist])
  print(paste("Number of differences for column", j, gsub("merged.", "", colnames(df_join)[j]), ":", num_diff))
}

# Lets identify the rows where there aren't any differences ...
system.time(goods <- which(rowSums(!df_join[(ncols * 2): ncol(df_join)]) == 0))
system.time(bads <- which(rowSums(!df_join[(ncols * 2): ncol(df_join)]) > 0))

# ... and separate both data sets
system.time(df_bad <- df_join[bads,])
system.time(df_goods <- df_join[goods,])

# Let's free up some memory now
remove(isEqual, df_join, dt_merged, dt_raw)

# this one is probably right. HOWEVER: this just creates a new vector which is unrelated to the original data frame. On the other side,
# it doesn't make sense to calculate the distance for equal strings
system.time(d2 <- df_bad[, 10] == df_bad[, 27])

# the following will kill R...
# d <- adist(df_bad[, 10], df_bad[, 27])
# ... so we use a different function
system.time(d <- stringdist(df_bad[, 10], df_bad[, 10 + ncols - 1]))
f <- factor(d, exclude = 0)
plot(f)

# this was just one column, let's concatenate the most imprtant name fields
parts_1 <- c(6:10)
parts_2 <- parts_1 + ncols -1

system.time(a <- apply(df_bad[, parts_1], 1, function (x) { paste(x, collapse = "")}))
system.time(b <- apply(df_bad[, parts_2], 1, function (x) { paste(x, collapse = "")}))

system.time(c <- stringdist(a, b))

View(cbind(a, b, c))

f <- factor(c, exclude = 0)
plot(f)

# we can now calculate the number of differences per column, more efficient than above!
# it'll be ugly, but let's plot them
col_sums <- colSums(!df_bad[(ncols * 2): ncol(df_bad)])
plot(col_sums)

# and lets do the rowsums as well (number of different fields per reocrd)
row_sums <- rowSums(!df_bad[(ncols * 2): ncol(df_bad)])
rf <- factor(row_sums)
plot(rf)

# the following is a list of both raw and validated records, ordered by source system key
dt_all <- rbindlist(list(dt_merged, dt_raw))
setorder(dt_all, merged.key)


print(paste("Number of validated records", kpi1))
print(paste("Number of raw records", kpi2))
print(paste("Number of records joining validated and raw (sanity check only)", kpi1))
