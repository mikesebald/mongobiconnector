library(data.table)
library(ggplot2)
library(plotly)

# ----------------------------------------------------------------------------
# we assume that we compare valid records from both data sets only. Invalid 
# records haven't been touched by the system, so there is no point in 
# comparing them
# ===> reworking this one currently!

# ----------------------------------------------------------------------------
# reading both files and compare column names to identify differences in the
# CSV export structure. So we are creating a data frame with the header names
# from the CSV exports. The data frame helps us to identify differences and
# to rearrange if necessary

#setwd("e:/R/compare_data/")
setwd("../compare_data/")
file.a <- "member-15.4.csv"
file.b <- "member-16.1.csv"

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

# this is the point where we should look at the source and rearrange,
# if necessary. Both data sets whould have the same columns
View(column.names)
sum(!column.names[,3])
sum(!column.names[,3], na.rm = TRUE)


# ----------------------------------------------------------------------------
# lets check for the number of rows and compare them

nrow.a <- nrow(dt.a)
nrow.b <- nrow(dt.b)
max.rows <- max(nrow.a, nrow.b)
total.rows <- data.frame(c("CDH 15.4", "CDH 16.1"),
                         c(nrow.a, nrow.b),
                         c((max.rows - nrow.a), (max.rows - nrow.b)),
                         stringsAsFactors = FALSE)
colnames(total.rows) <- c("system", "rows", "difference")

plot.rows <- plot_ly(total.rows, x = system, y = records, 
                     name = "Valid Rows", type = "bar") %>%
  add_trace(x = system, y = difference, name = "Difference")
layout(plot.rows, barmode = "stack")

# ----------------------------------------------------------------------------
# so now we know if there is a difference in the number of valid rows It is
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
# to do so, we review the column.names data frame created above and select the 
# relevant columns for subsetting
#
# TODO: Investigate how the selection of the relevant columns to subset can be 
# automated

# ----------------------------------------------------------------------------
# let's start with addresses and exclude the rows without an address type and 
# then let's get rid of duplicates

dt.a[, c(1, 2, 62:83), with = FALSE] %>%
  subset(postal_address.type != "") %>%
  unique() -> address.a

dt.b[, c(1, 2, 62:83), with = FALSE] %>%
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
colnames(address.rows) <- c("system", "rows", "difference")

plot.address.rows <- plot_ly(address.rows, x = system, y = rows, 
                             name = "Records with Addresses", type = "bar") %>%
  add_trace(x = system, y = difference, name = "Difference")
layout(plot.address.rows, barmode = "stack")



# ----------------------------------------------------------------------------
# then we should do the same for names and throw away the original input to 
# make some memory available
# TODO: name extraction from files
# TODO: investigate how much sense a call to rm() makes
rm(dt.a, dt.b)

# ----------------------------------------------------------------------------
# now we have deduplicated unique addresses per source system and are ready to 
# (INNER) join both data sets

setkeyv(address.a, c("record.source", "record.key", "postal_address.type"))
setkeyv(address.b, c("record.source", "record.key", "postal_address.type"))
system.time(address.ab <- merge(address.a, address.b))

# cleaning up the column names which have been messed up during the merge, make
# sure they get a correct suffix
colnames(address.ab) <- gsub("\\.x$", ".a", colnames(address.ab))
colnames(address.ab) <- gsub("\\.y$", ".b", colnames(address.ab))

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
# so the magic numbers here are 2, 3 and 4
# 2 = times x = the distance between the columns to compare
# 3 = the three columns on the left we don't have to compare (source, key, type)
# 4 = because we start at column 4 to compare fields
# Thanks to the post in http://datascienceplus.com/strategies-to-speedup-r-code/
# for helping on the performance side

View(data.table(colnames(address.ab)))

ncol.address <- ncol(address.a) - 3
df.address.ab <- as.data.frame(address.ab)
is.equal <- vector(length = nrow(address.ab))
system.time(
  for (j in 4:(ncol.address + 3)) {
    is.equal <- df.address.ab[j] == df.address.ab[j + ncol.address]
    df.address.ab <- cbind(df.address.ab, is.equal)
  }
)

# assign proper names to the comparison columns
col.start <- ncol.address * 2 + 4
col.end <- ncol(df.address.ab)

colnames(df.address.ab)[col.start:col.end] <-
  gsub("\\.a$", ".equal", colnames(df.address.ab[col.start:col.end]))

# some sample comparisons
street.compare <- df.address.ab[, c(1:3, c(0,
                                           ncol.address,
                                           ncol.address * 2) + 5)]
zip.compare <- df.address.ab[, c(1:3, c(0,
                                        ncol.address,
                                        ncol.address * 2) + 7)]
city.compare <- df.address.ab[, c(1:3, c(0,
                                         ncol.address,
                                         ncol.address * 2) + 8)]

# or combined
combined.compare <- df.address.ab[, c(1:3,
                                    c(0, ncol.address, ncol.address * 2) + 5,
                                    c(0, ncol.address, ncol.address * 2) + 7,
                                    c(0, ncol.address, ncol.address * 2) + 8)]

# Lets identify the rows where there aren't any differences ...
system.time(same <- which(
  rowSums(!df.address.ab[col.start : col.end]) == 0))

# ... and where we are having differences
system.time(different <- which(
  rowSums(!df.address.ab[col.start : col.end]) > 0))

# Let's separate the good from the bad
system.time(df_different <- df.address.ab[different,])
system.time(df_same <- df.address.ab[same,])

# we can now calculate the number of differences per column
# it'll be ugly, but let's plot them
col_sums <- colSums(!df_different[col.start: col.end])
plot(col_sums)





# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# old stuff below


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
