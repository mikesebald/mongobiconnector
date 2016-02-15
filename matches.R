library(data.table)
library(ggplot2)
library(plotly)

# ----------------------------------------------------------------------------
# reading both files and compare column names to identify differences in the
# CSV export structure. So we are creating a data frame with the header names
# from the CSV exports. The data frame helps us to identify differences and
# to rearrange if necessary

#setwd("e:/R/compare_data/")
setwd("../compare_data/")
file.golden.a <- "golden-record-15.4.csv"
file.golden.b <- "golden-record-16.1.csv"

# determine the number of columns per file using
# head -n 1 <filename> |grep -o "\," |wc -l
ncol.golden.a <- 113
ncol.golden.b <- 116

system.time(dt.golden.a <- fread(file.golden.a, sep = ",", header = TRUE, 
                                 na.strings = NULL, encoding = "UTF-8", 
                                 colClasses = rep("character", ncol.golden.a)))
system.time(dt.golden.b <- fread(file.golden.b, sep = ",", header = TRUE, 
                                 na.strings = NULL, encoding = "UTF-8", 
                                 colClasses = rep("character", ncol.golden.b)))

colnames.golden.a <- colnames(dt.golden.a)
colnames.golden.b <- colnames(dt.golden.b)

length(colnames.golden.a) <- max(length(colnames.golden.a), 
                                 length(colnames.golden.b))
length(colnames.golden.b) <- max(length(colnames.golden.a), 
                                 length(colnames.golden.b))

column.names.golden <- data.frame(colnames.golden.a, colnames.golden.b, 
                                  colnames.golden.a == colnames.golden.b,
                                  stringsAsFactors = FALSE)

# this is the point where we should look at the source and rearrange,
# if necessary. Both data sets whould have the same columns
View(column.names.golden)
sum(!column.names.golden[,3])
sum(!column.names.golden[,3], na.rm = TRUE)






