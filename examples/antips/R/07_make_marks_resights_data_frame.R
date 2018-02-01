#######################################
# CREATE MR DATA FRAMES FOR PLOTTING  #
# MR FITS                             #
#######################################

library(reshape2)
library(plyr)

source('utils/tighten.R')

#############################
# PREPARE MR DATA KNOWN AGE #
#############################

# get mr data
load('../data/mrobs_knownage.Rdata')

composite_map_knownage <- composite_map

mrdata <- melt(mrdata, varnames = c("id", "year"), value.name = "class")

# remove zero's and -1's
mrdata <- subset(mrdata, class != "0")
mrdata <- subset(mrdata, class != "-1")
mrdata <- tighten(mrdata)

# get marks
## remove resights from mr data
marks <- subset(mrdata, !duplicated(mrdata$id))
marks <- tighten(marks)

# get vector of marks per year
marks <- ddply(marks, .(year, class), summarise, count = length(id))
marks <- daply(marks, year ~ class, function(x) x$count)
marks[is.na(marks)] <- 0

# add years with no marks
marks <- c(marks[1:10], 0, 0, marks[11:16], 0)
names(marks) <- 1996:2014

# save
save(marks, file = '../data/marks_knownage.Rdata')

# get resights
## remove marks from mr data
resights <- subset(mrdata, duplicated(mrdata$id))
resights <- tighten(resights)

# sum by year and class
resights <- ddply(resights, .(year, class), summarise, count = length(id))
resights <- daply(resights, year ~ class, function(x) x$count)
resights[is.na(resights)] <- 0

# reorganise and add labels
resights <- resights[,colnames(resights)[order(as.integer(colnames(resights)))]]
colnames(resights) <- c(names(partition_map), names(composite_map_knownage))[as.integer(colnames(resights))]

# add years with no resights
resights <- rbind(resights[1:6,], array(0, dim = c(1, ncol(resights)), dimnames = list(year = 2006)), resights[7:14,])

# save
save(resights, file = '../data/resights_knownage.Rdata')

###############################
# PREPARE MR DATA UNKNOWN AGE #
###############################

# get mr data
load('../data/mrobs_unknownage.Rdata')

composite_map_unknownage <- composite_map

mrdata <- melt(mrdata, varnames = c("id", "year"), value.name = "class")

# remove zero's and -1's
mrdata <- subset(mrdata, class != "0")
mrdata <- subset(mrdata, class != "-1")
mrdata <- tighten(mrdata)

# get marks
## remove resights from mr data
marks <- subset(mrdata, !duplicated(mrdata$id))
marks <- tighten(marks)

# get vector of marks per year
marks <- ddply(marks, .(year, class), summarise, count = length(id))
marks <- daply(marks, year ~ class, function(x) x$count)
marks[is.na(marks)] <- 0

# add years with no marks
marks <- rbind(marks[1:12,], array(0, dim = c(1,ncol(marks)), dimnames = list(year = 2006)), marks[13:20,])

# assign composite group to sbr and fbr
#cln.sbr <- as.character(partition_label('sbr'))
#cln.fbr <- as.character(partition_label('fbr'))
#cln.br  <- as.character(partition_label('br'))
#r <- sum(marks[, cln.sbr]) / (sum(marks[, cln.sbr]) + sum(marks[, cln.fbr]))
#marks[, cln.sbr] <- marks[, cln.sbr] + r * marks[, cln.br]
#marks[, cln.fbr] <- marks[, cln.fbr] + (1 - r) * marks[, cln.br]
#marks <- marks[, -1]

# reorganise and add labels
marks <- marks[,colnames(marks)[order(as.integer(colnames(marks)))]]
colnames(marks) <- c(names(partition_map), names(composite_map_knownage), names(composite_map_unknownage))[as.integer(colnames(marks))]

# save
save(marks, file = '../data/marks_unknownage.Rdata')

# get resights
## remove marks from mr data
resights <- subset(mrdata, duplicated(mrdata$id))
resights <- tighten(resights)

# sum by year and class
resights <- ddply(resights, .(year, class), summarise, count = length(id))
resights <- daply(resights, year ~ class, function(x) x$count)
resights[is.na(resights)] <- 0

# reorganise and add labels
resights <- resights[,colnames(resights)[order(as.integer(colnames(resights)))]]
colnames(resights) <- c(names(partition_map), names(composite_map_knownage), names(composite_map_unknownage))[as.integer(colnames(resights))]

# add years with no resights
resights <- rbind(resights[1:10,], array(0, dim = c(1,ncol(resights)), dimnames = list(year = 2006)), resights[11:18,])

# save
save(resights, file = '../data/resights_unknownage.Rdata')

