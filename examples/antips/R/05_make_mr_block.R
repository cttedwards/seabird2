
library(seabird2)

# KNOWN AGE

load('../data/mrobs_knownage.Rdata')

# ORDER DATA FOR INPUT INTO SBD

# get first year
year <- colnames(mrdata)
firstyear <- function(x) year[min(which(x != "0" & x != "-1"))]
start <- apply(mrdata, 1, firstyear)

# chronological reorder
loc    <- order(as.numeric(start))
if (length(loc) > 0) {
    mrdata <- mrdata[loc,]
    start  <- start[loc]
}

# remove data rows with no information
loc <- which(start >= (max(as.numeric(year) - 1)) | is.na(start))
if (length(loc) > 0) {
    mrdata <- mrdata[-loc,]
    start  <- start[-loc]
}

# construct mr block
SeaBird.write_mark_recapture(mrdata, composite_labels = 37:46, composite_map = composite_map, file = '../sbd/mrdata_knownage.block.sbd', path = './', label = 'mrdata_knownage')

# UNKNOWN AGE

rm(list = ls())

load('../data/mrobs_unknownage.Rdata')

# ORDER DATA FOR INPUT INTO SBD

# get first year
year <- colnames(mrdata)
firstyear <- function(x) year[min(which(x != "0" & x != "-1"))]
start <- apply(mrdata, 1, firstyear)

# chronological reorder
loc    <- order(as.numeric(start))
if (length(loc) > 0) {
    mrdata <- mrdata[loc,]
    start  <- start[loc]
}

# remove data rows with no information
loc <- which(start >= (max(as.numeric(year) - 1)) | is.na(start))
if (length(loc) > 0) {
    mrdata <- mrdata[-loc,]
    start  <- start[-loc]
}

# construct mr block
SeaBird.write_mark_recapture(mrdata, composite_labels = 47:50, composite_map = composite_map, file = '../sbd/mrdata_unknownage.block.sbd', path = './', label = 'mrdata_unknownage')




