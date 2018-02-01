

load('../data/clean_mrdata.Rdata')

# DEFINE PARTITIONS 
partition_names <- list(
    "-1" = "-1",
    "0"  = "0",
    "1"  = "chick",
    "2"  = "a1",
    "3"  = "a2",
    "4"  = "a3",
    "5"  = "a4",
    "6"  = "a5",
    "B"  = "nbr",
    "S"  = "sbr",
    "T"  = "sbr",
    "F"  = "fbr",
    "G"  = "fbr",
    "N"  = "br",
    "M"  = "br",
    "A"  = "nbr"
)

partition_label <- function(value){
    partition_names[[match(as.factor(value), names(partition_names))]]
}

# KNOWN AGE
mrdata_knownage <- mrdata[['knownage']]

# label partitions
for (i in 1:length(partition_names))
    mrdata_knownage[mrdata_knownage == names(partition_names)[i]] <- partition_label(names(partition_names)[i])

# UNKNOWN AGE
mrdata_unknownage <- mrdata[['unknownage']]

# label partitions
for (i in 1:length(partition_names))
    mrdata_unknownage[mrdata_unknownage == names(partition_names)[i]] <- partition_label(names(partition_names)[i])

# SAVE
mrdat <- list()
mrdat[['knownage']] <- mrdata_knownage
mrdat[['unknownage']] <- mrdata_unknownage

save(mrdat, file = '../data/mrdat.Rdata')






