
load('../data/mrdat.Rdata')
load('../data/transition_matrix.Rdata')

partition_label <- function(value){
    match(as.factor(value), names(partition_map))
}

composite_label <- function(value, start = 0){
    match(as.factor(value), names(composite_map)) + start
}

# KNOWN AGE

mrdata <- mrdat[['knownage']]

# add ages to nbr/sbr/fbr/br partitions
firstmark <- function(x) which(x == "chick")
isbreeder <- function(x) if (any(x == "sbr") | any(x == "fbr") | any(x == "br") | any(x == "nbr")) TRUE else FALSE
rbreeder  <- function(x) which(x == "sbr" | x == "fbr" | x == "br" | x == "nbr")

addage    <- function(x, aplus = 15) {
    
    if (isbreeder(x)) {
        
        rloc <- rbreeder(x)
        
        rage <- rloc - firstmark(x)
        
        if (any(rage >= aplus)) rage[which(rage >= aplus)] <- "plus"
        
        x[rloc] <- paste("a", rage, x[rloc], sep = "")
    }
    
    return(x)
}

mrdata <- t(apply(mrdata, 1, addage))

# get partition map from class names in transition matrix
partition_map <- as.list(1:length(Tmatrix$names))
names(partition_map) <- Tmatrix$names

# create map for composite groups
composite_classes <- c(paste("a", 6:14, "s?f?br", sep = ""), "apluss?f?br")

composite_map <- list()
for (i in 1:length(composite_classes)) {
    composite_map[[composite_classes[i]]] <- as.integer(partition_map[regexpr(composite_classes[i], names(partition_map)) > 0])
}

names(composite_map) <- c(paste("a", 6:14, "br", sep = ""), "aplusbr")

# map integer labels onto mrdata names
for (i in 1:length(partition_map))
    mrdata[mrdata == names(partition_map)[i]] <- partition_label(names(partition_map)[i])

for (j in 1:length(composite_map))
    mrdata[mrdata == names(composite_map)[j]] <- composite_label(names(composite_map)[j], start = 36)

# save 
save(mrdata, partition_map, composite_map, file = '../data/mrobs_knownage.Rdata')


# UNKNOWN AGE

mrdata <- mrdat[['unknownage']]

# create map for composite groups
composite_classes <- c("a1.nbr", "a1.sbr", "a1.fbr", "a1.s?f?br") 

# to make the expression matching easier, create temporary partition map
partition_map_tmp <- partition_map
names(partition_map_tmp)[34:36] <- c("a15nbr", "a15sbr","a15fbr")

composite_map <- list()
for (i in 1:length(composite_classes)) {
    composite_map[[composite_classes[i]]] <- as.integer(partition_map_tmp[regexpr(composite_classes[i], names(partition_map_tmp)) > 0])
}

names(composite_map) <- c("nbr", "sbr", "fbr", "br") 

# map integer labels onto mrdata names
for (j in 1:length(composite_map))
    mrdata[mrdata == names(composite_map)[j]] <- composite_label(names(composite_map)[j], start = 36 + 10)

save(mrdata, composite_map, file = '../data/mrobs_unknownage.Rdata')










