
# read and organise data
dat <- read.table('../data-raw/antips_mrdata.txt', header = TRUE)

id  <- dat$id
sex <- dat$sex
year <- 1994:2014

dat <- as.matrix(dat[1:nrow(dat), 3:22])

dat <- cbind(dat[,1:12], '-1', dat[,13:20])
colnames(dat) <- year

# error checking functions
knownage   <- function(x) if (any(x == '1')) TRUE else FALSE
unknownage <- function(x) !knownage(x)

firstyear <- function(x) year[min(which(x == '1'))]
lastyear  <- function() max(year)

getage <- function(x) {
    yr <- firstyear(x):lastyear()
    x[match(yr, year)] <- as.character(seq(1:length(yr)))
    x
}

# check known ages are valid
validateage <- function(x) {
    
    id <<- id + 1
    if (any(x == '1')) {
        loc <- min(which(x == '1')) - 1
        if (loc > 0 & !all(x[1:loc] == '0' | x[1:loc] == '-1')) {
            message('error in first year for row ',id, ' col ', loc + 1)
        }
    } 
    x
}

id <- 0
dat2 <- t(apply(dat, 1, validateage))
# the individual was a breeder the previous year 
# therefore most likely to be a non-breeder
dat2[2245, 17] <- "B"

# in most years age category 7 individuals are classified as 'B' (non-breeders) 
# therefore rename so records are consistent
dat3 <- dat2
dat3[dat3 == '7'] <- 'B'

# for known ages individuals of class 'B' should start at age category 7 (i.e. age 6) not before
# therefore rename younger ages to match their age category
checkB <- function(x) {
    id <<- id + 1
    if (knownage(x)) {
        ages <- getage(x)
        for (i in 1:length(x)) {
            if (x[i] != '0' & x[i] != '-1' & suppressWarnings(is.na(as.numeric(x[i])))) {
                if (as.numeric(ages[i]) < 7) {
                    message('check row ', id)
                    x[i] <- ages[i]
                }
            }
        }
    }
    x
}

id <- 0
dat4 <- t(apply(dat3, 1, checkB))

# some age category assignments are incorrect
# therefore check age assignments for individuals of known age
checkage <- function(x) {
    id <<- id + 1
    if (knownage(x)) {
        ages <- getage(x)
        for (i in 1:length(x)) {
            if (x[i] != '0' & x[i] != '-1' & suppressWarnings(!is.na(as.numeric(x[i])))) {
                if (x[i] != ages[i]) {
                    x[i] <- ages[i]
                    message('corrected error for row ', id)
                }
            }
        }
    }
    x
}

id <- 0
dat5 <- t(apply(dat4, 1, checkage))

# resights of age category 2 (i.e. age 1) probably an error
# as it only happens in 2014
dat6 <- dat5
#dat6[dat6[,ncol(dat6)] == "2",]
dat6[dat6 == "2"] <- '0'

# save
loc.knownage   <- apply(dat6, 1, knownage)
loc.unknownage <- apply(dat6, 1, unknownage)

mrdata <- list()
mrdata[['knownage']]   <- dat6[loc.knownage,]
mrdata[['unknownage']] <- dat6[loc.unknownage,]
save(mrdata, file = '../data/clean_mrdata.Rdata')


