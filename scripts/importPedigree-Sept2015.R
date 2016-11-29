# the code from bold-modelselect-sept2014-pluscomp-jan2015.R
# that makes the pedigree:

# some individuals have been removed from the original pedigree
# file for being their own mothers or being duplicated:
# huh. Duplicated individuals?
# 6250 is duplicated, and is its own mother
# 6385 is its own mother
# 6451 is its own mother
# 6526 is its own mother
# 6565 is its own mother

# These are observations 5073, 5205, 5256, 5326, 5361

# I have manually removed these records from the
# original file.

# 29th July 2015
# found this note in file 'expl-modelselect-Jul2014-plusjan2015minbag.R':
# 4378, 5048, 6238, 6245

# These individuals were missing cohorts and were needed for 
# exploration and age analysis. Aded 2005 for 4378 (but this is
# unsure because capture date is a Shinichi 1st Jan date), 2009
# for 5048, and 2011 for 6238 and 6245


# 3rd July 2015
# One bird (bird ID 6833, asrid 5626) had a cohort of "xxx".
# this bird was tracked since it was a nestling, so the cohort
# was manually changed to 2013.


### pedigree ###
setwd("C:/Users/Issie/SkyDrive/PhD/masterdatasheets")

asrp <- read.table("DraftPedjuly2015-1995-2013.txt", header=T, na.strings="NA")

# add pedantics
library(pedantics)

# add bird 5644
# amended 9th June 2015: also need to add 7641 and 7649:
#asrp[6065,] <- c(5644, NA, NA, 2010, 6070, NA, NA)
#asrp[6066,] <- c(7641, NA, NA, 2013, 6071, NA, NA)
#asrp[6067,] <- c(7649, NA, NA, 2013, 6072, NA, NA)

# amended 11th September 2015:
# these birds were being added in to a length in the pedigree but
# the pedigree was not the same length so missing records were
# being generated. Now this is self-referencing:

asrp[length(asrp[,1])+1,] <- c(5644, NA, NA, 2010, 6070, NA, NA)
asrp[length(asrp[,1])+1,] <- c(7641, NA, NA, 2013, 6071, NA, NA)
asrp[length(asrp[,1])+1,] <- c(7649, NA, NA, 2013, 6072, NA, NA)


head(asrp)
tail(asrp)

# cut pedigree to just three columns

# and I want to fill in missing dams:

asrp1 <- asrp[5:7]
head(asrp1)
str(asrp1)

# so first, how many am I missing?
length(which(is.na(asrp1$asrdam)))

# missing only 331 dams

max(asrp1$asrdam, na.rm=T)

# make unique dam IDs that are from asrid's not currently
# in the data set:

damplus <- seq(max(asrp1$asrid, na.rm=T)+1,
               max(asrp1$asrid, na.rm=T)+length(which(is.na(asrp1$asrdam))),
               1)
damplus <- as.data.frame(damplus)
damplus$nos <- which(is.na(asrp1$asrdam))
head(damplus)
str(damplus)
tail(damplus)
which(damplus$damplus==asrp1$asrid)
# good! I don't want any overlap.

asrp1$nos <- seq(1, length(asrp1$asrid), 1)
asrp1$dam2 <- asrp1$asrdam
asrp1$dam2[is.na(asrp1$dam2)] <- 0
summary(asrp1)

for(i in 1:length(asrp1$asrid)){
  
  if(asrp1$dam2[i]==0){
    asrp1$dam2[i] <- damplus$damplus[match(i, damplus$nos)]    
  } else {
    asrp1$dam2[i] <- asrp1$asrdam[i]
  }
}

head(asrp1)
summary(asrp1)

# did the dam get inserted in the right place?:
head(damplus)
asrp1[220,]
asrp1[1159,]
# good :)

asrp1x <- data.frame(cbind("id"=asrp1$asrid, "dam"=asrp1$dam2, "sire"=asrp1$asrsire))
# now I have to add the new dams with no ancestors
# to the full pedigree
dams <- cbind("id"=damplus$damplus, "dam"=NA, "sire"=NA)
asrp1x <- rbind(asrp1x, dams)
str(asrp1x)
summary(asrp1x) # so the 331 inputted dams have no dams themselves
summary(asrp1)
head(asrp1x)
head(asrp1)

# fix the pedigree
asrp2x <- fixPedigree(asrp1x)
summary(asrp2x)
head(asrp2x)
tail(asrp2x)
# check the dam and sire are as they should be
asrp[which(asrp$asrid==5981),]
asrp[which(asrp$asrid==6063),]

# and make the inverse :)

# 4th August 2015: the inverse function needs a
# pedigree in the order ID, SIRE, DAM:
asrp3 <- asrp2x[,c(1,3,2)]
head(asrp3)


# but let's check whether they are truly different for
# my data:
asrpinv <- asreml.Ainverse(asrp2x)$ginv
asrpinv2 <- asreml.Ainverse(asrp3)$ginv

summary(asrpinv-asrpinv2)
# this suggests... not at all...

summary(asrpinv)
tail(asrpinv)
which(asrpinv$Ainverse>30)
asrpinv[2922,]
# so some individuals are incredibly related to themselves...
asrpinv[2920:2923,]
### this is the pedigree for the boldness analysis

####################################################
####################################################

# check out the pedigree:

# to make the plot, I want to structure by cohort:
asrp2x$cohort <- asrp$cohort[match(asrp2x$id, asrp$asrid)]
head(asrp2x)
tail(asrp2x)
asrp[which(asrp$asrid==5981),]
# set the "NA"s in cohort to be an arbitrary value (1994)
asrp2x$cohort[is.na(asrp2x$cohort)] <- 1994
summary(asrp2x)

drawPedigree(asrp2x, cohorts=asrp2x$cohort, 
             writeCohortLabels="y")
pedigreeinformation <- pedigreeStats(asrp2x)
pedStatSummary(pedigreeinformation)


# but I probably want summary statistics for the
# pedigree before the extra dams are added (to get
# true pedigree depth and number of individuals 
# etc)

asrp.beforedams <- data.frame(cbind("id"=asrp$asrid, "dam"=asrp$asrdam, "sire"=asrp$asrsire))
head(asrp.beforedams)
summary(asrp.beforedams)
str(asrp)
str(asrp.beforedams)
asrp.beforedams.fixed <- fixPedigree(asrp.beforedams)
head(asrp.beforedams.fixed)
summary(asrp.beforedams.fixed)
str(asrp.beforedams.fixed)

# check out the pedigree before I imputed dams for
# those missing dams:

drawPedigree(asrp.beforedams.fixed)
pedigreeinformation2 <- pedigreeStats(asrp.beforedams.fixed)
pedStatSummary(pedigreeinformation2)

# does it look different if I take the bird IDs?
asrp.beforedams2 <- data.frame(cbind("id"=asrp$birdid, "dam"=asrp$dam, "sire"=asrp$sire))
asrp.beforedams.fixed2 <- fixPedigree(asrp.beforedams2)
head(asrp.beforedams.fixed2)
drawPedigree(asrp.beforedams.fixed2)

# nope. The pedigree is the same every which way
# after fixing.