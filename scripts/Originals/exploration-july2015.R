# July 2015
# Isabel Winney

# exploring the exploration data without fixed effect
# selection.

# clear the workspace
rm(list=ls())


# now run the importPedigree-June2015.R file

# set working directory
setwd("./masterdatasheets")

# import useful libraries:
library(pedantics)
library(asreml)
library(lattice)
library(car)
library(lme4)
library(gvlma)
pin <- dget("C:/Users/Issie/SkyDrive/PhD/ASRemlR/pin.R")
source("C:/Users/Issie/SkyDrive/RFunctions/assigningtestorder-20150218.R")
source("C:/Users/Issie/SkyDrive/RFunctions/vif_lmer_notmycode_20150727.R")



# Load pedigree from importPedigree-June2015.R


### data ###
# I corrected two data points in this sheet in
# July 2015: I corrected the sex of bird 4999
# TR59059 from male to female for one capture,
# and I corrected the identity of TS66200 to be
# TS66202, see database for more information.

tent <- read.table("TENT-corrected-july2015.txt", header=T)

str(tent)
head(tent)
summary(tent)

# this data set has a variable 'oldcomp' and 'newcomp'
# in new comp, all major compromises are 1,
# talking is 2, extra noises are 3 and cheeps
# outside the tent are 4

# remove data where we don't know who the bird was:
tent1 <- subset(tent, tent$bto != "NA")
# and any tests that were less than 5 minutes, or were
# the second 5 minutes of a 10 minute test 
# (-->time==10 minutes)
tent1 <- subset(tent1, tent1$time==5)

tent1$juldate <- strptime(tent1$date, "%d/%m/%Y")$yday+1

tent1$befaft <- ifelse(tent1$juldate>150, "before", "after")

birdcohort <- read.table("birdbroodcohort-july2014.txt", header=T)
tent1$cohort <- birdcohort$Cohort[match(tent1$birdid, birdcohort$BirdID)]

tent1$year <- strptime(tent1$date, "%d/%m/%Y")$year+1900
table(tent1$year)

tent1$winter <- ifelse(tent1$befaft=="before", tent1$year, tent1$year-1)
table(tent1$winter)

tent1$cohort <- asrp$cohort[match(tent1$birdid, asrp$birdid)]
summary(tent1$cohort)
table(tent1$cohort)

tent1$age <- tent1$winter-tent1$cohort
summary(tent1$age)

# in February 2012, we changed the length of a 'run'
# from the length of one tent side to half the length
# of a tent side.
tent1$x2r <- ifelse(tent1$method==0, 2*tent1$runs, tent1$runs)

# but what if it was only November 2011 that was affected
# by this, i.e. the one winter when I was taught wrong?
# would this affect the results?

# make a new method and response variable, that classes
# only the 84 tests from November 2011 as being method 0

tent1$method2 <- ifelse(tent1$method==0,
                        ifelse(tent1$winter==2011,
                               0, 1),1)
table(tent1$method2, tent1$winter)

# and now make a new runs variable using this classification
# for method:
tent1$x2r.method2 <- ifelse(tent1$method2==0, 2*tent1$runs, tent1$runs)

hist(tent1$x2r, breaks=seq(-1,160,1))
hist(tent1$x2r.method2, breaks=seq(-1,160,1))





# I also want two variables: one for previous years of testing
# and one for within year test order, or delta between dates

tent1$birdwinter <- as.numeric(paste(tent1$birdid, tent1$winter, sep=""))
head(tent1$birdwinter)

tent1$birdiddate <- as.numeric(paste(tent1$birdid, tent1$date2, sep=""))
head(tent1$birdiddate)

# If I want data ordered within trip, I need a
# variable for a bird and a trip
# The first trip was in January 2010, so in winter
# 2009. To call this the first trip, subtract 2008
# from the winter. To differentiate this from the
# autumn time trips, use 0.5:

tent1$trip <- ifelse(tent1$juldate<150,
                         tent1$year-2009,
                         tent1$year-2008.5)

table(tent1$trip, tent1$year)
table(tent1$trip, tent1$winter)
# looking good!

# concatenate bird and trip:
tent1$birdtrip <- paste(tent1$birdid,
                        tent1$trip,
                        sep="a")
table(tent1$birdtrip)

# order the exploration data by using function
# testorder:

# this is for the order within winter trips, with
# birdwinter as the groups, and the date to provide
# the order of the data points
explorationordered <- testorder("birdtrip", "date2", 
                                tent1)

# is this the same as the variable I made in my original
# file? Run the first for{} loop in the expl-all-jan2015 etc etc
# file. This produces the data set and variable tentordered$withintrip
# that I can now compare to the variable I have just made:

summary(tentordered$withintrip-explorationordered$testorder)

explorationordered[6:10,]

# perfect!
# re-name testorder because the function will overwrite
# this variable when I use it again:
explorationordered$withintrip <- explorationordered$testorder

# I now need to have an order across trips

# but I don't think my ordering function can cope with
# this. It consecutively orders everything by the grouping
# variable, but I need things to be the same within, for
# example, one trip.



tentordered <- explorationordered[order(explorationordered$birdiddate,
                                        explorationordered$timeproc),]

# captures were already ordered by bird, capture date
# and capture time



### now for across trips

# make my lookup table
birdvisit <- data.frame("birdid"=unique(tentordered$birdid), 
                        "trip"=1)
head(birdvisit)
length(birdvisit$birdid)

tentordered$betweentrip <- 666
head(tentordered$betweentrip)


# make a variable of whole numbers or whole characters
# that defines each trip and bird:

tentordered$visit <- paste(tentordered$birdid, 
                           tentordered$winter, 
                           tentordered$year, sep="")


for (i in 1:length(tentordered$birdid)){
  
  # get the locatino of the bird in the lookup table:
  k <- match(tentordered$birdid[i], birdvisit$birdid)
  
  # the first part of this asks whether the bird and visit
  # are the same. If the bird and visit are not the same, the
  # data is updated:
  ifelse(i==1, 
         tentordered$betweentrip[i] <- birdvisit$trip[k],
         ifelse(tentordered$birdid[i]==tentordered$birdid[i-1],
                ifelse(tentordered$visit[i]==tentordered$visit[i-1],
                       tentordered$betweentrip[i] <- birdvisit$trip[k],
                       # below, the bird and visit are not the same so
                       # a higher value for visit order is used.
                       tentordered$betweentrip[i] <- birdvisit$trip[k]+1),
                tentordered$betweentrip[i] <- birdvisit$trip[k])
  )
  
  # the second part updates the lookup table for cases where the
  # bird and visit were different:
  ifelse(i==1, 
         tentordered$betweentrip[i] <- birdvisit$trip[k],
         ifelse(tentordered$birdid[i]==tentordered$birdid[i-1],
                ifelse(tentordered$visit[i]==tentordered$visit[i-1],
                       birdvisit$trip[k] <- birdvisit$trip[k],
                       birdvisit$trip[k] <- birdvisit$trip[k]+1),
                birdvisit$trip[k] <- birdvisit$trip[k])
  )  
}

head(tentordered$betweentrip)
table(tentordered$betweentrip)
length(tentordered$betweentrip)
tentordered[180:189,]

# cool! So 5 birds were tested across 4 visits, and 
# one bird was even tested on a fifth visit!

xyplot((flights+x2r)~betweentrip, 
       groups=birdid, data=tentordered, type="l")


# factors and numerics
tentordered$method <- as.factor(tentordered$method)
tentordered$obs <- factor(tentordered$obs)
tentordered$befaft <- as.factor(tentordered$befaft)
tentordered$cohort <- as.factor(tentordered$cohort)

# tent2
#tent2 <- subset(tentordered, tentordered$obs!="TB")
#tent2 <- subset(tent2, tent2$obs!="IN")
#tent2 <- subset(tent2, tent2$obs!="HR")
#tent2$obs <- factor(tent2$obs)
# 27/8/2014 - this is now obsolete. I am going
# to put all these tests in as a single observer,
# amalgamated with pip and Rob, to maximise the
# sample size.

tent2 <- tentordered

table(tent2$obs)
tent2$obs2 <- ifelse(tent2$obs=="AST", "AST",
                     ifelse(tent2$obs=="IW", "IW",
                            ifelse(tent2$obs=="JS", "JS", "O")))
table(tent2$obs2)


# I know my response variable is a poisson:
hist(tent2$flights+tent2$x2r, breaks=seq(-1,180,1))
# and I know asreml will not like poissons. In
# fact the handbook warns against using non-gaussian
# distributions. So, what transformation should I
# use on the data?:


boxcox((flights+x2r+0.5)~1+(1|birdid), 
       data=tent2, 
       lambda=seq(-0.5,0.50,0.001))


powerTransform((flights+x2r+0.5)~1+(1|birdid), 
               data=tent2)

# so the ideal transformation is 0.1. This is very
# close to a log transformation, and whilst the 95%
# intervals do not include a lambda of zero, they are
# very close. So, I will use a log transformation as
# a reasonable approximation to a gaussian distribution.


tent2$logfr <- log(tent2$flights+tent2$x2r+0.5)

# since I have been thinking about the effect of method,
# do two other versions as well. One where only November
# 2011 is doubled, and one where nothing is doubled:

tent2$logfr.november <- log(tent2$flights+tent2$x2r.method2+0.5)
tent2$logfr.notdoubled <- log(tent2$flights+tent2$runs+0.5)

hist(tent2$logfr)
hist(tent2$logfr.november)
hist(tent2$logfr.notdoubled)
# pretty similar looking.

# mean-centre within and between trip order
mord <- as.data.frame(tapply(tent2$betweentrip, tent2$birdid, FUN=mean, na.rm=T))
nam <- as.data.frame(rownames(mord))
mord2 <- cbind(nam, mord)
names(mord2) <- c("birdid", "mbtwn")
tent2 <- merge(tent2, mord2, all.x=T)
tent2$indbtwn <- tent2$betweentrip - tent2$mbtwn

mord <- as.data.frame(tapply(tent2$withintrip, tent2$birdid, FUN=mean, na.rm=T))
nam <- as.data.frame(rownames(mord))
mord2 <- cbind(nam, mord)
names(mord2) <- c("birdid", "mwin")
tent2 <- merge(tent2, mord2, all.x=T)
tent2$indwin <- tent2$withintrip - tent2$mwin

# and time processed
mord <- as.data.frame(tapply(tent2$timeproc, tent2$birdid, FUN=mean, na.rm=T))
nam <- as.data.frame(rownames(mord))
mord2 <- cbind(nam, mord)
names(mord2) <- c("birdid", "mtime")
tent2 <- merge(tent2, mord2, all.x=T)
tent2$indtime <- tent2$timeproc - tent2$mtime


# scaling
tent2$zindbtwn <- as.numeric(scale(tent2$indbtwn))
tent2$zindwin <- as.numeric(scale(tent2$indwin))
tent2$zmbtwn <- as.numeric(scale(tent2$mbtwn))
tent2$zmwin <- as.numeric(scale(tent2$mwin))
tent2$zindtime <- as.numeric(scale(tent2$indtime))
tent2$zmtime <- as.numeric(scale(tent2$mtime))

### checks
summary(tent2$zindwin)
summary(tent2$zindbtwn)
summary(tent2$winter)

hist(tent2$zindwin)
# well. That's pretty poor. I'm guessing it is
# because most birds are tested once and the 
# maximum is three times.
# I can always fit this order as a factor
table(tent2$withintrip)
# but there are only eight in the three group...
# and 72 repeated observations within trips in
# general.

# asrid and asrdam

tent2$asrid <- asrp$asrid[match(tent2$birdid, asrp$birdid)]
head(tent2$asrid)

tent2$check <- asrp$birdid[match(tent2$birdid, asrp$birdid)]

tent2$check2 <- ifelse(tent2$check==tent2$birdid, 0, 1)
summary(tent2$check2)
# excellent



tent2$asrdam <- asrp1x$dam[match(tent2$asrid, asrp1x$id)]
summary(tent2$asrdam)
which(is.na(tent2$asrdam))


tent2$factorasrid <- as.factor(tent2$asrid)
tent2$factorasrdam <- as.factor(tent2$asrdam)

# sort out my base levels:
table(tent2$sex)
table(tent2$winter)
table(tent2$befaft)
table(tent2$oldcomp)
table(tent2$method)
table(tent2$obs2)

tent2$sex <- as.factor(ifelse(tent2$sex=="M", "0", tent2$sex))
table(tent2$sex)

tent2$winter <- as.factor(ifelse(tent2$winter=="2013", "0", tent2$winter))
table(tent2$winter)

tent2$befaft <- as.factor(ifelse(tent2$befaft=="before", "0", tent2$befaft))
table(tent2$befaft)

tent2$method <- as.factor(ifelse(tent2$method=="1", "0", "1"))
table(tent2$method)

tent2$obs2 <- as.factor(ifelse(tent2$obs2=="IW", "0", tent2$obs2))
table(tent2$obs2)

# In bold model selection, I became interested
# in the influence of the social mother and social
# brood and it looked as if there could be an effect
# of one or other or both if only I had better sample 
# sizes. I wanted to try both in this model selection
# procedure as well because I expect that amongst the
# fledglings tested for exploration will be more from
# the same social brood and maybe same mother

# interest in the effect of the social mother:
birdcohort <- read.table("birdbroodcohort-july2014.txt", header=T)
head(birdcohort)

tent2$socialbrood <- birdcohort$social[match(tent2$birdid, birdcohort$BirdID)]
summary(tent2$socialbrood)

tent2$natalbrood <- birdcohort$BroodRef[match(tent2$birdid, birdcohort$BirdID)]
summary(tent2$natalbrood)

# get the parents
broods <- read.table("brood-parent-july2014-myDBparents.txt", header=T, na.strings="NA")
head(broods)

tent2$socmum <- broods$SocialMumID[match(tent2$socialbrood, broods$BroodRef)]
head(tent2$socmum)
summary(tent2$socmum)

tent2$socmum <- as.factor(tent2$socmum)
tent2$socialbrood <- as.factor(tent2$socialbrood)



tent2$oldcomp <- as.factor(tent2$oldcomp)


# In going through and inputting capture references
# on 23/1/2014, I found two bird id errors that 
# are now corrected in the main data sheets, and
# I found three individuals that were tested twice
# on the same day. I think I should exclude these
# individuals' second test scores from that day
# in case of extreme habituation effects (this
# might actually improve my bird id term...)

# these individuals are 
# TS66117, capture 16727
# TS66389, capture 19192
# TT31351, capture 21278

tent2 <- subset(tent2, tent2$captureref!=16727)
tent2 <- subset(tent2, tent2$captureref!=19192)
tent2 <- subset(tent2, tent2$captureref!=21278)

###########

# 11th Jan 2015
# I would prefer to show how waiting times
# in the bag influence bird activity (answer
# from below, they don't seem to, but I want
# to put it in the formal stepwise deletion
# procedure)

tentbag <- subset(tent2, tent2$minbag!="NA")

xyplot(flights+x2r~minbag, groups=asrid, type="b", data=tent2)
# there is an overall negative relationship
# but looking at the individual responses shows
# a lot of variability. This is something that
# should be fitted at the population and the
# individual level to tease apart the effect.

mord <- as.data.frame(tapply(tentbag$minbag, tentbag$birdid, FUN=mean, na.rm=T))
nam <- as.data.frame(rownames(mord))
mord2 <- cbind(nam, mord)
names(mord2) <- c("birdid", "mbag")
tentbag <- merge(tentbag, mord2, all.x=T)
tentbag$ibag <- tentbag$minbag - tentbag$mbag

tentbag$zmbag <- scale(tentbag$mbag)
tentbag$zibag <- scale(tentbag$ibag)
summary(tentbag)




xyplot((flights+x2r)~befaft, groups=birdid, data=tent2, type="b")
xyplot((flights+x2r)~befaft, groups=birdwinter, data=tent2, type="b")

xyplot(log(flights+x2r+0.5)~mtime, groups=birdid, data=tent2, type="b")
xyplot(log(flights+x2r+0.5)~indtime, groups=birdid, data=tent2, type="b")


# re-scale everything else for this data set:

tentbag$zindbtwn <- as.numeric(scale(tentbag$indbtwn))
tentbag$zindwin <- as.numeric(scale(tentbag$indwin))
tentbag$zmbtwn <- as.numeric(scale(tentbag$mbtwn))
tentbag$zmwin <- as.numeric(scale(tentbag$mwin))
tentbag$zindtime <- as.numeric(scale(tentbag$indtime))
tentbag$zmtime <- as.numeric(scale(tentbag$mtime))

################

# I need the base model, the intercept only
# model, and the significance of bird id for
# the first part of my results

tent2$zero <- 0

asrex0 <- asreml(fixed=logfr~zero,
                   data=tent2,
                   na.method.X="omit",
                   na.method.Y="omit",
                   maxiter=20)

asrex0.1 <- asreml(fixed=logfr~zero,
                 random=~ide(factorasrid, var=T, init=1),
                 data=tent2,
                 ginverse=list(factorasrid=asrpinv),
                 na.method.X="omit",
                 na.method.Y="omit",
                 maxiter=20)

plot(asrex0.1)
# not the most perfect fitted against residual plots,
# but a nice qq showing reasonable model prediction 
# and there are no trends through time in the residuals.
summary(asrex0)$varcomp
summary(asrex0.1)$varcomp
summary(asrex0.1, all=T)$coef.fi
1-pchisq(2*(asrex0.1$loglik-asrex0$loglik),1)


# intercept-only animal model:

explorationanimal1 <- asreml(fixed=logfr~1,
                   random=~ide(factorasrid, var=T, init=1)+
                     ped(factorasrid, var=T, init=1)+
                     ide(factorasrdam, var=T, init=1),
                   data=tent2,
                   ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                   na.method.X="omit",
                   na.method.Y="omit",
                   maxiter=20)

plot(explorationanimal1)
# fitted vs residuals is better than before, the
# qq is still nice.
summary(explorationanimal1)$varcomp


# what if I use the other methods for calculating number of runs???

# first, only doubling November 2011:
exploration.doubleNov1 <- asreml(fixed=logfr.november~1,
                   random=~ide(factorasrid, var=T, init=1),
                   data=tent2,
                   ginverse=list(factorasrid=asrpinv),
                   na.method.X="omit",
                   na.method.Y="omit",
                   maxiter=20)

exploration.doubleNov2 <- asreml(fixed=logfr.november~1,
                             random=~ide(factorasrid, var=T, init=1)+
                               ped(factorasrid, var=T, init=1)+
                               ide(factorasrdam, var=T, init=1),
                             data=tent2,
                             ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                             na.method.X="omit",
                             na.method.Y="omit",
                             maxiter=20)

# second, not doubling at all:
exploration.nodouble1 <- asreml(fixed=logfr.notdoubled~1,
                                 random=~ide(factorasrid, var=T, init=1),
                                 data=tent2,
                                 ginverse=list(factorasrid=asrpinv),
                                 na.method.X="omit",
                                 na.method.Y="omit",
                                 maxiter=20)

exploration.nodouble2 <- asreml(fixed=logfr.notdoubled~1,
                                 random=~ide(factorasrid, var=T, init=1)+
                                   ped(factorasrid, var=T, init=1)+
                                   ide(factorasrdam, var=T, init=1),
                                 data=tent2,
                                 ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                                 na.method.X="omit",
                                 na.method.Y="omit",
                                 maxiter=20)

summary(exploration.doubleNov1)$varcomp
summary(exploration.nodouble1)$varcomp
summary(asrex0.1)$varcomp

summary(exploration.doubleNov2)$varcomp
summary(exploration.nodouble2)$varcomp
summary(explorationanimal1)$varcomp

# actually, no. The strong pedigree term is still
# present in all models, along with a modest maternal
# ID term. So it doesn't seem to make a difference.

# to be frank, the protocol prior is not truly known
# before I started. There might be an argument to only
# use data I know to be ok e.g. 2012 onwards. This
# would cut 120 samples, but let's try:

tent.from2012 <- subset(tent2, tent2$year>2011)
table(tent.from2012$year)

exploration.from2012.1 <- asreml(fixed=logfr~1,
                                 random=~ide(factorasrid, var=T, init=1),
                                 data=tent.from2012,
                                 ginverse=list(factorasrid=asrpinv),
                                 na.method.X="omit",
                                 na.method.Y="omit",
                                 maxiter=20)

exploration.from2012.2 <- asreml(fixed=logfr~1,
                                 random=~ide(factorasrid, var=T, init=1)+
                                   ped(factorasrid, var=T, init=1)+
                                   ide(factorasrdam, var=T, init=1),
                                 data=tent.from2012,
                                 ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                                 na.method.X="omit",
                                 na.method.Y="omit",
                                 maxiter=20)


summary(exploration.from2012.1)$varcomp
summary(exploration.from2012.2)$varcomp
# amazing! So, repeatability is very high in this
# data set, and in the animal model version the Va
# is three times higher than either Vpe or Vm effects!
# very exciting :) Not least because the results are
# so consistent!


################################################################
# let's return to basics.

# first, an intercept-only model of repeatability
# for all data:

# with no random effect:
exploration.alldata1 <- asreml(fixed=logfr~zero,
                 data=tent2,
                 na.method.X="omit",
                 na.method.Y="omit",
                 maxiter=20)

# with the random effect of bird id:
exploration.alldata2 <- asreml(fixed=logfr~zero,
                   random=~ide(factorasrid, var=T, init=1),
                   data=tent2,
                   ginverse=list(factorasrid=asrpinv),
                   na.method.X="omit",
                   na.method.Y="omit",
                   maxiter=20)

plot(exploration.alldata1)
plot(exploration.alldata2)
# not the most perfect fitted against residual plots,
# but a nice qq showing reasonable model prediction 
# and there are no trends through time in the residuals.
summary(exploration.alldata1)$varcomp
summary(exploration.alldata2)$varcomp
summary(exploration.alldata2, all=T)$coef.fi
1-pchisq(2*(exploration.alldata2$loglik-exploration.alldata1$loglik),1)

pin(exploration.alldata2, r2expl~V1/(V1+V2))
length(tent2[,1])
length(unique(tent2$asrid))
table(table(tent2$asrid))


# for the compromised data, is there any data
# that is statistically no different from compromised data?

exploration.alldata3 <- asreml(fixed=logfr~factor(newcomp),
                               random=~ide(factorasrid, var=T, init=1),
                               data=tent2,
                               ginverse=list(factorasrid=asrpinv),
                               na.method.X="omit",
                               na.method.Y="omit",
                               maxiter=20)

plot(exploration.alldata3)

plot(tent2$newcomp, exploration.alldata3$residuals)
# good looking residuals.

summary(exploration.alldata3, all=T)$coef.fi

# so the newcomp==4, which is tests disrupted by some
# cheeping, are statistically no different from tests
# that were not compromised! So, I will amalgamate these
# in to the non-compromised category:

tent2$comp.amalgamated <- ifelse(tent2$newcomp==0,0,
                                 ifelse(tent2$newcomp==4,0,1))

table(tent2$newcomp, tent2$comp.amalgamated)





# remove compromised data to see if there is an
# effect similar to that seen in boldness:

tentnocomp <- subset(tent2, tent2$comp.amalgamated==0)

exploration.nocomp1 <- asreml(fixed=logfr~zero,
                               random=~ide(factorasrid, var=T, init=1),
                               data=tentnocomp,
                               ginverse=list(factorasrid=asrpinv),
                               na.method.X="omit",
                               na.method.Y="omit",
                               maxiter=20)
plot(exploration.nocomp1)
summary(exploration.nocomp1)$varcomp
pin(exploration.nocomp1, r2expl~V1/(V1+V2))
# quite low repeatability without compromised data, I
# guess because of a loss of data but not sure
table(tent2$newcomp, tent2$winter)
# so losing compromised data means removing mostly 
# recent data, which we know to be taken well by
# Alfredo et al.

length(tentnocomp[,1])
length(tent2[,1])
# a loss of 122 data points. How does behaviour
# covary between compromised and non-compromised
# situations?

# let's simplify to just a yes or no compromised:
tent2$factorcomp <- as.factor(ifelse(tent2$comp.amalgamated==0,0,1))

exploration.compcovary1 <- asreml(fixed=logfr~factorcomp,
                              random=~factorasrid:us(factorcomp),
                              data=tent2,
                              na.method.X="omit",
                              na.method.Y="omit",
                              maxiter=500)

summary(exploration.compcovary1)$varcomp
summary(exploration.compcovary1, all=T)$coef.fi
# so here there is covariance between the compromsied and 
# non-compromised situations. Good covariance. And the non
# compromised situation also shows good repeatability, which
# suggests that the loss of repeatability with the loss of
# the non-compromsied data is a result of a loss of sample
# size rather than a lack of repeatability. I think.


# but what is heritability like without the compromised data?
exploration.nocomp2 <- asreml(fixed=logfr~1,
                              random=~ide(factorasrid, var=T, init=1)+
                                ped(factorasrid, var=T, init=1)+
                                ide(factorasrdam, var=T, init=1),
                              data=tentnocomp,
                              ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                              na.method.X="omit",
                              na.method.Y="omit",
                              maxiter=20)
plot(exploration.nocomp2)
summary(exploration.nocomp2)$varcomp
# oh. The results are the same. variance is partitioned
# to the pedigree regardless.


# given that the main results of interest are similar
# in both cases but there is a lot more power if the
# compromised data is included, keep the compromised 
# data in the data set.

# but this makes it hard to know whether the compromised
# boldness data should be in there or not. With boldness,
# it is really clear that the compromised data looks
# dodgy when plotted. Maybe I should consider this:

# let's plot against test order

tent2 <- testorder("birdid", "date2", tent2)
tent2$testorder
tent2[175:181,]


xyplot((flights+x2r)~testorder, 
       groups=birdid, type="b", data=tent2)
xyplot((flights+x2r)~testorder, 
       groups=birdid, type="b", data=tentnocomp)

xyplot(log(flights+x2r+0.5)~testorder, 
       groups=birdid, type="b", data=tent2)
xyplot(log(flights+x2r+0.5)~testorder, 
       groups=birdid, type="b", data=tentnocomp)


# so. With the boldness data, the compromised
# data looks very different from the normal data:
# the birds take far far longer to enter. With this
# data, there is not an easily perceived difference 
# to me between the compromised and non-compromised
# data. In fact, a lot of birds do very similar
# behaviour between the first and second test even
# when one or other is compromised.

# I am still happier keeping the compromised data in 
# this data set than in the boldness data set. However,
# in the boldness data set it becomes harder to lay
# out in the paper why the boldness data seems so 
# adversely affected, because it is my perception of
# whether it is affected and my perception can be
# incorrect.


####################################################
# sigh. Right. On with business.

# Separate the sexes:
table(tent2$sex)
# I have males as zeroes, females as ones:

tent2.F <- subset(tent2, tent2$sex==1)
table(tent2.F$sex)

tent2.M <- subset(tent2, tent2$sex==0)
table(tent2.M$sex)


# repeatability in females:

exploration.F <- asreml(fixed=logfr~zero,
                               random=~ide(factorasrid, var=T, init=1),
                               data=tent2.F,
                               ginverse=list(factorasrid=asrpinv),
                               na.method.X="omit",
                               na.method.Y="omit",
                               maxiter=20)

plot(exploration.F)
# not many zero data points in females

summary(exploration.F, all=T)$coef.fi
summary(exploration.F)$varcomp
# females have very high repeatability...

# significance of bird id:
exploration.F2 <- asreml(fixed=logfr~zero,
                        data=tent2.F,
                        na.method.X="omit",
                        na.method.Y="omit",
                        maxiter=20)


1-pchisq(2*(exploration.F$loglik-exploration.F2$loglik),1)

pin(exploration.F, r2expl~V1/(V1+V2))
length(tent2.F[,1])
length(unique(tent2.F$asrid))
table(table(tent2.F$asrid))



# repeatability in males:
exploration.M <- asreml(fixed=logfr~zero,
                        random=~ide(factorasrid, var=T, init=1),
                        data=tent2.M,
                        ginverse=list(factorasrid=asrpinv),
                        na.method.X="omit",
                        na.method.Y="omit",
                        maxiter=20)

plot(exploration.M)
# less good residuals than in females, interesting.
summary(exploration.M, all=T)$coef.fi
summary(exploration.M)$varcomp
# and much lower repeatability. How odd!

# significance of bird id:
exploration.M2 <- asreml(fixed=logfr~zero,
                         data=tent2.M,
                         na.method.X="omit",
                         na.method.Y="omit",
                         maxiter=20)


1-pchisq(2*(exploration.M$loglik-exploration.M2$loglik),1)

pin(exploration.M, r2expl~V1/(V1+V2))
length(tent2.M[,1])
length(unique(tent2.M$asrid))
table(table(tent2.M$asrid))



# I have fitted sex as a fixed effect before but I don't
# think I have done sex-specific repeatability like this.
# It's quite exciting! But I don't necessarily believe it.
# after all, repeatability can be affected by repeat sample
# sizes and variances and all kinds of quirks in the data.

table(table(tent2.F$birdid))
table(table(tent2.M$birdid))
# for example, there are a lot more repeat samples in males.
# However, males do seem to be more trappable than females:

table(tent2$sex, tent2$winter)
# in the recent winters particularly, a lot more males than
# females were tested. I presume a lot more males than females
# were caught. Why is that?
# I've emailed Mirre about this interesting problem.



# what happens in the non-compromised data set? Is the result
# the same?



tent2.Fnocomp <- subset(tentnocomp, tentnocomp$sex==1)
table(tent2.Fnocomp$sex)

tent2.Mnocomp <- subset(tentnocomp, tentnocomp$sex==0)
table(tent2.Mnocomp$sex)


# repeatability in females:

exploration.Fnocomp <- asreml(fixed=logfr~zero,
                        random=~ide(factorasrid, var=T, init=1),
                        data=tent2.Fnocomp,
                        ginverse=list(factorasrid=asrpinv),
                        na.method.X="omit",
                        na.method.Y="omit",
                        maxiter=20)

plot(exploration.Fnocomp)
# less data again

summary(exploration.Fnocomp, all=T)$coef.fi
summary(exploration.Fnocomp)$varcomp
# females have very high repeatability again.



# repeatability in males:
exploration.Mnocomp <- asreml(fixed=logfr~zero,
                        random=~ide(factorasrid, var=T, init=1),
                        data=tent2.Mnocomp,
                        ginverse=list(factorasrid=asrpinv),
                        na.method.X="omit",
                        na.method.Y="omit",
                        maxiter=20)

plot(exploration.Mnocomp)
# very skewey
summary(exploration.Mnocomp, all=T)$coef.fi
summary(exploration.Mnocomp)$varcomp
# no significant repeatability! Despite being the larger
# data set. How curious!
# but the principle that females are more repeatable
# than males at exploration still stands in this 
# reduced data, set, so that is interesting. There
# seem to be more zeroes in the male data set. Is
# this from a particular year or something?

table(tent2.Mnocomp$logfr, tent2.Mnocomp$trip)
# more in 2011 but not lots and lots more. I wonder
# what is driving lower repeatability in males?
# or perhaps it is because of the greater numbers
# of tests carried out in males. 



####################
# next is to see if repeatability varies between winters.
# I am not interested in winters 2009 and 2010 because
# there is not enough data
table(tent2$winter)

tent2.2011 <- subset(tent2, tent2$winter==2011)


exploration.2011 <- asreml(fixed=logfr~zero,
                              random=~ide(factorasrid, var=T, init=1),
                              data=tent2.2011,
                              ginverse=list(factorasrid=asrpinv),
                              na.method.X="omit",
                              na.method.Y="omit",
                              maxiter=20)

plot(exploration.2011)
# merm. Quite sparse
summary(exploration.2011, all=T)$coef.fi
summary(exploration.2011)$varcomp
# yeah.
# interestingly, this is the year of the predation
# in early 2012 and late 2011. Perhaps the addition
# of a predator changed behaviour?


# significance of bird id:
exploration.2011.2 <- asreml(fixed=logfr~zero,
                         data=tent2.2011,
                         na.method.X="omit",
                         na.method.Y="omit",
                         maxiter=20)


1-pchisq(2*(exploration.2011$loglik-exploration.2011.2$loglik),1)

pin(exploration.2011, r2expl~V1/(V1+V2))
length(tent2.2011[,1])
length(unique(tent2.2011$asrid))
table(table(tent2.2011$asrid))




### winter 2012
tent2.2012 <- subset(tent2, tent2$winter==2012)


exploration.2012 <- asreml(fixed=logfr~zero,
                           random=~ide(factorasrid, var=T, init=1),
                           data=tent2.2012,
                           ginverse=list(factorasrid=asrpinv),
                           na.method.X="omit",
                           na.method.Y="omit",
                           maxiter=20)

plot(exploration.2012)
# Bit better. This is one summer after predation.
summary(exploration.2012, all=T)$coef.fi
summary(exploration.2012)$varcomp

# more consistency.

# significance of bird id:
exploration.2012.2 <- asreml(fixed=logfr~zero,
                             data=tent2.2012,
                             na.method.X="omit",
                             na.method.Y="omit",
                             maxiter=20)


1-pchisq(2*(exploration.2012$loglik-exploration.2012.2$loglik),1)

pin(exploration.2012, r2expl~V1/(V1+V2))
length(tent2.2012[,1])
length(unique(tent2.2012$asrid))
table(table(tent2.2012$asrid))

##################################################

tent2.2013 <- subset(tent2, tent2$winter==0)


exploration.2013 <- asreml(fixed=logfr~zero,
                           random=~ide(factorasrid, var=T, init=1),
                           data=tent2.2013,
                           ginverse=list(factorasrid=asrpinv),
                           na.method.X="omit",
                           na.method.Y="omit",
                           maxiter=20)

plot(exploration.2013)
# more zeroes than the other two times but not bad.
# qq a bit funny.
summary(exploration.2013, all=T)$coef.fi
summary(exploration.2013)$varcomp

# and, for some reason, they are ultra-consistent
# this winter. How weird.

# it would be really nice to know what happened in 
# the 2014 winter but I don't have the pedigree for
# these birds. Therefore I am not adding the data.

# significance of bird id:
exploration.2013.2 <- asreml(fixed=logfr~zero,
                             data=tent2.2013,
                             na.method.X="omit",
                             na.method.Y="omit",
                             maxiter=20)


1-pchisq(2*(exploration.2013$loglik-exploration.2013.2$loglik),1)

pin(exploration.2013, r2expl~V1/(V1+V2))
length(tent2.2013[,1])
length(unique(tent2.2013$asrid))
table(table(tent2.2013$asrid))

###################################################


####### across winter trips? I could do across
# winters as well #######

table(tent2$withintrip)
tent2.acrosstrips <- subset(tent2, tent2$withintrip==1)


exploration.acrosstrips <- asreml(fixed=logfr~zero,
                           random=~ide(factorasrid, var=T, init=1),
                           data=tent2.acrosstrips,
                           ginverse=list(factorasrid=asrpinv),
                           na.method.X="omit",
                           na.method.Y="omit",
                           maxiter=20)

plot(exploration.acrosstrips)
# good
summary(exploration.acrosstrips, all=T)$coef.fi
summary(exploration.acrosstrips)$varcomp

# quite respectable. I'd like to know what happens
# across winters though. I think the sample size
# will be a lot smaller but I'd like to know:

# first, keep the variable testorder that I made
# earlier for the order across all tests:

tent2$alltestorder <- tent2$testorder

tent2 <- testorder("birdwinter", "date2", tent2)
tent2$testorder

tent2[80:85,]
# seems to have worked.


# now get the across winter subset for the across-winter model:


tent2.acrosswinter <- subset(tent2, tent2$testorder==1)

summary(tent2.acrosswinter)



exploration.acrosswinter <- asreml(fixed=logfr~zero,
                                  random=~ide(factorasrid, var=T, init=1),
                                  data=tent2.acrosswinter,
                                  ginverse=list(factorasrid=asrpinv),
                                  na.method.X="omit",
                                  na.method.Y="omit",
                                  maxiter=20)

plot(exploration.acrosswinter)
# good
summary(exploration.acrosswinter, all=T)$coef.fi
summary(exploration.acrosswinter)$varcomp

# That is very nice :) This matches the boldness
# stuff better in terms of the boldness is expressed
# as across year, so across winter makes more sense
# here.

# significance of bird id:
exploration.acrosswinter2 <- asreml(fixed=logfr~zero,
                             data=tent2.acrosswinter,
                             na.method.X="omit",
                             na.method.Y="omit",
                             maxiter=20)


1-pchisq(2*(exploration.acrosswinter$loglik-
              exploration.acrosswinter2$loglik),1)

pin(exploration.acrosswinter, r2expl~V1/(V1+V2))
length(tent2.acrosswinter[,1])
length(unique(tent2.acrosswinter$asrid))
table(table(tent2.acrosswinter$asrid))



###################################################################

# now to the animal model:
# This goes in Table 3 of the main text

explorationanimal1 <- asreml(fixed=logfr~1,
                             random=~ide(factorasrid, var=T, init=1)+
                               ped(factorasrid, var=T, init=1)+
                               ide(factorasrdam, var=T, init=1),
                             data=tent2,
                             ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                             na.method.X="omit",
                             na.method.Y="omit",
                             maxiter=20)

plot(explorationanimal1)
# fitted vs residuals is better than before, the
# qq is still nice.
summary(explorationanimal1, all=T)$coef.fi
summary(explorationanimal1)$varcomp



# how big are h2, m2, pe2?
pin(explorationanimal1, pe2tent~V1/(V1+V2+V3+V4))
pin(explorationanimal1, h2tent~V2/(V1+V2+V3+V4))
pin(explorationanimal1, m2tent~V3/(V1+V2+V3+V4))


# and as a proportion of personality?

pin(explorationanimal1, pe2tent~V1/(V1+V2+V3))
pin(explorationanimal1, h2tent~V2/(V1+V2+V3))
pin(explorationanimal1, m2tent~V3/(V1+V2+V3))


# let's do the significances.
# don't need to do Vpe, it is boundary.

# Va

explorationanimal2 <- asreml(fixed=logfr~1,
                             random=~ide(factorasrid, var=T, init=1)+
                               ide(factorasrdam, var=T, init=1),
                             data=tent2,
                             ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                             na.method.X="omit",
                             na.method.Y="omit",
                             maxiter=20)

1-pchisq(2*(explorationanimal1$loglik-
              explorationanimal2$loglik),1)

# Vm


explorationanimal3 <- asreml(fixed=logfr~1,
                             random=~ide(factorasrid, var=T, init=1)+
                               ped(factorasrid, var=T, init=1),
                             data=tent2,
                             ginverse=list(factorasrid=asrpinv),
                             na.method.X="omit",
                             na.method.Y="omit",
                             maxiter=20)

1-pchisq(2*(explorationanimal1$loglik-
              explorationanimal3$loglik),1)


# heritability
pin(explorationanimal1, h2~V2/(V1+V2+V3+V4))
# maternal effects
pin(explorationanimal1, m2~V3/(V1+V2+V3+V4))

# as a proportion of the repeatable portion of exploration:
pin(explorationanimal1, h2pers~V2/(V1+V2+V3))


###################################################################

##### the fixed effects #####

# Habituation is important, so I need to look at
# short term and long term habituation - within 
# and across winter trips.
# Whether a test is compromised matters. Compromised
# data could be a result of human disturbance or
# other forms of disturbance.
# I know from my earlier work that the tests disturbed
# by cheeping of other sparrows only are the same as
# non-compromised tests, and have been re-classified
# within this script as non-compromised.
# which winter the test was taken in, and whether it
# was before or after Christmas.
# the observer (with the observers that observed
# one or a couple of tests grouped together).

# methodological fixed effects that I want to test
# first:
# Method of counting runs - prior to Feb 2012 the
# method for counting runs was potentially different,
# and was definitely different in November 2011


# how long the birds spend in the bag could be important.
length(tent2[,1])
summary(tent2$minbag)
# so 42 missing values, data set goes from
# 450 to 408

tentbag <- subset(tent2, tent2$minbag!="NA")

xyplot(log(flights+x2r+0.5)~minbag, groups=asrid, type="b", data=tent2)
# there is an overall negative relationship
# but looking at the individual responses shows
# a lot of variability. This is something that
# should be fitted at the population and the
# individual level to tease apart the effect.

mord <- as.data.frame(tapply(tentbag$minbag, tentbag$birdid, FUN=mean, na.rm=T))
nam <- as.data.frame(rownames(mord))
mord2 <- cbind(nam, mord)
names(mord2) <- c("birdid", "mbag")
tentbag <- merge(tentbag, mord2, all.x=T)
tentbag$ibag <- tentbag$minbag - tentbag$mbag

tentbag$zmbag <- scale(tentbag$mbag)
tentbag$zibag <- scale(tentbag$ibag)

tentbag$zmbag <- as.numeric(tentbag$zmbag)
tentbag$zibag <- as.numeric(tentbag$zibag)

# model bag1 was in the original script but is not
# needed in this script.

# so, model time spend in the bag, method,
# time of day:
xyplot(logfr~mtime, groups=asrid, data=tentbag, type="b")
xyplot(logfr~indtime, groups=asrid, data=tentbag, type="b")
# maybe a slight positive effect of time, not much:

# how about whether there is a seasonal change in
# exploration? It isn't part of my main focus but
# I can test whether exploration score increases
# before and after Christmas.

bag2 <- asreml(fixed=logfr~zmbag+zibag+zmtime+zindtime+
                 relevel(method,"1")+befaft+obs2+winter,
               random=~ide(factorasrid, var=T, init=1),
               data=tentbag,
               ginverse=list(factorasrid=asrpinv),
               na.method.X="omit",
               na.method.Y="omit",
               maxiter=20)

plot(bag2)
summary(bag2)$varcomp
summary(bag2, all=T)$coef.fi
# so there's a positive effect of time of day and a
# negative effect of method, but the method effect
# might be because of year effects and observer effects.
# put these in to the model.
# added them, there is still a general effect of time
# but there is no longer an effect of method. Keep
# method out of the models (the dependent variable
# was already doubled to account for this anyway)
# and keep time.

# nothing here suggests the time spent in the bag makes
# a difference at all. This can stay out of the models too.

# before and after Christmas does not matter so 
# I can remove this.

# so, within and between trip habituation,
# sex, time, winter, compromised, before or
# after winter, observer.

# check VIFs for lmer objects:

vif.lme(lmer(logfr~zmbtwn+zindbtwn+
         factor(withintrip)+sex+zmtime+
         zindtime+winter+factorcomp+obs2+(1|factorasrid),
       data=tent2))
# looking good! The factors are high for normal
# VIF but they have to be corrected for having
# multiple levels, so after this they look very 
# good! Observer shows most correlation with the
# output, which is very interesting.


test1 <- gvlma(lm(logfr~zmbtwn+zindbtwn+
                    factor(withintrip)+sex+zmtime+
                    zindtime+winter+factorcomp+obs2,
                  data=tent2))
summary(test1)
# wow! only breaking one assumption of skewness
# and otherwise doing very well :)
plot(test1, onepage=FALSE)

# good!

# Animal model with fixed effects.
# This goes in the supplement:

explorationfixed1 <- asreml(fixed=logfr~zmbtwn+zindbtwn+
                              factor(withintrip)+sex+zmtime+
                              zindtime+winter+factorcomp+obs2,
                 random=~ide(factorasrid, var=T, init=1)+
                   ped(factorasrid, var=T, init=1)+
                   ide(factorasrdam, var=T, init=1),
                 data=tent2,
                 ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                 na.method.X="omit",
                 na.method.Y="omit",
                 maxiter=20)

# there is a WARNING with this model that asrid 
# changed by 3.32% on the last iteration. However,
# on looking at the results, the asrid component is
# tiny, so a change of 3% is a result of the tiny
# estimate and is not a problem.

plot(explorationfixed1)
# a slight hump but it's not bad at all, and
# would even out with more data

plot(tent2$zmbtwn, explorationfixed1$residuals)
plot(tent2$zindbtwn, explorationfixed1$residuals)
plot(tent2$withintrip, explorationfixed1$residuals)
plot(tent2$sex, explorationfixed1$residuals)
plot(tent2$zmtime, explorationfixed1$residuals)
plot(tent2$zindtime, explorationfixed1$residuals)
# most birds tested once, hence the great density at zero.
plot(tent2$winter, explorationfixed1$residuals)
# interesting heterogeneity there. Sample size?
table(tent2$winter) #yes.
plot(tent2$factorcomp, explorationfixed1$residuals)
plot(tent2$obs2, explorationfixed1$residuals)
# good stuff!

# random effect results:
summary(explorationfixed1)$varcomp
# fixed effect results:
summary(explorationfixed1, all=T)$coef.fi
# fixed effect significances:
wald.asreml(explorationfixed1, ssType="conditional", denDF="numeric")

# what are the h2 and m2 estimates?

pin(explorationfixed1, h2~V2/(V1+V2+V3+V4))
pin(explorationfixed1, m2~V3/(V1+V2+V3+V4))

# as a proportion of the repeatable portion of exploration:
pin(explorationfixed1, h2pers~V2/(V1+V2+V3))

# 95% confidence intervals for variance components
library(nadiv)

aiCI(explorationfixed1)
# I wonder if I can plot these in a barplot...
# So, at the moment I have a stacked barplot, but
# to plot the 95% bars and show they are different
# from zero I would need a side-by-side set of barplots.

# maybe I can do something like this for the supplement.



# I still need significance for these variance components.
# Vpe

explorationfixed2 <- asreml(fixed=logfr~zmbtwn+zindbtwn+
                              factor(withintrip)+sex+zmtime+
                              zindtime+winter+factorcomp+obs2,
                            random=~ped(factorasrid, var=T, init=1)+
                              ide(factorasrdam, var=T, init=1),
                            data=tent2,
                            ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                            na.method.X="omit",
                            na.method.Y="omit",
                            maxiter=20)

1-pchisq(2*(explorationfixed1$loglik-explorationfixed2$loglik),1)


# Va
explorationfixed3 <- asreml(fixed=logfr~zmbtwn+zindbtwn+
                              factor(withintrip)+sex+zmtime+
                              zindtime+winter+factorcomp+obs2,
                            random=~ide(factorasrid, var=T, init=1)+
                              ide(factorasrdam, var=T, init=1),
                            data=tent2,
                            ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                            na.method.X="omit",
                            na.method.Y="omit",
                            maxiter=20)

1-pchisq(2*(explorationfixed1$loglik-explorationfixed3$loglik),1)



# Vm

explorationfixed4 <- asreml(fixed=logfr~zmbtwn+zindbtwn+
                              factor(withintrip)+sex+zmtime+
                              zindtime+winter+factorcomp+obs2,
                            random=~ide(factorasrid, var=T, init=1)+
                              ped(factorasrid, var=T, init=1),
                            data=tent2,
                            ginverse=list(factorasrid=asrpinv),
                            na.method.X="omit",
                            na.method.Y="omit",
                            maxiter=20)

1-pchisq(2*(explorationfixed1$loglik-explorationfixed4$loglik),1)


##############################################################

##############################################################

# should I present poisson versions as well since
# transforming data is not ideal?

tent2$total <- tent2$flights + tent2$x2r
asrex16.p <- asreml(fixed=total~zindbtwn+sex+winter+
                      factor(newcomp),
                  random=~ide(factorasrid, var=T, init=1)+
                    ped(factorasrid, var=T, init=1)+
                    ide(factorasrdam, var=T, init=1),
                  data=tent2,
                  ginverse=list(factorasrid=asrpinv,
                                factorasrdam=asrpinv),
                  na.method.X="omit",
                  na.method.Y="omit",
                  family=asreml.poisson(),
                  maxiter=20)

plot(asrex16.p)
summary(asrex16.p)$varcomp
summary(asrex16.p, all=T)$coef.fi

# this data isn't so poisson.

# However, in the ASReml-R manual, there are
# notes on GLMMs. They say poissons with many
# samples per random effect level can perform
# well. However inference with random effects
# can be poor, and especially in binomial models
# the variance components can be very badly estimated.

# the manual then says:
# "Therefore, we cannot recommend the use of this 
# technique [GLMMs] for general use"

# there are other places where GLMMs are recommended
# against on the internet, including Arthur Gilmore's
# tutorial slides.


##########################################################
##########################################################
# checkup on whether year should be random,
# as recommended by Eryn at ESEB 2015

str(tent2$winter)

explorationyear1 <- asreml(fixed=logfr~1,
                             random=~ide(factorasrid, var=T, init=1)+
                               ped(factorasrid, var=T, init=1)+
                               ide(factorasrdam, var=T, init=1)+
                               winter,
                             data=tent2,
                             ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                             na.method.X="omit",
                             na.method.Y="omit",
                             maxiter=20)

plot(explorationyear1)
# interesting flat peak in the middle of the residuals
# histogram
summary(explorationyear1)$varcomp
summary(explorationanimal1)$varcomp

# how interesting! Winter here mostly takes from the
# residual variance. As with the other models, my
# conclusions on heritability and etc are not changed
# but my (non-significant) maternal effect is smaller.
# something to think about and discuss with someone.

# because when you include other variables like this
# what happens when you do the heritability of personality?
# is winter in that denominator or not?
# is it theoretically better to present this or to
# present with year as a fixed effect?

##########################################################
##########################################################
# sample sizes:

length(tent2[,1])
length(unique(tent2$asrid))
table(table(tent2$asrid))
summary(tent2$flights+tent2$x2r)
sd(tent2$flights+tent2$x2r)
summary(tent2$minbag)
table(tent2$minbag)
sd(tent2$minbag, na.rm=T)

table(tent2$winter)
table(tent2$asrid[which(tent2$winter==2009)])
table(tent2$asrid[which(tent2$winter==2010)])


# how many dams were filled in dams in the pedigree?
summary(damplus)
length(which(tent2$asrdam>6072))
tent2$asrid[which(tent2$asrdam>6072)]


# how many compromised tests are in the data set
table(tent2$comp.amalgamated)
