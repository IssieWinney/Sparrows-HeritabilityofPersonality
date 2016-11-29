# Isabel Winney
# 2nd December 2015

# this is the script to extract data on a novel object test
# for analysis in ASReml-R ('boldness' test). The data includes data
# from 2011-2014 carried out by me (2011-2013) and Alfredo Sanchez-Tojar
# (2014) and initial tests conducted by Ian Cleasby (2009).

#################################################################################
### Loading the data
#################################################################################

# clear the workspace
rm(list=ls())

# set working directory
setwd("./masterdatasheets")


# load in desirable packages:

library(pedantics)
library(asreml)
library(lattice)
library(car)
library(lme4)
library(gvlma)
pin <- dget("../../RFunctions/pin.R")
source("../../RFunctions/assigningtestorder-20150218.R")
source("../../RFunctions/vif_lmer_notmycode_20150727.R")


# read in boldness data
# (extra data was marked as compromised in June 2015)
# (relevant video information is kept in file May2015-2011-14VideoInfofromchecked2015MarchDB.txt)
# (notes relating to each observation are in the original excel file
# V9PMASTER-2011checkedDec2014-videoinfocheckedMay2015-2012checkedJune2015)
boldnessdata <- read.table("V9PMASTER-foranalysisJune2015.txt", 
                           header=T, na.strings="NA")

# take a look
View(boldnessdata)
summary(boldnessdata)
str(boldnessdata)

# the data is structured with a single row per observation of a bird.
# Each test has two rows: one for the male and one for the female.
# Times are decimalised to the nearest six seconds, with ambiguous times rounded down e.g. 33 seconds is 0.5 min.

#################################################################################
### Data definitions
#################################################################################

# videoname					Reference ID for the video made at the nest box.
# numericdate				Date that the video was taken, MS excel's numeric version.
# date						Date that the video was taken.
# takenonwrongday			Was the video taken when the brood was 9 days old (0, most desirable), 
#								taken when the brood was some other age (1), 
#								or was taken when the brood was probably nine days old but this date is not certain (2)
# numerictime				Time of day that the video was started, MS excel's numeric version.
# time						Time of day that the video was started.
# broodname					Reference ID for the brood in the nest box.
# nestbox					Nestbox ID for the nest box on Lundy that the brood was in.
# observer					ID of the person that watched the video. JS data was checked by IW.
# sex						Sex of the specific bird being observed: male (M) or female (F).
# antenna.yforpresent		Did the nest box have an RFID antenna around the entrance: no (n) or yes (y).
# perchpresent.1forpresent	Did the nest box have a perch: no (0) or yes (1).
# method					How was the paper presented to the sparrows: 
#								(1) covering the entire/near entire nest entrance on the inside of the hole
#								(2) covering half the nest entrance on the outside of the hole
#								(3) covering half the nest entrance on the inside of the hole
#								(4) paper is beside hole on the outside of the nest box
#								(5) pen hung in front of nest box hole, no paper
#								(6) paper folded in half and placed in nest box hole
#								(7) paper on top of nest box
# arrivallatency			Time since the start of the video that the bird is first seen in the video.
# nestboxcontact			Time since the start of the video that the bird first touches the nest box (roof, perch).
# perchcontact				Time since the start of the video that the bird first touches the nest box perch 
#								(if there is one, otherwise the nest box entrance hole).
# peckcontact				Time since the start of the video that the bird first pecks the paper.
# pecks						Number of times that the bird pecks the paper (not reliable).
# inbox						Time since the start of the video that the bird first enters the nest box.
# birdorderentry			Was this bird the first (1) or second (2) of the pair to enter the nest box.
# firstvisitlength			Length of the first visit by the focal bird after passing the paper for the first time in the video 
#								- exit time minus entry time (inbox) for the first visit.
# secondvisitlength			Length of the second visit (see firstvisitlength).
# rewatchcompDec2014		Whether a test was ok (0) or compromised by test specific events (1) after data checks in December 2014.
# compJune2015				Whether a test was ok (0) or compromised (1) after data checks in June 2015 - use this value.
# comp.1human.2paper.3other	Was a test not compromised (0), first compromised by people (1), 
# 								first compromised by a mild problem with the paper such as the paper being bent (2),
# 								or more seriously compromised (3), such as by a bird removing the paper or by video failure.


#################################################################################
### Cleaning the data and adding important variables
#################################################################################


# Does each video occur twice? (once for each male and female)
table(table(boldnessdata$videoname))
# One video does not: VK0362a and VK0362. The paper fell
# down during the first attempt at the video, but not 
# before the male had entered the nest box (and created
# a data point). This first attempt at the video is
# labelled a.
boldnessdata[which(boldnessdata$videoname=="VK0362a"),]
# This shows that VK0362a is considered compromised and compromised by 'other'
# factors. Therefore it is one of the more seriously compromised videos that is
# removed before analysis (see creation of subset boldnessdata7).


# Add the identity of the male and female:
# Load in the video information:
{
videoinfo <- read.table("May2015-2011-14VideoInfofromchecked2015MarchDB.txt", 
                        header=T, na.strings="NA")
						
# this video information says inside that the social parents are from my own social parent pedigree
# from 2009 to 2011. But actually it is to 2013. My social parent pedigree has NA parents filled in if
# we know they were parents for all offspring or if we have some ring information and it fits some genetic
# information as well. Exact notes elsewhere.
						
head(videoinfo)
summary(videoinfo)
View(videoinfo)
str(videoinfo)


# do all the videos in the boldness data sheet have
# a record in the video data sheet?
boldnessdata$videocheck <- as.character(videoinfo$DVDNumber)[match(
    boldnessdata$videoname, videoinfo$DVDNumber)]
boldnessdata$videocheck2 <- ifelse(boldnessdata$videoname==boldnessdata$videocheck,
                                   0, 1)
summary(boldnessdata$videocheck2)
# it's all there. However, I should be careful matching these
# together because they are both factors, and I don't want 
# factor levels to be returned by accident.



# add the male's ID, first checking that it is not a factor
str(videoinfo$SocialDadID.2009to11frommyDBparents)
boldnessdata$maleID <- videoinfo$SocialDadID.2009to11frommyDBparents[match(
  boldnessdata$videoname, videoinfo$DVDNumber)]

# and the female's ID
str(videoinfo$SocialMumID.2009to11frommyDBparents)
boldnessdata$femaleID <- videoinfo$SocialMumID.2009to11frommyDBparents[match(
  boldnessdata$videoname, videoinfo$DVDNumber)]


# is it the right data?
boldnessdata[which(boldnessdata$videoname=="VK0330"),]
videoinfo[which(videoinfo$DVDNumber=="VK0330"),]

# they match, check another one
boldnessdata[which(boldnessdata$videoname=="VN0519"),]
videoinfo[which(videoinfo$DVDNumber=="VN0519"),]

}

# which bird is the focal bird?
boldnessdata$birdID <- ifelse(boldnessdata$sex=="F",
                              boldnessdata$femaleID,
                              boldnessdata$maleID)
head(boldnessdata)
tail(boldnessdata)
str(boldnessdata$birdID)
summary(boldnessdata$birdID)
# only 20 times when the bird isn't known :)

# who is the partner?
boldnessdata$partnerID <- ifelse(boldnessdata$sex=="F",
                              boldnessdata$maleID,
                              boldnessdata$femaleID)
head(boldnessdata)
tail(boldnessdata)
str(boldnessdata$partnerID)
summary(boldnessdata$partnerID)




# VK0089 - it is suspected the piece of paper fell down 
# before this test began, or was never put up, so this
# video is excluded from the data set:

boldnessdata1 <- subset(boldnessdata, boldnessdata$videoname!="VK0089")
head(boldnessdata1)
summary(boldnessdata1)
str(boldnessdata1)
which(boldnessdata1$videoname=="VK0089")


# calculating Julian Dates from dd/mm/yy in R:
boldnessdata1$juliandate <- strptime(boldnessdata1$date, "%d/%m/%Y")$yday+1
head(boldnessdata1$juliandate)
hist(boldnessdata1$juliandate)

# extracting year from  dd/mm/yy in R:
boldnessdata1$year <- strptime(boldnessdata1$date, "%d/%m/%Y")$year +1900
head(boldnessdata1$year)
table(boldnessdata1$year)


# The dependent variable is the latency from when a 
# bird first touches the nest box to when a bird 
# enters the nest box. Another possibility for a
# dependent variable is just when the bird enters
# the nest box. However, this is dependent upon when
# the bird first arrived in the area, since a bird
# could (at one extreme) be watching me put up the
# paper or (at another extreme) be taking a half-hour
# dust bath some distance away and therefore not seen
# the stimulus for a long time. What seems more likely
# is that return time would be dependent on the 
# foraging frequency and location, so return time could
# be a repeatable trait but for the wrong reasons (i.e.
# because it is dependent on a bird's foraging trip
# length). For these reasons, the latency from first 
# touching the box - a clearly defined point where the
# bird is known to have seen the stimulus - to when
# the bird enters the box and accepts the stimulus 
# is better as a dependent variable.

#################################################################################
# so, make my dependent variable: the latency from touching
# to entering the nestbox
boldnessdata1$entrylatency <- boldnessdata1$inbox-boldnessdata1$nestboxcontact
summary(boldnessdata1$entrylatency)
str(boldnessdata1$entrylatency)
hist(boldnessdata1$entrylatency, breaks=seq(0,60,0.5))
#################################################################################


# make a birdyear variable for matching up data later:
summary(boldnessdata1$birdID)
summary(boldnessdata1$year)
boldnessdata1$birdyear <- paste(boldnessdata1$birdID, boldnessdata1$year, sep="")
head(boldnessdata1$birdyear)
summary(boldnessdata1$birdyear)
head(boldnessdata1)

# I used seven different methods in these videos, 1-4
# are paper based and 5 uses a pen (so is intrinsically
# a different test and reaction). 6 and 7 are Ian playing
# with paper in the hole or on the nestbox roof. Paper tests 2,4,6 and 7
# have very small sample size, and the paper was either
# not a standardised size (methods 2 and 6) or was not on
# the entrance and ignored (4 and 7).
# Methods 2, 4, 5, 6, and 7 are excluded from the data.

# however, I also want to know which was the first, second,
# third etc test within a year or across years. For this,
# I can use my function testorder.

# first, I want to remove the non-paper related test:

boldnessdata2 <- subset(boldnessdata1, boldnessdata1$method!=5)

# now I should work out what the partner is doing. I should
# split the responses in to the boldest and shyest 50% based on
# the responses before cutting out the different types of
# paper test.

# the data I will use for this is a subset of boldnessdata2: 
# the dataset where both the video with no paper and the videos 
# that used a pen are cut out. However, videos with birds with
# no birdID are left in since they inform the population 
# average behaviour. From this, I need to remove the birds
# that did not arrive in the video (have no value for 
# nestboxcontact) because they did not take part in the test (or they did 
# not come within a standardised distance of the paper) and so are not 
# considered to have the same test experience. Note that
# arrivallatency is not used here for whether a bird turned
# up because this variable is the time that any bird arrived 
# in the video but the value depends on how zoomed in the video is.

summary(boldnessdata2$nestboxcontact)
length(boldnessdata2$nestboxcontact)

birdsinvideo <- subset(boldnessdata2, boldnessdata2$nestboxcontact!="NA")

summary(birdsinvideo$nestboxcontact)
length(birdsinvideo$nestboxcontact)

# split by sex (since individuals will be matched to partners,
# but males and females vary)

femalesinvideo <- subset(birdsinvideo, birdsinvideo$sex=="F")
malesinvideo <- subset(birdsinvideo, birdsinvideo$sex=="M")

# check the right birds are in the right subset:
table(birdsinvideo$sex)
table(femalesinvideo$sex)
table(malesinvideo$sex)
# check there is just one video per bird:
table(table(femalesinvideo$videoname))
table(table(malesinvideo$videoname))

# order by entry latency, with the boldest first
sort(femalesinvideo$entrylatency, na.last=T)
length(femalesinvideo$entrylatency)
# middle is at 186, entry latency 5.0

femalesinvideo$boldshy <- ifelse(femalesinvideo$entrylatency<5.0, "bold", "shy")
head(femalesinvideo)
femalesinvideo$boldshy
femalesinvideo$boldshy[is.na(femalesinvideo$boldshy)] <- "shy"
femalesinvideo[290:310,]
table(femalesinvideo$boldshy)
# that will do.


# males
sort(malesinvideo$entrylatency, na.last=T)
length(malesinvideo$entrylatency)
# middle is at 181, entry latency 13.9 for 182
# males have far more NA values than females

malesinvideo$boldshy <- ifelse(malesinvideo$entrylatency<13.9, "bold", "shy")
head(malesinvideo)
malesinvideo$boldshy
malesinvideo$boldshy[is.na(malesinvideo$boldshy)] <- "shy"
malesinvideo[100:110,]
table(malesinvideo$boldshy)
# that will do.

# add to the original data frame.
boldnessdata2$partnerboldshy <- ifelse(boldnessdata2$sex=="F",
                                malesinvideo$boldshy[match(boldnessdata2$videoname, malesinvideo$videoname)],
                                femalesinvideo$boldshy[match(boldnessdata2$videoname, femalesinvideo$videoname)])

head(boldnessdata2)
tail(boldnessdata2)
table(boldnessdata2$partnerboldshy)



# now I can cut out the birds whose identities I do not know.

boldnessdata3 <- subset(boldnessdata2, boldnessdata2$birdID!="NA")
summary(boldnessdata2$birdID)
length(boldnessdata2$birdID)
length(boldnessdata3$birdID)


# order within birds within years:

boldnessdataordered <- testorder("birdyear", "juliandate", boldnessdata3)
boldnessdataordered[1:10,]
tail(boldnessdataordered)
table(boldnessdataordered$testorder)

# one bird was tested six times in a year...

xyplot(entrylatency~testorder, groups=birdyear, 
       data=boldnessdataordered, type="b")
xyplot(entrylatency~testorder, groups=birdID, 
       data=boldnessdataordered, type="b")

# some birds scores go up, most birds scores go down with repeated
# testing.

# at this point I should separate the within-individual
# from the between-individual effect to show that habituation
# occurs despite the unbalanced experimental design (variable
# numbers of repeat samples per individual).

# I do this now before data is removed where I still know the ID of the bird.

# I should account for experience with the test. Ideally
# I do this with a data set that has all paper tests in
# it, to get the best idea of what an individual's 
# experience level is at a given test in the final data
# set.
experiencemean <- aggregate(boldnessdataordered$testorder, 
                            by=list(boldnessdataordered$birdID), FUN=mean)
head(experiencemean)
boldnessdataordered$testorder[which(boldnessdataordered$birdID==3302)]
boldnessdataordered$testorder[which(boldnessdataordered$birdID==3764)]
# also good!

boldnessdataordered$meanexperience <- experiencemean$x[match(
  boldnessdataordered$birdID, experiencemean$Group.1)]

head(boldnessdataordered)
head(experiencemean)

boldnessdataordered$imc.experience <- boldnessdataordered$testorder-boldnessdataordered$meanexperience
hist(boldnessdataordered$imc.experience)
# plot of experience is a bit stochastic
# because of the majority of birds tested once or twice.





# what about testing across years? Perhaps this is confounded 
# with year?
# Plot it to see, but just take the first measures to make
# visualisation easier
firstmeasuresonly <- subset(boldnessdataordered, boldnessdataordered$testorder==1)
xyplot(entrylatency~year, groups=birdID, 
       data=firstmeasuresonly, type="b")

# there is no visual trend across years - just that 
# some birds are more shy, some don't change.
boldnessdataordered$logentry <- log(boldnessdataordered$entrylatency+0.5)
summary(boldnessdataordered$logentry)

lmer1 <- lmer(logentry~year+(1|birdID), data=boldnessdataordered)
plot(lmer1)
summary(lmer1)
# this suggests a general increase in shyness across
# years. This might be biologically meaningful or it
# might be a trend due to increased shyness across the
# years sampled

lmer1 <- lmer(logentry~relevel(factor(year), ref=5)+(1|birdID), 
              data=boldnessdataordered)
plot(lmer1)
summary(lmer1)
# so each year there is a population trend to become 
# more shy...
# is 2011 particularly bold due to the alternative methods
# used in this year?

table(boldnessdata3$method)

# remove all apart from methods 1 and 3:
boldnessdata4 <- subset(boldnessdataordered, boldnessdataordered$method!=2)
boldnessdata4 <- subset(boldnessdata4, boldnessdata4$method<4)

table(boldnessdata4$method)
summary(boldnessdata4)

lmer1 <- lmer(logentry~year+(1|birdID), 
              data=boldnessdata4)
plot(lmer1)
summary(lmer1)
# no, there is still a general trend for a year effect.


# some males had more than one brood at a time - they
# were socially polygamous. I want to exclude all tests
# for these males in the years for which they were 
# polygamous because their polygamy will have affected 
# their return times to the brood and will have meant the
# males were tested more often.

# the following males are classed as polygamous following
# the rule of having two or more broods with hatch dates <25 days
# apart with different females provided the first brood did
# not fail:
# 3684 in 2011
# 4060 in 2011
# 4673 in 2011
# 4682 in 2012 and 2013
# 4768 in 2012
# 4895 in 2012 and 2014
# 4960 in 2012 and 2013
# 4991 in 2012
# 5461 in 2013
# 5466 in 2014
# 5489 in 2013
# 6391 in 2014
# 6790 in 2014
# 6816 in 2014
# 7267 in 2014
### this list includes males that don't turn up in my data set!

polygamousmales <- c(36842011, 40602011, 46732011, 46822012, 46822013,
                     47682012, 48952012, 48952014, 49602012, 49602013,
                     49912012, 54612013, 54662014, 54892013, 63912014,
                     67902014, 68162014, 72672014)
polygamousmales

# find these males in the data set
polygamousmales[match(boldnessdata4$birdyear, polygamousmales)]

# mark these individuals out in the original data
boldnessdata4$polygamy <- polygamousmales[match(boldnessdata4$birdyear, polygamousmales)]
boldnessdata4$polygamy

boldnessdata4[715:725,]
str(boldnessdata4$polygamy)

# make the other individuals value less:
boldnessdata4$polygamy[is.na(boldnessdata4$polygamy)] <- 0
boldnessdata4[697:714,]
str(boldnessdata4$polygamy)
table(boldnessdata4$polygamy)


# and now keep only the monogamous individuals:

boldnessdata5 <- subset(boldnessdata4, boldnessdata4$polygamy==0)
head(boldnessdata5)
str(boldnessdata5)
summary(boldnessdata5)





# for this investigation, I should remove the NAs for entry latency:
boldnessdata6 <- subset(boldnessdata5, boldnessdata5$entrylatency!="NA")
summary(boldnessdata6)


# one last thing: in several videos, the video was compromised
# for serious reasons including the female being inside the
# nest box when the video started, a bird removing the paper
# before the partner had been in the video etc. So, I want
# to remove these quite seriously affected videos from the
# analysis before I consider anything else:

boldnessdata7 <- subset(boldnessdata6, boldnessdata6$comp.1human.2paper.3other<3)
summary(boldnessdata7)


# my entry latencies are not normal. In the past I have log-
# transformed this variable, but is this still appropriate?

boxcox((entrylatency+0.5)~1+(1|birdID), 
       data=boldnessdata7, 
       lambda=seq(-0.5,0.50,0.001))


powerTransform((entrylatency+0.5)~1+(1|birdID), 
       data=boldnessdata7)


# it looks like a log is still a good transformation to use...

# also, make negative so that bolder individuals have
# higher boldness values
boldnessdata7$logentrylatency <- -1*log(boldnessdata7$entrylatency+0.5)
summary(boldnessdata7$logentrylatency)
# plot
hist(boldnessdata7$logentrylatency, breaks=seq(-4,1,0.1),
     main="After transformation", xlab="-1*log(boldness+0.5)")

# sort of normal ish.
# before, it was like this:
hist(boldnessdata7$entrylatency, breaks=seq(0,60,0.5),
     main="Before transformation", xlab="boldness",
     col=rgb(0,0,0.5,0.2), cex.lab=1.5)
# this is figure boldness-beforetransformation-june2015.jpg


# fixed effects: how many offspring are in the brood?
# let's consider each sex separately:
boldnessdata7$offspring <- videoinfo$OffspringNo[match(
  boldnessdata7$videoname, videoinfo$DVDNumber)]
head(boldnessdata7)

xyplot(entrylatency~offspring|sex, groups=birdID,
       data=boldnessdata7, type="l")
xyplot(logentrylatency~offspring|sex, groups=birdID,
       data=boldnessdata7, type="l")

# a clear trend. More offspring means more motivation to
# enter - unless more offspring occur later in the season
# and the number of offspring is confounded with experience

xyplot(logentrylatency~offspring|testorder, data=boldnessdata7, 
       groups=birdID, type="b")
# the effect of number of offspring seems more minor but is
# still present.

# I should IMC experience and number of offspring.
offspringmean <- aggregate(boldnessdata7$offspring, 
                           by=list(boldnessdata7$birdID), FUN=mean)
head(offspringmean)
boldnessdata7$offspring[which(boldnessdata7$birdID==3302)]
boldnessdata7$offspring[which(boldnessdata7$birdID==3764)]
# good

# add this mean to boldnessdata7:
boldnessdata7$meanoffspring <- offspringmean$x[match(
  boldnessdata7$birdID, offspringmean$Group.1)]
head(boldnessdata7)
head(offspringmean)

# subtract mean from each data point to get mean-centred data
boldnessdata7$imc.offspring <- boldnessdata7$offspring-boldnessdata7$meanoffspring
hist(boldnessdata7$imc.offspring)
# offspring is very normally distributed

#################################################################################
### Adding the pedigree
#################################################################################

# load pedigree from the importPedigree file.
# add the ASReml ID to the data frame (this is a bird ID made for an analysis in
# ASReml that minimises the number of missing IDs because a long time ago ASReml 
# complained when the bird IDs were not very continuous because of lots of missing IDs)

source("scripts/importPedigree-Sept2015.R")

# now add the bird ID (asrid) to the data frame:

boldnessdata7$asrid <- asrp$asrid[match(boldnessdata7$birdID, asrp$birdid)]
head(boldnessdata7$asrid)

# check whether the match is working as desired:
boldnessdata7$check <- asrp$birdid[match(boldnessdata7$birdID, asrp$birdid)]

boldnessdata7$check2 <- ifelse(boldnessdata7$check==boldnessdata7$birdID, 0, 1)
summary(boldnessdata7$check2)

# is every bird accounted for?
boldnessdata7$birdID[which(is.na(boldnessdata7$check2))]

# excellent!

# whilst I can get the asrid's from asrp, I cannot get complete dam
# information from here because some dams are not assigned. Therefore
# I get the dam information from my pedigree with inputted dams, asrp1x:

boldnessdata7$asrdam <- asrp1x$dam[match(boldnessdata7$asrid, asrp1x$id)]
summary(boldnessdata7$asrdam)
which(is.na(boldnessdata7$asrdam))

######################################################################
### set up factors etc for analysis
######################################################################



# set factors:

boldnessdata7$factorbirdID <- as.factor(boldnessdata7$birdID)
boldnessdata7$factorpartnerID <- as.factor(boldnessdata7$partnerID)
boldnessdata7$factoryear <- as.factor(boldnessdata7$year)
boldnessdata7$factorentryorder <- as.factor(boldnessdata7$birdorderentry)


### different from before: factorcomp is now the June 2015
# classification not the December 2014 classification
boldnessdata7$factorcomp <- as.factor(boldnessdata7$compJune2015)
boldnessdata7$factorcomp123 <- as.factor(boldnessdata7$comp.1human.2paper.3other)
boldnessdata7$factormethod <- as.factor(boldnessdata7$method)

# and scale the continuous covariates:
boldnessdata7$zmeanoffspring <- scale(boldnessdata7$meanoffspring, 
                                      scale=T, center=T)
summary(boldnessdata7$zmeanoffspring)

boldnessdata7$zmeanexperience <- scale(boldnessdata7$meanexperience, 
                                       scale=T, center=T)
summary(boldnessdata7$zmeanexperience)

boldnessdata7$zimc.offspring <- scale(boldnessdata7$imc.offspring, 
                                      scale=T, center=T)
summary(boldnessdata7$zimc.offspring)

boldnessdata7$zimc.experience <- scale(boldnessdata7$imc.experience, 
                                       scale=T, center=T)
summary(boldnessdata7$zimc.experience)


boldnessdata7$factorasrid <- as.factor(boldnessdata7$asrid)
boldnessdata7$factorasrdam <- as.factor(boldnessdata7$asrdam)


head(boldnessdata7)

# make a zero variable for the models when I don't have any fixed and random effects:
boldnessdata7$zero <- 0


#####################################################
# data sets including compromised data
#####################################################


# Make a data set with compromised data included, 
# split by sex:
fbold6 <- subset(boldnessdata7, boldnessdata7$sex=="F")
mbold6 <- subset(boldnessdata7, boldnessdata7$sex=="M")



# make data sets per year to calculate repeatability:

subset2011 <- subset(boldnessdata7, boldnessdata7$year==2011)
table(boldnessdata7$year)
table(subset2011$year)

subset2012 <- subset(boldnessdata7, boldnessdata7$year==2012)
table(boldnessdata7$year)
table(subset2012$year)

subset2013 <- subset(boldnessdata7, boldnessdata7$year==2013)
table(boldnessdata7$year)
table(subset2013$year)

subset2014 <- subset(boldnessdata7, boldnessdata7$year==2014)
table(boldnessdata7$year)
table(subset2014$year)


# data set to calculate repeatability across years from the first test within a year only
firsttestcomp <- subset(boldnessdata7, 
                        boldnessdata7$testorder==1)
summary(firsttestcomp)

#####################################################
# data sets minus compromised data
#####################################################


# data set minus compromised data. This is data where something happened in the video
# that leads us to mark it as compromised, and this is also data where the video was
# taken on the wrong day and therefore the nestlings are not nine days old:

boldnessdatamincomp <- subset(boldnessdata7, boldnessdata7$compJune2015==0)
summary(boldnessdatamincomp$compJune2015)

boldnessdatamincomp2 <- subset(boldnessdatamincomp, 
                               boldnessdatamincomp$takenonwrongday==0)

head(boldnessdatamincomp2)
summary(boldnessdatamincomp2$takenonwrongday)


# per sex

fboldmincomp2 <- subset(boldnessdatamincomp2, boldnessdatamincomp2$sex=="F")
summary(fboldmincomp2)
table(table(fboldmincomp2$asrid))

mboldmincomp2 <- subset(boldnessdatamincomp2, boldnessdatamincomp2$sex=="M")
summary(mboldmincomp2)
table(table(mboldmincomp2$asrid))

# per year

subset2011mincomp2 <- subset(boldnessdatamincomp2, boldnessdatamincomp2$year==2011)
table(boldnessdatamincomp2$year)
table(subset2011mincomp2$year)

# first test per year


firsttestmincomp2 <- subset(boldnessdatamincomp2, 
                            boldnessdatamincomp2$testorder==1)
summary(firsttestmincomp2)




#####################################################
# data sets for analysing fixed effects
#####################################################

# with no missing data.
# including compromised data:

# but, now I need two new data sets to investigate the
# fixed effects: one that contains compromised data and
# one that doesn't, and I need to re-scale the covariates
# for each:

boldnessdata8 <- boldnessdata7[-which(is.na(boldnessdata7$partnerboldshy)),]
summary(boldnessdata8)
head(boldnessdata8)
which(is.na(boldnessdata8$partnerboldshy))
table(boldnessdata8$takenonwrongday)
length(boldnessdata7[,1])
length(boldnessdata8[,1])
length(which(is.na(boldnessdata7$partnerboldshy)))

# great. Looks like the right data has been removed and 
# the data set is ok afterwards. Re-scale continuous covariates:
boldnessdata8$zmeanoffspring <- scale(boldnessdata8$meanoffspring, 
                                      scale=T, center=T)
summary(boldnessdata8$zmeanoffspring)

boldnessdata8$zmeanexperience <- scale(boldnessdata8$meanexperience, 
                                       scale=T, center=T)
summary(boldnessdata8$zmeanexperience)

boldnessdata8$zimc.offspring <- scale(boldnessdata8$imc.offspring, 
                                      scale=T, center=T)
summary(boldnessdata8$zimc.offspring)

boldnessdata8$zimc.experience <- scale(boldnessdata8$imc.experience, 
                                       scale=T, center=T)
summary(boldnessdata8$zimc.experience)






# exlcuding compromised data:


# I need to exclude the cases where partner bold/shy is 
# not known again:


boldnessdatamincomp.partner <- boldnessdatamincomp[-which(is.na(boldnessdatamincomp$partnerboldshy)),]
summary(boldnessdatamincomp.partner)
head(boldnessdatamincomp.partner)
which(is.na(boldnessdatamincomp.partner$partnerboldshy))
table(boldnessdatamincomp.partner$takenonwrongday)
length(boldnessdatamincomp[,1])
length(boldnessdatamincomp.partner[,1])
length(which(is.na(boldnessdatamincomp$partnerboldshy)))

# great. Looks like the right data has been removed and 
# the data set is ok afterwards. Re-scale continuous covariates:
boldnessdatamincomp.partner$zmeanoffspring <- scale(boldnessdatamincomp.partner$meanoffspring, 
                                                    scale=T, center=T)
summary(boldnessdatamincomp.partner$zmeanoffspring)

boldnessdatamincomp.partner$zmeanexperience <- scale(boldnessdatamincomp.partner$meanexperience, 
                                                     scale=T, center=T)
summary(boldnessdatamincomp.partner$zmeanexperience)

boldnessdatamincomp.partner$zimc.offspring <- scale(boldnessdatamincomp.partner$imc.offspring, 
                                                    scale=T, center=T)
summary(boldnessdatamincomp.partner$zimc.offspring)

boldnessdatamincomp.partner$zimc.experience <- scale(boldnessdatamincomp.partner$imc.experience, 
                                                     scale=T, center=T)
summary(boldnessdatamincomp.partner$zimc.experience)

#####################################################
#####################################################


# new data set needed for the fixed effects analysis, one with no missing data:

boldnessdatamincomp2.partner <- boldnessdatamincomp2[-which(
  is.na(boldnessdatamincomp2$partnerboldshy)),]

summary(boldnessdatamincomp2.partner)

# Re-scale continuous covariates:
boldnessdatamincomp2.partner$zmeanoffspring <- scale(boldnessdatamincomp2.partner$meanoffspring, 
                                                     scale=T, center=T)
summary(boldnessdatamincomp2.partner$zmeanoffspring)

boldnessdatamincomp2.partner$zmeanexperience <- scale(boldnessdatamincomp2.partner$meanexperience, 
                                                      scale=T, center=T)
summary(boldnessdatamincomp2.partner$zmeanexperience)

boldnessdatamincomp2.partner$zimc.offspring <- scale(boldnessdatamincomp2.partner$imc.offspring, 
                                                     scale=T, center=T)
summary(boldnessdatamincomp2.partner$zimc.offspring)

boldnessdatamincomp2.partner$zimc.experience <- scale(boldnessdatamincomp2.partner$imc.experience, 
                                                      scale=T, center=T)
summary(boldnessdatamincomp2.partner$zimc.experience)










#####################################################
# bivariate data set excluding compromised data:
#####################################################

# match partner behaviours to videos:


head(mboldmincomp)
head(fboldmincomp)
head(videoinfo)

# check the matches will be unique:
table(table(fboldmincomp$videoname))
table(table(mboldmincomp$videoname))
table(table(videoinfo$DVDNumber))
str(fboldmincomp$videoname)
str(mboldmincomp$videoname)
str(videoinfo$DVDNumber)

# now add the latencies:
videoinfo$flatency <- fboldmincomp2$logentrylatency[match
                                                    (videoinfo$DVDNumber,fboldmincomp2$videoname)]
# and give a check:
videoinfo$fvid <- as.character(fboldmincomp2$videoname[match
                                                       (videoinfo$DVDNumber,fboldmincomp2$videoname)])
head(videoinfo)

### males ###
videoinfo$mlatency <- mboldmincomp2$logentrylatency[match
                                                    (videoinfo$DVDNumber,mboldmincomp2$videoname)]
# and give a check:
videoinfo$mvid <- as.character(mboldmincomp2$videoname[match
                                                       (videoinfo$DVDNumber,mboldmincomp2$videoname)])
head(videoinfo)
tail(videoinfo)

boldnessdatamincomp[which(boldnessdatamincomp$videoname=="VN0831"),]
tail(videoinfo)


### plot it
videoinfo$factormum <- as.factor(videoinfo$SocialMumID.2009to11frommyDBparents)
videoinfo$factordad <- as.factor(videoinfo$SocialDadID.2009to11frommyDBparents)

# knock out the videos that aren't personality videos:
videoinfo$NAfinder <- paste(videoinfo$fvid, videoinfo$mvid, sep="")
head(videoinfo$NAfinder)

videoinfo2 <- subset(videoinfo, videoinfo$NAfinder!="NANA")
head(videoinfo2)
summary(videoinfo2)
videoinfo2$NAfinder

xyplot(flatency~mlatency, groups=factordad, 
       data=videoinfo2, type="b")

xyplot(flatency~mlatency, groups=factormum, 
       data=videoinfo2, type="b")

# these suggest the partner does have a huge role to play.

# but their role is in the specific behaviour and might
# not be evident just from their ID.




videoinfo2$asrmum <- asrp$asrid[match(
  videoinfo2$SocialMumID.2009to11frommyDBparents, asrp$birdid)]

summary(videoinfo2$asrmum)
summary(videoinfo2$SocialMumID.2009to11frommyDBparents)

videoinfo2$asrdad <- asrp$asrid[match(
  videoinfo2$SocialDadID.2009to11frommyDBparents, asrp$birdid)]

summary(videoinfo2$asrdad)
summary(videoinfo2$SocialDadID.2009to11frommyDBparents)


videoinfo2$factorasrdad <- as.factor(videoinfo2$asrdad)
videoinfo2$factorasrmum <- as.factor(videoinfo2$asrmum)




#####################################################
# bivariate data set including compromised data:
#####################################################




videoinfo$flatencywithcomp <- fbold6$logentrylatency[match(videoinfo$DVDNumber,
                                                           fbold6$videoname)]
videoinfo$mlatencywithcomp <- mbold6$logentrylatency[match(videoinfo$DVDNumber,
                                                           mbold6$videoname)]

head(videoinfo)

videoinfo$fvidcomp <- fbold6$compJune2015[match(videoinfo$DVDNumber,
                                                fbold6$videoname)]
videoinfo$mvidcomp <- mbold6$compJune2015[match(videoinfo$DVDNumber,
                                                mbold6$videoname)]
head(videoinfo)
tail(videoinfo)
table(videoinfo$fvidcomp)
table(videoinfo$mvidcomp)

videoinfo$NAfindercomp <- paste(videoinfo$fvidcomp, videoinfo$mvidcomp, sep="")

videoinfocomp <- subset(videoinfo, videoinfo$NAfindercomp!="NANA")

summary(videoinfocomp)
head(videoinfocomp)


# factor for whether a test was compromised

videoinfocomp$fcomp <- fbold6$factorcomp[match(videoinfocomp$DVDNumber,
                                               fbold6$videoname)]
table(fbold6$factorcomp)
table(videoinfocomp$fcomp)

videoinfocomp$mcomp <- mbold6$factorcomp[match(videoinfocomp$DVDNumber,
                                               mbold6$videoname)]
table(mbold6$factorcomp)
table(videoinfocomp$mcomp)




videoinfocomp$factorfcomp <- as.factor(videoinfocomp$fvidcomp)
videoinfocomp$factormcomp <- as.factor(videoinfocomp$mvidcomp)
table(videoinfocomp$factorfcomp)
table(videoinfocomp$factormcomp)
