# Isabel Winney
# 26th May 2015

# this is an analysis of the paper test ('boldness' test)
# that includes data from 2011-2014 and one test from 2009.

# updated 9th June 2015 - I checked through and compromised 
# extra data from 2012 so now using the june version of the
# data rather than the May version

# load in desirable packages:

# Video VK0311 the data record for the second (non-focal) 
# male has been removed.

# video information is kept in file 
# May2015-2011-14VideoInfofromchecked2015MarchDB.txt but there
# are several amendments between this file and the information in
# the database; see column changedfromDB for details

# clear the workspace
rm(list=ls())

# set working directory
setwd("./masterdatasheets")


library(pedantics)
library(asreml)
library(lattice)
library(car)
library(lme4)
pin <- dget("C:/Users/Issie/SkyDrive/PhD/ASRemlR/pin.R")
source("C:/Users/Issie/SkyDrive/RFunctions/assigningtestorder-20150218.R")


# read in boldness data
# this is the old May version
#boldnessdata <- read.table("V9PMASTER-foranalysisMay2015.txt", header=T)
# this is the new June version
boldnessdata <- read.table("V9PMASTER-foranalysisJune2015.txt", 
                           header=T, na.strings="NA")

# take a look
View(boldnessdata)
summary(boldnessdata)
str(boldnessdata)

# two records for every video?
table(table(boldnessdata$videoname))

# There is a video VK0362a and VK0362. The paper fell
# down during the first attempt at the video, but not 
# before the male had entered the nest box (and created
# a data point). This first attempt at the video is
# labelled a.

# to this, I need to add the id of the male and the
# female, and work out the focal bird. Load in the
# video information:
videoinfo <- read.table("May2015-2011-14VideoInfofromchecked2015MarchDB.txt", 
                        header=T, na.strings="NA")
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



# I had to change the names of some nestboxes that
# had spaces in the names by removing the spaces: 
# Recycling 2.

# VK0089 - it is suspected the piece of paper fell down 
# before this test began, or was never put up, so this
# video is excluded from the data set.

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

# so, make my dependent variable: the latency from touching
# to entering the nestbox
boldnessdata1$entrylatency <- boldnessdata1$inbox-boldnessdata1$nestboxcontact
summary(boldnessdata1$entrylatency)
str(boldnessdata1$entrylatency)
hist(boldnessdata1$entrylatency, breaks=seq(0,60,0.5))



# make a birdyear variable:
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

# first, I want to remove the non-paper related test,
# and birds that have no bird ID

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
# nestboxcontact) because we have no idea what their boldness
# should have been because they did not turn up. Note that
# arrivallatency is not used here for whether a bird turned
# up because this variable is the time that any bird that subsequently
# went to the nest box arrived in the video but the value depends
# on how zoomed in the video is.

summary(boldnessdata2$nestboxcontact)
length(boldnessdata2$nestboxcontact)

birdsinvideo <- subset(boldnessdata2, boldnessdata2$nestboxcontact!="NA")

summary(birdsinvideo$nestboxcontact)
length(birdsinvideo$nestboxcontact)

# split by sex (since individuals will be matched to partners,
# but males and females vary)

femalesinvideo <- subset(birdsinvideo, birdsinvideo$sex=="F")
malesinvideo <- subset(birdsinvideo, birdsinvideo$sex=="M")

table(birdsinvideo$sex)
table(femalesinvideo$sex)
table(malesinvideo$sex)
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
# order within year:

boldnessdataordered <- testorder("birdyear", "juliandate", boldnessdata3)
boldnessdataordered[1:10,]
tail(boldnessdataordered)
table(boldnessdataordered$testorder)
# one bird was tested six times in a year...

xyplot(entrylatency~testorder, groups=birdyear, 
       data=boldnessdataordered, type="b")
xyplot(entrylatency~testorder, groups=birdID, 
       data=boldnessdataordered, type="b")
# some birds are up, most birds are down with repeated
# testing.

# at this point I should separate the within-individual
# from the between-individual effect to show that habituation
# occurs despite the unbalanced experimental design (variable
# numbers of repeat samples per individual)

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
# the rule of two or more broods with hatch dates <25 days
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





# for this investigation, I should just look at data
# with the NAs removed for entry latency:
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
head(videoinfo2)

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


# adding the pedigree:

boldnessdata7$asrid <- asrp$asrid[match(boldnessdata7$birdID, asrp$birdid)]
head(boldnessdata7$asrid)

boldnessdata7$check <- asrp$birdid[match(boldnessdata7$birdID, asrp$birdid)]

boldnessdata7$check2 <- ifelse(boldnessdata7$check==boldnessdata7$birdID, 0, 1)
summary(boldnessdata7$check2)

boldnessdata7$birdID[which(is.na(boldnessdata7$check2))]

# excellent!

# whilst I can get the asrid's from asrp, I cannot get complete dam
# information from here because some dams are not assigned. Therefore
# I get the dam information from my pedigree with inputted dams, asrp1x

boldnessdata7$asrdam <- asrp1x$dam[match(boldnessdata7$asrid, asrp1x$id)]
summary(boldnessdata7$asrdam)
which(is.na(boldnessdata7$asrdam))




# more importantly for model fitting, are the residuals
# kind of normal?

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


head(boldnessdata7)


boldnessdata7$zero <- 0

boldmod1 <- asreml(fixed=logentrylatency~zero,
                   random=~factorbirdID,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit")

plot(boldmod1)
# not bad!
summary(boldmod1)$varcomp
summary(boldmod1, all=T)$coef.fi
pin(boldmod1, r2withcomp~V1/(V1+V2))
length(boldnessdata7[,1])

# how significant is this?
boldmod0 <- asreml(fixed=logentrylatency~zero,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit")

1-pchisq(2*(boldmod1$loglik-boldmod0$loglik),1)



boldmod1 <- asreml(fixed=logentrylatency~1,
                   random=~factoryear/factorbirdID,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit")
plot(boldmod1)
summary(boldmod1)$varcomp
# this suggests that birds are a little more repeatable within
# years. What if we include an overall bird id term?

boldmod1 <- asreml(fixed=logentrylatency~1,
                   random=~factoryear/factorbirdID+factorbirdID,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit")
plot(boldmod1)
summary(boldmod1)$varcomp
# overall bird id trumps within year bird id. This suggests
# some consistency across years. However, this consistency
# is not super high.

# what if I look within years, to see if some years aren't
# ok?
boldmod1 <- asreml(fixed=logentrylatency~1,
                   random=~diag(factoryear):factorbirdID,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit")

summary(boldmod1)
# 2011 and 2014 are the most repeatable years.
# 2012 and 2013 are dubious - but see compromised removed
# versions.
# not enough data for an us() structure - too few data
# points from 2009

# are birds that enter first more repeatable?
boldmod1 <- asreml(fixed=logentrylatency~factorentryorder,
                   random=~us(factorentryorder):factorbirdID,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit")

summary(boldmod1)
# birds are similarly repeatable. There is
# little relationship between behaviour when entering first
# or second

# but I'm not sure I trust the results of these types of
# model where the residual covariance is the same between
# both traits.
# is boldness data more variable when it is compromised?
boldmod1 <- asreml(fixed=logentrylatency~1,
                   random=~us(factorcomp):factorbirdID,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit")

summary(boldmod1)
# errr.... birds seem to be more consistent when compromised... I guess
# because it takes them a long time to go in!!!
# also, covariance between compromised and non-compromised is weak (though
# maybe this is a lack of data...)

# what if compromised is also a fixed effect?
boldmod1 <- asreml(fixed=logentrylatency~1+factorcomp,
                   random=~us(factorcomp):factorbirdID,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit")

summary(boldmod1)
summary(boldmod1, all=T)$coef.fi
# now compromised indiviudlas have much lower bird ID
# variance --> can account for the difference in variance
# with the fixed effect.
# compromised tests are much more shy...

# perhaps this will be shown by comparing human compromised to other
# types of compromised:

boldmod1 <- asreml(fixed=logentrylatency~1,
                   random=~us(factorcomp123):factorbirdID,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit",
                   maxiter=1000)


summary(boldmod1)
# so without the fixed effect there is a positive
# correlation between normal and human-affected behaviour but
# nothing of importance between normal and paper-affected
# behaviour. Is this because of the lack of fixed effect?


boldmod1 <- asreml(fixed=logentrylatency~1+factorcomp123,
                   random=~us(factorcomp123):factorbirdID,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit",
                   maxiter=1000)

summary(boldmod1)
summary(boldmod1, all=T)$coef.fi


# no. Perhaps it is because of test order?
boldmod1 <- asreml(fixed=logentrylatency~1+factorcomp123+testorder,
                   random=~us(factorcomp123):factorbirdID,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit",
                   maxiter=1000)
# not so stable: factorbirdID!factorcomp123.2:1 changed by 1.9% 
# on the last iteration
summary(boldmod1)
summary(boldmod1, all=T)$coef.fi
# this doesn't have much benefit...

# what happens if I knock out compromised data?
# in May I did this:
#boldnessdatamincomp <- subset(boldnessdata7, boldnessdata7$rewatchcompDec2014==0)
# in June I do this:
boldnessdatamincomp <- subset(boldnessdata7, boldnessdata7$compJune2015==0)
summary(boldnessdatamincomp$compJune2015)

# is the transformation still ok?
boxcox((entrylatency+0.5)~1+(1|birdID), 
       data=boldnessdatamincomp, 
       lambda=seq(-0.5,0.50,0.001))
powerTransform((entrylatency+0.5)~1+(1|birdID), 
               data=boldnessdatamincomp)



# and RESCALE the continuous covariates:
boldnessdatamincomp$zmeanoffspring <- scale(boldnessdatamincomp$meanoffspring, 
                                      scale=T, center=T)
summary(boldnessdatamincomp$zmeanoffspring)

boldnessdatamincomp$zmeanexperience <- scale(boldnessdatamincomp$meanexperience, 
                                       scale=T, center=T)
summary(boldnessdatamincomp$zmeanexperience)

boldnessdatamincomp$zimc.offspring <- scale(boldnessdatamincomp$imc.offspring, 
                                      scale=T, center=T)
summary(boldnessdatamincomp$zimc.offspring)

boldnessdatamincomp$zimc.experience <- scale(boldnessdatamincomp$imc.experience, 
                                       scale=T, center=T)
summary(boldnessdatamincomp$zimc.experience)






boldmod1 <- asreml(fixed=logentrylatency~1,
                   random=~diag(factoryear):factorbirdID,
                   data=boldnessdatamincomp,
                   na.method.X="omit",
                   na.method.Y="omit")
plot(boldmod1)
summary(boldmod1) 
# struggling far more with within-year repeatability now.
# 2012 is particularly low, perhaps because of sample
# sizes
table(boldnessdatamincomp$year)

# is there overall repeatability?
boldmod1 <- asreml(fixed=logentrylatency~1,
                   random=~diag(factoryear):factorbirdID+factorbirdID,
                   data=boldnessdatamincomp,
                   na.method.X="omit",
                   na.method.Y="omit")
plot(boldmod1)
# unstable, parameter changed on last iteration
summary(boldmod1) # 2009 and 2013 boundary
# maybe I can't mix the two - birdID takes the variance from
# the other years so the birdID variance for these other years can't 
# be calculated.

boldmod1 <- asreml(fixed=logentrylatency~1,
                   random=~factorbirdID,
                   data=boldnessdatamincomp,
                   na.method.X="omit",
                   na.method.Y="omit")

summary(boldmod1, all=T)$coef.fi
summary(boldmod1)$varcomp
pin(boldmod1, r2withcomp~V1/(V1+V2))
length(boldnessdatamincomp[,1])

# how significant is this?
boldmod0 <- asreml(fixed=logentrylatency~zero,
                   data=boldnessdatamincomp,
                   na.method.X="omit",
                   na.method.Y="omit")

1-pchisq(2*(boldmod1$loglik-boldmod0$loglik),1)
length(boldnessdatamincomp[,1])
length(unique(boldnessdatamincomp$birdID))
table(table(boldnessdatamincomp$birdID))






boldmod1 <- asreml(fixed=logentrylatency~1,
                   random=~us(factorentryorder):factorbirdID,
                   data=boldnessdatamincomp,
                   na.method.X="omit",
                   na.method.Y="omit")

summary(boldmod1)
# now birds that enter second are more repeatable, though
# they are also more variable...


# are individuals repeatable across years i.e. taking 
# the first test only?
firsttestnocomp <- subset(boldnessdatamincomp, 
                          boldnessdatamincomp$testorder==1)
summary(firsttestnocomp)


boldmod1 <- asreml(fixed=logentrylatency~1,
                   random=~factorbirdID,
                   data=firsttestnocomp,
                   na.method.X="omit",
                   na.method.Y="omit")

summary(boldmod1, all=T)$coef.fi
summary(boldmod1)$varcomp
pin(boldmod1, r2withcomp~V1/(V1+V2))

# how significant is this?
boldmod0 <- asreml(fixed=logentrylatency~zero,
                   data=firsttestnocomp,
                   na.method.X="omit",
                   na.method.Y="omit")

1-pchisq(2*(boldmod1$loglik-boldmod0$loglik),1)
# not significant
length(firsttestnocomp[,1])
length(unique(firsttestnocomp$birdID))
table(table(firsttestnocomp$birdID))






# before, I checked what the difference was between males
# and females. I would like to do this again:

boldmod1 <- asreml(fixed=logentrylatency~1,
                   random=~diag(sex):factorbirdID,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit")

summary(boldmod1)
# :o females are more repeatable here! but not by much

# but what if the compromised data is knocked out?

boldmod1 <- asreml(fixed=logentrylatency~1,
                   random=~diag(sex):factorbirdID,
                   data=boldnessdatamincomp,
                   na.method.X="omit",
                   na.method.Y="omit")

plot(boldmod1)
summary(boldmod1)
# males are more repeatable than females once the compromised
# data is removed, but females have some repeatability :) 
# this is in contrast to the previous data set that had no 2014 data.

table(boldnessdatamincomp$year)
table(boldnessdatamincomp$sex)
table(boldnessdatamincomp$year, boldnessdatamincomp$sex)
# though this includes birds that don't enter.
# this also suggests the lower repeatability in 2012-13
# is not from a lack of data.


# but then again, males and females may have different
# residual variance for boldness. can I either do the
# sexes in separate models, or make this in to a multivariate
# model?

# separate the sexes
fboldmincomp <- subset(boldnessdatamincomp, boldnessdatamincomp$sex=="F")
summary(fboldmincomp)
table(table(fboldmincomp$birdID))

mboldmincomp <- subset(boldnessdatamincomp, boldnessdatamincomp$sex=="M")
summary(mboldmincomp)
table(table(mboldmincomp$birdID))



boldmodf <- asreml(fixed=logentrylatency~1,
                   random=~factorbirdID,
                   data=fboldmincomp,
                   na.method.X="omit",
                   na.method.Y="omit")

plot(boldmodf)
# some bumps at the tails of the qq plot. But females
# are repeatable!!!
summary(boldmodf, all=T)$coef.fi
summary(boldmodf)$varcomp
pin(boldmodf, r2withcomp~V1/(V1+V2))
length(fboldmincomp[,1])

# how significant is this?
boldmodf0 <- asreml(fixed=logentrylatency~zero,
                   data=fboldmincomp,
                   na.method.X="omit",
                   na.method.Y="omit")

1-pchisq(2*(boldmodf$loglik-boldmodf0$loglik),1)
length(fboldmincomp[,1])
length(unique(fboldmincomp$birdID))
table(table(fboldmincomp$birdID))





boldmodm <- asreml(fixed=logentrylatency~1,
                   random=~factorbirdID,
                   data=mboldmincomp,
                   na.method.X="omit",
                   na.method.Y="omit")

plot(boldmodm)
# residual versus fitted not so good here,
# perhaps due to the reduced sample size

summary(boldmodm, all=T)$coef.fi
summary(boldmodm)$varcomp
pin(boldmodm, r2withcomp~V1/(V1+V2))
length(mboldmincomp[,1])

# how significant is this?
boldmodm0 <- asreml(fixed=logentrylatency~zero,
                    data=mboldmincomp,
                    na.method.X="omit",
                    na.method.Y="omit")

1-pchisq(2*(boldmodm$loglik-boldmodm0$loglik),1)
# interestingly, borderline not significant
length(mboldmincomp[,1])
length(unique(mboldmincomp$birdID))
table(table(mboldmincomp$birdID))


# so now, females are significantly repeatable
# but males are borderline. Strikes me to be 
# down to sample size...






# repeatability now has a lower value in males compared
# to females but is not so significant in males,
# which could be attributable to the sample size.
length(mboldmincomp[,1])
length(fboldmincomp[,1])
# I have 88 more data points on females than on males...

# so 2014 made a big difference...
fmin2014 <- subset(fboldmincomp, fboldmincomp$year!=2014)
summary(fmin2014)

boldmodf <- asreml(fixed=logentrylatency~1,
                   random=~factorbirdID,
                   data=fmin2014,
                   na.method.X="omit",
                   na.method.Y="omit")

plot(boldmodf)
summary(boldmodf)
# what a weird weird change from adding another year 
# of data...


# well.
# Is this because I have more detail per female or something?
length(fmin2014$birdID)
length(unique(fmin2014$birdID))

length(fboldmincomp$birdID)
length(unique(fboldmincomp$birdID))
# marginally more tests per female. Doesn't explain the change

xyplot(logentrylatency~juliandate, groups=birdID, data=fboldmincomp, type="l")
xyplot(logentrylatency~juliandate, groups=birdID, data=fmin2014, type="l")

xyplot(logentrylatency~juliandate, groups=birdyear, data=fboldmincomp, type="l")
xyplot(logentrylatency~juliandate, groups=birdyear, data=fmin2014, type="l")
# ah. The birdyear plot shows some individuals at the top end of the
# scale that are new and consistent, and some new individuals in the
# middle etc that are consistent too.

# in which year are the individuals that become more shy?
xyplot(logentrylatency~juliandate, groups=year, data=fboldmincomp, type="p")
# well it's a bit hard to tell like that
xyplot(logentrylatency~juliandate|factoryear, groups=birdyear, data=fboldmincomp, type="l")
# 2012! I wonder why they all became shy. That's so weird.

# 9th June. After knocking out more data points in 2012, the
# data looks very sparse in that year...

###################################################################

# what happens if I fit partner ID as a second random effect?
# this is suboptimal because the partner now occurs as a data
# point and as a random effect. A bivariate would be best.

boldmod1 <- asreml(fixed=logentrylatency~1,
                   random=~factorbirdID+factorpartnerID,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit")

plot(boldmod1)
summary(boldmod1)

# this suggests little influence of the partner. Is this
# because of a lack of samples? Because the compromised 
# data is in? What?

boldmod1 <- asreml(fixed=logentrylatency~1,
                   random=~factorbirdID+factorpartnerID,
                   data=boldnessdatamincomp,
                   na.method.X="omit",
                   na.method.Y="omit")

plot(boldmod1)
summary(boldmod1)
# now there definitely isn't enough power to detect the
# partner effect. Is it also small because this is the
# partner's ID rather than what behaviour they showed
# on the day? Let's look at the plot of partner behaviour
# against each other.

# To do this, I need to match partner behaviours together
# and I can do this on the videoinfo data frame and the
# single-sex data frames:

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

# so, let's try a bivariate model!

bivariate1 <- asreml(fixed=cbind(flatency, mlatency)~trait,
                     random=~diag(trait):factordad+
                       diag(trait):factormum,
                     rcov=~units:us(trait,init=c(0.1,0.1,0.1)),
                     data=videoinfo2,
                     maxiter=5000,
                     na.method.Y="include",
                     na.method.X="include")
# should be diag() for the random effects because males
# and females are not tested for the opposite sex latency


summary(bivariate1)$varcomp
# This suggests there is a strong phenotypic
# correlation between the pair (see residual covariance)
# but the impact of the partner's ID can't be calculated.

# how interesting: in this model where the females lend
# extra support to the calculation of residual variance,
# males show higher repeatability than females!

# I can do a single-sex one:

bivariate2 <- asreml(fixed=cbind(flatency, mlatency)~trait,
                     random=~us(trait):factordad,
                     rcov=~units:us(trait,init=c(0.1,0.1,0.1)),
                     data=videoinfo2,
                     maxiter=5000,
                     na.method.Y="include",
                     na.method.X="include")

summary(bivariate2)$varcomp
# this shows little effect of the male on the female.


bivariate3 <- asreml(fixed=cbind(flatency, mlatency)~trait,
                     random=~us(trait):factormum,
                     rcov=~units:us(trait,init=c(0.1,0.1,0.1)),
                     data=videoinfo2,
                     maxiter=5000,
                     na.method.Y="include",
                     na.method.X="include")

summary(bivariate3)$varcomp
# and here a good effect of the female ID for male behaviour
# and a small amount of covariance

# what happens if I account for number of offspring?

bivariate4 <- asreml(fixed=cbind(flatency, mlatency)~trait+OffspringNo,
                     random=~diag(trait):factordad+
                       diag(trait):factormum,
                     rcov=~units:us(trait,init=c(0.1,0.1,0.1)),
                     data=videoinfo2,
                     maxiter=5000,
                     na.method.Y="include",
                     na.method.X="include")

summary(bivariate4)$varcomp
summary(bivariate4, all=T)$coef.fi

# fledglings have an effect but male now influences
# female latency!

# what if I us us() one at a time on male and female
# ID? Will  I learn something from this?

bivariate5 <- asreml(fixed=cbind(flatency, mlatency)~trait+OffspringNo,
                     random=~us(trait, init=c(1,0.1,1)):factordad+
                       diag(trait):factormum,
                     rcov=~units:us(trait,init=c(0.1,0.1,0.1)),
                     data=videoinfo2,
                     maxiter=5000,
                     na.method.Y="include",
                     na.method.X="include")

summary(bivariate5)$varcomp
summary(bivariate5, all=T)$coef.fi

bivariate6 <- asreml(fixed=cbind(flatency, mlatency)~trait+OffspringNo,
                     random=~us(trait, init=c(1,0.1,1)):factormum+
                       diag(trait):factordad,
                     rcov=~units:us(trait,init=c(0.1,0.1,0.1)),
                     data=videoinfo2,
                     maxiter=5000,
                     na.method.Y="include",
                     na.method.X="include")

summary(bivariate6)$varcomp
summary(bivariate6, all=T)$coef.fi


# interesting that the female one is stronger (5 estimates not 4). 
# I would bet it is because of the increased number of identified females 
# combined with higher sample size for female boldness.

# but it still shows a lack of within-individual phenotypic
# covariance but the strong between-individual phenotypic 
# covariance. I would bet I just haven't got the power for
# the within-individual parts. I would be best off presenting
# the bivariate with both bird IDs and diag() as the bird ID
# variance structure. So, model bivariate1.


# the power may be low if I have low numbers of individuals
# that switch partners.
# To know how many birds switch partners, I will use the
# video info but knock out pairs where I don't know the
# partner ID and partner behaviour (since these wouldn't
# contribute to the model):

summary(videoinfo2$flatency)

partnerpairs1 <- subset(videoinfo2, videoinfo2$flatency!="NA")

summary(partnerpairs1$flatency)
summary(partnerpairs1$mlatency)

partnerpairs2 <- subset(partnerpairs1, partnerpairs1$mlatency!="NA")

summary(partnerpairs2$SocialMumID.2009to11frommyDBparents)
summary(partnerpairs2$SocialDadID.2009to11frommyDBparents)

table(partnerpairs2$fvidcomp)
table(partnerpairs2$mvidcomp)

# great! But my general sample size must be a bit low...
length(partnerpairs2[,1])
# yeah...

length(unique(partnerpairs2$SocialMumID.2009to11frommyDBparents))
length(unique(partnerpairs2$SocialDadID.2009to11frommyDBparents))

partnerpairs2$pairID <- paste(partnerpairs2$SocialMumID.2009to11frommyDBparents,
                              partnerpairs2$SocialDadID.2009to11frommyDBparents,
                              sep="")
summary(partnerpairs2$pairID)
head(partnerpairs2$pairID)
length(unique(partnerpairs2$pairID))
sort(unique(partnerpairs2$pairID))
# 9 females change partner, only once

# flip for males
partnerpairs2$pairID2 <- paste(partnerpairs2$SocialDadID.2009to11frommyDBparents,
                              partnerpairs2$SocialMumID.2009to11frommyDBparents,
                              sep="")

sort(unique(partnerpairs2$pairID2))


pairs1 <- data.frame("pairID"=unique(partnerpairs2$pairID))
pairs1
pairs1$femaleID <- partnerpairs2$SocialMumID.2009to11frommyDBparents[
  match(pairs1$pairID, partnerpairs2$pairID)]
pairs1$maleID <- partnerpairs2$SocialDadID.2009to11frommyDBparents[
  match(pairs1$pairID, partnerpairs2$pairID)]

head(pairs1)
# excellent :)
table(table(pairs1$femaleID))
table(table(pairs1$maleID))


# would the power be sufficient if I included the compromised
# data? Perhaps the compromised data would also change the 
# result though.

fbold6 <- subset(boldnessdata7, boldnessdata7$sex=="F")
mbold6 <- subset(boldnessdata7, boldnessdata7$sex=="M")

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


partnerpairs3 <- subset(videoinfocomp, videoinfocomp$flatencywithcomp!="NA")

summary(partnerpairs3$flatencywithcomp)
summary(partnerpairs3$mlatencywithcomp)

partnerpairs4 <- subset(partnerpairs3, partnerpairs3$mlatencywithcomp!="NA")

summary(partnerpairs4$mlatencywithcomp)

summary(partnerpairs4$SocialMumID.2009to11frommyDBparents)
summary(partnerpairs4$SocialDadID.2009to11frommyDBparents)


length(partnerpairs4[,1])
# much better sample size!

length(unique(partnerpairs4$SocialMumID.2009to11frommyDBparents))
length(unique(partnerpairs4$SocialDadID.2009to11frommyDBparents))

partnerpairs4$pairID <- paste(partnerpairs4$SocialMumID.2009to11frommyDBparents,
                              partnerpairs4$SocialDadID.2009to11frommyDBparents,
                              sep="")
summary(partnerpairs4$pairID)
head(partnerpairs4$pairID)
length(unique(partnerpairs4$pairID))
sort(unique(partnerpairs4$pairID))
# 9 females change partner, only once

# how can I do this automatically?

pairs2 <- data.frame("pairID"=unique(partnerpairs4$pairID))
pairs2
pairs2$femaleID <- partnerpairs4$SocialMumID.2009to11frommyDBparents[
  match(pairs2$pairID, partnerpairs4$pairID)]
pairs2$maleID <- partnerpairs4$SocialDadID.2009to11frommyDBparents[
  match(pairs2$pairID, partnerpairs4$pairID)]

head(pairs2)
# excellent :)
table(table(pairs2$femaleID))
table(table(pairs2$maleID))


# flip for males
partnerpairs4$pairID2 <- paste(partnerpairs4$SocialDadID.2009to11frommyDBparents,
                               partnerpairs4$SocialMumID.2009to11frommyDBparents,
                               sep="")

sort(unique(partnerpairs4$pairID2))




####
# whether the focal bird enters first or second:
videoinfocomp$fentryorder <- fbold6$factorentryorder[match(videoinfocomp$DVDNumber,
                                               fbold6$videoname)]
table(fbold6$factorentryorder)
table(videoinfocomp$fentryorder)

videoinfocomp$mentryorder <- mbold6$factorentryorder[match(videoinfocomp$DVDNumber,
                                               mbold6$videoname)]
table(mbold6$factorentryorder)
table(videoinfocomp$mentryorder)
####




# now let's see if there is still a correlation:
xyplot(flatencywithcomp~mlatencywithcomp, groups=factordad, 
       data=videoinfocomp, type="b")

xyplot(flatencywithcomp~mlatencywithcomp, groups=factormum, 
       data=videoinfocomp, type="b")



# will this data work with a bivariate analysis?
bivariate7 <- asreml(fixed=cbind(flatencywithcomp, mlatencywithcomp)~trait,
                     random=~us(trait, init=c(1,0.1,1)):factormum+
                       us(trait, init=c(1,0.1,1)):factordad,
                     rcov=~units:us(trait,init=c(0.1,0.1,0.1)),
                     data=videoinfocomp,
                     maxiter=5000,
                     na.method.Y="include",
                     na.method.X="include")
# trait:factordad!trait.mlatencywithcomp:flatencywithcomp changed by
# 1.32%
summary(bivariate7)$varcomp
summary(bivariate7, all=T)$coef.fi

bivariate7.1 <- asreml(fixed=cbind(flatencywithcomp, mlatencywithcomp)~trait,
                     random=~diag(trait):factormum+
                       us(trait, init=c(1,0.1,1)):factordad,
                     rcov=~units:us(trait,init=c(0.1,0.1,0.1)),
                     data=videoinfocomp,
                     maxiter=5000,
                     na.method.Y="include",
                     na.method.X="include")

summary(bivariate7.1)$varcomp
1-pchisq(2*(bivariate7$loglik-bivariate7.1$loglik),1)

# so so so. This says female ID influences male behaviour.
# Male behaviour seems not to be repeatable in this case.
# But this might be what we expect when males are more likely
# to a) enter second and b) be compromised.

table(boldnessdata7$rewatchcompDec2014, boldnessdata7$sex)
table(boldnessdata7$birdorderentry, boldnessdata7$sex)

# so perhaps we should trust more the original analysis 
# without the compromised data
summary(bivariate6)$varcomp


# or perhaps I can include the compromised factor as fixed?
videoinfocomp$fcomp <- fbold6$factorcomp[match(videoinfocomp$DVDNumber,
                                              fbold6$videoname)]
table(fbold6$factorcomp)
table(videoinfocomp$fcomp)

videoinfocomp$mcomp <- mbold6$factorcomp[match(videoinfocomp$DVDNumber,
                                               mbold6$videoname)]
table(mbold6$factorcomp)
table(videoinfocomp$mcomp)


bivariate8 <- asreml(fixed=cbind(flatencywithcomp, mlatencywithcomp)~trait+
                       trait:mvidcomp+trait:fvidcomp,
                     random=~us(trait, init=c(1,0.1,1)):factormum+
                       us(trait, init=c(1,0.1,1)):factordad,
                     rcov=~units:us(trait,init=c(0.1,0.1,0.1)),
                     data=videoinfocomp,
                     maxiter=5000,
                     na.method.Y="include",
                     na.method.X="include")
# trait:factordad!trait.mlatencywithcomp:flatencywithcomp changed by 2.17%
summary(bivariate8)$varcomp
summary(bivariate8, all=T)$coef.fi
# both males and females show the large negative (shy) effect
# when a test is compromised, and are acutally bolder when their
# partner is not compromised!

# is the female covariance significant?
bivariate9 <- asreml(fixed=cbind(flatencywithcomp, mlatencywithcomp)~trait+
                       trait:mcomp+trait:fcomp,
                     random=~diag(trait):factormum+
                       us(trait, init=c(1,0.1,1)):factordad,
                     rcov=~units:us(trait,init=c(0.1,0.1,0.1)),
                     data=videoinfocomp,
                     maxiter=5000,
                     na.method.Y="include",
                     na.method.X="include")

summary(bivariate9)$varcomp
1-pchisq(2*(bivariate8$loglik-bivariate9$loglik),1)
# no. Never mind.

# what if male covariance is under constraint?

bivariate10 <- asreml(fixed=cbind(flatencywithcomp, mlatencywithcomp)~trait+
                       trait:mcomp+trait:fcomp,
                     random=~us(trait, init=c(1,0.1,1)):factormum+
                       diag(trait):factordad,
                     rcov=~units:us(trait,init=c(0.1,0.1,0.1)),
                     data=videoinfocomp,
                     maxiter=5000,
                     na.method.Y="include",
                     na.method.X="include")

bivariate11 <- asreml(fixed=cbind(flatencywithcomp, mlatencywithcomp)~trait+
                        trait:mcomp+trait:fcomp,
                      random=~diag(trait):factormum+
                        diag(trait):factordad,
                      rcov=~units:us(trait,init=c(0.1,0.1,0.1)),
                      data=videoinfocomp,
                      maxiter=5000,
                      na.method.Y="include",
                      na.method.X="include")

1-pchisq(2*(bivariate10$loglik-bivariate11$loglik),1)
# still no. So never mind.

# what about the phenotypic covariance?
bivariate12 <- asreml(fixed=cbind(flatencywithcomp, mlatencywithcomp)~trait+
                        trait:mcomp+trait:fcomp,
                      random=~diag(trait):factormum+
                        diag(trait):factordad,
                      rcov=~units:diag(trait),
                      data=videoinfocomp,
                      maxiter=5000,
                      na.method.Y="include",
                      na.method.X="include")

summary(bivariate12)$varcomp
1-pchisq(2*(bivariate11$loglik-bivariate12$loglik),1)
# yes that is very important. So I know there is a 
# phenotypic correlation but I probably lack the power
# to draw it apart.


# but to know if the entering first or second or if the
# compromised data might affect the results, I should 
# analyse in the single sex data sets whether it makes
# a difference:

univariate1 <- asreml(fixed=logentrylatency~1,
                     random=~factorbirdID:us(factorentryorder),
                     data=mbold6,
                     maxiter=5000,
                     na.method.Y="omit",
                     na.method.X="omit")

summary(univariate1)$varcomp
# males entering second are less consistent.

univariate2 <- asreml(fixed=logentrylatency~1,
                      random=~factorbirdID:us(factorcomp),
                      data=mbold6,
                      maxiter=5000,
                      na.method.Y="omit",
                      na.method.X="omit")

summary(univariate2)$varcomp
# males that are compromised are more consistent
table(mbold6$factorcomp, mbold6$factorentryorder)
# males entering second are far more likely to be 
# compromised, which goes against the higher repeatability
# for compromised individuals but lower for second entries.


# females?
univariate3 <- asreml(fixed=logentrylatency~1,
                      random=~factorbirdID:us(factorentryorder),
                      data=fbold6,
                      maxiter=5000,
                      na.method.Y="omit",
                      na.method.X="omit")

summary(univariate3)$varcomp
# females seem consistent in both cases...

univariate4 <- asreml(fixed=logentrylatency~1,
                      random=~factorbirdID:us(factorcomp),
                      data=fbold6,
                      maxiter=5000,
                      na.method.Y="omit",
                      na.method.X="omit")

summary(univariate4)$varcomp
# like males, females are more repeatable when compromised
# but, unlike males, they are more repeatable when they
# enter second. They also show positive covariance between
# the different situations: entering first or second, or
# being compromised or not compromised, suggesting the behaviour
# for females may show consistency across contexts (how
# different is this conclusion to before when I had 2009-13 data!!!)


# what about the different types of compromised?

univariate5 <- asreml(fixed=logentrylatency~1,
                      random=~factorbirdID:us(factorcomp123),
                      data=fbold6,
                      maxiter=5000,
                      na.method.Y="omit",
                      na.method.X="omit")
summary(univariate5)

# this analysis might be easier with males (where more
# are compromised...)

# 25th June 2015: I added factorcomp123 as a fixed
# effect
univariate6 <- asreml(fixed=logentrylatency~factorcomp123,
                      random=~factorbirdID:us(factorcomp123),
                      data=mbold6,
                      maxiter=5000,
                      na.method.Y="omit",
                      na.method.X="omit")
summary(univariate6)$varcomp

# so there is a negative relationship between
# 0 and 2. This could be driven by a few males who
# are more experienced when they are tested a second time...

univariate7 <- asreml(fixed=logentrylatency~testorder+factorcomp123,
                      random=~factorbirdID:us(factorcomp123, init=c(1,0.1,1,0.1,0.1,1)),
                      data=mbold6,
                      maxiter=5000,
                      na.method.Y="omit",
                      na.method.X="omit")
# factorbirdID:factorcomp123!factorcomp123.2:0 changed by 2.26%
summary(univariate7)$varcomp
summary(univariate7, all=T)$coef.fi

# huh? This suggests not much effect of more testing...


xyplot(logentrylatency~testorder, data=mbold6,
       groups=factorbirdID, type="b")

# huh. No. There doesn't seem to be a relationship
# with more testing...
# what are females like?

xyplot(logentrylatency~testorder, data=fbold6,
       groups=factorbirdID, type="b")
# maybe they get bolder... maybe there is sex-specific
# plasticity? Is this why there was low female repeatability
# in the previous 2009-13 data set?

xyplot(logentrylatency~testorder|factoryear, data=fbold6,
       groups=factorbirdID, type="b")
# 2012 looks awful

xyplot(logentrylatency~testorder|factoryear, data=mbold6,
       groups=factorbirdID, type="b")
# quite possibly... some males show plasticity
# and others don't.

# that 2012 data looks really weird...

xyplot(logentrylatency~testorder|factoryear, data=mbold6,
       groups=factorcomp, type="b")

xyplot(logentrylatency~testorder|factoryear, data=fbold6,
       groups=factorcomp, type="b")

# herm. The compromised tests are generally very shy...

xyplot(logentrylatency~testorder|factoryear, data=mbold6,
       groups=factorcomp123, type="b")

xyplot(logentrylatency~testorder|factoryear, data=fbold6,
       groups=factorcomp123, type="b")

# compromised data looks very different in both data sets.
# but that might be because I see the joined up lines not
# the real data

xyplot(logentrylatency~testorder|factorcomp, data=mbold6,
       groups=birdyear, type="b")

xyplot(logentrylatency~testorder|factorcomp, data=fbold6,
       groups=birdyear, type="b")

# yes, the compromised stuff is pretty shy...
# but could I correct for that as a fixed effect?
# birds that aren't compromised show weird relationships
# between 1st and 2nd tests (2012!)



###
# I wonder. Given how different 2012 is, are there
# tests that should be compromised but are not???
# Only solution is to check the data.

# these make files boldness-testorder-comp-year-June2015
# and boldness-testorder-nocomp-year-June2015

xyplot(logentrylatency~testorder|factoryear, data=boldnessdata7,
       groups=factorbirdID, type="b",
       scales=list(cex=2), pch=16, lwd=2, cex.axis=2)

xyplot(logentrylatency~testorder|factoryear, data=boldnessdatamincomp,
       groups=factorbirdID, type="b",
       scales=list(cex=2), pch=16, lwd=2, cex.axis=2)

###################################################################


# perhaps it is worth asking whether this entry latency and
# the time to arrive are correlated:
plot(boldnessdata1$nestboxcontact, boldnessdata1$entrylatency)
# no! Only in the sense that the longer it takes a bird to
# arrive, the less time it has to enter, so there are no birds
# with late arrival and long entry latency.


# I'd like to look at Julian date and year as interesting
# variables by which individuals may vary both intercept
# and slope of boldness



# take a look at the data:

xyplot(entrylatency~juliandate|year, 
       data=boldnessdata1, groups=birdID, type="l")

xyplot(entrylatency~juliandate, 
       data=boldnessdata1, groups=birdID, type="l")

plot(factor(boldnessdata1$birdID),boldnessdata1$entrylatency)
plot(factor(boldnessdata1$birdID),boldnessdata1$entrylatency+0.5, log="y")

# so the data is very skewed

# quick diversion in to what happens if I just use time in:
plot(factor(boldnessdata1$birdID),boldnessdata1$inbox, log="y")
# pretty similar



# what might affect my data?

# whether the nestbox has a perch:

plot(factor(boldnessdata1$perchpresent.1forpresent),
        boldnessdata1$entrylatency)
# it seems the birds will be a bit quicker without a perch,
# though other factors may be involved e.g. perches more
# likely to be knocked off as a season progresses --> perch
# loss and experience correlate. Though this isn't going to
# be a big factor - most perches were missing before I came.

table(boldnessdata1$perchpresent.1forpresent, boldnessdata1$year)
# well, there aren't many cases without a perch, and they
# are mostly in 2011. An analysis of whether there is or is
# not a perch is conflated with the year of testing instead.

# what about the antenna on the front?

plot(boldnessdata1$antenna.yforpresent,
     boldnessdata1$entrylatency, notch=T)
# not much difference...


# who enters first?
table(boldnessdata1$sex, boldnessdata1$birdorderentry)
# Huh? Entry orders of 1.5 and 3???

# sex?
plot(boldnessdata1$sex, boldnessdata1$entrylatency, notch=T)



##########
# quick diversion using the arena data set made for 
# chapter 4 in file 'makingdataset-mar2015.R'
# is there covariance between days in an overall
# sense (rather than the matching 1st to 1st test
# sense as I have done in the script?)
# load the data set from this R file.
ar$factorage <- as.factor(ar$age)
ar$logtot <- log(ar$total+0.5)
ar$factorbirdID <- as.factor(ar$birdid)

arcurious1 <- asreml(fixed=logtot~1,
                   random=~us(factorage):factorbirdID,
                   data=ar,
                   na.method.X="omit",
                   na.method.Y="omit")

plot(arcurious1)
summary(arcurious1)
# coooool :)





################################################################


# now, let's run an animal model :)

boldnessdata7$factorasrid <- as.factor(boldnessdata7$asrid)
boldnessdata7$factorasrdam <- as.factor(boldnessdata7$asrdam)

boldanimal1 <- asreml(fixed=logentrylatency~1,
                   random=~ide(factorasrid, var=T, init=1) +
                     ped(factorasrid, var=T, init=1) + 
                     ide(factorasrdam, var=T, init=1),
                   ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit")

plot(boldanimal1)
# I never like that the fit against residuals plot shows very
# low fits have lower residuals and higher fits have higher
# residuals. I put it down to a mix of the transformation and
# the lack of information from fixed effects.
summary(boldanimal1)
# wow! Is that what I think it is?! Is that heritability???!!!

# I can't get too excited. There are no fixed effects for one
# thing, and for another the compromised data is in here.

boldnessdatamincomp <- subset(boldnessdata7, boldnessdata7$compJune2015==0)

boldanimal2 <- asreml(fixed=logentrylatency~1,
                      random=~ide(factorasrid, var=T, init=1) +
                        ped(factorasrid, var=T, init=1) + 
                        ide(factorasrdam, var=T, init=1),
                      ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                      data=boldnessdatamincomp,
                      na.method.X="omit",
                      na.method.Y="omit")

plot(boldanimal2) 
#this shows a bit more skew in residuals vs fitted
# and the qq shows this is because the first and
# last quantiles are a bit above and below the expected
# residual values, though the 1:1 relationship seems
# to work well between residuals of -1.5 and +1.
summary(boldanimal2)



# what happens when I incorporate a pedigree in to
# my bivariate models?

videoinfo2 <- subset(videoinfo, videoinfo$NAfinder!="NANA")
head(videoinfo2)
summary(videoinfo2)

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



# covariances are not estimated. This is because there is
# no within-individual covariance - a male is not measured
# for female latency and vice versa!
bivariateanimal1 <- asreml(fixed=cbind(flatency, mlatency)~trait,
                     random=~diag(trait):ide(factorasrdad)+
                       diag(trait):ide(factorasrmum) +
                       diag(trait):ped(factorasrdad)+
                       diag(trait):ped(factorasrmum),
                     rcov=~units:us(trait,init=c(0.1,0.1,0.1)),
                     ginverse=list(factorasrdad=asrpinv, factorasrmum=asrpinv),
                     data=videoinfo2,
                     maxiter=500,
                     na.method.Y="include",
                     na.method.X="include")

summary(bivariateanimal1)
# so. The male and female have phenotypic repeatability for their
# own traits. For genetic stuff, females have detectable Va for 
# boldness, and males have a detectable Va(indirect) on female
# boldness but not on their own. However, four effects here went
# to boundary.

# what if the compromised data is in there?

bivariateanimal2 <- asreml(fixed=cbind(flatencywithcomp, mlatencywithcomp)~trait,
                           random=~diag(trait):ide(factorasrdad)+
                             diag(trait):ide(factorasrmum) +
                             diag(trait):ped(factorasrdad)+
                             diag(trait):ped(factorasrmum),
                           rcov=~units:us(trait,init=c(0.1,0.1,0.1)),
                           ginverse=list(factorasrdad=asrpinv, factorasrmum=asrpinv),
                           data=videoinfo2,
                           maxiter=500,
                           na.method.Y="include",
                           na.method.X="include")

summary(bivariateanimal2)
