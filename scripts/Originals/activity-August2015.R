# July 2015
# Isabel Winney

# this is the neatened version of the arena
# data analysis from before handing in.

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


### data ###

ar <- read.table("AR-jan2015.txt", header=T)
broods <- read.table("brood-parent-july2014-myDBparents.txt",
                     header=T, na.strings="NA")

ar$cohort <- as.POSIXlt(as.Date(ar$date, "%d/%m/%Y"))$year+1900

ar$loc1 <- factor(ifelse(ar$where=="Sparrowbench", 4, ar$where))

ar$loc2 <- factor(ifelse(ar$loc1==2, 3, ar$loc))

ar$noise1 <- factor(ifelse(ar$noise=="c", "3", ar$noise))

ar$loc3 <- as.factor(ifelse(ar$loc1==4, "Shed", "LM"))

ar$noise2 <- factor(ifelse(ar$noise=="c", "2", ar$noise))


# how should the total number of squares entered
# by the nestling be transformed:


boxcox((total-0.5)~1+(1|asrid), 
       data=ar, 
       lambda=seq(-0.5,0.50,0.001))

# coming closer to 0.2.


powerTransform((total-0.5)~1+(1|asrid), 
               data=ar)


# so, there may be multiple transformations that
# I could apply. However, I could also consider
# that I want the above to match with my previous
# analyses for boldness and exploration, which
# both took a log transformation ok enough. 
# See what the diagnostics plots and vifs etc
# are like for this data before coming to a fixed
# conclusion.



ar$logtot <- log(ar$total-0.5)
summary(ar$logtot)
# some birds have no total for activity. These
# are likely to be tests where the test was aborted.

ar$noiseasr <- as.factor(ifelse(ar$noise=="y", "0", ar$noise))

ar$loc3asr <- as.factor(ifelse(ar$loc3=="Shed", "0", ar$loc3))

ar$releaserasr <- as.factor(ifelse(ar$releaser=="IW", "0", ar$releaser))

ar$cohortasr <- as.factor(ifelse(ar$cohort=="2013", "0", ar$cohort))

ar$asrid <- asrp$asrid[match(ar$birdid, asrp$birdid)]
summary(ar$asrid)
summary(ar$birdid)

broods$mumcheck <- asrp$birdid[match(broods$SocialMumID, asrp$birdid)]
broods$asrmum <- asrp$asrid[match(broods$SocialMumID, asrp$birdid)]
broods$asrdad <- asrp$birdid[match(broods$SocialDadID, asrp$birdid)]

broods$check <- ifelse(broods$SocialMumID==broods$mumcheck, 0, 1)
summary(broods$check)

ar$socmum <- broods$asrmum[match(ar$social, broods$BroodRef)]

ar$geneticmum <- asrp$asrdam[match(ar$asrid, asrp$asrid)]
summary(ar$geneticmum)

ar$noise2 <- factor(ifelse(ar$noise2=="3", "0", ar$noise2))


mord <- as.data.frame(tapply(ar$wdorder, ar$asrid, FUN=mean, na.rm=T))
nam <- as.data.frame(rownames(mord))
mord2 <- cbind(nam, mord)
names(mord2) <- c("asrid", "mwdord")
ar <- merge(ar, mord2, all.x=T)
head(ar)
ar$indwdord <- ar$wdorder - ar$mwdord



# thinking about which square a nestling ends its
# activity on. I don't want this as a random effect
# because it could mess with my estimates of the
# other random effects. What I can do instead is
# express it as a probability of ending on a given
# square:
table(ar$end)
plot(factor(ar$end), ar$total, log="y")
# so the total activity isn't much different
# but there is a big difference in how many
# end in a given place. The nestlings prefer
# to go to the direction where the camera is
# (on a tripod above numbers 1 and 16).

xyplot(total~end, groups=asrid, data=ar, type="b")
# maybe it's not going to make much difference, but
# I want to look in to it more.

e1 <- table(ar$end)
e2 <- data.frame(e1/length(ar$end))

ar$endprob <- e2$Freq[match(ar$end, e2$Var1)]
summary(ar$endprob)

xyplot(total~endprob, groups=asrid, data=ar, type="b")
# not too bad but we'll see.
# now the decision: should this be mean centred
# or done as an overall correlation? To be honest,
# I am not interested in the overall vs individual
# effect, just in controlling for it. Since inference
# is not important for this fixed effect, I think
# include it as a normal single continuous fixed
# effect


# factors
ar$cohort <- as.factor(ar$cohort)
ar$comp <- as.factor(ar$comp)
ar$natal <- as.factor(ar$natal)
ar$social <- as.factor(ar$social)
ar$date <- as.factor(ar$date)
ar$start.1 <- as.factor(ar$start.1)
ar$end <- as.factor(ar$end)
ar$cf <- as.factor(ar$cf)
ar$releaser <- as.factor(ar$releaser)
ar$where <- as.factor(ar$where)
ar$noise <- as.factor(ar$noise)
ar$birdid <- as.factor(ar$birdid)
ar$timedif <- as.numeric(ar$timedif)
ar$age <- as.factor(ar$age)
ar$factorasrid <- as.factor(ar$asrid)
ar$factorasrdam <- as.factor(ar$geneticmum)
ar$socmum <- as.factor(ar$socmum)


# create ar1, where the individuals with no measure
# for total squares entered are excluded
ar1 <- subset(ar, ar$total!="NA")


# is the transformation still ok?

boxcox((total-0.5)~1+(1|asrid), 
       data=ar1, 
       lambda=seq(-0.5,0.50,0.001))

# coming closer to 0.2.


powerTransform((total-0.5)~1+(1|asrid), 
               data=ar1)

# ok enough. Not the most perfect, but it 
# will do.


# check all my categorical effects have the right
# base level for ASReml-R

# and use the base level with the most samples

table(ar1$cf)
table(ar1$cohort)
table(ar1$releaser)
table(ar1$comp)
table(ar1$where)
table(ar1$loc1)
table(ar1$loc3)
table(ar1$noise)
table(ar1$noise2)
table(ar1$age)

# these need changing:
ar1$cf <- as.factor(ifelse(ar1$cf=="y", "0", ar1$cf))
table(ar1$cf)

ar1$cohort <- as.factor(ifelse(ar1$cohort=="2013", "0", ar1$cohort))
table(ar1$cohort)

ar1$releaser <- as.factor(ifelse(ar1$releaser=="IW", "0", ar1$releaser))
table(ar1$releaser)

ar1$where <- as.factor(ifelse(ar1$where=="Stevebench", "0", ar1$where))
table(ar1$where)

ar1$loc1 <- as.factor(ifelse(ar1$loc1=="4", "0", ar1$loc1))
table(ar1$loc1)

ar1$loc3 <- as.factor(ifelse(ar1$loc3=="Shed", "0", ar1$loc3))
table(ar1$loc3)

ar1$noise <- as.factor(ifelse(ar1$noise=="y", "0", ar1$noise))
table(ar1$noise)

ar1$age <- as.factor(ifelse(ar1$age=="12", "0", ar1$age))
table(ar1$age)


ar1$zero <- 0


summary(ar1$total)
summary(ar1$endprob)



# is the desired transformation the same on day
# 12 and 10?

table(ar$age)
table(ar1$age)

ar10 <- subset(ar1, ar1$age==1)
ar12 <- subset(ar1, ar1$age==0)

boxcox((total-0.5)~1+(1|asrid), 
       data=ar10, 
       lambda=seq(-0.5,0.50,0.001))

boxcox((total-0.5)~1+(1|asrid), 
       data=ar12, 
       lambda=seq(-0.5,0.50,0.001))

# similar. The day 10 data is heading towards
# 0.25

hist(ar10$total, breaks=seq(-1,90,1))

hist(ar12$total, breaks=seq(-1,90,1))
# there is very weird zero inflation in the day
# 12 data, suggesting that behaviour may be different
# on this day compared to the other days. Nestlings
# appear to have a freeze or move response.


# an important point to consider now is whether to
# present an analysis of all data or just data from
# 12 day old nestlings. The thing is, the nestling
# hierarchy work is only based on day 12 nestlings
# meaning it would be useful to have only day 12
# heritability somewhere. Perhaps I should continue
# with the all data analysis, because I know the
# results don't really differ whether I use all
# or some data (see below), and there are 25 more
# individuals if I use all data:

length(unique(ar12$asrid))
length(unique(ar1$asrid))

# but for 25 individuals, does it really matter?


# two individuals were tested an extra time by
# accident:

table(ar1$wd1to3, ar1$age)


# I should probably remove these to make things
# easier with the fixed effects model, but there
# is no worry yet. Plus, both occurred on day 10.
# So if I chose to do the day 12 model, they would
# not be there anyway.



###########################################################

###########################################################

# ok, great. I can look at my intercept-only repeatabilities
# now.


# all data:
asrarena0 <- asreml(fixed=logtot~zero,
                    data=ar1,
                    na.method.X="omit",
                    na.method.Y="omit",
                    maxiter=20)

asrarena0.1 <- asreml(fixed=logtot~zero,
                    random=~ide(factorasrid, var=T, init=1),
                    data=ar1,
                    ginverse=list(factorasrid=asrpinv),
                    na.method.X="omit",
                    na.method.Y="omit",
                    maxiter=20)

plot(asrarena0.1)
# wiggle wiggle. Not perfect. But it
# is transformed count data, so I know
# it will not be perfect.
summary(asrarena0)$varcomp
summary(asrarena0.1)$varcomp
summary(asrarena0.1, all=T)$coef.fi
1-pchisq(2*(asrarena0.1$loglik-asrarena0$loglik),1)

# the repeatability:
pin(asrarena0.1, r2alldata~V1/(V1+V2))

# sample sizes:
length(unique(ar1$asrid))
table(table(ar1$asrid))
length(ar1$asrid)
table(ar1$where)
table(ar1$noise)
summary(ar1$total)
sd(ar1$total)

# how many ones and twos and threes and at each age?
table(ar1$total)
table(ar1$total, ar1$age)


# how many nestlings reached the edge of the arena
# (i.e. had a score of 1 for edge) and therefore might 
# have just reached the edge before we started counting:
table(ar1$edge)
# but this number is not accurate because I did not record
# distance to the edge until part way through 2012

table(ar1$age)
table(ar1$asrid[which(ar1$age==3)])

# were any dams filled in?
summary(damplus)
which(ar1$asrdam>6072)
# no


# how much data is compromised:
table(ar1$comp)

###

# what is repeatability like within days:


# within day 10:

arenaday10 <- asreml(fixed=logtot~zero,
                      random=~ide(factorasrid, var=T, init=1),
                      data=ar10,
                      ginverse=list(factorasrid=asrpinv),
                      na.method.X="omit",
                      na.method.Y="omit",
                      maxiter=20)

plot(arenaday10)
# quite poor residuals. It doesn't help that
# this is not the ideal transformation for 
# this data: the box-cox recommended  a lambda
# nearer 0.25 than 0.
summary(arenaday10)$varcomp
summary(arenaday10, all=T)$coef.fi


# significance:
arenaday10.2 <- asreml(fixed=logtot~zero,
                     data=ar10,
                     na.method.X="omit",
                     na.method.Y="omit",
                     maxiter=20)


1-pchisq(2*(arenaday10$loglik-arenaday10.2$loglik),1)

# the repeatability:
pin(arenaday10, r2day10~V1/(V1+V2))

# sample sizes:
length(unique(ar10$asrid))
table(table(ar10$asrid))
length(ar10$asrid)



# what is heritability like on day 10?

arenaday10.animal <- asreml(fixed=logtot~1,
                     random=~ide(factorasrid, var=T, init=1)+
                       ped(factorasrid, var=T, init=1)+
                       ide(factorasrdam, var=T, init=1)+
                       social,
                     data=ar10,
                     ginverse=list(factorasrid=asrpinv,
                                   factorasrdam=asrpinv),
                     na.method.X="omit",
                     na.method.Y="omit",
                     maxiter=20)

plot(arenaday10.animal)
summary(arenaday10.animal, all=T)$coef.fi
summary(arenaday10.animal)$varcomp
# very small excess variance in this data set.

# how big are h2, m2, s2?
pin(arenaday10.animal, pe2arena~V1/(V1+V2+V3+V4+V5))
pin(arenaday10.animal, h2arena~V2/(V1+V2+V3+V4+V5))
pin(arenaday10.animal, m2arena~V3/(V1+V2+V3+V4+V5))
pin(arenaday10.animal, s2arena~V4/(V1+V2+V3+V4+V5))


# as proportions of personality?

pin(arenaday10.animal, pe2arena~V1/(V1+V2+V3+V4))
pin(arenaday10.animal, h2arena~V2/(V1+V2+V3+V4))
pin(arenaday10.animal, m2arena~V3/(V1+V2+V3+V4))
pin(arenaday10.animal, s2arena~V4/(V1+V2+V3+V4))


# significance of each term

# Vpe

arenaday10.animal2 <- asreml(fixed=logtot~1,
                            random=~ped(factorasrid, var=T, init=1)+
                              ide(factorasrdam, var=T, init=1)+
                              social,
                            data=ar10,
                            ginverse=list(factorasrid=asrpinv,
                                          factorasrdam=asrpinv),
                            na.method.X="omit",
                            na.method.Y="omit",
                            maxiter=20)


1-pchisq(2*(arenaday10.animal$loglik-arenaday10.animal2$loglik),1)


# Va

arenaday10.animal3 <- asreml(fixed=logtot~1,
                            random=~ide(factorasrid, var=T, init=1)+
                              ide(factorasrdam, var=T, init=1)+
                              social,
                            data=ar10,
                            ginverse=list(factorasrid=asrpinv,
                                          factorasrdam=asrpinv),
                            na.method.X="omit",
                            na.method.Y="omit",
                            maxiter=20)


1-pchisq(2*(arenaday10.animal$loglik-arenaday10.animal3$loglik),1)


# Vm

arenaday10.animal4 <- asreml(fixed=logtot~1,
                            random=~ide(factorasrid, var=T, init=1)+
                              ped(factorasrid, var=T, init=1)+
                              social,
                            data=ar10,
                            ginverse=list(factorasrid=asrpinv),
                            na.method.X="omit",
                            na.method.Y="omit",
                            maxiter=20)



1-pchisq(2*(arenaday10.animal$loglik-arenaday10.animal4$loglik),1)


# social brood

arenaday10.animal5 <- asreml(fixed=logtot~1,
                            random=~ide(factorasrid, var=T, init=1)+
                              ped(factorasrid, var=T, init=1)+
                              ide(factorasrdam, var=T, init=1),
                            data=ar10,
                            ginverse=list(factorasrid=asrpinv,
                                          factorasrdam=asrpinv),
                            na.method.X="omit",
                            na.method.Y="omit",
                            maxiter=20)


1-pchisq(2*(arenaday10.animal$loglik-arenaday10.animal5$loglik),1)


##########################################################


# within day 12:

arenaday12 <- asreml(fixed=logtot~zero,
                     random=~ide(factorasrid, var=T, init=1),
                     data=ar12,
                     ginverse=list(factorasrid=asrpinv),
                     na.method.X="omit",
                     na.method.Y="omit",
                     maxiter=20)

plot(arenaday12)
# wow. I hadn't expected such good residuals
# with so many zeroes. However, the fitted against
# residuals plot looks like the variance decreases
# at higher values. This seems to be due to the 
# higher number of zeroes at the left side of the
# plot determining the scale at which I look at
# the plot, and also the lower numbers of measures
# at the high end

summary(arenaday12, all=T)$coef.fi
summary(arenaday12)$varcomp

# R2
pin(arenaday12, r2day12~V1/(V1+V2))

# significance:

arenaday12.2 <- asreml(fixed=logtot~zero,
                     data=ar12,
                     na.method.X="omit",
                     na.method.Y="omit",
                     maxiter=20)

1-pchisq(2*(arenaday12$loglik-arenaday12.2$loglik),1)

# sample sizes
length(ar12$asrid)
length(unique(ar12$asrid))
table(table(ar12$asrid))


# what is heritability like on day 12?

arenaday12.animal <- asreml(fixed=logtot~1,
                            random=~ide(factorasrid, var=T, init=1)+
                              ped(factorasrid, var=T, init=1)+
                              ide(factorasrdam, var=T, init=1)+
                              social,
                            data=ar12,
                            ginverse=list(factorasrid=asrpinv,
                                          factorasrdam=asrpinv),
                            na.method.X="omit",
                            na.method.Y="omit",
                            maxiter=20)

plot(arenaday12.animal)
summary(arenaday12.animal, all=T)$coef.fi
summary(arenaday12.animal)$varcomp
# Wow. Look at that social brood effect. That's serious!


# how big are h2, m2, s2?
pin(arenaday12.animal, pe2arena~V1/(V1+V2+V3+V4+V5))
pin(arenaday12.animal, h2arena~V2/(V1+V2+V3+V4+V5))
pin(arenaday12.animal, m2arena~V3/(V1+V2+V3+V4+V5))
pin(arenaday12.animal, s2arena~V4/(V1+V2+V3+V4+V5))

# as proportions of personality?

pin(arenaday12.animal, pe2arena~V1/(V1+V2+V3+V4))
pin(arenaday12.animal, h2arena~V2/(V1+V2+V3+V4))
pin(arenaday12.animal, m2arena~V3/(V1+V2+V3+V4))
pin(arenaday12.animal, s2arena~V4/(V1+V2+V3+V4))


# significances:

# Vpe

arenaday12.animal.2 <- asreml(fixed=logtot~1,
                            random=~ped(factorasrid, var=T, init=1)+
                              ide(factorasrdam, var=T, init=1)+
                              social,
                            data=ar12,
                            ginverse=list(factorasrid=asrpinv,
                                          factorasrdam=asrpinv),
                            na.method.X="omit",
                            na.method.Y="omit",
                            maxiter=20)

1 - pchisq(2*(arenaday12.animal$loglik-arenaday12.animal.2$loglik),1)


# Va

arenaday12.animal.3 <- asreml(fixed=logtot~1,
                            random=~ide(factorasrid, var=T, init=1)+
                              ide(factorasrdam, var=T, init=1)+
                              social,
                            data=ar12,
                            ginverse=list(factorasrid=asrpinv,
                                          factorasrdam=asrpinv),
                            na.method.X="omit",
                            na.method.Y="omit",
                            maxiter=20)

1 - pchisq(2*(arenaday12.animal$loglik-arenaday12.animal.3$loglik),1)


# Vm is boundary --> no need to test

# Social

arenaday12.animal.4 <- asreml(fixed=logtot~1,
                            random=~ide(factorasrid, var=T, init=1)+
                              ped(factorasrid, var=T, init=1)+
                              ide(factorasrdam, var=T, init=1),
                            data=ar12,
                            ginverse=list(factorasrid=asrpinv,
                                          factorasrdam=asrpinv),
                            na.method.X="omit",
                            na.method.Y="omit",
                            maxiter=20)

1 - pchisq(2*(arenaday12.animal$loglik-arenaday12.animal.4$loglik),1)










######################################################
xyplot(total~wd1to3, groups=asrid, data=ar1, type="b")
xyplot(logtot~wd1to3, groups=asrid, data=ar1, type="b")
# well, some individuals stay high, some individuals
# seem to vary over tests.

####################################################

# what is repeatability like across days:

# take the first measure per day:

ar.firstmeasure <- subset(ar1, ar1$wd1to3==1)

arenafirstmeasure <- asreml(fixed=logtot~zero,
                     random=~ide(factorasrid, var=T, init=1),
                     data=ar.firstmeasure,
                     ginverse=list(factorasrid=asrpinv),
                     na.method.X="omit",
                     na.method.Y="omit",
                     maxiter=20)

plot(arenafirstmeasure)
# that's the worst residuals plot of the lot.
# Look at the wiggle in that QQ! Look at the
# trend of the fitted vs residuals! It's because
# of zero inflation, I would bet
table(ar.firstmeasure$total)
# 91 one values (minimum possible value).

# well. Not a lot to be done.
summary(arenafirstmeasure, all=T)$coef.fi
summary(arenafirstmeasure)$varcomp
# repeatable, not by loads but it is.

pin(arenafirstmeasure, r2first~V1/(V1+V2))


# significance
arenafirstmeasure.2 <- asreml(fixed=logtot~zero,
                     data=ar.firstmeasure,
                     na.method.X="omit",
                     na.method.Y="omit",
                     maxiter=20)

1-pchisq(2*(arenafirstmeasure$loglik-arenafirstmeasure.2$loglik),1)

# sample sizes

length(unique(ar.firstmeasure$asrid))
table(table(ar.firstmeasure$asrid))
length(ar.firstmeasure$asrid)

####################################################
# animal model, all data:
arena.animal1 <- asreml(fixed=logtot~1,
                            random=~ide(factorasrid, var=T, init=1)+
                              ped(factorasrid, var=T, init=1)+
                              ide(factorasrdam, var=T, init=1)+
                              social,
                            data=ar1,
                            ginverse=list(factorasrid=asrpinv,
                                          factorasrdam=asrpinv),
                            na.method.X="omit",
                            na.method.Y="omit",
                            maxiter=20)

plot(arena.animal1)
# much better on the zeroes. Which is a relief...
summary(arena.animal1, all=T)$coef.fi
summary(arena.animal1)$varcomp


# how big are h2, m2, s2?
pin(arena.animal1, pe2arena~V1/(V1+V2+V3+V4+V5))
pin(arena.animal1, h2arena~V2/(V1+V2+V3+V4+V5))
pin(arena.animal1, m2arena~V3/(V1+V2+V3+V4+V5))
pin(arena.animal1, s2arena~V4/(V1+V2+V3+V4+V5))

# proportions of personality?

pin(arena.animal1, pe2arena~V1/(V1+V2+V3+V4))
pin(arena.animal1, h2arena~V2/(V1+V2+V3+V4))
pin(arena.animal1, m2arena~V3/(V1+V2+V3+V4))
pin(arena.animal1, s2arena~V4/(V1+V2+V3+V4))



# for calculating confidence limits, Wolak 2012 (supplementary
# information) advises against using aiCI because this just 
# uses the standard  errors and assumes they are symmetrical
# about the esimate. Instead, the recommended function is
# proLik.
profileA <- proLik(full.model = arena.animal1, 
                   component = "R!variance", 
                   negative = FALSE, 
                   nsample.units = 4, nse = 3.5)

profileA
profileA$LCL
profileA$UCL

# significances:

# Vpe

arena.animal2 <- asreml(fixed=logtot~1,
                        random=~ped(factorasrid, var=T, init=1)+
                          ide(factorasrdam, var=T, init=1)+
                          social,
                        data=ar1,
                        ginverse=list(factorasrid=asrpinv,
                                      factorasrdam=asrpinv),
                        na.method.X="omit",
                        na.method.Y="omit",
                        maxiter=20)

1-pchisq(2*(arena.animal1$loglik-arena.animal2$loglik),1)


# Va

arena.animal3 <- asreml(fixed=logtot~1,
                        random=~ide(factorasrid, var=T, init=1)+
                          ide(factorasrdam, var=T, init=1)+
                          social,
                        data=ar1,
                        ginverse=list(factorasrid=asrpinv,
                                      factorasrdam=asrpinv),
                        na.method.X="omit",
                        na.method.Y="omit",
                        maxiter=20)

1-pchisq(2*(arena.animal1$loglik-arena.animal3$loglik),1)



# Social (Vm is boundary)

arena.animal4 <- asreml(fixed=logtot~1,
                        random=~ide(factorasrid, var=T, init=1)+
                          ped(factorasrid, var=T, init=1)+
                          ide(factorasrdam, var=T, init=1),
                        data=ar1,
                        ginverse=list(factorasrid=asrpinv,
                                      factorasrdam=asrpinv),
                        na.method.X="omit",
                        na.method.Y="omit",
                        maxiter=20)

1-pchisq(2*(arena.animal1$loglik-arena.animal4$loglik),1)



########################################################
########################################################

# what is the deal with compromised data?
# will excluding it change my interpretation?

arnocomp <- subset(ar1, ar1$comp==0)
summary(arnocomp)
length(unique(arnocomp$asrid))

activitynocomp1 <- asreml(fixed=logtot~zero,
                      random=~ide(factorasrid, var=T, init=1),
                      data=arnocomp,
                      ginverse=list(factorasrid=asrpinv),
                      na.method.X="omit",
                      na.method.Y="omit",
                      maxiter=20)

summary(activitynocomp1)$varcomp
summary(asrarena0.1)$varcomp
# weeny bit more repeatable without it.


# any day-specific differences?

arnocomp10 <- subset(arnocomp, arnocomp$age==1)
arnocomp12 <- subset(arnocomp, arnocomp$age==0)


activitynocomp10 <- asreml(fixed=logtot~zero,
                          random=~ide(factorasrid, var=T, init=1),
                          data=arnocomp10,
                          ginverse=list(factorasrid=asrpinv),
                          na.method.X="omit",
                          na.method.Y="omit",
                          maxiter=20)


activitynocomp12 <- asreml(fixed=logtot~zero,
                          random=~ide(factorasrid, var=T, init=1),
                          data=arnocomp12,
                          ginverse=list(factorasrid=asrpinv),
                          na.method.X="omit",
                          na.method.Y="omit",
                          maxiter=20)

summary(activitynocomp10)$varcomp
summary(arenaday10)$varcomp
# slightly lower repeatability
summary(activitynocomp12)$varcomp
summary(arenaday12)$varcomp
# mixed, Vpe down but Vr down a bit too.
# No real major change --> keep compromised data!


# animal model?

activitynocomp2 <- asreml(fixed=logtot~zero,
                          random=~ide(factorasrid, var=T, init=1)+
                            ped(factorasrid, var=T, init=1)+
                            ide(factorasrdam, var=T, init=1),
                          data=arnocomp,
                          ginverse=list(factorasrid=asrpinv,
                                        factorasrdam=asrpinv),
                          na.method.X="omit",
                          na.method.Y="omit",
                          maxiter=20)

summary(activitynocomp2)$varcomp
summary(arena.animal1)$varcomp
# really, really similar. The conclusions will be
# fine if I include this data.

########################################################
########################################################

# now I need to consider fixed effects.
# The following are important:
# whether a test was first, second, third (fit as a factor,
# unlike when I did these models before)
# whether a test was compromised
# who released the bird
# social brood size
# where the bird stopped moving in the arena
# the order in which the brood was tested, which
# is a bit confounded with whether it was a bird's
# first, second, third test but we will see how the
# vif's are before including or excluding it.
# whether the test area is noisy.
# where the bird was tested (check for effect).
# time that a nestling was tested.
# cohort
# age

# stuff that is interesting but I will not be including
# for now:
# mass: I am not including this because it tests a 
# specific hypothesis for whether state underpins this
# behaviour, and this hypothesis is addressed elsewhere.
# the interaction of testing order and brood size:
# this should not be included because the testing order
# (between one and 15) is confounded with the brood size
# social group size: this is related to both brood size
# and testing order, so I think for now the testing order
# should encompass the effects of being in a group that
# is smaller and therefore waits less and a group that is
# larger and therefore waits more.
# group size (because of confounding, see above).
# natal brood size

# to check effect first: temperature, temperature^2, 
# cross-fostered, noise, time, and location

plot(ar1$temp, ar1$total, log="y")
# maybe something there, we'll see

plot(ar1$cf, ar1$total, log="y") 
#looks like no difference

plot(ar1$noise, ar1$total, log="y")
# a bit weird.
table(ar1$noise)
# because there are just 10 samples where
# the noise environment was changing. I used
# to amalgamate these with the non-noisy
# environment because of no difference, so let's 
# see what happens this time


plot(ar1$time, ar1$total, log="y")
# this looks like a slight negative relationship
# but might be because of a high number of tests
# in the morning.


plot(ar1$where, ar1$total, log="y")
# definitely different. Will I have any sample
# size problems?
table(ar1$where)
# should be ok. Though without the temperature
# information I will be down to two categories
# (no temperature is known for the lambing shed)

summary(ar1$temp)
ar1.t <- subset(ar1, ar1$temp!="NA")
summary(ar1.t$temp)

table(ar1.t$where)
# so this still has to be tested on all data if temp
# is not significant.

ar1.t$ztemp <- scale(ar1.t$temp)
ar1.t$ztime <- scale(ar1.t$time)

# so to check first: method things that I expect not to have
# an effect:
arenafixed1 <- asreml(fixed=logtot~cf + ztemp + I((ztemp)^2) + 
                        where + noise + ztime,
                    random=~ide(factorasrid, var=T, init=1),
                    data=ar1.t,
                    ginverse=list(factorasrid=asrpinv),
                    na.method.X="omit",
                    na.method.Y="omit",
                    maxiter=20)


plot(arenafixed1)
# not too bad...
plot(ar1.t$cf, arenafixed1$residuals)
plot(ar1.t$temp, arenafixed1$residuals)
plot(ar1.t$where, arenafixed1$residuals)
plot(ar1.t$noise, arenafixed1$residuals)
plot(ar1.t$time, arenafixed1$residuals)

# apart from the residuals for time, which
# are skewed towards being in the morning 
# but despite this are fairly randomly distributed,
# the residuals are pretty nice.

# so from this model, are there any methodological
# things I cal leave out from the final model?

summary(arenafixed1, all=T)$coef.fi

# temperature, cross-fostering, and location don't
# matter in this. They have next to no effect size.
# but I need to check location in a model with the
# main data set because two of the four location
# factors are missing here.


# ok, so do this again minus the fixed effects of
# temperature and cross-fostering

ar1$ztime <- scale(ar1$time)

arenafixed2 <- asreml(fixed=logtot~ where + noise + ztime,
                      random=~ide(factorasrid, var=T, init=1),
                      data=ar1,
                      ginverse=list(factorasrid=asrpinv),
                      na.method.X="omit",
                      na.method.Y="omit",
                      maxiter=20)


plot(arenafixed2)
# ok
plot(ar1$where, arenafixed2$residuals)
# more variance in the extreme groups than
# groups in the middle, but this is because
# of lack of sample size in the middle groups
# compared to the edge groups
plot(ar1$noise, arenafixed2$residuals)
plot(ar1$time, arenafixed2$residuals)

# these both look fine. Have a look at the results:
summary(arenafixed2, all=T)$coef.fi

# hum. The 'where' is not different within Steve's
# shed, and is very similar within the lambing shed,
# but there might be differences between the two.

# could I make a lambing shed versus Steve's shed
# factor? That might make sense since differentiating
# within these locations is confusing but between the
# locations there are large differences in noise etc.

table(ar1$where)
ar1$lambVSsteve <- ifelse(ar1$where==0, "steve",
                          ifelse(ar1$where==3, "steve",
                                 "lamb"))
table(ar1$lambVSsteve)

# and for the noise variable, I think the middle category
# can't stay. As before, it is very similar to category
# to so amalgamate:

table(ar1$noise2)
# well, I already did it earlier :)

# one last check:

arenafixed3 <- asreml(fixed=logtot~ relevel(factor(lambVSsteve),2) + 
                        noise2 + ztime,
                      random=~ide(factorasrid, var=T, init=1),
                      data=ar1,
                      ginverse=list(factorasrid=asrpinv),
                      na.method.X="omit",
                      na.method.Y="omit",
                      maxiter=20)

plot(arenafixed3)
plot(factor(ar1$lambVSsteve), arenafixed3$residuals)
# hum. Many more samples in steve's shed.
plot(ar1$noise2, arenafixed3$residuals)
plot(ar1$time, arenafixed3$residuals)

# ok, so how are the two locations?
summary(arenafixed3, all=T)$coef.fi

# good! keep all of these.



# right, remove the two tests for individuals that
# were tested four times in one day rather than 
# three

table(ar1$wd1to3)

ar1.minus4th <- subset(ar1, ar1$wd1to3<4)

table(ar1.minus4th$wd1to3)

ar1.minus4th$zsocbroodsz <- scale(ar1.minus4th$socbroodsz)
ar1.minus4th$ztime <- scale(ar1.minus4th$time)
ar1.minus4th$zmord <- scale(ar1.minus4th$mwdord)
ar1.minus4th$ziord <- scale(ar1.minus4th$indwdord)
ar1.minus4th$zendprob <- scale(ar1.minus4th$endprob)

# turn ziord and zmord in to numeric (otherwise they
# turn up as two-part variables and the model doesn't
# like it):

ar1.minus4th$zmord <- as.numeric(ar1.minus4th$zmord)
ar1.minus4th$ziord <- as.numeric(ar1.minus4th$ziord)

summary(ar1.minus4th$zmord)
summary(ar1.minus4th$ziord)

hist(ar1.minus4th$zmord)
hist(ar1.minus4th$ziord)
hist(ar1.minus4th$zsocbroodsz)
hist(ar1.minus4th$ztime)
# time is a bit skewy
hist(ar1.minus4th$zendprob)
# and this isn't even close to gaussian...

ar1.minus4th$lambVSsteve <- as.factor(ar1.minus4th$lambVSsteve)

# well, let's take a look at the vifs and the model checks:
vif.lme(lmer(logtot~relevel(lambVSsteve,2) + 
               noise2 + ztime + zmord + ziord + factor(wd1to3) + 
               zsocbroodsz + cohort + releaser + age + 
               comp + zendprob+(1|factorasrid),
             data=ar1.minus4th))

vif(lm(logtot~relevel(lambVSsteve,2) + 
               noise2 + ztime + zmord + ziord + factor(wd1to3) + 
               zsocbroodsz + cohort + releaser + age + 
               comp + zendprob,
             data=ar1.minus4th))

# well, judging by this I have some issues:
# lambVSsteve is high (2.7 - 3) but is a factor, so fine
# noise2 is high (2.3) again a factor so fine
# zmord and ziord are high (3.5 - 4)
# wd1to3 is high but a factor
# social brood size is high (2.7)

# so, testing order and social brood size have similar
# effects, unsurprisingly since a big brood means bigger
# test order numbers. What if I don't use centred test
# order?

ar1.minus4th$zorder <- scale(ar1.minus4th$wdorder)
hist(ar1.minus4th$zorder) #not perfect...


vif.lme(lmer(logtot~relevel(lambVSsteve,2) + 
               noise2 + ztime + zorder + factor(wd1to3) + 
               zsocbroodsz + cohort + releaser + age + 
               comp + zendprob+(1|factorasrid),
             data=ar1.minus4th))

vif(lm(logtot~relevel(lambVSsteve,2) + 
         noise2 + ztime + zorder + factor(wd1to3) + 
         zsocbroodsz + cohort + releaser + age + 
         comp + zendprob,
       data=ar1.minus4th))

# testing order is still rather high.



test1 <- gvlma(lm(logtot~relevel(lambVSsteve,2) + 
                    noise2 + ztime + zorder + factor(wd1to3) + 
                    zsocbroodsz + cohort + releaser + age + 
                    comp + zendprob,
                  data=ar1.minus4th))
summary(test1)



plotme <- cbind("activity"=log(ar1.minus4th$total+0.5), 
                "time"=ar1.minus4th$time,
                "testorder"=ar1.minus4th$wdorder, 
                "socialbroodsz"=ar1.minus4th$socbroodsz,
                "endprob"=ar1.minus4th$endprob,
                "one2three"=ar1.minus4th$wd1to3)

splom(plotme)

# well, the reason for the problem is 
# clearer now. The order of testing within
# a day is dependent on the size of the
# brood. On top of that, it is clearly 
# correlated with the order of testing
# within a day (first, second, third)

# ok. Since there are two types of test
# order within a day that are partially
# dependent, how can I chose between them?
# Which one has better vifs? Which is more
# biologically relevant?

# first, with only 1 to 3
vif.lme(lmer(logtot~relevel(lambVSsteve,2) + 
               noise2 + ztime + factor(wd1to3) + 
               zsocbroodsz + cohort + releaser + age + 
               comp + zendprob+(1|factorasrid),
             data=ar1.minus4th))

# now with order of testing within the brood, 1 to 15
vif.lme(lmer(logtot~relevel(lambVSsteve,2) + 
               noise2 + ztime + zorder + 
               zsocbroodsz + cohort + releaser + age + 
               comp + zendprob+(1|factorasrid),
             data=ar1.minus4th))

# well, there is nothing to chose between them.
# which has a greater effect in a model?


arenafixed4 <- asreml(fixed=logtot~relevel(factor(lambVSsteve),2) + 
                        noise2 + ztime + factor(wd1to3) + 
                      zsocbroodsz + cohort + releaser + age + 
                      comp + zendprob,
                      random=~ide(factorasrid, var=T, init=1) +
                        ped(factorasrid, var=T, init=1) +
                        ide(factorasrdam, var=T, init=1) +
                        social,
                      data=ar1.minus4th,
                      ginverse=list(factorasrid=asrpinv,
                                    factorasrdam=asrpinv),
                    na.method.X="omit",
                    na.method.Y="omit",
                    maxiter=20)

arenafixed5 <- asreml(fixed=logtot~relevel(factor(lambVSsteve),2) + 
                        noise2 + ztime + zorder + 
                        zsocbroodsz + cohort + releaser + age + 
                        comp + zendprob,
                      random=~ide(factorasrid, var=T, init=1) +
                        ped(factorasrid, var=T, init=1) +
                        ide(factorasrdam, var=T, init=1) +
                        social,
                      data=ar1.minus4th,
                      ginverse=list(factorasrid=asrpinv,
                                    factorasrdam=asrpinv),
                      na.method.X="omit",
                      na.method.Y="omit",
                      maxiter=20)

summary(arenafixed4, all=T)$coef.fi
arenafixed4$loglik
summary(arenafixed5, all=T)$coef.fi
arenafixed5$loglik

# well, they both have a great effect but 
# 1) the log likelihood of the model is better
# with wd1to3 and
# 2) wd1to3 has a clear definition that is habituation,
# whereas the test order within a day is a mix of
# habituation and waiting time.

# therefore, I am going with wd1to3. I don't
# think it matters though-  both have similar
# effects anyway!



# so my model for table Sxxx is model 4!


arenafixed4 <- asreml(fixed=logtot~relevel(factor(lambVSsteve),2) + 
                        noise2 + ztime + factor(wd1to3) + 
                        zsocbroodsz + cohort + releaser + age + 
                        comp + zendprob,
                      random=~ide(factorasrid, var=T, init=1) +
                        ped(factorasrid, var=T, init=1) +
                        ide(factorasrdam, var=T, init=1) +
                        social,
                      data=ar1.minus4th,
                      ginverse=list(factorasrid=asrpinv,
                                    factorasrdam=asrpinv),
                      na.method.X="omit",
                      na.method.Y="omit",
                      maxiter=20)

plot(arenafixed4)
# very nice!

plot(ar1.minus4th$lambVSsteve, arenafixed4$residuals)
plot(ar1.minus4th$noise2, arenafixed4$residuals)
plot(ar1.minus4th$time, arenafixed4$residuals)
plot(ar1.minus4th$wd1to3, arenafixed4$residuals)
plot(ar1.minus4th$socbroodsz, arenafixed4$residuals)
plot(ar1.minus4th$cohort, arenafixed4$residuals)
plot(ar1.minus4th$releaser, arenafixed4$residuals)
plot(ar1.minus4th$age, arenafixed4$residuals)
# well that is not perfect... but that is the
# age structure... better as a factor.
plot(ar1.minus4th$comp, arenafixed4$residuals)
plot(ar1.minus4th$endprob, arenafixed4$residuals)

# ok, quite good, all in all.

summary(arenafixed4)$varcomp
summary(arenafixed4, all=T)$coef.fi

wald.asreml(arenafixed4, ssType="conditional", denDF="numeric")


# sample sizes
length(unique(ar1.minus4th$asrid))
length(ar1.minus4th$asrid)





# significances:

# Vpe

arenafixed4.2 <- asreml(fixed=logtot~relevel(factor(lambVSsteve),2) + 
                        noise2 + ztime + factor(wd1to3) + 
                        zsocbroodsz + cohort + releaser + age + 
                        comp + zendprob,
                      random=~ped(factorasrid, var=T, init=1) +
                        ide(factorasrdam, var=T, init=1) +
                        social,
                      data=ar1.minus4th,
                      ginverse=list(factorasrid=asrpinv,
                                    factorasrdam=asrpinv),
                      na.method.X="omit",
                      na.method.Y="omit",
                      maxiter=20)


1-pchisq(2*(arenafixed4$loglik-arenafixed4.2$loglik), 1)


# Va

arenafixed4.3 <- asreml(fixed=logtot~relevel(factor(lambVSsteve),2) + 
                        noise2 + ztime + factor(wd1to3) + 
                        zsocbroodsz + cohort + releaser + age + 
                        comp + zendprob,
                      random=~ide(factorasrid, var=T, init=1) +
                        ide(factorasrdam, var=T, init=1) +
                        social,
                      data=ar1.minus4th,
                      ginverse=list(factorasrid=asrpinv,
                                    factorasrdam=asrpinv),
                      na.method.X="omit",
                      na.method.Y="omit",
                      maxiter=20)



1-pchisq(2*(arenafixed4$loglik-arenafixed4.3$loglik), 1)

# Vm is boundary --> no test needed

# Vs

arenafixed4.4 <- asreml(fixed=logtot~relevel(factor(lambVSsteve),2) + 
                        noise2 + ztime + factor(wd1to3) + 
                        zsocbroodsz + cohort + releaser + age + 
                        comp + zendprob,
                      random=~ide(factorasrid, var=T, init=1) +
                        ped(factorasrid, var=T, init=1) +
                        ide(factorasrdam, var=T, init=1),
                      data=ar1.minus4th,
                      ginverse=list(factorasrid=asrpinv,
                                    factorasrdam=asrpinv),
                      na.method.X="omit",
                      na.method.Y="omit",
                      maxiter=20)


1-pchisq(2*(arenafixed4$loglik-arenafixed4.4$loglik), 1)



###############################################################


# at ESEB 2015, Patrick Fitze asked whether I had tried an
# interaction of mass and time, because heavy nestlings
# might beg less intently as the day progresses whilst
# light nestlings would continue to beg:

ar1.minus4th$zmass <- scale(ar1.minus4th$mass)
summary(ar1.minus4th$zmass)
sd(ar1.minus4th$zmass, na.rm=T)

plot(ar1$time, ar1$mass)


arenafixed5 <- asreml(fixed=logtot~ztime*zmass,
                      random=~ide(factorasrid, var=T, init=1) +
                        ped(factorasrid, var=T, init=1) +
                        ide(factorasrdam, var=T, init=1) +
                        social,
                      data=ar1.minus4th,
                      ginverse=list(factorasrid=asrpinv,
                                    factorasrdam=asrpinv),
                      na.method.X="omit",
                      na.method.Y="omit",
                      maxiter=20)

summary(arenafixed5, all=T)$coef.fi
# not much effect here. But a very interesting
# diversion :)

###############################################################
###############################################################

# a short diversion on nestling sex.

# one very big difference between nestlings is 
# that they may be male or female. Does this change
# the inferences that I make about nestling activity?
# are males particularly large and therefore active?
# are females more competitive and therefore active?

# I pulled the table sys_SexEstimates from the database
# that Alfredo released in March 2015 in order to explore
# whether sex, or the sex ratio of a brood, has an
# effect on nestling activity:

sex.estimates <- read.table("sys_SexEstimates-fromAlfredoCheckedMarch2015DB-20150820.txt",
                            header=T, na.strings="NA")

head(sex.estimates)
summary(sex.estimates)
str(sex.estimates)

ar1$sex <- sex.estimates$SexEstimate[match(ar1$birdid, sex.estimates$BirdID)]
summary(ar1$sex)
str(ar1$sex)

# lots where I don't know sex...

# first things first, is there a difference between males
# and females in their mean activity levels?


sexandarena1 <- asreml(fixed=logtot~factor(sex),
                       random=~ide(factorasrid, var=T, init=1),
                       data=ar1,
                       ginverse=list(factorasrid=asrpinv),
                       na.method.X="omit",
                       na.method.Y="omit",
                       maxiter=20)

summary(sexandarena1, all=T)$coef.fi

# yes. Is it because of mass?
ar1$zmass <- scale(ar1$mass)

sexandarena2 <- asreml(fixed=logtot~factor(sex) + zmass,
                       random=~ide(factorasrid, var=T, init=1),
                       data=ar1,
                       ginverse=list(factorasrid=asrpinv),
                       na.method.X="omit",
                       na.method.Y="omit",
                       maxiter=20)

summary(sexandarena2, all=T)$coef.fi
# nope. Something separate.

# when all other fixed effects are in the model,
# does mass still have an effect i.e. it is not
# year and observer effects:


sexandarena3 <- asreml(fixed=logtot~relevel(factor(lambVSsteve),2) + 
                        noise2 + ztime + factor(wd1to3) + 
                        zsocbroodsz + cohort + releaser + age + 
                        comp + zendprob + sex,
                      random=~ide(factorasrid, var=T, init=1) +
                        ped(factorasrid, var=T, init=1) +
                        ide(factorasrdam, var=T, init=1) +
                        social,
                      data=ar1,
                      ginverse=list(factorasrid=asrpinv,
                                    factorasrdam=asrpinv),
                      na.method.X="omit",
                      na.method.Y="omit",
                      maxiter=20)

summary(sexandarena3, all=T)$coef.fi
summary(sexandarena3)$varcomp

# but the end result is really similar for variance
# components, compared to the model of all data.
# the understanding about this trait's heritability
# won't change with the addition of sex

# what about the repeatability for each sex?

ar1.F <- subset(ar1, ar1$sex==0)
ar1.M <- subset(ar1, ar1$sex==1)

summary(ar1.F)
summary(ar1.M)

# for females:

sexandarena4 <- asreml(fixed=logtot~1,
                       random=~ide(factorasrid, var=T, init=1),
                       data=ar1.F,
                       ginverse=list(factorasrid=asrpinv),
                       na.method.X="omit",
                       na.method.Y="omit",
                       maxiter=20)

summary(sexandarena4)$varcomp

# for males:

sexandarena5 <- asreml(fixed=logtot~1,
                       random=~ide(factorasrid, var=T, init=1),
                       data=ar1.M,
                       ginverse=list(factorasrid=asrpinv),
                       na.method.X="omit",
                       na.method.Y="omit",
                       maxiter=20)

summary(sexandarena5)$varcomp


# really very similar. So, a difference in baseline
# activity but not one that is linked to variance
# in activity levels.

# just quickly: what about sex ratio of the brood?

broodsexratio <- aggregate(ar1$sex, by=list(ar1$social), 
                           FUN=mean, na.rm=T)
broodsexratio

broodsexratio$x <- ifelse(broodsexratio$x=="NaN", "NA", broodsexratio$x)
broodsexratio

str(broodsexratio)
ar1$sexratio <- broodsexratio$x[match(ar1$social, broodsexratio$Group.1)]

head(broodsexratio)
ar1$social[1:10]
ar1$sexratio[1:10]

str(ar1$sexratio)
ar1$sexratio <- as.numeric(ar1$sexratio)

sexandarena6 <- asreml(fixed=logtot~factor(sex) + sexratio,
                       random=~ide(factorasrid, var=T, init=1),
                       data=ar1,
                       ginverse=list(factorasrid=asrpinv),
                       na.method.X="omit",
                       na.method.Y="omit",
                       maxiter=20)

summary(sexandarena6, all=T)$coef.fi
# sex matters, sex ratio doesn't seem to

plot(jitter(ar1$sexratio, 5), ar1$total, log="y")
plot(factor(ar1$sex), ar1$total, log="y")
# so, males are a little more active

###############################################################
###############################################################

# at ESEB 2015, Eryn had the suggestion that year should
# go in to the model as a random effect, despite the few
# categories of year, because it will change how the
# variance is paritioned. Well, it seems sensible to have
# a go:

str(ar1$cohort)

yearrandom1 <- asreml(fixed=logtot~factor(sex) + sexratio,
                       random=~ide(factorasrid, var=T, init=1)+
                        ped(factorasrid, var=T, init=1)+
                        ide(factorasrdam, var=T, init=1)+
                        social+
                        cohort,
                       data=ar1,
                       ginverse=list(factorasrid=asrpinv,
                                     factorasrdam=asrpinv),
                       na.method.X="omit",
                       na.method.Y="omit",
                       maxiter=20)

plot(yearrandom1)
# funny residuals peak
summary(yearrandom1)$varcomp

# well, that doesn't really surprise me considering there
# are just three cohorts. But it is something to consider
# in the other analyses, whether year should be a random
# effect

###############################################################
###############################################################


# the arena data is collected with some error, which we
# would like to quantify.
# So, a second observer called Andres watched a subset of 
# data in 2013. Andres was trained in person with a couple
# of videos, and subsequently left to watch all videos alone,
# naive of other scorings of the same video. The same video
# was observed separately by myself (IW).


ar.Andres <- read.table("AR-AndresWatched2013-20150819.txt", header=T)

head(ar.Andres)
summary(ar.Andres)
str(ar.Andres)


# I want to match this with the data I collected:
head(ar)

ar$chickorder <- paste(ar$chick, ar$acrossdorder, sep="-")
head(ar$chickorder)

ar.Andres$chickorder <- paste(ar.Andres$ChickCode, ar.Andres$TESTNO, sep="-")
head(ar.Andres$chickorder)


# match data sets by these identifiers:
ar.Andres$IWtotal <- ar$total[match(ar.Andres$chickorder, ar$chickorder)]

ar.Andres$IWtotal
ar.Andres$total.sqs

# plot

plot(jitter(ar.Andres$IWtotal), jitter(ar.Andres$total.sqs), 
     xlab="Nestling activity by observer 1 (jittered)",
     ylab="Nestling activity by observer 2 (jittered)",
     pch=16, col=rgb(0,0,0,0.5),
     ylim=c(0,27), xlim=c(0,27),
     cex.lab=1.5, cex.axis=1.5, cex=1.5)

lines(x=c(0,27),y=c(0,27), lty=2, lwd=3, col=rgb(0,0,0,0.5))

cor.test(ar.Andres$IWtotal, ar.Andres$total.sqs, 
         method="pearson")
cor.test(ar.Andres$IWtotal, ar.Andres$total.sqs, 
         method="kendall")
cor.test(ar.Andres$IWtotal, ar.Andres$total.sqs, 
         method="spearman")
# so these observations are highly correlated
# using all tests. A pearson test looks at whether
# x and y are correlated. A kendall test asks whether
# the ranks of the variables are correlated, a spearman
# test asks whether the variables are associated
# but not necessarily in a line. So I want a pearson's
# test to show the good correlation between the variables:

text(x=10, y=25, "Pearson's product-moment correlation
R=0.96, t=38.6, df=142, p<0.001",
     cex=1.5)


# save this plot as Supplementary Figure xxx
# resolution 700x700

# how much did I ask Andres to watch?

length(ar.Andres[,1])
length(unique(ar.Andres$ChickCode))

# as a rough proportion of the total available videos?
length(ar.Andres[,1])/length(ar[,1])



length(unique(ar.Andres$ChickCode))/length(unique(ar$asrid))

#################################################################
#################################################################
# following comment from Echo:
# The latest version is much better than before. However, it will 
# still be good to know why excluding day 13 after the pilot study.
# It really depends on the reason of this to see whether or not day
# 13 should be included in this analysis. Also did you try excluding 
# data from day 13 and see if the results remain the same? Haven't 
# renew the software licence might not be able to convince the reviewers 
# if they requests extra analyses.

# Later on I read your further explanation in your comment on Table
# 1. Now I can understand why you keep this and yes, potentially 
# this is the best solution if considering the power. Nevertheless,
# the reviewers might not be convinced.

#################################################################

# This is how the comment is dealt with:
# would the results from the animal models be
# the same if day 13 daya was excluded?

# exclude day 13
{
  which(ar1$age==3)
ar1.minusday13 <- ar1[-which(ar1$age==3),]
table(ar1.minusday13)
head(ar1.minusday13)
}

# day 13 model in MCMCglmm because I have no ASReml-R
# licence at the time of running the model:
# pedigree:
{
  ar1.minusday13$animal <- ar1.minusday13$asrid
  ar1.minusday13$animal <- as.factor(ar1.minusday13$animal)
  mcmcglmminv <- inverseA(asrp2x)$Ainv 
  head(mcmcglmminv)
}

# priors (slightly strengthened inverse-Wishart),
# model, model plots
{
  priorarena.animal <- list(R=list(V=1, nu=0.005),
                            G=list(G1=list(V=1, nu=0.005),
                                   G2=list(V=1, nu=0.005),
                                   G3=list(V=1, nu=0.005),
                                   G4=list(V=1, nu=0.005)))
  
  arena.animal.minus13 <- MCMCglmm(logtot~1,
                        random=~factorasrid+
                          animal+factorasrdam+
                          social,
                        data=ar1.minusday13,
                        prior=priorarena.animal,
                        family="gaussian",
                        ginverse=list(animal=mcmcglmminv),
                        nitt=1000000,
                        burnin=200000,
                        thin=800)
  
  plot(arena.animal.minus13)
  # rather similar to the 
  autocorr(arena.animal.minus13$Sol)
  autocorr(arena.animal.minus13$VCV)
  # unacceptable autocorrelation, though the model is
  # a check of whether the estimates are similar between
  # this and the full data set so I will not run it for
  # as many hours as would be needed to prevent the autocorrelation.
}