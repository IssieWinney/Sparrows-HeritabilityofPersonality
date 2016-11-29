# June 2015
# Isabel Winney
# code required to make the data sets and run the models for 
# boldness data is in file boldness-june2015.R

# what are the sample sizes:
length(unique(boldnessdatamincomp2$broodname))
length(unique(boldnessdatamincomp2$asrid))
table(table(boldnessdatamincomp2$asrid))
length(boldnessdatamincomp2[,1])
summary(boldnessdatamincomp2$entrylatency)
sd(boldnessdatamincomp2$entrylatency)
table(boldnessdatamincomp2$year)
boldnessdatamincomp2$asrid[which(boldnessdatamincomp2$year==2009)]

# how many dams were filled in dams in the pedigree?
summary(damplus)
length(which(boldnessdatamincomp2$asrdam>6072))
tent2$asrid[which(boldnessdatamincomp2$asrdam>6072)]

# how many compromised tests were removed:
table(boldnessdata7$compJune2015)


# Make a data set with compromised data included, 
# split by sex:
fbold6 <- subset(boldnessdata7, boldnessdata7$sex=="F")
mbold6 <- subset(boldnessdata7, boldnessdata7$sex=="M")



#######################################################################
#######################################################################

# summary info:
# how many broods had videos taken on the wrong day:
boldnessdata$broodname[which(boldnessdata$takenonwrongday>0)]
# but for N087 and N083, we actually are just uncertain whether
# it was the right day. For the broods in 2009, these were part
# of testing the protocol.

# how much boldness data in the data I want to use is compromised:
table(boldnessdata7$compJune2015)

# but, how much did I remove before this point that was 'seriously
# compromised'? This is out of the useable data that is the right
# method, where we know the identity of the bird, etc
table(boldnessdata6$comp.1human.2paper.3other)

# so in total 156 individual tests but 10 of these had to be
# removed.


# how many dams were filled in? Bear in mind, the
# imported dam IDs start at 6073.
length(which(boldnessdata7$asrdam>6072))
# and how many unique dams is that?
length(unique(boldnessdata7$asrdam[which(boldnessdata7$asrdam>6072)]))



# now the summaries for the Results:
# how many broods were recorded?
length(unique(boldnessdata7$broodname))
length(unique(boldnessdatamincomp$broodname))

# how many records per individual?
table(table(boldnessdata7$asrid))
table(table(boldnessdatamincomp$asrid))
length(unique(boldnessdata7$asrid))
length(unique(boldnessdatamincomp$asrid))

# what is the behaviour like?
summary(boldnessdata7$entrylatency)
sd(boldnessdata7$entrylatency)

summary(boldnessdatamincomp$entrylatency)
sd(boldnessdatamincomp$entrylatency)

#######################################################################
#######################################################################


# the repeatability of boldness across all data points:

boldmod1 <- asreml(fixed=logentrylatency~zero,
                   random=~factorasrid,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit")
plot(boldmod1)
# not bad! I used this plot in the report as figure xxx
summary(boldmod1)$varcomp
summary(boldmod1, all=T)$coef.fi
pin(boldmod1, r2withcomp~V1/(V1+V2))

# how significant is this?
boldmod0 <- asreml(fixed=logentrylatency~zero,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit")

1-pchisq(2*(boldmod1$loglik-boldmod0$loglik),1)

# what is the sample size:
length(boldnessdata7[,1])
length(unique(boldnessdata7$asrid))
table(table(boldnessdata7$asrid))
summary(boldnessdata7$asrid)
summary(boldnessdata7$logentrylatency)


### males, all data ###

boldmodm <- asreml(fixed=logentrylatency~1,
                   random=~factorasrid,
                   data=mbold6,
                   na.method.X="omit",
                   na.method.Y="omit")

plot(boldmodm)
# residual versus fitted not so good here,
# perhaps due to the reduced sample size.
# the transformation seems a little less 
# generally appropriate

summary(boldmodm, all=T)$coef.fi
summary(boldmodm)$varcomp
pin(boldmodm, r2withcomp~V1/(V1+V2))

# how significant is this?
boldmodm0 <- asreml(fixed=logentrylatency~zero,
                    data=mbold6,
                    na.method.X="omit",
                    na.method.Y="omit")

1-pchisq(2*(boldmodm$loglik-boldmodm0$loglik),1)
length(mbold6[,1])
length(unique(mbold6$asrid))
table(table(mbold6$asrid))


### females, all data ###

boldmodf <- asreml(fixed=logentrylatency~1,
                   random=~factorasrid,
                   data=fbold6,
                   na.method.X="omit",
                   na.method.Y="omit")

plot(boldmodf)
# female residuals look far better, though it is the females
# that show deviation at low fitted values

summary(boldmodf, all=T)$coef.fi
summary(boldmodf)$varcomp
pin(boldmodf, r2withcomp~V1/(V1+V2))

# how significant is this?
boldmodf0 <- asreml(fixed=logentrylatency~zero,
                    data=fbold6,
                    na.method.X="omit",
                    na.method.Y="omit")

1-pchisq(2*(boldmodf$loglik-boldmodf0$loglik),1)
length(fbold6[,1])
length(unique(fbold6$asrid))
table(table(fbold6$asrid))


### for 2011 (2009 has too small a sample size)

subset2011 <- subset(boldnessdata7, boldnessdata7$year==2011)
table(boldnessdata7$year)
table(subset2011$year)

boldmod2011 <- asreml(fixed=logentrylatency~zero,
                   random=~factorasrid,
                   data=subset2011,
                   na.method.X="omit",
                   na.method.Y="omit")
plot(boldmod2011)
# not bad!
summary(boldmod2011)$varcomp
summary(boldmod2011, all=T)$coef.fi
pin(boldmod2011, r2withcomp~V1/(V1+V2))

# how significant is this?
boldmod2011.2 <- asreml(fixed=logentrylatency~zero,
                   data=subset2011,
                   na.method.X="omit",
                   na.method.Y="omit")

1-pchisq(2*(boldmod2011$loglik-boldmod2011.2$loglik),1)

# what is the sample size:
length(subset2011[,1])
length(unique(subset2011$asrid))
table(table(subset2011$asrid))
summary(subset2011$asrid)
summary(subset2011$logentrylatency)



### for 2012

subset2012 <- subset(boldnessdata7, boldnessdata7$year==2012)
table(boldnessdata7$year)
table(subset2012$year)

boldmod2012 <- asreml(fixed=logentrylatency~zero,
                      random=~factorasrid,
                      data=subset2012,
                      na.method.X="omit",
                      na.method.Y="omit")
plot(boldmod2012)
# not bad!
summary(boldmod2012)$varcomp
summary(boldmod2012, all=T)$coef.fi
pin(boldmod2012, r2withcomp~V1/(V1+V2))

# how significant is this?
boldmod2012.2 <- asreml(fixed=logentrylatency~zero,
                        data=subset2012,
                        na.method.X="omit",
                        na.method.Y="omit")

1-pchisq(2*(boldmod2012$loglik-boldmod2012.2$loglik),1)

# what is the sample size:
length(subset2012[,1])
length(unique(subset2012$asrid))
table(table(subset2012$asrid))



### for 2013

subset2013 <- subset(boldnessdata7, boldnessdata7$year==2013)
table(boldnessdata7$year)
table(subset2013$year)

boldmod2013 <- asreml(fixed=logentrylatency~zero,
                      random=~factorasrid,
                      data=subset2013,
                      na.method.X="omit",
                      na.method.Y="omit")
plot(boldmod2013)
# bit skewed
summary(boldmod2013)$varcomp
summary(boldmod2013, all=T)$coef.fi
pin(boldmod2013, r2withcomp~V1/(V1+V2))

# how significant is this?
boldmod2013.2 <- asreml(fixed=logentrylatency~zero,
                        data=subset2013,
                        na.method.X="omit",
                        na.method.Y="omit")

1-pchisq(2*(boldmod2013$loglik-boldmod2013.2$loglik),1)

# what is the sample size:
length(subset2013[,1])
length(unique(subset2013$asrid))
table(table(subset2013$asrid))



### for 2014

subset2014 <- subset(boldnessdata7, boldnessdata7$year==2014)
table(boldnessdata7$year)
table(subset2014$year)

boldmod2014 <- asreml(fixed=logentrylatency~zero,
                      random=~factorasrid,
                      data=subset2014,
                      na.method.X="omit",
                      na.method.Y="omit")
plot(boldmod2014)
# bit better
summary(boldmod2014)$varcomp
summary(boldmod2014, all=T)$coef.fi
pin(boldmod2014, r2withcomp~V1/(V1+V2))

# how significant is this?
boldmod2014.2 <- asreml(fixed=logentrylatency~zero,
                        data=subset2014,
                        na.method.X="omit",
                        na.method.Y="omit")

1-pchisq(2*(boldmod2014$loglik-boldmod2014.2$loglik),1)

# what is the sample size:
length(subset2014[,1])
length(unique(subset2014$asrid))
table(table(subset2014$asrid))



### across year repeatability, first test per year

firsttestcomp <- subset(boldnessdata7, 
                          boldnessdata7$testorder==1)
summary(firsttestcomp)


firstyearcomp1 <- asreml(fixed=logentrylatency~1,
                   random=~factorasrid,
                   data=firsttestcomp,
                   na.method.X="omit",
                   na.method.Y="omit")

plot(firstyearcomp1)
summary(firstyearcomp1, all=T)$coef.fi
summary(firstyearcomp1)$varcomp
pin(firstyearcomp1, r2withcomp~V1/(V1+V2))

# how significant is this?
firstyearcomp0 <- asreml(fixed=logentrylatency~zero,
                   data=firsttestcomp,
                   na.method.X="omit",
                   na.method.Y="omit")

1-pchisq(2*(firstyearcomp1$loglik-firstyearcomp0$loglik),1)
# not significant
length(firsttestcomp[,1])
length(unique(firsttestcomp$asrid))
table(table(firsttestcomp$asrid))


###
# what is the compromised data doing to the repeatability
# of boldness?
boldcomptest <- asreml(fixed=logentrylatency~factorcomp,
                   random=~us(factorcomp):factorasrid,
                   data=boldnessdata7,
                   na.method.X="omit",
                   na.method.Y="omit",
                   maxiter=1000)
plot(boldcomptest)
summary(boldcomptest, all=T)$coef.fi
summary(boldcomptest)$varcomp
# this suggests there is some covariance between a test
# that is normal and a test that is compromised, though
# there is not repeatability in compromised tests. This
# is probably because of sample size.
table(boldnessdata7$asrid, boldnessdata7$factorcomp)
# there's a fair number of birds with a compromised and
# a normal test, the number of repeat compromised tests
# is lower but there are some.

# can I correlate compromised and non-compromised tests?
# make a data set of compromised data
compromisedboldness <- subset(boldnessdata7, boldnessdata7$factorcomp==1)
summary(compromisedboldness)
# label the tests in the data by whether it was a bird's
# first, second, third, blah, ever
compromisedboldness2 <- testorder("asrid", "numericdate", compromisedboldness)
head(compromisedboldness2)
# in this particular data set, the variable 'testorder'
# is now DIFFERENT from the main data set!!!

# and do the same to the normal data excluding compromised data:
boldnessdatamincomp3 <- testorder("asrid", "numericdate", boldnessdatamincomp2)
head(boldnessdatamincomp3)
# again, now 'testorder' is NOT the same as the main data
# sets in this subset!!!

# and match the compromised data to the non-compromised
# data wherever possible using an identifier that is
# unique in both data sets, comprising the bird's identity
# and the order of the test within a data set:
compromisedboldness2$bird.order <- paste(compromisedboldness2$asrid,
                                         compromisedboldness2$testorder,
                                         sep="a")
head(compromisedboldness2$bird.order)

boldnessdatamincomp3$bird.order <- paste(boldnessdatamincomp3$asrid,
                                         boldnessdatamincomp3$testorder,
                                         sep="a")
head(boldnessdatamincomp3$bird.order)


boldnessdatamincomp3$comp.latency <- compromisedboldness2$logentrylatency[
  match(boldnessdatamincomp3$bird.order, compromisedboldness2$bird.order)]

# now plot the comparison!
xyplot(comp.latency~logentrylatency, data=boldnessdatamincomp3, 
       groups=factorasrid, type="b")
# from this, it looks like birds are very clearly stratified
# with their normal entry latency, but this entry latency 
# changes vertically wrt the y axis, rather than being correlated
# between the two very much. Many of these birds show greater
# variance in y than x, hinting that compromised tests inflate
# the between-individual variance and the within-individual variance.
# but this could just as easily be how I have matched up the data.
# but if so, why would it be consistently that the lines all go that
# way?


######## lastly, an animal model with all tests
boldanimal1 <- asreml(fixed=logentrylatency~1,
                      random=~ide(factorasrid, var=T, init=1) +
                        ped(factorasrid, var=T, init=1) + 
                        ide(factorasrdam, var=T, init=1),
                      ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                      data=boldnessdata7,
                      na.method.X="omit",
                      na.method.Y="omit")

plot(boldanimal1)
summary(boldanimal1)
summary(boldanimal1, all=T)$coef.fi

# significance of bird ID

boldanimal1.minID <- asreml(fixed=logentrylatency~1,
                      random=~ped(factorasrid, var=T, init=1) + 
                        ide(factorasrdam, var=T, init=1),
                      ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                      data=boldnessdata7,
                      na.method.X="omit",
                      na.method.Y="omit")

1-pchisq(2*(boldanimal1$loglik-boldanimal1.minID$loglik),1)

# significance of Va term


# no need for significance of maternal effect - effect goes to boundary

boldanimal1.minVa <- asreml(fixed=logentrylatency~1,
                            random=~ide(factorasrid, var=T, init=1) + 
                              ide(factorasrdam, var=T, init=1),
                            ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                            data=boldnessdata7,
                            na.method.X="omit",
                            na.method.Y="omit")

1-pchisq(2*(boldanimal1$loglik-boldanimal1.minVa$loglik),1)


##############################################################
##############################################################

# now minus the compromised data:
# and minus data taken on the wrong day --> ultra-conservative
# boldness repeatability in general

boldnessdatamincomp <- subset(boldnessdata7, boldnessdata7$compJune2015==0)
summary(boldnessdatamincomp$compJune2015)

boldnessdatamincomp2 <- subset(boldnessdatamincomp, 
                               boldnessdatamincomp$takenonwrongday==0)

head(boldnessdatamincomp2)
summary(boldnessdatamincomp2$takenonwrongday)


# is my log transformation still appropriate?:

boxcox((entrylatency+0.5)~1+(1|birdID), 
       data=boldnessdatamincomp2, 
       lambda=seq(-0.5,0.50,0.001))

powerTransform((entrylatency+0.5)~1+(1|birdID), 
               data=boldnessdatamincomp2)

# yes still good.


mincomp1 <- asreml(fixed=logentrylatency~zero,
                   random=~factorasrid,
                   data=boldnessdatamincomp2,
                   na.method.X="omit",
                   na.method.Y="omit")
plot(mincomp1)
# not bad!
summary(mincomp1)$varcomp
summary(mincomp1, all=T)$coef.fi
pin(mincomp1, r2withcomp~V1/(V1+V2))

# how significant is this?
mincomp0 <- asreml(fixed=logentrylatency~zero,
                   data=boldnessdatamincomp2,
                   na.method.X="omit",
                   na.method.Y="omit")

1-pchisq(2*(mincomp1$loglik-mincomp0$loglik),1)

# what is the sample size:
length(boldnessdatamincomp2[,1])
length(unique(boldnessdatamincomp2$asrid))
table(table(boldnessdatamincomp2$asrid))




### males ###

fboldmincomp2 <- subset(boldnessdatamincomp2, boldnessdatamincomp2$sex=="F")
summary(fboldmincomp2)
table(table(fboldmincomp2$asrid))

mboldmincomp2 <- subset(boldnessdatamincomp2, boldnessdatamincomp2$sex=="M")
summary(mboldmincomp2)
table(table(mboldmincomp2$asrid))


mincompm <- asreml(fixed=logentrylatency~1,
                   random=~factorasrid,
                   data=mboldmincomp2,
                   na.method.X="omit",
                   na.method.Y="omit")

plot(mincompm)
# sucky a bit
summary(mincompm, all=T)$coef.fi
summary(mincompm)$varcomp
pin(mincompm, r2withcomp~V1/(V1+V2))

# how significant is this?
mincompm0 <- asreml(fixed=logentrylatency~zero,
                    data=mboldmincomp2,
                    na.method.X="omit",
                    na.method.Y="omit")

1-pchisq(2*(mincompm$loglik-mincompm0$loglik),1)
length(mboldmincomp2[,1])
length(unique(mboldmincomp2$asrid))
table(table(mboldmincomp2$asrid))


### females ###

mincompf <- asreml(fixed=logentrylatency~1,
                   random=~factorasrid,
                   data=fboldmincomp2,
                   na.method.X="omit",
                   na.method.Y="omit")

plot(mincompf)
# female residuals look far better, though it is the females
# that show deviation at low fitted values

summary(mincompf, all=T)$coef.fi
summary(mincompf)$varcomp
pin(mincompf, r2withcomp~V1/(V1+V2))

# how significant is this?
mincompf0 <- asreml(fixed=logentrylatency~zero,
                    data=fboldmincomp2,
                    na.method.X="omit",
                    na.method.Y="omit")

1-pchisq(2*(mincompf$loglik-mincompf0$loglik),1)
length(fboldmincomp2[,1])
length(unique(fboldmincomp2$asrid))
table(table(fboldmincomp2$asrid))


###
### for 2011 (2009 has too small a sample size)

subset2011mincomp2 <- subset(boldnessdatamincomp2, boldnessdatamincomp2$year==2011)
table(boldnessdatamincomp2$year)
table(subset2011mincomp2$year)

boldmod2011mincomp <- asreml(fixed=logentrylatency~zero,
                      random=~factorasrid,
                      data=subset2011mincomp2,
                      na.method.X="omit",
                      na.method.Y="omit")
plot(boldmod2011mincomp)
# very very sparse...
summary(boldmod2011mincomp)$varcomp
summary(boldmod2011mincomp, all=T)$coef.fi
pin(boldmod2011mincomp, r2withcomp~V1/(V1+V2))

# how significant is this?
boldmod2011mincomp.2 <- asreml(fixed=logentrylatency~zero,
                        data=subset2011mincomp2,
                        na.method.X="omit",
                        na.method.Y="omit")

1-pchisq(2*(boldmod2011mincomp$loglik-boldmod2011mincomp.2$loglik),1)

# what is the sample size:
length(subset2011mincomp2[,1])
length(unique(subset2011mincomp2$asrid))
table(table(subset2011mincomp2$asrid))



### for 2012

subset2012mincomp2 <- subset(boldnessdatamincomp2, 
                            boldnessdatamincomp2$year==2012)
table(boldnessdatamincomp2$year)
table(subset2012mincomp2$year)

boldmod2012mincomp <- asreml(fixed=logentrylatency~zero,
                      random=~factorasrid,
                      data=subset2012mincomp2,
                      na.method.X="omit",
                      na.method.Y="omit")
plot(boldmod2012mincomp)
# meh
summary(boldmod2012mincomp)$varcomp
summary(boldmod2012mincomp, all=T)$coef.fi
pin(boldmod2012mincomp, r2withcomp~V1/(V1+V2))


# boundary --> no need for a significance test. p=1

# what is the sample size:
length(subset2012mincomp2[,1])
length(unique(subset2012mincomp2$asrid))
table(table(subset2012mincomp2$asrid))



### for 2013

subset2013mincomp2 <- subset(boldnessdatamincomp2, 
                            boldnessdatamincomp2$year==2013)
table(boldnessdatamincomp2$year)
table(subset2013mincomp2$year)

boldmod2013mincomp <- asreml(fixed=logentrylatency~zero,
                      random=~factorasrid,
                      data=subset2013mincomp2,
                      na.method.X="omit",
                      na.method.Y="omit")
plot(boldmod2013mincomp)
# at least there are data points
summary(boldmod2013mincomp)$varcomp
summary(boldmod2013mincomp, all=T)$coef.fi
pin(boldmod2013mincomp, r2withcomp~V1/(V1+V2))

# how significant is this?
boldmod2013mincomp.2 <- asreml(fixed=logentrylatency~zero,
                        data=subset2013mincomp2,
                        na.method.X="omit",
                        na.method.Y="omit")

1-pchisq(2*(boldmod2013mincomp$loglik-boldmod2013mincomp.2$loglik),1)

# what is the sample size:
length(subset2013mincomp2[,1])
length(unique(subset2013mincomp2$asrid))
table(table(subset2013mincomp2$asrid))



### for 2014

subset2014mincomp2 <- subset(boldnessdatamincomp2,
                            boldnessdatamincomp2$year==2014)
table(boldnessdatamincomp2$year)
table(subset2014mincomp2$year)

boldmod2014mincomp <- asreml(fixed=logentrylatency~zero,
                      random=~factorasrid,
                      data=subset2014mincomp2,
                      na.method.X="omit",
                      na.method.Y="omit")
plot(boldmod2014mincomp)
# bit better
summary(boldmod2014mincomp)$varcomp
summary(boldmod2014mincomp, all=T)$coef.fi
pin(boldmod2014mincomp, r2withcomp~V1/(V1+V2))

# how significant is this?
boldmod2014mincomp.2 <- asreml(fixed=logentrylatency~zero,
                        data=subset2014mincomp2,
                        na.method.X="omit",
                        na.method.Y="omit")

1-pchisq(2*(boldmod2014mincomp$loglik-boldmod2014mincomp.2$loglik),1)

# what is the sample size:
length(subset2014mincomp2[,1])
length(unique(subset2014mincomp2$asrid))
table(table(subset2014mincomp2$asrid))



### across year repeatability, first test per year

firsttestmincomp2 <- subset(boldnessdatamincomp2, 
                        boldnessdatamincomp2$testorder==1)
summary(firsttestmincomp2)


firstyearmincomp1 <- asreml(fixed=logentrylatency~1,
                         random=~factorasrid,
                         data=firsttestmincomp2,
                         na.method.X="omit",
                         na.method.Y="omit")

plot(firstyearmincomp1)# ok residuals but clear lack of data
summary(firstyearmincomp1, all=T)$coef.fi
summary(firstyearmincomp1)$varcomp
pin(firstyearmincomp1, r2withcomp~V1/(V1+V2))

# how significant is this?
firstyearmincomp0 <- asreml(fixed=logentrylatency~zero,
                         data=firsttestmincomp2,
                         na.method.X="omit",
                         na.method.Y="omit")

1-pchisq(2*(firstyearmincomp1$loglik-firstyearmincomp0$loglik),1)
# not significant
length(firsttestmincomp2[,1])
length(unique(firsttestmincomp2$asrid))
table(table(firsttestmincomp2$asrid))


######## lastly, an animal model with the restricted dataset
boldanimalmincomp1 <- asreml(fixed=logentrylatency~1,
                      random=~ide(factorasrid, var=T, init=1) +
                        ped(factorasrid, var=T, init=1) + 
                        ide(factorasrdam, var=T, init=1),
                      ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                      data=boldnessdatamincomp2,
                      na.method.X="omit",
                      na.method.Y="omit")

plot(boldanimalmincomp1)
# very nice! The line is gone.
summary(boldanimalmincomp1)$varcomp
summary(boldanimalmincomp1, all=T)$coef.fi

# proportion of behaviour that is Va
pin(boldanimalmincomp1, h2nocomp~V2/(V1+V2+V3+V4))

# proportion of personality that is Va
pin(boldanimalmincomp1, h2nocomp~V2/(V1+V2+V3))




# how big are h2, m2, pe2?
pin(boldanimalmincomp1, pe2bold~V1/(V1+V2+V3+V4))
pin(boldanimalmincomp1, h2bold~V2/(V1+V2+V3+V4))
pin(boldanimalmincomp1, m2bold~V3/(V1+V2+V3+V4))


# and as proportions of personality?
pin(boldanimalmincomp1, pe2bold~V1/(V1+V2+V3))
pin(boldanimalmincomp1, h2bold~V2/(V1+V2+V3))
pin(boldanimalmincomp1, m2bold~V3/(V1+V2+V3))


# extracting specific values is a pain. Example:
sum(pin(boldanimalmincomp1, pe2bold~V1/(V1+V2+V3+V4))[[1]])



# significance of bird ID

boldanimalmincomp1.minID <- asreml(fixed=logentrylatency~1,
                            random=~ped(factorasrid, var=T, init=1) + 
                              ide(factorasrdam, var=T, init=1),
                            ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                            data=boldnessdatamincomp2,
                            na.method.X="omit",
                            na.method.Y="omit")

1-pchisq(2*(boldanimalmincomp1$loglik-boldanimalmincomp1.minID$loglik),1)

# significance of Va term


# no need for significance of maternal effect - effect goes to boundary

boldanimalmincomp1.minVa <- asreml(fixed=logentrylatency~1,
                            random=~ide(factorasrid, var=T, init=1) + 
                              ide(factorasrdam, var=T, init=1),
                            ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                            data=boldnessdatamincomp2,
                            na.method.X="omit",
                            na.method.Y="omit")

1-pchisq(2*(boldanimalmincomp1$loglik-boldanimalmincomp1.minVa$loglik),1)


# quick mental diversion: boldness could be a property of a 
# pair, not an individual. In this case, we would find higher
# pair ID repeatability than individual repeatability. Let's
# test this:

boldnessdata7$pairID <- paste(boldnessdata7$maleID, boldnessdata7$femaleID, sep="")
head(boldnessdata7$pairID)

boldnessdata7$factorpairID <- as.factor(boldnessdata7$pairID)

pairrepeatbility1 <- asreml(fixed=logentrylatency~1,
                            random=~factorasrid+factorpairID,
                            data=boldnessdata7,
                            na.method.X="omit",
                            na.method.Y="omit")

plot(pairrepeatbility1)
summary(pairrepeatbility1)$varcomp
# wowee! would you look at that?! So boldness is more a 
# property of the pair than the individual! Which means
# even more that a bivariate approach would be cool.
# HOWEVER, this analysis is pseudoreplicated for partners
# because each pair can turn up twice for the same test
# but each bird just turns up once --> more replicates
# per pairID means a stronger effect. Fitting partnerID
# as a random effect partially solves this problem:

pairrepeatbility2 <- asreml(fixed=logentrylatency~1,
                            random=~factorasrid+factorpartnerID,
                            data=boldnessdata7,
                            na.method.X="omit",
                            na.method.Y="omit")

plot(pairrepeatbility2)
summary(pairrepeatbility2)$varcomp
# and here we see the partner has a much smaller 
# contribution.

##############################################################
##############################################################

# what fixed effects are very important?
# compromised yes or no (only when compromised data is included)
# entering first or second
# sex
# test order within a year
# year - probably as factor
# method
# partner behaviour

which(is.na(boldnessdata7$factorcomp))
which(is.na(boldnessdata7$testorder))
which(is.na(boldnessdata7$partnerboldshy))
which(is.na(boldnessdata7$offspring))
which(is.na(boldnessdata7$birdorderentry))
# damn. Some partner's behaviours are missing. I have
# to exclude them. I can just do this for the fixed effect
# models (for the bivariate, it will not matter to have
# missing data in one of the two covariates).

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



boxplot(logentrylatency~partnerboldshy, data=boldnessdata8)

# what is more important? year of testing, or more testing
# over more years?
firstmeasuresonly <- subset(boldnessdatamincomp, boldnessdatamincomp$testorder==1)
xyplot(logentrylatency~year, groups=birdID, 
       data=firstmeasuresonly, type="b")
# I don't see any clear within-individual trend,
# but maybe plasticity changes within years. But
# I can only check this from 2011-14 because there
# is not enough sample size in 2009.

# more minor considerations:
# time of day?
xyplot(logentrylatency~numerictime, groups=birdID, data=boldnessdatamincomp, type="b")
# the greatest number of tests is in the morning, but there are
# lots of straight lines through time --> suggests behaviour
# is relatively stable through time

# perch?
table(boldnessdatamincomp$perchpresent.1forpresent)
# I don't think there is enough sample size.
# maybe I should be thinking the same for method
table(boldnessdatamincomp$method)

# taken on wrong day?
table(boldnessdatamincomp$takenonwrongday)
# um. Do analysis with them in and out?

# time of arrival in the video?
xyplot(entrylatency~nestboxcontact, groups=birdID, data=boldnessdatamincomp, type="b")
# looks pretty random. Obviously there is a 50:50 
# cutoff because a bird that arrives at 30 min only
# has 30 min to enter.

# test order should be 1) individual mean centred because
# it varies between individuals and 2) scaled to make
# it comparable (however it is the only continuous effect
# here...)

# number of offspring. This is a more important
# consideration because it affects the birds motivation
# to complete the test.


xyplot(logentrylatency~imc.offspring|sex, data=boldnessdata8, 
       groups=birdID, type="b")
# a lot more disordered... but males might show
# a good offspring-dependent trend... what about experience?
xyplot(logentrylatency~imc.experience|sex, data=boldnessdata8, 
       groups=birdID, type="b")
# again, quite a mess and hard to tell. Overall effects?

xyplot(logentrylatency~meanoffspring|sex, data=boldnessdata8, 
       groups=birdID, type="b")
xyplot(logentrylatency~meanexperience|sex, data=boldnessdata8, 
       groups=birdID, type="b")
# nothing stands out on these plots. Except that
# males seem to invest more with more mean offspring. 
# Perhaps males that are investing in within-pair offspring
# will be bolder in general, to fit the strategy, but I
# don't know.

# I have no biological justification for why there would
# be a between-individual relationship between mean experience 
# and latency, unless birds that were tested more bred more, 
# so were of higher quality and from their quality have different
# behaviour and more offspring?
# So I guess there is a reason to fit the mean. But then I am
# left with loads of covariates just like last time!



# I could ask whether the fixed effects that relate to
# the construction of the nestbox matter:

fixedeffectsnestbox <- asreml(fixed=logentrylatency~factormethod+
                                antenna.yforpresent+
                                factor(perchpresent.1forpresent),
                        random=~factorbirdID,
                        data=boldnessdata8,
                        na.method.X="omit",
                        na.method.Y="omit")

summary(fixedeffectsnestbox, all=T)$coef.fi
# but this suggests they all matter a bit whereas in the next model:

fixedeffects1 <- asreml(fixed=logentrylatency~factorcomp+partnerboldshy+
                          factorentryorder+sex+factormethod+zmeanoffspring+
                          relevel(factoryear, ref="2014")+antenna.yforpresent+
                          zimc.offspring+zmeanexperience+zimc.experience+
                          factor(perchpresent.1forpresent),
                   random=~factorbirdID,
                   data=boldnessdata8,
                   na.method.X="omit",
                   na.method.Y="omit")

summary(fixedeffects1)$varcomp
summary(fixedeffects1, all=T)$coef.fi
# the nestbox construction effects are fairly mild.
# however, having an antenna still matters significantly,
# and there is a similar effect size for not having a 
# perch though it is not significant. However, having 
# an antenna is inextricably linked to the identity of
# the nest box except in 2009, because once the antennas
# went on they never moved. This means I can't realistically
# separate antenna effects from nestbox effects. I have
# written this in the methods.


pin(fixedeffects1, r2withcomp~V1/(V1+V2))
# so the repeatability is very low when compromised
# data is accounted for as a fixed effect.
# also, you could argue that the only relevant factors
# are the major ones (partner behaviour, enter first or second,
# compromised test), and experience as fixed.

# and to be blunt, there is very little change in variance
# because the effect of these nestbox based fixed effects
# is minor compared to things like the influence of the
# partner. So I think I should just acknowledge the 
# other possible fixed effects but say that due to modest
# sample size with each one and no purpose in doing
# further correction since we believe the major things were
# accounted for anyway, they were left out...

# plot residuals against fixed effects:
resids <- fixedeffects1$residuals
plot(factor(boldnessdata8$partnerboldshy), resids)
plot(boldnessdata8$factorentryorder, resids)
plot(boldnessdata8$meanexperience, resids)
plot(boldnessdata8$meanoffspring, resids)
plot(boldnessdata8$imc.experience, resids)
plot(boldnessdata8$imc.offspring, resids)
plot(boldnessdata8$factormethod, resids)
plot(boldnessdata8$sex, resids)
plot(boldnessdata8$factorcomp, resids)
plot(boldnessdata8$perchpresent.1forpresent, resids)
plot(boldnessdata8$antenna.yforpresent, resids)
# these all look pretty good :)


# so let's leave the nestbox side of things out:

# now check variance inflation factors, to see if any
# variables are similar to each other in their effect
# on entry latency:

library(car)
library(lme4)
vif(lm(logentrylatency~factorcomp+partnerboldshy+
         factorentryorder+factormethod+
         relevel(factoryear, ref="2014")+
         zmeanoffspring+zimc.offspring+sex+
         zmeanexperience+zimc.experience, data=boldnessdata8))
# these are all really rather low. Great!

# this is an interesting package that computes
# several summary statistics for whether some
# data fits the assumptions of a linear model:
install.packages("gvlma")
library(gvlma)
test1 <- gvlma(lm(logentrylatency~factorcomp+partnerboldshy+
                    factorentryorder+factormethod+
                    relevel(factoryear, ref="2014")+
                    zmeanoffspring+zimc.offspring+sex+
                    zmeanexperience+zimc.experience, data=boldnessdata8))
summary(test1)
plot(test1, onepage=FALSE)
# how interesting! So the assumption of Kurtosis
# i.e. various forms of peakedness and skew is
# slightly not met. But to be honest that looks
# pretty good to me! The data went in to a gaussian
# mixed model pretty well.

# Also, just as a reminder, all these model fit tests
# are done without random effects. Is there a vif for
# random effects out there?

source("C:/Users/Issie/SkyDrive/RFunctions/vif_lmer_notmycode_20150727.R")


vif.lme(lmer(logentrylatency~factorcomp+partnerboldshy+
               factorentryorder+factormethod+
               relevel(factoryear, ref="2014")+
               zmeanoffspring+zimc.offspring+sex+
               zmeanexperience+zimc.experience+
        (1|factorasrid),data=boldnessdata8))

# coooooool! Year 2011 and the interactions of sex
# and number of offspring are around 2 but 
# it is all quite good.

fixedeffects1.1 <- asreml(fixed=logentrylatency~factorcomp+partnerboldshy+
                          factorentryorder+factormethod+
                          relevel(factoryear, ref="2014")+zmeanoffspring+
                          zimc.offspring+sex+zmeanexperience+zimc.experience,
                          random=~ide(factorasrid, var=T, init=1) +
                            ped(factorasrid, var=T, init=1) + 
                            ide(factorasrdam, var=T, init=1),
                          ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                        data=boldnessdata8,
                        na.method.X="omit",
                        na.method.Y="omit")
# entry order determines whether the variance is partitioned
# towards the individual or towards the genetic variance...
summary(fixedeffects1.1)$varcomp
summary(fixedeffects1.1, all=T)$coef.fi
wald.asreml(fixedeffects1.1, ssType="conditional", denDF="numeric")
pin(fixedeffects1.1, h2withcomp~V2/(V1+V2+V3+V4))

# the nestbox construction effects are fairly mild.
pin(fixedeffects1.1, r2withcomp~(V1+V2+V3)/(V1+V2+V3+V4))
# so the repeatability is very low when compromised
# data is accounted for as a fixed effect.
# also, you could argue that the only relevant factors
# are the major ones (partner behaviour, enter first or second,
# compromised test), and experience as fixed.

# and to be blunt, there is very little change in variance
# because the effect of these nestbox based fixed effects
# is minor compared to things like the influence of the
# partner. So I think I should just acknowledge the 
# other possible fixed effects but say that due to modest
# sample size with each one and no purpose in doing
# further correction since we believe the major things were
# accounted for anyway, they were left out...

# plot residuals against fixed effects:
resids <- fixedeffects1.1$residuals
plot(factor(boldnessdata8$partnerboldshy), resids)
plot(boldnessdata8$factorentryorder, resids)
plot(boldnessdata8$meanexperience, resids)
plot(boldnessdata8$meanoffspring, resids)
plot(boldnessdata8$imc.experience, resids)
plot(boldnessdata8$imc.offspring, resids)
plot(boldnessdata8$factormethod, resids)
plot(boldnessdata8$sex, resids)
plot(boldnessdata8$factorcomp, resids)
# these all look pretty good :)
# it looks alright really.

# test significance of random effects:
# permanent environment
fixedeffects1.1.minPE <- asreml(fixed=logentrylatency~factorcomp+partnerboldshy+
                                  factorentryorder+factormethod+
                                  relevel(factoryear, ref="2014")+
                                  zmeanoffspring+zimc.offspring+sex+
                                  zmeanexperience+zimc.experience,
                          random=~ped(factorasrid, var=T, init=1) + 
                            ide(factorasrdam, var=T, init=1),
                          ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                          data=boldnessdata8,
                          na.method.X="omit",
                          na.method.Y="omit")

1-pchisq(2*(fixedeffects1.1$loglik-fixedeffects1.1.minPE$loglik),1)



# additive genetic:
fixedeffects1.1.minA <- asreml(fixed=logentrylatency~factorcomp+partnerboldshy+
                                 factorentryorder+factormethod+
                                 relevel(factoryear, ref="2014")+
                                 zmeanoffspring+zimc.offspring+sex+
                                 zmeanexperience+zimc.experience,
                          random=~ide(factorasrid, var=T, init=1) +
                            ide(factorasrdam, var=T, init=1),
                          ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                          data=boldnessdata8,
                          na.method.X="omit",
                          na.method.Y="omit")

1-pchisq(2*(fixedeffects1.1$loglik-fixedeffects1.1.minA$loglik),1)


# minus maternal effects
fixedeffects1.1.minM <- asreml(fixed=logentrylatency~factorcomp+partnerboldshy+
                                 factorentryorder+factormethod+
                                 relevel(factoryear, ref="2014")+
                                 zmeanoffspring+zimc.offspring+sex+
                                 zmeanexperience+zimc.experience,
                               random=~ide(factorasrid, var=T, init=1) +
                                 ped(factorasrid, var=T, init=1),
                               ginverse=list(factorasrid=asrpinv),
                               data=boldnessdata8,
                               na.method.X="omit",
                               na.method.Y="omit")

1-pchisq(2*(fixedeffects1.1$loglik-fixedeffects1.1.minM$loglik),1)


##################

# but if I exclude the compromised data instead...

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


# now, with the NEW and RESCALED data set, run the same models:

vif.lme(lmer(logentrylatency~partnerboldshy+
               factorentryorder+factormethod+
               relevel(factoryear, ref="2014")+
               zmeanoffspring+zimc.offspring+sex+
               zmeanexperience+zimc.experience+
               (1|factorasrid),data=boldnessdatamincomp.partner))
# pretty similar to the previous model. Good!

test2 <- gvlma(lm(logentrylatency~partnerboldshy+
                    factorentryorder+factormethod+
                    relevel(factoryear, ref="2014")+
                    zmeanoffspring+zimc.offspring+sex+
                    zmeanexperience+zimc.experience,
                  data=boldnessdatamincomp.partner))
summary(test2)
plot(test2, onepage=FALSE)
# nice! not bad :)


# I put this model in Table S1 of the supplement:

fixedeffects2 <- asreml(fixed=logentrylatency~partnerboldshy+
                          factorentryorder+factormethod+
                          relevel(factoryear, ref="2014")+
                          zmeanoffspring+zimc.offspring+sex+
                          zmeanexperience+zimc.experience,
                        random=~ide(factorasrid, var=T, init=1) +
                          ped(factorasrid, var=T, init=1) + 
                          ide(factorasrdam, var=T, init=1),
                        ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                        data=boldnessdatamincomp.partner,
                        na.method.X="omit",
                        na.method.Y="omit")

summary(fixedeffects2)$varcomp
summary(fixedeffects2, all=T)$coef.fi
wald.asreml(fixedeffects2, ssType="conditional", denDF="numeric")
pin(fixedeffects2, h2nocomp~V2/(V1+V2+V3+V4))
pin(fixedeffects2, r2nocomp~(V1+V2+V3)/(V1+V2+V3+V4))
# repeatability is good.

# plot residuals against fixed effects:
resids <- fixedeffects2$residuals
plot(factor(boldnessdatamincomp.partner$partnerboldshy), resids)
plot(boldnessdatamincomp.partner$factorentryorder, resids)
plot(boldnessdatamincomp.partner$testorder, resids)
plot(boldnessdatamincomp.partner$factormethod, resids)
plot(boldnessdatamincomp.partner$sex, resids)
plot(boldnessdatamincomp.partner$meanexperience, resids)
plot(boldnessdatamincomp.partner$meanoffspring, resids)
plot(boldnessdatamincomp.partner$imc.experience, resids)
plot(boldnessdatamincomp.partner$imc.offspring, resids)



# sample sizes:
length(unique(boldnessdatamincomp.partner$asrid))
length(boldnessdatamincomp.partner$asrid)


### need to know significance:
fixedeffects2.PE <- asreml(fixed=logentrylatency~partnerboldshy+
                          factorentryorder+factormethod+
                          relevel(factoryear, ref="2014")+
                          zmeanoffspring+zimc.offspring+sex+
                          zmeanexperience+zimc.experience,
                        random=~ped(factorasrid, var=T, init=1) + 
                          ide(factorasrdam, var=T, init=1),
                        ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                        data=boldnessdatamincomp.partner,
                        na.method.X="omit",
                        na.method.Y="omit")

1-pchisq(2*(fixedeffects2$loglik-fixedeffects2.PE$loglik),1)


fixedeffects2.A <- asreml(fixed=logentrylatency~partnerboldshy+
                          factorentryorder+factormethod+
                          relevel(factoryear, ref="2014")+
                          zmeanoffspring+zimc.offspring+sex+
                          zmeanexperience+zimc.experience,
                        random=~ide(factorasrid, var=T, init=1) +
                          ide(factorasrdam, var=T, init=1),
                        ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                        data=boldnessdatamincomp.partner,
                        na.method.X="omit",
                        na.method.Y="omit")

1-pchisq(2*(fixedeffects2$loglik-fixedeffects2.A$loglik),1)


fixedeffects2.M <- asreml(fixed=logentrylatency~partnerboldshy+
                          factorentryorder+factormethod+
                          relevel(factoryear, ref="2014")+
                          zmeanoffspring+zimc.offspring+sex+
                          zmeanexperience+zimc.experience,
                        random=~ide(factorasrid, var=T, init=1) +
                          ped(factorasrid, var=T, init=1),
                        ginverse=list(factorasrid=asrpinv),
                        data=boldnessdatamincomp.partner,
                        na.method.X="omit",
                        na.method.Y="omit")

1-pchisq(2*(fixedeffects2$loglik-fixedeffects2.M$loglik),1)



# entry order and partner behaviour matter so much.
# I wonder whether having a bold partner makes you
# enter sooner?
table(boldnessdata8$partnerboldshy, boldnessdata8$birdorderentry)
# well duh of course it makes you enter second - the partner
# got there first











#################################################################
#################################################################
# Figures for boldness.

# Histogram of the distribution of boldness data:
# split by sex, and include individuals that never
# entered the nest box:

# excluding compromised data for the main text:


par(mar=c(5,5,1,1))
hist(fboldmincomp2$entrylatency, ylim=c(0,30),
     breaks=seq(0,50,0.5), col=rgb(1,0,0,0.5),
     xlab="Entry latency (minutes)",
     cex.lab=1.8, cex.axis=1.5, main="")
hist(mboldmincomp2$entrylatency, ylim=c(0,30),
     breaks=seq(0,50,0.5), col=rgb(0,0,1,0.5),
     cex.lab=1.8, cex.axis=1.5,main="", add=T)
legend(x=30, y=30, legend=c("Female","Male"),
       fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)),
       cex=1.5)
lines(x=c(5,5), y=c(0,30), col=rgb(1,0,0,0.5), lwd=3, lty=2)
lines(x=c(13.9,13.9), y=c(0,30), col=rgb(0,0,1,0.5), lwd=3, lty=2)

par(mar=c(5,4,4,2))



# including compromised data for the supplement:

par(mar=c(5,5,1,1))
hist(fbold6$entrylatency, ylim=c(0,32),
     breaks=seq(0,60,0.5), col=rgb(1,0,0,0.5),
     xlab="Entry latency (minutes)",
     cex.lab=1.8, cex.axis=1.5, main="")
hist(mbold6$entrylatency, ylim=c(0,32),
     breaks=seq(0,60,0.5), col=rgb(0,0,1,0.5),
     cex.lab=1.8, cex.axis=1.5,main="", add=T)
legend(x=40, y=30, legend=c("Female","Male"),
       fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)),
       cex=1.5)
lines(x=c(5,5), y=c(0,32), col=rgb(1,0,0,0.5), lwd=3, lty=2)
lines(x=c(13.9,13.9), y=c(0,32), col=rgb(0,0,1,0.5), lwd=3, lty=2)

par(mar=c(5,4,4,2))





##############################################################
##############################################################

# a bivariate analysis

# this has male and female latency at the same time as 
# the response variables and thanks to Loeske Kruuk's advice
# I can now fit the fixed effect of compromised on one or
# other trait and not both:

# in terms of covariance, no male is tested for female latency
# and no female is tested for male latency. So if I fit a within-
# individual covariance I am fitting the male or female's individual
# ability to change their partner? but isn't this fitted with a
# diag() structure? Not truly. This is because pairs will be measured
# at multiple points in time, and individuals may also change partners,
# which means there will be a change in male and female phenotype
# that follows the ID of the male and female even when partner
# IDs change. However. The power to pull apart these effects will
# depend on the number of measures that I have for each male and
# female 

videoinfocomp$factorfcomp <- as.factor(videoinfocomp$fvidcomp)
videoinfocomp$factormcomp <- as.factor(videoinfocomp$mvidcomp)
table(videoinfocomp$factorfcomp)
table(videoinfocomp$factormcomp)



bivariatewithcomp1 <- asreml(fixed=cbind(flatencywithcomp, mlatencywithcomp)~trait+
                        at(trait,2):factormcomp+at(trait,1):factorfcomp+
                          at(trait,2):mentryorder+at(trait,1):fentryorder,
                      random=~us(trait):factormum+
                        us(trait):factordad,
                      rcov=~units:us(trait),
                      data=videoinfocomp,
                      maxiter=5000,
                      na.method.Y="include",
                      na.method.X="include")

summary(bivariatewithcomp1)$varcomp
summary(bivariatewithcomp1, all=T)$coef.fi
# so, this is very difficult to interpret. Males appear
# to not have much repeatability or much influence over
# females here, whereas the opposite is true for females.
# Is this a distortion that comes from the compromised
# data?

# if I try to do this model with all us() variance structures
# and no fixed effects, the model does not converge after 50k
# iterations. I suspect there is insufficient information from
# birds that swap pairs for the model to converge on an answer
# without further information (eg from fixed effects). Therefore
# I have diagonalised the male and female IDs so that the model
# does not try to work out within-individual covariance. I have
# kept the covariance structure for the residual so that I will
# know the error.

bivariatenocomp1 <- asreml(fixed=cbind(flatency, mlatency)~trait,
                             random=~us(trait, var=T, init=c(2,0.1,2)):factormum+
                               us(trait, var=T, init=c(2,0.1,2)):factordad,
                             rcov=~units:us(trait, var=T, init=c(2,0.1,2)),
                             data=videoinfocomp,
                             maxiter=1000000,
                             na.method.Y="include",
                             na.method.X="include")

# even after one million iterations, this did not converge.
# The one including compromised data easily converged. This
# is because of more data and therefore more birds that switched
# partners. The compromised behaviour may be more correlated 
# as well because the compromising event may promote similarity.


summary(bivariatenocomp1)$varcomp
summary(bivariatenocomp1, all=T)$coef.fi

# the significance of the covariances...

bivariatenocomp1.minF <- asreml(fixed=cbind(flatency, mlatency)~trait,
                           random=~diag(trait, var=T, init=c(1,1)):factormum+
                             us(trait, var=T, init=c(2,0.1,2)):factordad,
                           rcov=~units:us(trait, var=T, init=c(2,0.1,2)),
                           data=videoinfocomp,
                           maxiter=1000000,
                           na.method.Y="include",
                           na.method.X="include")

1-pchisq(2*(bivariatenocomp1$loglik-bivariatenocomp1.minF$loglik),1)


bivariatenocomp1.minM <- asreml(fixed=cbind(flatency, mlatency)~trait,
                                random=~diag(trait, var=T, init=c(1,1)):factordad+
                                  diag(trait, var=T, init=c(1,1)):factormum,
                                rcov=~units:us(trait, var=T, init=c(2,0.1,2)),
                                data=videoinfocomp,
                                maxiter=1000000,
                                na.method.Y="include",
                                na.method.X="include")

1-pchisq(2*(bivariatenocomp1$loglik-bivariatenocomp1.minM$loglik),1)
summary(bivariatenocomp1.minM)$varcomp

##### HOWEVER! There is an argument to scale both male
# and female boldness so that the covariances are on
# comparable scales!

videoinfocomp$zflatency <- scale(videoinfocomp$flatency)
videoinfocomp$zmlatency <- scale(videoinfocomp$mlatency)

scaledbivariatenocomp1 <- asreml(fixed=cbind(zflatency, zmlatency)~trait,
                           random=~us(trait, var=T, init=c(2,1,2)):factormum+
                             us(trait, var=T, init=c(2,1,2)):factordad,
                           rcov=~units:us(trait, var=T, init=c(2,1,2)),
                           data=videoinfocomp,
                           maxiter=1000000,
                           na.method.Y="include",
                           na.method.X="include")
# this truly fails... Properly terminates. I can't use this.


# so, what if I put in a fixed effect? Say, experience
# of the male or female:
videoinfocomp$f.ind.order <- fboldmincomp2$imc.experience[match
                        (videoinfocomp$DVDNumber,fboldmincomp2$videoname)]
summary(videoinfocomp$f.ind.order)

videoinfocomp$m.ind.order <- mboldmincomp2$imc.experience[match
                        (videoinfocomp$DVDNumber,mboldmincomp2$videoname)]
summary(videoinfocomp$m.ind.order)

bivariatenocomp2 <- asreml(fixed=cbind(flatency, mlatency)~trait+
                             at(trait,1):f.ind.order+at(trait,2):m.ind.order,
                           random=~us(trait, var=T, init=c(2,0.1,2)):factormum+
                             us(trait, var=T, init=c(2,0.1,2)):factordad,
                           rcov=~units:us(trait, var=T, init=c(2,0.1,2)),
                           data=videoinfocomp,
                           maxiter=100000,
                           na.method.Y="include",
                           na.method.X="include")

# ok this doesn't want to converge either, so I guess it is
# not a problem that can be solved easily. For some reason,
# including whether a bird entered first or second led to 
# convergence. Perhaps because factors are easier than linear
# effects to fit, or because it is very informative with
# respect to model fit.

bivariatenocomp3 <- asreml(fixed=cbind(flatency, mlatency)~trait+
                               at(trait,2):mentryorder+at(trait,1):fentryorder,
                             random=~us(trait):factormum+
                               us(trait):factordad,
                             rcov=~units:us(trait),
                             data=videoinfocomp,
                             maxiter=5000,
                             na.method.Y="include",
                             na.method.X="include")

summary(bivariatenocomp3)$varcomp



# is entering first or second actually part of whether
# a bird is bold or shy rather than whether the bird
# has an influence from the partner or not?

bivariatewithcomp2 <- asreml(fixed=cbind(flatencywithcomp, mlatencywithcomp)~trait+
                               at(trait,2):mentryorder+at(trait,1):fentryorder,
                             random=~us(trait):factormum+
                               us(trait):factordad,
                             rcov=~units:us(trait),
                             data=videoinfocomp,
                             maxiter=5000,
                             na.method.Y="include",
                             na.method.X="include")

summary(bivariatewithcomp2)$varcomp
summary(bivariatewithcomp2, all=T)$coef.fi
# males are a lot more repeatable in this model and it
# is the females that affect the males apparently! But 
# that can just as easily be male investment for the
# specific female as the female influencing the male
# specifically. Either way, it seems that the female
# is driving the expression of behaviour in these relationships.
# What seems quite clear is that the female isn't varying
# to the ID of the male. She covaries her behaviour with
# what he does, but there is no repeatable part from male
# ID on the female's behaviour. Perhaps this is because
# so many males never enter. Does it matter if the female
# is then classed as entering first because females have
# a high likelihood of being first?

# You could argue that entering first or second doesn't
# matter, because whilst we are trying to consider the
# influence of one bird on the other at that moment in
# time, this would be to discount everything these two
# birds have done together prior to this point, so in a
# way it is silly.

# I can do an animal model: all the data is there. But 
# the animal model mostly goes to boundary. As a result,
# what do we really find out? I don't think we know when
# this much is missing from the model.


bivariateanimal2 <- asreml(fixed=cbind(flatencywithcomp, mlatencywithcomp)~trait+
                             at(trait,2):mvidcomp+at(trait,1):fvidcomp,
                           random=~us(trait):ide(factorasrdad)+
                             us(trait):ide(factorasrmum) +
                             us(trait):ped(factorasrdad)+
                             us(trait):ped(factorasrmum),
                           rcov=~units:diag(trait),
                           ginverse=list(factorasrdad=asrpinv, factorasrmum=asrpinv),
                           data=videoinfo2,
                           maxiter=500,
                           na.method.Y="include",
                           na.method.X="include")

summary(bivariateanimal2)$varcomp


# no, I don't think we understand much from the quantitative
# genetic model. It comes out as low genetic influence on both
# and given the low power to detect a genetic effect in this
# data set anyway I think it is not reasonable to expect this
# much from the data.

# 27th July 2015
# and Julia suggests that, if there is no genetic influence on the 
# univariate traits, then there is no point looking for a genetic
# covariance because there won't be anything there.


##########################################################
##########################################################
# checkup on whether year should be random,
# as recommended by Eryn at ESEB 2015

str(boldnessdatamincomp2$factoryear)

boldyear1 <- asreml(fixed=logentrylatency~1,
                    random=~ide(factorasrid, var=T, init=1) +
                      ped(factorasrid, var=T, init=1) + 
                      ide(factorasrdam, var=T, init=1)+
                      factoryear,
                    ginverse=list(factorasrid=asrpinv, 
                                  factorasrdam=asrpinv),
                    data=boldnessdatamincomp2,
                    na.method.X="omit",
                    na.method.Y="omit")

plot(boldyear1)
summary(boldyear1)$varcomp
summary(boldanimalmincomp1)$varcomp
# not much change to the Vpe term, the pedigree
# term is down a little and the maternal effect
# term is estimated in this model, which is nice.
# Seems like year takes a little from the additive
# genetic estimate, but it wasn't significant, and
# my conclusions aren't changing as a result.

##########################################################
##########################################################
# how many males and how many females changed partners?

summary(videoinfocomp$flatency)

partnerpairs1 <- subset(videoinfocomp, videoinfocomp$flatency!="NA")

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
