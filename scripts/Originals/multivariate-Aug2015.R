# 7th August 2015
# Isabel Winney

# I need a trivariate data frame to investigate whether
# boldness, exploration, and nestling activity covary.

# please load the data sets created with the boldness,
# activity, and exploration scripts
# boldness-june2015.R
# exploration-july2015.R
# activity-August2015.R


# give all observations a unique label:

tent2$labels <- seq(1, length(tent2$asrid), 1)
summary(tent2$labels)

boldnessdatamincomp2$labels <- seq(length(tent2$asrid)+1, 
                      length(tent2$asrid)+length(boldnessdatamincomp2$asrid), 1)
summary(boldnessdatamincomp2$labels)

ar1$labels <- seq(length(tent2$asrid)+length(boldnessdatamincomp2$asrid)+1, 
                  length(tent2$asrid)+length(boldnessdatamincomp2$asrid)+
                    length(ar1$asrid), 1)
summary(ar1$labels)



# make my trivariate data frame
tri <- as.data.frame(seq(1, max(ar1$labels), 1))
names(tri) <- "labels"
head(tri)
length(tri$labels)

# and add the bird ids
for (i in 1:length(tri$labels)){
  tri$asrid[i] <- ifelse(tri$labels[i]<min(boldnessdatamincomp2$labels), 
                        tent2$asrid[match(tri$labels[i], tent2$labels)],
                        ifelse(tri$labels[i]<min(ar1$labels),
                               boldnessdatamincomp2$asrid[match(tri$labels[i], 
                                                                boldnessdatamincomp2$labels)],
                               ar1$asrid[match(tri$labels[i], ar1$labels)]))
}

summary(tri$asrid)
which(tri$asrid==tri$labels)
tri[300,]
tent2[which(tent2$labels==300),"asrid"]
# good :)

# great!


# now pull out the dependent variables

# logfr
tri$logfr <- tent2$logfr[match(tri$labels, tent2$labels)]
summary(tri$logfr)
summary(tent2$logfr)

# total flights and runs
tent2$total <- tent2$flights + tent2$x2r
tri$fr <- tent2$total[match(tri$labels, tent2$labels)]
summary(tri$fr)
summary(tent2$total)

# logentrylatency
tri$logentrylatency <- boldnessdatamincomp2$logentrylatency[match(tri$labels, 
                                                                  boldnessdatamincomp2$labels)]
summary(tri$logentrylatency)
summary(boldnessdatamincomp2$logentrylatency)

# entry latency
tri$entrylatency <- boldnessdatamincomp2$entrylatency[match(tri$labels, 
                                                            boldnessdatamincomp2$labels)]
summary(tri$entrylatency)
summary(boldnessdatamincomp2$entrylatency)

# logtot
tri$logtot <- ar1$logtot[match(tri$labels, ar1$labels)]
summary(tri$logtot)
summary(ar1$logtot)

# total
tri$total <- ar1$total[match(tri$labels, ar1$labels)]
summary(tri$total)
summary(ar1$total)

# set asrid to be a factor:

tri$factorasrid <- as.factor(tri$asrid)

##################################################

# see the figures script for the cross plots comparing
# the behaviours within each bird
# Figures-maintext-Aug2015.R

##################################################
##################################################

# discovered package MVN. This tests whether a
# distribution is multivariate normal.
# So I can use this package to test the normality
# of my trivariate model.

library(MVN)

names(tri)
mardiaTest(tri[,10:12], qqplot=T)

# ok. I guess this package doesn't work with
# non-overlapping data frames...

##################################################
##################################################





# scale and centre my y-variables:
tri$zentry <- scale(tri$logentrylatency)
tri$zlogfr <- scale(tri$logfr)
tri$zlogtot <- scale(tri$logtot)

summary(tri)

trivariate1 <- asreml(fixed=cbind(zentry, zlogfr, zlogtot)~trait,
                  random=~us(trait,init=c(1,0.1,1,0.1,0.1,1)):ide(factorasrid),
                  rcov= ~ units:diag(trait,init=c(0.1,0.1,0.1)),
                  data=tri,
                  ginverse=list(factorasrid=asrpinv),
                  maxiter=100,
                  na.method.Y="include",
                  na.method.X="include")

summary(trivariate1)$varcomp
summary(trivariate1, all=T)$coef.fi

# but not in the model.

### test the significances of each covariance by fixing
# each covariance in turn to zero

# first run a model to keep the starting values
trivariate.Gparam <- asreml(fixed=cbind(zentry, zlogfr, zlogtot)~trait,
                      random=~us(trait,init=c(1,0.1,1,0.1,0.1,1)):ide(factorasrid),
                      rcov= ~ units:diag(trait,init=c(0.1,0.1,0.1)),
                      data=tri,
                      ginverse=list(factorasrid=asrpinv),
                      maxiter=100,
                      na.method.Y="include",
                      na.method.X="include",
                      start.values=T)

# put those starting values in a table and ensure the
# R!variance is fixed at one (it doesn't do anything
# in these models)
ftable <- trivariate.Gparam$gammas.table
ftable[,3] <- "U"
ftable[7,2] <- 1
ftable[7,3] <- "F"
ftable


# now make three sets of G parameter tables where
# a given covariance is set at zero (be for boldness-
# exploration, for example):
ftablebe <- ftable
ftableba <- ftable
ftableae <- ftable

# constrain boldness:exploration
ftablebe[2,2] <- 0
ftablebe[2,3] <- "F"

# constrain boldness:activity
ftableba[4,2] <- 0
ftableba[4,3] <- "F"

# constrain activity:exploration
ftableae[5,2] <- 0
ftableae[5,3] <- "F"

# check the specifications:
ftablebe
ftableba
ftableae
# looking good!


# now, re-run the model and constrain
# one covariance at a time:

# boldness:exploration
trivariate1.be <- asreml(fixed=cbind(zentry, zlogfr, zlogtot)~trait,
                      random=~us(trait,init=c(1,0.1,1,0.1,0.1,1)):ide(factorasrid),
                      rcov= ~ units:diag(trait,init=c(0.1,0.1,0.1)),
                      data=tri,
                      ginverse=list(factorasrid=asrpinv),
                      maxiter=100,
                      na.method.Y="include",
                      na.method.X="include",
                      G.param=ftablebe)

summary(trivariate1.be)$varcomp
summary(trivariate1.be, all=T)$coef.fi

# significance:
1-pchisq(2*(trivariate1$loglik-trivariate1.be$loglik),1)



# boldness:activity


trivariate1.ba <- asreml(fixed=cbind(zentry, zlogfr, zlogtot)~trait,
                         random=~us(trait,init=c(1,0.1,1,0.1,0.1,1)):ide(factorasrid),
                         rcov= ~ units:diag(trait,init=c(0.1,0.1,0.1)),
                         data=tri,
                         ginverse=list(factorasrid=asrpinv),
                         maxiter=100,
                         na.method.Y="include",
                         na.method.X="include",
                         G.param=ftableba)

summary(trivariate1.ba)$varcomp
summary(trivariate1.ba, all=T)$coef.fi

# significance:
1-pchisq(2*(trivariate1$loglik-trivariate1.ba$loglik),1)



# activity:exploration


trivariate1.ae <- asreml(fixed=cbind(zentry, zlogfr, zlogtot)~trait,
                         random=~us(trait,init=c(1,0.1,1,0.1,0.1,1)):ide(factorasrid),
                         rcov= ~ units:diag(trait,init=c(0.1,0.1,0.1)),
                         data=tri,
                         ginverse=list(factorasrid=asrpinv),
                         maxiter=100,
                         na.method.Y="include",
                         na.method.X="include",
                         G.param=ftableae)

summary(trivariate1.ae)$varcomp
summary(trivariate1.ae, all=T)$coef.fi

# significance:
1-pchisq(2*(trivariate1$loglik-trivariate1.ae$loglik),1)



### phenotypic correlations?
# covariance * sqrt ((between-indiv var / all var trait 1) * (between-indiv var / all var trait 2))

# boldness and exploration
pin(trivariate1, covs~V2*sqrt((V1/(V1+V8))*(V3/(V3+V9))))

# boldness and activity
pin(trivariate1, covs~V4*sqrt((V1/(V1+V8))*(V6/(V6+V10))))

# exploration and activity
pin(trivariate1, covs~V5*sqrt((V3/(V3+V9))*(V6/(V6+V10))))


############################################################

# out of curiosity, can there be genetic covariance
# without phenotypic covariance?


tri$asrdam <- asrp1x$dam[match(tri$asrid, asrp1x$id)]

tri$factorasrdam <- as.factor(tri$asrdam)

trivariate.va <- asreml(fixed=cbind(zentry, zlogfr, zlogtot)~trait,
                      random=~us(trait,init=c(1,0.1,1,0.1,0.1,1)):ide(factorasrid) +
                        us(trait,init=c(1,0.1,1,0.1,0.1,1)):ped(factorasrid) +
                        us(trait,init=c(1,0.1,1,0.1,0.1,1)):ide(factorasrdam),
                      rcov= ~ units:diag(trait,init=c(0.1,0.1,0.1)),
                      data=tri,
                      ginverse=list(factorasrid=asrpinv,
                                    factorasrdam=asrpinv),
                      maxiter=5000,
                      na.method.Y="include",
                      na.method.X="include")

summary(trivariate.va)$varcomp
summary(trivariate.va, all=T)$coef.fi
# it doesn't converge, but in essence no. There isn't
# a genetic correlation going on here. There is a small 
# genetic correlation between activity and exploration
# but I just don't quite trust it when the model is
# unable to converge like this.

###############################################

###############################################

# what are the sample sizes for each comparison?

# to find this out, use the data set 'cross1'
# made for plotting the between individual correlations
# in script 'Figures-maintext-Aug2015.R'

head(cross1)

# this data set has the individual mean, minima
# and maxima for whenever an individual was 
# measured for a trait. So, to get the sample
# sizes we look at how many individuals have
# measures for two traits:

summary(cross1$tmean + cross1$bmean)
length(which(is.na(cross1$tmean + cross1$bmean)))

# so to get the number of individuals measured
# for both exploration and boldness, we take these
# NA values from the total number of individuals
# measured:
length(cross1[,1]) - length(which(is.na(cross1$tmean + cross1$bmean)))

# exploration and activity:
length(cross1[,1]) - length(which(is.na(cross1$tmean + cross1$amean)))

# boldness and activity:
length(cross1[,1]) - length(which(is.na(cross1$amean + cross1$bmean)))
