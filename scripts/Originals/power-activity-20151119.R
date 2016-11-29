# 31st July 2015
# power analysis for the activity data set from
# 2011-2013

# the code for generating the pedigree with unknown dams
# filled in is importPedigree-June2015.R and the pedigree
# to use from this is asrp1x, which is fixed and turned
# in to the matrix asrpinv in the aforementioned code

# the fixed pedigree is asrp2x

# my measured individuals are as follows:

# the variable keep marks out my measured individuals
# with a 1, and my unmeasured individuals with 0

ar1$keep <- 1
asrp1x$actikeep <- ar1$keep[match(asrp1x$id, ar1$asrid)]
asrp1x$actikeep[is.na(asrp1x$actikeep)] <- 0
birds <- unique(ar1$asrid)
birds <- as.data.frame(birds)
birds$natal <- ar1$natal[match(birds$birds, ar1$asrid)]
head(birds)
str(birds)
str(asrp1x$actikeep)
table(asrp1x$actikeep)
length(unique(ar1$asrid))
length(unique(ar1$birdid))


# natal brood as a shared environmental variable

asrp1x$actinatal <- ar1$natal[match(asrp1x$id, ar1$asrid)]
table(table(asrp1x$actinatal))
summary(asrp1x$actinatal)
length(asrp1x$actinatal)
# 515 mums filled in --> right for number of birds


# make the data frames to store the variables

heritabilities.a <- seq(0, 0.3, by=0.02)

natalv <- seq(0,0.3, by=0.05)

n <- 1000

p_values.acti <- array(dim=c(n, length(heritabilities.a), length(natalv)))

power.acti <-array(dim=c(length(heritabilities.a), length(natalv)))

h2.acti <- array(dim=c(n, length(heritabilities.a), length(natalv)))

r2.acti <- array(dim=c(n, length(heritabilities.a), length(natalv)))



# I need birds

# i need the broods
natal.power <- unique(ar1$natal)
natal.power <- as.data.frame(natal.power)
head(natal.power)

# then I run two models, one including and one excluding
# a pedigree term, to determine the heritability of the trait
# and the log likelihoods of the models

# first, set a working directory to save images of the 
# workspace:
setwd("C:/Users/Issie/SkyDrive/PhD/Chapter1-heritability/ASReml/afterThesis")



for(j in 1:length(heritabilities.a)){
  for(i in 1:n){
    
    # simulate phenotypes over the whole (fixed) pedigree
    simphen <- phensim(asrp2x, 
                       randomA=heritabilities.a[j],
                       randomE=1-heritabilities.a[j])$phenotypes
    
    # I only want the trait values for the birds that I have sampled
    simphen$sampled <- asrp1x$actikeep[match(simphen$id, asrp1x$id)]
    simphen1 <- subset(simphen, simphen$sampled==1)
    simphen1 <- data.frame(simphen1)
    
    simphen1$zero <- 0
    simphen1$asrid <- as.factor(simphen1$id)
    
    for(k in 1:length(natalv)){
      
      natal.power$natvar <- rnorm(length(natal.power[,1]), 0, sqrt(natalv[k]))
      
      simphen1$natal <- birds$natal[match(simphen1$asrid, birds$birds)]
      
      simphen1$natvar <- natal.power$natvar[
        match(simphen1$natal, natal.power[,1])]
      
      simphen1$trait <- simphen1$trait_1 + simphen1$natvar
      
      simphen1$natal <- as.factor(simphen1$natal)
      
      # models - a genetic effect of individual and
      # a phenotypic effect of brood
      power1 <- asreml(fixed=trait~1,
                       random=~ped(asrid, var=T, init=1) +
                         natal,
                       data=simphen1,
                       ginverse=list(asrid=asrpinv),
                       na.method.X="omit",
                       na.method.Y="omit",
                       trace=F,
                       maxiter=50)
      # NOTE: trace=F is a new addition on 15th June 2015
      # to try and reduce the amount of printout that R has
      # to buffer when running this power analysis. I hope
      # this sorts the problem with RStudio randomly crashing
      # during this loop (and not being up to date with the
      # printout at the point of crashing)
      
      power0 <- asreml(fixed=trait~1,
                       random=~natal,
                       data=simphen1,
                       na.method.X="omit",
                       na.method.Y="omit",
                       trace=F,
                       maxiter=50)
      
      p_values.acti[i,j,k] <- 1-pchisq(2*(power1$loglik-power0$loglik),1)
      h2.acti[i,j,k] <- summary(power1)$varcomp[1,3]/
        (summary(power1)$varcomp[1,3]+summary(power1)$varcomp[2,3]+
           summary(power1)$varcomp[3,3])
      
      r2.acti[i,j,k] <- summary(power1)$varcomp[2,3]/
        (summary(power1)$varcomp[1,3]+summary(power1)$varcomp[2,3]+
           summary(power1)$varcomp[3,3])
      
      print(paste("i=",i,"j=",j,"k=",k))
      
    }
    
  }
  
  # save after every 1000 iterations for each heritability
  save.image("actipower4August2015.RData")
  
}

# set directory back to preferred:
setwd("C:/Users/Issie/SkyDrive/PhD/masterdatasheets")


# this loop completed with no crashes on the night of
# 4th - 5th August






for(j in 1:length(heritabilities.a)){
  for(k in 1:length(natalv)){
    power.acti[j,k]<-table(p_values.acti[,j,k]<0.05)["TRUE"]/n
  }
}


power.acti[is.na(power.acti)] <- 0

power.acti

# I need to make a data frame with x = heritabilities,
# y = natalv, z = power

plot.power.act <- c(power.acti[1,], power.acti[2,], power.acti[3,], power.acti[4,],
                power.acti[5,], power.acti[6,], power.acti[7,], power.acti[8,],
                power.acti[9,], power.acti[10,], power.acti[11,], power.acti[12,],
                power.acti[13,], power.acti[14,], power.acti[15,], power.acti[16,])
plot.power.act <- as.data.frame(plot.power.act)
plot.power.act$h2 <- rep(heritabilities.a, each=7)
plot.power.act$natv <- rep(natalv, times=16)
plot.power.act


# now I cam make a 3d plot of my data!!!
# this is part of figure S1
library(lattice)
wireframe(plot.power.act~natv*h2, data=plot.power.act,
          scales=list(arrows=F, xlim=c(0,1), cex=1.5),
          xlab=list(label="    maternal 
                    effect variance",
                    cex=2),
          ylab=list(label="heritability    ",
                    cex=2),
          zlab=list(label="power - nestling activity",
                    cex=2, rot=90),
          zlim=c(0:1))
# resolution on 5th August 2015 - 700x700 pixels


library(rgl)
plot3d(x=plot.power$natv,
       y=plot.power$h2,
       z=plot.power$plot.power)

library(nadiv) # to use aiCI()

# false positives
plot(plot.power[1:7,3], plot.power[1:7,1],
     xlab="maternal effect variance",
     ylab="type I error - activity",
     pch=16, cex.lab=1.2, cex.axis=1.2)

actity1 <- plot.power[1:7,1]





###################################################
###################################################
# for reference, run the same analysis with
# 1) only day 10 individuals
# 2) only day 12 individuals


# determine which individuals were measured
# on days 10 or 12:

ar10$keep <- 1
asrp1x$actikeep10 <- ar10$keep[match(asrp1x$id, ar10$asrid)]
asrp1x$actikeep10[is.na(asrp1x$actikeep10)] <- 0
birds10 <- unique(ar10$asrid)
birds10 <- as.data.frame(birds10)
birds10$natal <- ar10$natal[match(birds10$birds, ar10$asrid)]
head(birds10)
str(birds10)
str(asrp1x$actikeep10)
table(asrp1x$actikeep10)
length(unique(ar10$asrid))
length(unique(ar10$birdid))


ar12$keep <- 1
asrp1x$actikeep12 <- ar12$keep[match(asrp1x$id, ar12$asrid)]
asrp1x$actikeep12[is.na(asrp1x$actikeep12)] <- 0
birds12 <- unique(ar12$asrid)
birds12 <- as.data.frame(birds12)
birds12$natal <- ar12$natal[match(birds12$birds, ar12$asrid)]
head(birds12)
str(birds12)
str(asrp1x$actikeep12)
table(asrp1x$actikeep12)
length(unique(ar12$asrid))
length(unique(ar12$birdid))



# first, day 10:




p_values.acti10 <- array(dim=c(n, length(heritabilities.a), length(natalv)))

power.acti10 <-array(dim=c(length(heritabilities.a), length(natalv)))

h2.acti10 <- array(dim=c(n, length(heritabilities.a), length(natalv)))

r2.acti10 <- array(dim=c(n, length(heritabilities.a), length(natalv)))



# I need birds

# i need the broods
natal.power10 <- unique(ar10$natal)
natal.power10 <- as.data.frame(natal.power10)
head(natal.power10)

# then I run two models, one including and one excluding
# a pedigree term, to determine the heritability of the trait
# and the log likelihoods of the models

# first, set a working directory to save images of the 
# workspace:
setwd("C:/Users/Issie/SkyDrive/PhD/Chapter1-heritability/ASReml/afterThesis")



for(j in 1:length(heritabilities.a)){
  for(i in 1:n){
    
    # simulate phenotypes over the whole (fixed) pedigree
    simphen <- phensim(asrp2x, 
                       randomA=heritabilities.a[j],
                       randomE=1-heritabilities.a[j])$phenotypes
    
    # I only want the trait values for the birds that I have sampled
    simphen$sampled <- asrp1x$actikeep10[match(simphen$id, asrp1x$id)]
    simphen1 <- subset(simphen, simphen$sampled==1)
    simphen1 <- data.frame(simphen1)
    
    simphen1$zero <- 0
    simphen1$asrid <- as.factor(simphen1$id)
    
    for(k in 1:length(natalv)){
      
      natal.power10$natvar <- rnorm(length(natal.power10[,1]), 0, sqrt(natalv[k]))
      
      simphen1$natal <- birds10$natal[match(simphen1$asrid, birds10$birds10)]
      
      simphen1$natvar <- natal.power10$natvar[
        match(simphen1$natal, natal.power10[,1])]
      
      simphen1$trait <- simphen1$trait_1 + simphen1$natvar
      
      simphen1$natal <- as.factor(simphen1$natal)
      
      # models - a genetic effect of individual and
      # a phenotypic effect of brood
      power1 <- asreml(fixed=trait~1,
                       random=~ped(asrid, var=T, init=1) +
                         natal,
                       data=simphen1,
                       ginverse=list(asrid=asrpinv),
                       na.method.X="omit",
                       na.method.Y="omit",
                       trace=F,
                       maxiter=50)
      # NOTE: trace=F is a new addition on 15th June 2015
      # to try and reduce the amount of printout that R has
      # to buffer when running this power analysis. I hope
      # this sorts the problem with RStudio randomly crashing
      # during this loop (and not being up to date with the
      # printout at the point of crashing)
      
      power0 <- asreml(fixed=trait~1,
                       random=~natal,
                       data=simphen1,
                       na.method.X="omit",
                       na.method.Y="omit",
                       trace=F,
                       maxiter=50)
      
      p_values.acti10[i,j,k] <- 1-pchisq(2*(power1$loglik-power0$loglik),1)
      
      h2.acti10[i,j,k] <- summary(power1)$varcomp[1,3]/
        (summary(power1)$varcomp[1,3]+summary(power1)$varcomp[2,3]+
           summary(power1)$varcomp[3,3])
      
      r2.acti10[i,j,k] <- summary(power1)$varcomp[2,3]/
        (summary(power1)$varcomp[1,3]+summary(power1)$varcomp[2,3]+
           summary(power1)$varcomp[3,3])
      
      print(paste("i=",i,"j=",j,"k=",k))
      
    }
    
  }
  
  # save after every 1000 iterations for each heritability
  save.image("actipower4August2015.RData")
  
}


# no crashes on 8th-9th August 2015



#########################
# now day 12

p_values.acti12 <- array(dim=c(n, length(heritabilities.a), length(natalv)))

power.acti12 <-array(dim=c(length(heritabilities.a), length(natalv)))

h2.acti12 <- array(dim=c(n, length(heritabilities.a), length(natalv)))

r2.acti12 <- array(dim=c(n, length(heritabilities.a), length(natalv)))



# I need birds

# i need the broods
natal.power12 <- unique(ar12$natal)
natal.power12 <- as.data.frame(natal.power12)
head(natal.power12)

# then I run two models, one including and one excluding
# a pedigree term, to determine the heritability of the trait
# and the log likelihoods of the models


for(j in 1:length(heritabilities.a)){
  for(i in 1:n){
    
    # simulate phenotypes over the whole (fixed) pedigree
    simphen <- phensim(asrp2x, 
                       randomA=heritabilities.a[j],
                       randomE=1-heritabilities.a[j])$phenotypes
    
    # I only want the trait values for the birds that I have sampled
    simphen$sampled <- asrp1x$actikeep12[match(simphen$id, asrp1x$id)]
    simphen1 <- subset(simphen, simphen$sampled==1)
    simphen1 <- data.frame(simphen1)
    
    simphen1$zero <- 0
    simphen1$asrid <- as.factor(simphen1$id)
    
    for(k in 1:length(natalv)){
      
      natal.power12$natvar <- rnorm(length(natal.power12[,1]), 0, sqrt(natalv[k]))
      
      simphen1$natal <- birds12$natal[match(simphen1$asrid, birds12$birds12)]
      
      simphen1$natvar <- natal.power12$natvar[
        match(simphen1$natal, natal.power12[,1])]
      
      simphen1$trait <- simphen1$trait_1 + simphen1$natvar
      
      simphen1$natal <- as.factor(simphen1$natal)
      
      # models - a genetic effect of individual and
      # a phenotypic effect of brood
      power1 <- asreml(fixed=trait~1,
                       random=~ped(asrid, var=T, init=1) +
                         natal,
                       data=simphen1,
                       ginverse=list(asrid=asrpinv),
                       na.method.X="omit",
                       na.method.Y="omit",
                       trace=F,
                       maxiter=50)
      # NOTE: trace=F is a new addition on 15th June 2015
      # to try and reduce the amount of printout that R has
      # to buffer when running this power analysis. I hope
      # this sorts the problem with RStudio randomly crashing
      # during this loop (and not being up to date with the
      # printout at the point of crashing)
      
      power0 <- asreml(fixed=trait~1,
                       random=~natal,
                       data=simphen1,
                       na.method.X="omit",
                       na.method.Y="omit",
                       trace=F,
                       maxiter=50)
      
      p_values.acti12[i,j,k] <- 1-pchisq(2*(power1$loglik-power0$loglik),1)
      
      h2.acti12[i,j,k] <- summary(power1)$varcomp[1,3]/
        (summary(power1)$varcomp[1,3]+summary(power1)$varcomp[2,3]+
           summary(power1)$varcomp[3,3])
      
      r2.acti12[i,j,k] <- summary(power1)$varcomp[2,3]/
        (summary(power1)$varcomp[1,3]+summary(power1)$varcomp[2,3]+
           summary(power1)$varcomp[3,3])
      
      print(paste("i=",i,"j=",j,"k=",k))
      
    }
    
  }
  
  # save after every 1000 iterations for each heritability
  save.image("actipower4August2015.RData")
  
}

# set directory back to preferred:
setwd("C:/Users/Issie/SkyDrive/PhD/masterdatasheets")


# crashed once on 9th August