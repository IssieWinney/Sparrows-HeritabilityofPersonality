# July amendment: all data power analysis has 
# no zero fixed effect fitted.


# 11th June 2015
# power analysis for the boldness data set from
# 2009-2014

# the code for generating the pedigree with unknown dams
# filled in is importPedigree-June2015.R and the pedigree
# to use from this is asrp1x, which is fixed and turned
# in to the matrix asrpinv in the aforementioned code

# the fixed pedigree is asrp2x

# my measured individuals are as follows:

# the variable keep marks out my measured individuals
# with a 1, and my unmeasured individuals with 0

boldnessdata7$keep <- 1
asrp1x$boldkeep <- boldnessdata7$keep[match(asrp1x$id, boldnessdata7$asrid)]
asrp1x$boldkeep[is.na(asrp1x$boldkeep)] <- 0
birds <- unique(boldnessdata7$asrid)
birds <- as.data.frame(birds)
birds$genmum <- boldnessdata7$asrdam[match(birds$birds, boldnessdata7$asrid)]
head(birds)
str(birds)
str(asrp1x$boldkeep)
table(asrp1x$boldkeep)
length(unique(boldnessdata7$asrid))
length(unique(boldnessdata7$birdID))


# maternal ID as a shared environmental variable?

asrp1x$boldnatmum <- boldnessdata7$asrdam[match(asrp1x$id, boldnessdata7$asrid)]
table(table(asrp1x$boldnatmum))
summary(asrp1x$boldnatmum)
length(asrp1x$boldnatmum)
# 221 mums filled in --> right for number of birds


# make the data frames to store the variables

heritabilities.b <- seq(0, 0.3, by=0.02)

natalv <- seq(0,0.3, by=0.05)

n <- 1000

p_values.bold <- array(dim=c(n, length(heritabilities.b), length(natalv)))

power.bold <-array(dim=c(length(heritabilities.b), length(natalv)))

h2.bold <- array(dim=c(n, length(heritabilities.b), length(natalv)))

r2.bold <- array(dim=c(n, length(heritabilities.b), length(natalv)))



# I need birds

# i need the mums
mums.power <- unique(boldnessdata7$asrdam)
mums.power <- as.data.frame(mums.power)
head(mums.power)

# then I run two models, one including and one excluding
# a pedigree term, to determine the heritability of the trait
# and the log likelihoods of the models

# first, set a working directory to save images of the 
# workspace:
setwd("C:/Users/Issie/SkyDrive/PhD/Chapter1-heritability/ASReml/afterThesis")

for(j in 1:length(heritabilities.b)){
  for(i in 1:n){
    
    # simulate phenotypes over the whole (fixed) pedigree
    simphen <- phensim(asrp2x, 
                       randomA=heritabilities.b[j],
                       randomE=1-heritabilities.b[j])$phenotypes
    
    # I only want the trait values for the birds that I have sampled
    simphen$sampled <- asrp1x$boldkeep[match(simphen$id, asrp1x$id)]
    simphen1 <- subset(simphen, simphen$sampled==1)
    simphen1 <- data.frame(simphen1)
    
    simphen1$zero <- 0
    simphen1$asrid <- as.factor(simphen1$id)
    
    for(k in 1:length(natalv)){
      
      mums.power$mumvar <- rnorm(length(mums.power[,1]), 0, sqrt(natalv[k]))
      
      simphen1$genmum <- birds$genmum[match(simphen1$asrid, birds$birds)]
      
      simphen1$mumvar <- mums.power$mumvar[
        match(simphen1$genmum, mums.power[,1])]
      
      simphen1$trait <- simphen1$trait_1 + simphen1$mumvar
      
      simphen1$genmum <- as.factor(simphen1$genmum)
      
      # models - a genetic effect of individual and
      # a phenotypic effect of mother
      power1 <- asreml(fixed=trait~1,
                       random=~ped(asrid, var=T, init=1) +
                         ide(genmum, var=T, init=1),
                       data=simphen1,
                       ginverse=list(asrid=asrpinv, genmum=asrpinv),
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
                       random=~ide(genmum, var=T, init=1),
                       data=simphen1,
                       ginverse=list(genmum=asrpinv),
                       na.method.X="omit",
                       na.method.Y="omit",
                       trace=F,
                       maxiter=50)
      
      p_values.bold[i,j,k] <- 1-pchisq(2*(power1$loglik-power0$loglik),1)
      h2.bold[i,j,k] <- summary(power1)$varcomp[1,3]/
        (summary(power1)$varcomp[1,3]+summary(power1)$varcomp[2,3]+
           summary(power1)$varcomp[3,3])
      
      r2.bold[i,j,k] <- summary(power1)$varcomp[2,3]/
        (summary(power1)$varcomp[1,3]+summary(power1)$varcomp[2,3]+
           summary(power1)$varcomp[3,3])
      
      print(paste("i=",i,"j=",j,"k=",k))
      
    }
    
  }
  
  # save after every 1000 iterations for each heritability
  save.image("boldpower1Aug2015.RData")
  
}

# set directory back to preferred:
setwd("C:/Users/Issie/SkyDrive/PhD/masterdatasheets")

# 2nd August 2015 - no crashes whilst running this loop.


for(j in 1:length(heritabilities.b)){
  for(k in 1:length(natalv)){
    power.bold[j,k]<-table(p_values.bold[,j,k]<0.05)["TRUE"]/n
  }
}


power.bold[is.na(power.bold)] <- 0

power.bold

# I need to make a data frame with x = heritabilities,
# y = natalv, z = power

plot.power <- c(power.bold[1,], power.bold[2,], power.bold[3,], power.bold[4,],
                power.bold[5,], power.bold[6,], power.bold[7,], power.bold[8,],
                power.bold[9,], power.bold[10,], power.bold[11,], power.bold[12,],
                power.bold[13,], power.bold[14,], power.bold[15,], power.bold[16,])
plot.power <- as.data.frame(plot.power)
plot.power$h2 <- rep(heritabilities.b, each=7)
plot.power$natv <- rep(natalv, times=16)
plot.power


# now I cam make a 3d plot of my data!!!
# this is part of figure S1
library(lattice)
wireframe(plot.power~natv*h2, data=plot.power,
          scales=list(arrows=F, xlim=c(0,1), cex=1.5),
          xlab=list(label="maternal 
                    effect variance",
                    cex=2),
          ylab=list(label="heritability",
                    cex=2),
          zlab=list(label="full power - boldness",
                    cex=2, rot=90),
          zlim=c(0:1))

library(rgl)
plot3d(x=plot.power$natv,
       y=plot.power$h2,
       z=plot.power$plot.power)

library(nadiv) # to use aiCI()

# false positives
plot(plot.power[1:7,3], plot.power[1:7,1],
     xlab="maternal effect variance",
     ylab="type I error - boldness",
     pch=16, cex.lab=1.2, cex.axis=1.2)

boldty1 <- plot.power[1:7,1]

#############################################################
#############################################################

# however, this is the most generous data set that includes
# compromised data and data taken on the wrong day. What
# about the most conservative data set, that excludes data
# taken on the wrong day and that is compromised:


# This is the power analysis to put in the supplement:


boldnessdatamincomp2$keep <- 1
asrp1x$conservativeboldkeep <- boldnessdatamincomp2$keep[match(asrp1x$id, boldnessdatamincomp2$asrid)]
asrp1x$conservativeboldkeep[is.na(asrp1x$conservativeboldkeep)] <- 0
birds.con <- unique(boldnessdatamincomp2$asrid)
birds.con <- as.data.frame(birds.con)
birds.con$genmum <- boldnessdatamincomp2$asrdam[match(birds.con$birds, 
                                                      boldnessdatamincomp2$asrid)]
head(birds.con)
str(birds.con)
str(asrp1x$conservativeboldkeep)
table(asrp1x$conservativeboldkeep)
length(unique(boldnessdatamincomp2$asrid))
length(unique(boldnessdatamincomp2$birdID))


# maternal ID as a shared environmental variable?

asrp1x$conservativeboldnatmum <- boldnessdatamincomp2$asrdam[match(asrp1x$id,
                                                                   boldnessdatamincomp2$asrid)]
table(table(asrp1x$conservativeboldnatmum))
summary(asrp1x$conservativeboldnatmum)
length(asrp1x$conservativeboldnatmum)
# 184 mums filled in --> right for number of birds


# make the data frames to store the variables


p_values.bold.con <- array(dim=c(n, length(heritabilities.b), length(natalv)))

power.bold.con <-array(dim=c(length(heritabilities.b), length(natalv)))

h2.bold.con <- array(dim=c(n, length(heritabilities.b), length(natalv)))

r2.bold.con <- array(dim=c(n, length(heritabilities.b), length(natalv)))



# I need birds

# i need the mums
mums.power.con <- unique(boldnessdatamincomp2$asrdam)
mums.power.con <- as.data.frame(mums.power.con)
head(mums.power.con)

# then I run two models, one including and one excluding
# a pedigree term, to determine the heritability of the trait
# and the log likelihoods of the models

setwd("C:/Users/Issie/SkyDrive/PhD/Chapter1-heritability/ASReml/afterThesis")


for(j in 1:length(heritabilities.b)){
  for(i in 1:n){
    
    # simulate phenotypes over the whole (fixed) pedigree
    simphen <- phensim(asrp2x, 
                       randomA=heritabilities.b[j],
                       randomE=1-heritabilities.b[j])$phenotypes
    
    # I only want the trait values for the birds that I have sampled
    simphen$sampled <- asrp1x$conservativeboldkeep[match(simphen$id, asrp1x$id)]
    simphen1 <- subset(simphen, simphen$sampled==1)
    simphen1 <- data.frame(simphen1)
    
    simphen1$zero <- 0
    simphen1$asrid <- as.factor(simphen1$id)
    
    for(k in 1:length(natalv)){
      
      mums.power.con$mumvar <- rnorm(length(mums.power.con[,1]), 0, sqrt(natalv[k]))
      
      simphen1$genmum <- birds.con$genmum[match(simphen1$asrid, birds.con$birds)]
      
      simphen1$mumvar <- mums.power.con$mumvar[
        match(simphen1$genmum, mums.power.con[,1])]
      
      simphen1$trait <- simphen1$trait_1 + simphen1$mumvar
      
      simphen1$genmum <- as.factor(simphen1$genmum)
      
      # models - a genetic effect of individual and
      # a phenotypic effect of mother
      power1 <- asreml(fixed=trait~1,
                       random=~ped(asrid, var=T, init=1) +
                         ide(genmum, var=T, init=1),
                       data=simphen1,
                       ginverse=list(asrid=asrpinv, genmum=asrpinv),
                       na.method.X="omit",
                       na.method.Y="omit",
                       trace=F,
                       maxiter=50)
      
      power0 <- asreml(fixed=trait~1,
                       random=~ide(genmum, var=T, init=1),
                       data=simphen1,
                       ginverse=list(genmum=asrpinv),
                       na.method.X="omit",
                       na.method.Y="omit",
                       trace=F,
                       maxiter=50)
      
      p_values.bold.con[i,j,k] <- 1-pchisq(2*(power1$loglik-power0$loglik),1)
      h2.bold.con[i,j,k] <- summary(power1)$varcomp[1,3]/
        (summary(power1)$varcomp[1,3]+summary(power1)$varcomp[2,3]+
           summary(power1)$varcomp[3,3])
      
      r2.bold.con[i,j,k] <- summary(power1)$varcomp[2,3]/
        (summary(power1)$varcomp[1,3]+summary(power1)$varcomp[2,3]+
           summary(power1)$varcomp[3,3])
      
      print(paste("i=",i,"j=",j,"k=",k))
      
    }
    
  }
  # save after every 1000 iterations for each heritability
  save.image("boldpower1Aug2015.RData")

}

# set directory back to preferred:
setwd("C:/Users/Issie/SkyDrive/PhD/masterdatasheets")


# one crash at j = 16


for(j in 1:length(heritabilities.b)){
  for(k in 1:length(natalv)){
    power.bold.con[j,k]<-table(p_values.bold.con[,j,k]<0.05)["TRUE"]/n
  }
}


power.bold.con[is.na(power.bold.con)] <- 0

power.bold.con


# make data frame:
plot.power.bold <- c(power.bold.con[1,], power.bold.con[2,], power.bold.con[3,], power.bold.con[4,],
                power.bold.con[5,], power.bold.con[6,], power.bold.con[7,], power.bold.con[8,],
                power.bold.con[9,], power.bold.con[10,], power.bold.con[11,], power.bold.con[12,],
                power.bold.con[13,], power.bold.con[14,], power.bold.con[15,], power.bold.con[16,])
plot.power.bold <- as.data.frame(plot.power.bold)
plot.power.bold$h2 <- rep(heritabilities.b, each=7)
plot.power.bold$natv <- rep(natalv, times=16)
plot.power.bold


# plot it. Part of figure S1
library(lattice)
wireframe(plot.power.bold~natv*h2, data=plot.power.bold,
          scales=list(arrows=F, xlim=c(0,1), cex=1.5),
          xlab=list(label="    maternal 
                    effect variance",
                    cex=2),
          ylab=list(label="heritability    ",
                    cex=2),
          zlab=list(label="power - boldness",
                    cex=2, rot=90),
          zlim=c(0:1))

