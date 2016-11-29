# 31st July 2015
# power analysis for the exploration data set from
# 2010-2014

# the code for generating the pedigree with unknown dams
# filled in is importPedigree-June2015.R and the pedigree
# to use from this is asrp1x, which is fixed and turned
# in to the matrix asrpinv in the aforementioned code

# the fixed pedigree is asrp2x

# my measured individuals are as follows:

# the variable keep marks out my measured individuals
# with a 1, and my unmeasured individuals with 0

tent2$keep <- 1
asrp1x$explkeep <- tent2$keep[match(asrp1x$id, tent2$asrid)]
asrp1x$explkeep[is.na(asrp1x$explkeep)] <- 0
birds <- unique(tent2$asrid)
birds <- as.data.frame(birds)
birds$genmum <- tent2$asrdam[match(birds$birds, tent2$asrid)]
head(birds)
str(birds)
str(asrp1x$explkeep)
table(asrp1x$explkeep)
length(unique(tent2$asrid))
length(unique(tent2$birdid))


# maternal ID as a shared environmental variable?

asrp1x$explnatmum <- tent2$asrdam[match(asrp1x$id, tent2$asrid)]
table(table(asrp1x$explnatmum))
summary(asrp1x$explnatmum)
length(asrp1x$explnatmum)
# 282 mums filled in --> right for number of birds


# make the data frames to store the variables

heritabilities.e <- seq(0, 0.3, by=0.02)

natalv <- seq(0,0.3, by=0.05)

n <- 1000

p_values.expl <- array(dim=c(n, length(heritabilities.e), length(natalv)))

power.expl <-array(dim=c(length(heritabilities.e), length(natalv)))

h2.expl <- array(dim=c(n, length(heritabilities.e), length(natalv)))

r2.expl <- array(dim=c(n, length(heritabilities.e), length(natalv)))



# I need birds

# i need the mums
mums.power <- unique(tent2$asrdam)
mums.power <- as.data.frame(mums.power)
head(mums.power)

# then I run two models, one including and one excluding
# a pedigree term, to determine the heritability of the trait
# and the log likelihoods of the models

# first, set a working directory to save images of the 
# workspace:
setwd("C:/Users/Issie/SkyDrive/PhD/Chapter1-heritability/ASReml/afterThesis")



for(j in 1:length(heritabilities.e)){
  for(i in 1:n){
    
    # simulate phenotypes over the whole (fixed) pedigree
    simphen <- phensim(asrp2x, 
                       randomA=heritabilities.e[j],
                       randomE=1-heritabilities.e[j])$phenotypes
    
    # I only want the trait values for the birds that I have sampled
    simphen$sampled <- asrp1x$explkeep[match(simphen$id, asrp1x$id)]
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
      
      p_values.expl[i,j,k] <- 1-pchisq(2*(power1$loglik-power0$loglik),1)
      h2.expl[i,j,k] <- summary(power1)$varcomp[1,3]/
        (summary(power1)$varcomp[1,3]+summary(power1)$varcomp[2,3]+
           summary(power1)$varcomp[3,3])
      
      r2.expl[i,j,k] <- summary(power1)$varcomp[2,3]/
        (summary(power1)$varcomp[1,3]+summary(power1)$varcomp[2,3]+
           summary(power1)$varcomp[3,3])
      
      print(paste("i=",i,"j=",j,"k=",k))
      
    }
    
  }
  
  # save after every 1000 iterations for each heritability
  save.image("explpower31July2015.RData")
  
}

# set directory back to preferred:
setwd("C:/Users/Issie/SkyDrive/PhD/masterdatasheets")


# 31st July-1st August. One crash at j=11,
# one at j=16.





# 16th June 2015. RStudio crashed once fitting the
# models for a heritability of 0.3. No other crashes
# whilst running this loop


for(j in 1:length(heritabilities.e)){
  for(k in 1:length(natalv)){
    power.expl[j,k]<-table(p_values.expl[,j,k]<0.05)["TRUE"]/n
  }
}


power.expl[is.na(power.expl)] <- 0

power.expl

# I need to make a data frame with x = heritabilities,
# y = natalv, z = power

plot.power.expl <- c(power.expl[1,], power.expl[2,], power.expl[3,], power.expl[4,],
                power.expl[5,], power.expl[6,], power.expl[7,], power.expl[8,],
                power.expl[9,], power.expl[10,], power.expl[11,], power.expl[12,],
                power.expl[13,], power.expl[14,], power.expl[15,], power.expl[16,])
plot.power.expl <- as.data.frame(plot.power.expl)
plot.power.expl$h2 <- rep(heritabilities.e, each=7)
plot.power.expl$natv <- rep(natalv, times=16)
plot.power.expl


# plot the power
library(lattice)
wireframe(plot.power.expl~natv*h2, data=plot.power.expl,
          scales=list(arrows=F, xlim=c(0,1), cex=1.5),
          xlab=list(label="    maternal 
                    effect variance",
                    cex=2),
          ylab=list(label="heritability    ",
                    cex=2),
          zlab=list(label="power - exploration",
                    cex=2, rot=90),
          zlim=c(0:1))
# resolution on 3rd August 2015 - 700x700 pixels


library(rgl)
plot3d(x=plot.power.expl$natv,
       y=plot.power.expl$h2,
       z=plot.power.expl$plot.power.expl)

library(nadiv) # to use aiCI()

# false positives
plot(plot.power.expl[1:7,3], plot.power.expl[1:7,1],
     xlab="maternal effect variance",
     ylab="type I error - explness",
     pch=16, cex.lab=1.2, cex.axis=1.2)

explty1 <- plot.power[1:7,1]


