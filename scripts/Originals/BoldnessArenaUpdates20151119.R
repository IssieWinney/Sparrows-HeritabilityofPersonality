load("./BoldnessArenaUpdates20151119.RData")

library(asreml)
library(pedantics)


for(j in 16:length(heritabilities.b)){
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
  
}


arenafixed6 <- asreml(fixed=logtot~relevel(factor(lambVSsteve),2) + 
                        noise2 + ztime + factor(wd1to3) + 
                        zsocbroodsz + cohort + releaser + age + 
                        comp + zendprob + I(ztime^2),
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

plot(arenafixed6)

# fixed effect residuals
plot(ar1.minus4th$lambVSsteve, arenafixed6$residuals)
plot(ar1.minus4th$noise2, arenafixed6$residuals)
plot(ar1.minus4th$time, arenafixed6$residuals)
plot((ar1.minus4th$time^2), arenafixed6$residuals)
plot(ar1.minus4th$wd1to3, arenafixed6$residuals)
plot(ar1.minus4th$socbroodsz, arenafixed6$residuals)
plot(ar1.minus4th$cohort, arenafixed6$residuals)
plot(ar1.minus4th$releaser, arenafixed6$residuals)
plot(ar1.minus4th$age, arenafixed6$residuals)
plot(ar1.minus4th$comp, arenafixed6$residuals)
plot(ar1.minus4th$endprob, arenafixed6$residuals)


# model summary

summary(arenafixed6)$varcomp
summary(arenafixed6, all=T)$coef.fi

# fixed effect significances
wald.arenafixed6 <- wald.asreml(arenafixed6, 
                                ssType="conditional", 
                                denDF="numeric")


wald.arenafixed6

# random effect significances:

# Vpe

arenafixed6.2 <- asreml(fixed=logtot~relevel(factor(lambVSsteve),2) + 
                          noise2 + ztime + factor(wd1to3) + 
                          zsocbroodsz + cohort + releaser + age + 
                          comp + zendprob + I(ztime^2),
                        random=~ped(factorasrid, var=T, init=1) +
                          ide(factorasrdam, var=T, init=1) +
                          social,
                        data=ar1.minus4th,
                        ginverse=list(factorasrid=asrpinv,
                                      factorasrdam=asrpinv),
                        na.method.X="omit",
                        na.method.Y="omit",
                        maxiter=20)


1-pchisq(2*(arenafixed6$loglik-arenafixed6.2$loglik), 1)


# Va

arenafixed6.3 <- asreml(fixed=logtot~relevel(factor(lambVSsteve),2) + 
                          noise2 + ztime + factor(wd1to3) + 
                          zsocbroodsz + cohort + releaser + age + 
                          comp + zendprob + I(ztime^2),
                        random=~ide(factorasrid, var=T, init=1) +
                          ide(factorasrdam, var=T, init=1) +
                          social,
                        data=ar1.minus4th,
                        ginverse=list(factorasrid=asrpinv,
                                      factorasrdam=asrpinv),
                        na.method.X="omit",
                        na.method.Y="omit",
                        maxiter=20)



1-pchisq(2*(arenafixed6$loglik-arenafixed6.3$loglik), 1)

# Vs

arenafixed6.4 <- asreml(fixed=logtot~relevel(factor(lambVSsteve),2) + 
                          noise2 + ztime + factor(wd1to3) + 
                          zsocbroodsz + cohort + releaser + age + 
                          comp + zendprob + I(ztime^2),
                        random=~ide(factorasrid, var=T, init=1) +
                          ped(factorasrid, var=T, init=1) +
                          ide(factorasrdam, var=T, init=1),
                        data=ar1.minus4th,
                        ginverse=list(factorasrid=asrpinv,
                                      factorasrdam=asrpinv),
                        na.method.X="omit",
                        na.method.Y="omit",
                        maxiter=20)


1-pchisq(2*(arenafixed6$loglik-arenafixed6.4$loglik), 1)



save.image("./BoldnessArenaUpdatesOutput20151119.RData")