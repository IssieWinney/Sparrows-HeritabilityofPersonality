
library(asreml)


load("BoldnessFixedEffects20151204.RData")


# save output:

sink("./BoldnessFixedEffectssOutput20151204.Rout", 
     type = c("output", "message"))

# new data set needed:

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



# sample sizes:

length(unique(boldnessdatamincomp2.partner$asrid))
length(boldnessdatamincomp2.partner$asrid)



# now, with the NEW and RESCALED data set, run the same models:


fixedeffects.nocomp <- asreml(fixed=logentrylatency~partnerboldshy+
                          factorentryorder+factormethod+
                          relevel(factoryear, ref="2014")+
                          zmeanoffspring+zimc.offspring+sex+
                          zmeanexperience+zimc.experience,
                        random=~ide(factorasrid, var=T, init=1) +
                          ped(factorasrid, var=T, init=1) + 
                          ide(factorasrdam, var=T, init=1),
                        ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                        data=boldnessdatamincomp2.partner,
                        na.method.X="omit",
                        na.method.Y="omit")

hist(fixedeffects.nocomp$residuals)
qqnorm(fixedeffects.nocomp$residuals)
qqline(fixedeffects.nocomp$residuals) 
# small departures at tails of distribution, but pretty good generally.
resids <- fixedeffects.nocomp$residuals
plot(factor(boldnessdatamincomp2.partner$partnerboldshy), resids)
plot(boldnessdatamincomp2.partner$factorentryorder, resids)
plot(boldnessdatamincomp2.partner$testorder, resids)
plot(boldnessdatamincomp2.partner$factormethod, resids)
plot(boldnessdatamincomp2.partner$sex, resids)
plot(boldnessdatamincomp2.partner$year, resids)
plot(boldnessdatamincomp2.partner$meanexperience, resids)
plot(boldnessdatamincomp2.partner$meanoffspring, resids)
plot(boldnessdatamincomp2.partner$imc.experience, resids)
plot(boldnessdatamincomp2.partner$imc.offspring, resids)
# they all look alright.

# what are the variance components like?
summary(fixedeffects.nocomp)$varcomp

# fixed effects
summary(fixedeffects.nocomp, all=T)$coef.fi

# and the significances for the fixed effects
fixedeffects.nocomp.wald <- wald.asreml(fixedeffects.nocomp, ssType="conditional", 
                                   denDF="numeric")

fixedeffects.nocomp.wald


### need to know significance for the random effects:
fixedeffects.nocomp.PE <- asreml(fixed=logentrylatency~partnerboldshy+
                             factorentryorder+factormethod+
                             relevel(factoryear, ref="2014")+
                             zmeanoffspring+zimc.offspring+sex+
                             zmeanexperience+zimc.experience,
                           random=~ped(factorasrid, var=T, init=1) + 
                             ide(factorasrdam, var=T, init=1),
                           ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                           data=boldnessdatamincomp2.partner,
                           na.method.X="omit",
                           na.method.Y="omit")

1-pchisq(2*(fixedeffects.nocomp$loglik-fixedeffects.nocomp.PE$loglik),1)
2*(fixedeffects.nocomp$loglik-fixedeffects.nocomp.PE$loglik)

# for genetics:
fixedeffects.nocomp.A <- asreml(fixed=logentrylatency~partnerboldshy+
                            factorentryorder+factormethod+
                            relevel(factoryear, ref="2014")+
                            zmeanoffspring+zimc.offspring+sex+
                            zmeanexperience+zimc.experience,
                          random=~ide(factorasrid, var=T, init=1) +
                            ide(factorasrdam, var=T, init=1),
                          ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                          data=boldnessdatamincomp2.partner,
                          na.method.X="omit",
                          na.method.Y="omit")


1-pchisq(2*(fixedeffects.nocomp$loglik-fixedeffects.nocomp.A$loglik),1)
2*(fixedeffects.nocomp$loglik-fixedeffects.nocomp.A$loglik)


# for maternal effects:
fixedeffects.nocomp.M <- asreml(fixed=logentrylatency~partnerboldshy+
                            factorentryorder+factormethod+
                            relevel(factoryear, ref="2014")+
                            zmeanoffspring+zimc.offspring+sex+
                            zmeanexperience+zimc.experience,
                          random=~ide(factorasrid, var=T, init=1) +
                            ped(factorasrid, var=T, init=1),
                          ginverse=list(factorasrid=asrpinv),
                          data=boldnessdatamincomp2.partner,
                          na.method.X="omit",
                          na.method.Y="omit")


1-pchisq(2*(fixedeffects.nocomp$loglik-fixedeffects.nocomp.M$loglik),1)
2*(fixedeffects.nocomp$loglik-fixedeffects.nocomp.M$loglik)


# I could ask whether the fixed effects that relate to
# the construction of the nestbox matter:

fixedeffectsnestbox.nocomp <- asreml(fixed=logentrylatency~factormethod+
                                antenna.yforpresent+
                                factor(perchpresent.1forpresent),
                              random=~factorbirdID,
                              data=boldnessdatamincomp2.partner,
                              na.method.X="omit",
                              na.method.Y="omit")


fixedeffectsnestbox.nocomp.wald <- wald.asreml(fixedeffectsnestbox.nocomp, 
                                               ssType="conditional", denDF="numeric")

fixedeffectsnestbox.nocomp.wald
# perch matters a bit in these models but is confounded
# by the identity of the nest box and therefore of the
# individual.


# version including these:


fixedeffects.nocomp2 <- asreml(fixed=logentrylatency~partnerboldshy+
                          factorentryorder+factormethod+
                          relevel(factoryear, ref="2014")+
                          zmeanoffspring+zimc.offspring+sex+
                          zmeanexperience+zimc.experience +
                          antenna.yforpresent+ factormethod+
                          factor(perchpresent.1forpresent),
                        random=~ide(factorasrid, var=T, init=1) +
                          ped(factorasrid, var=T, init=1) + 
                          ide(factorasrdam, var=T, init=1),
                        ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                        data=boldnessdatamincomp2.partner,
                        na.method.X="omit",
                        na.method.Y="omit")


# plots of model fit:
hist(fixedeffects.nocomp2$residuals)
qqnorm(fixedeffects.nocomp2$residuals)
qqline(fixedeffects.nocomp2$residuals) 
# small departures at tails of distribution as before.
resids <- fixedeffects.nocomp2$residuals
plot(factor(boldnessdatamincomp2.partner$partnerboldshy), resids)
plot(boldnessdatamincomp2.partner$factorentryorder, resids)
plot(boldnessdatamincomp2.partner$testorder, resids)
plot(boldnessdatamincomp2.partner$factormethod, resids)
plot(boldnessdatamincomp2.partner$sex, resids)
plot(boldnessdatamincomp2.partner$year, resids)
plot(boldnessdatamincomp2.partner$meanexperience, resids)
plot(boldnessdatamincomp2.partner$meanoffspring, resids)
plot(boldnessdatamincomp2.partner$imc.experience, resids)
plot(boldnessdatamincomp2.partner$imc.offspring, resids)
plot(boldnessdatamincomp2.partner$antenna.yforpresent, resids)
plot(factor(boldnessdatamincomp2.partner$perchpresent.1forpresent), resids)
# perch present is skewey because of the lack of nestboxes
# without perches. This is not ideal to include in the
# model because of this small sample size and the aforementioned
# problem that the bird and nestbox identity are confounded
# with whether or not the nestbox had a perch.


# random effects
summary(fixedeffects.nocomp2)$varcomp

# fixed effects

summary(fixedeffects.nocomp2, all=T)$coef.fi

fixedeffects.nocomp2.wald <- wald.asreml(fixedeffects.nocomp2, 
                                         ssType="conditional", denDF="numeric")

fixedeffects.nocomp2.wald


### need to know significance:
fixedeffects.nocomp2.PE <- asreml(fixed=logentrylatency~partnerboldshy+
                          factorentryorder+factormethod+
                          relevel(factoryear, ref="2014")+
                          zmeanoffspring+zimc.offspring+sex+
                          zmeanexperience+zimc.experience +
                          antenna.yforpresent+ factormethod+
                          factor(perchpresent.1forpresent),
                        random=~ped(factorasrid, var=T, init=1) + 
                          ide(factorasrdam, var=T, init=1),
                        ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                        data=boldnessdatamincomp2.partner,
                        na.method.X="omit",
                        na.method.Y="omit")

1-pchisq(2*(fixedeffects.nocomp2$loglik-fixedeffects.nocomp2.PE$loglik),1)
2*(fixedeffects.nocomp2$loglik-fixedeffects.nocomp2.PE$loglik)


fixedeffects.nocomp2.A <- asreml(fixed=logentrylatency~partnerboldshy+
                          factorentryorder+factormethod+
                          relevel(factoryear, ref="2014")+
                          zmeanoffspring+zimc.offspring+sex+
                          zmeanexperience+zimc.experience +
                          antenna.yforpresent+ factormethod+
                          factor(perchpresent.1forpresent),
                        random=~ide(factorasrid, var=T, init=1) +
                          ide(factorasrdam, var=T, init=1),
                        ginverse=list(factorasrid=asrpinv, factorasrdam=asrpinv),
                        data=boldnessdatamincomp2.partner,
                        na.method.X="omit",
                        na.method.Y="omit")

1-pchisq(2*(fixedeffects.nocomp2$loglik-fixedeffects.nocomp2.A$loglik),1)
2*(fixedeffects.nocomp2$loglik-fixedeffects.nocomp2.A$loglik)


fixedeffects.nocomp2.M <- asreml(fixed=logentrylatency~partnerboldshy+
                          factorentryorder+factormethod+
                          relevel(factoryear, ref="2014")+
                          zmeanoffspring+zimc.offspring+sex+
                          zmeanexperience+zimc.experience +
                          antenna.yforpresent+ factormethod+
                          factor(perchpresent.1forpresent),
                        random=~ide(factorasrid, var=T, init=1) +
                          ped(factorasrid, var=T, init=1),
                        ginverse=list(factorasrid=asrpinv),
                        data=boldnessdatamincomp2.partner,
                        na.method.X="omit",
                        na.method.Y="omit")

1-pchisq(2*(fixedeffects.nocomp2$loglik-fixedeffects.nocomp2.M$loglik),1)
2*(fixedeffects.nocomp2$loglik-fixedeffects.nocomp2.M$loglik)


# bivariate models:

bivariate.Dec2015 <- asreml(fixed=cbind(flatency, mlatency)~trait,
                      random=~us(trait):factormum+
                        us(trait):factordad,
                      rcov=~units:diag(trait),
                      data=videoinfo2,
                      maxiter=5000,
                      na.method.Y="include",
                      na.method.X="include")


bivariate.Dec2015$converge
bivariate.Dec2015$last.message
# wow! So this one converged!

summary(bivariate.Dec2015, all=T)$coef.fi
summary(bivariate.Dec2015)$varcomp



### phenotypic correlations?
# covariance * sqrt ((between-indiv var / all var trait 1) * (between-indiv var / all var trait 2))

# female ID covariance
pin(bivariate.Dec2015, covs~V2*sqrt((V1/(V1+V8))*(V3/(V3+V9))))
# check
0.13900491*sqrt((0.28083244/(0.28083244+0.86109792))*
                  (0.10754783/(0.10754783+1.07619801)))
# matches.

# male ID covariance
pin(bivariate.Dec2015, covs~V5*sqrt((V4/(V4+V8))*(V6/(V6+V9))))



# significance of mother:

bivariate.M.Dec2015 <- asreml(fixed=cbind(flatency, mlatency)~trait,
                            random=~diag(trait):factormum+
                              us(trait):factordad,
                            rcov=~units:diag(trait),
                            data=videoinfo2,
                            maxiter=5000,
                            na.method.Y="include",
                            na.method.X="include")

bivariate.M.Dec2015$last.message
# but this one has not converged. So I am still not completely
# sure what the significance should be of the covariance.

# variance components:
summary(bivariate.M.Dec2015)$varcomp
# without the female ID covariance, the influence of the female
# drops down. But the model has also not converged. The next model
# where the male is knocked out supports the original model.


# significance of father:

bivariate.F.Dec2015 <- asreml(fixed=cbind(flatency, mlatency)~trait,
                            random=~us(trait):factormum+
                              diag(trait):factordad,
                            rcov=~units:diag(trait),
                            data=videoinfo2,
                            maxiter=5000,
                            na.method.Y="include",
                            na.method.X="include")


bivariate.F.Dec2015$last.message
# and this one has not converged either

summary(bivariate.F.Dec2015)$varcomp
# but in support of the other models, no effect
# of the male ID on female behaviour in this model.

# influence of offspring:

videoinfo2$zOffspringNo <- scale(videoinfo2$OffspringNo)


bivariate.offspring.Dec2015 <- asreml(fixed=cbind(flatency, mlatency)~trait + 
                              trait:zOffspringNo,
                            random=~us(trait):factormum+
                              us(trait):factordad,
                            rcov=~units:diag(trait),
                            data=videoinfo2,
                            maxiter=500,
                            na.method.Y="include",
                            na.method.X="include")

summary(bivariate.offspring.Dec2015)$varcomp
# very similar conclusions when the number of offspring
# in the nest are accounted for (no individual mean
# centering in this case).

summary(bivariate.offspring.Dec2015, all=T)$coef.fi
# overall influence of offspring: bolder for more offspring

# influence of compromised data:

bivariate.compromised.Dec2015 <- asreml(fixed=cbind(flatencywithcomp, mlatencywithcomp)~trait+
                       at(trait,2):mvidcomp+at(trait,1):fvidcomp,
                     random=~us(trait):factormum+
                       us(trait):factordad,
                     rcov=~units:diag(trait),
                     data=videoinfocomp,
                     maxiter=500,
                     na.method.Y="include",
                     na.method.X="include")

summary(bivariate.compromised.Dec2015, all=T)$coef.fi

summary(bivariate.compromised.Dec2015)$varcomp
# there is a far greater influence of the female on
# the male's behaviour when there is more data. The
# shame is that I don't trust that the influence is
# not from the compromising of the video e.g. if
# both are compromised at the same time by the same
# event, or if one clears away the paper for the other
# or the many other ways in which a test can be compromised.



# sample sizes for non-compromised data:
# number of observations of females and males is the same
# as the full data set, but to prove this:
length(videoinfo2$flatency) - length(which(is.na(videoinfo2$flatency)))
length(videoinfo2$mlatency) - length(which(is.na(videoinfo2$mlatency)))

# on how many occasions is boldness known for
# both males and females?
names(videoinfo2)

# return whether a particular value of the NAfinder
# (which is a paste of whether a boldness value was
# returned for males and females in a given video)
# contains the string "NA":
grepl("NA", videoinfo2$NAfinder)
table(grepl("NA", videoinfo2$NAfinder))
# which means 108 pairs exist without an NA, so where
# the behaviour of both individuals has been logged.

# stop logging the console output:

sink(NULL)

# save the workspace:

save.image("./BoldnessFixedEffectssOutput20151204.RData")
