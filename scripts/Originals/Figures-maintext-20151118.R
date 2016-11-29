# Isabel Winney
# 5th August 2015


setwd("C:/Users/Issie/SkyDrive/PhD/Chapter1-heritability/Figures")

# I want three side-by-side bar plots of the
# variance components of the all data animal
# models, with 95% confidence intervals calculated
# by likelihood profiles, and I want a plot of 
# the proportion of personality accounted for by
# each.

# first plot: variance components plus 95%

# I have three models: 
# boldanimalmincomp1 for boldness
# explorationanimal1 for exploration
# arena.animal1 for activity


# I want to make 95% confidence intervals for
# all of them using their profile likelihoods,
# which can be done with function proLik in
# package nadiv

library(nadiv)

# do the profile likelihoods for each model variance component:

# boldanimalmincomp1

{bold.pe1 <- proLik(full.model = boldanimalmincomp1, 
                   component = "ide(factorasrid, var = T)!id", 
                   negative = FALSE, 
                   nsample.units = 4, nse = 3.5)

bold.a1 <- proLik(full.model = boldanimalmincomp1, 
                   component = "ped(factorasrid, var = T)!ped", 
                   negative = FALSE, 
                   nsample.units = 4, nse = 3.5)
# pe and m are near boundary --> beware estimates warning
bold.m1 <- proLik(full.model = boldanimalmincomp1, 
                   component = "ide(factorasrdam, var = T)!id", 
                   negative = FALSE, 
                   nsample.units = 4, nse = 3.5)

bold.r1 <- proLik(full.model = boldanimalmincomp1, 
                   component = "R!variance", 
                   negative = FALSE, 
                   nsample.units = 4, nse = 3.5)
}

# explorationanimal1

{expl.pe1 <- proLik(full.model = explorationanimal1, 
                   component = "ide(factorasrid, var = T)!id", 
                   negative = FALSE, 
                   nsample.units = 4, nse = 3.5)
# pe is boundary --> be cautious
expl.a1 <- proLik(full.model = explorationanimal1, 
                  component = "ped(factorasrid, var = T)!ped", 
                  negative = FALSE, 
                  nsample.units = 4, nse = 3.5)

expl.m1 <- proLik(full.model = explorationanimal1, 
                  component = "ide(factorasrdam, var = T)!id", 
                  negative = FALSE, 
                  nsample.units = 4, nse = 3.5)

expl.r1 <- proLik(full.model = explorationanimal1, 
                  component = "R!variance", 
                  negative = FALSE, 
                  nsample.units = 4, nse = 3.5)
}


# arena.animal1

{acti.pe1 <- proLik(full.model = arena.animal1, 
                   component = "ide(factorasrid, var = T)!id", 
                   negative = FALSE, 
                   nsample.units = 4, nse = 3.5)

acti.a1 <- proLik(full.model = arena.animal1, 
                  component = "ped(factorasrid, var = T)!ped", 
                  negative = FALSE, 
                  nsample.units = 4, nse = 3.5)

acti.m1 <- proLik(full.model = arena.animal1, 
                  component = "ide(factorasrdam, var = T)!id", 
                  negative = FALSE, 
                  nsample.units = 4, nse = 3.5)
# boundary = m
acti.s1 <- proLik(full.model = arena.animal1, 
                  component = "social!social.var", 
                  negative = FALSE, 
                  nsample.units = 4, nse = 3.5)

acti.r1 <- proLik(full.model = arena.animal1, 
                  component = "R!variance", 
                  negative = FALSE, 
                  nsample.units = 4, nse = 3.5)
}





# these are the model estimates for the variance
# components:
figure.estimates <- data.frame("estimates"=c(summary(boldanimalmincomp1)$varcomp[[1,2]], # Vpe
                      summary(boldanimalmincomp1)$varcomp[[2,2]], # Va
                      summary(boldanimalmincomp1)$varcomp[[3,2]], # Vm
                      summary(boldanimalmincomp1)$varcomp[[4,2]], # Vr
                      summary(explorationanimal1)$varcomp[[1,2]],
                      summary(explorationanimal1)$varcomp[[2,2]],
                      summary(explorationanimal1)$varcomp[[3,2]],
                      summary(explorationanimal1)$varcomp[[4,2]],
                      summary(arena.animal1)$varcomp[[1,2]],
                      summary(arena.animal1)$varcomp[[2,2]],
                      summary(arena.animal1)$varcomp[[3,2]],
                      summary(arena.animal1)$varcomp[[4,2]],
                      summary(arena.animal1)$varcomp[[5,2]]),
                      "big.labels"=c("boldness","boldness","boldness","boldness",
                                     "exploration","exploration","exploration","exploration",
                                     "activity","activity","activity","activity","activity"),
                      "small.labels"=c("Vpe", "Va", "Vm", "Vr",
                                      "Vpe", "Va", "Vm", "Vr",
                                      "Vpe", "Va", "Vm", "Vs", "Vr"),
                      "lowerbounds"=c(bold.pe1$LCL, bold.a1$LCL, bold.m1$LCL, bold.r1$LCL,
                                      expl.pe1$LCL, expl.a1$LCL, expl.m1$LCL, expl.r1$LCL,
                                      acti.pe1$LCL, acti.a1$LCL, acti.m1$LCL, acti.s1$LCL, acti.r1$LCL),
                      "upperbounds"=c(bold.pe1$UCL, bold.a1$UCL, bold.m1$UCL, bold.r1$UCL,
                                      expl.pe1$UCL, expl.a1$UCL, expl.m1$UCL, expl.r1$UCL,
                                      acti.pe1$UCL, acti.a1$UCL, acti.m1$UCL, acti.s1$UCL, acti.r1$UCL))

figure.estimates
str(figure.estimates)

# I need unique labels so that all my points
# are plotted separately:
figure.estimates$labels <- paste(figure.estimates$big.labels,
                                 figure.estimates$small.labels,
                                 sep=", ")

# I want my variables to be plotted in the specific order I
# specified. However, since the variables will be plotted
# as one y-variable each, the first i.e. 'boldness, Vpe',
# would be plotted next to y=0, and I actually want the
# opposite. So, re-order the factor levels of my labels in 
# reverse order:
figure.estimates$labels <- factor(figure.estimates$labels, 
                                 levels = rev(figure.estimates$labels))
figure.estimates$labels

# y-axis labels:

small.labels=c(expression(italic("V")[pe]),
               expression(italic("V")[a]),
               expression(italic("V")[m]),
               expression(italic("V")[r]),
               expression(italic("V")[pe]),
               expression(italic("V")[a]),
               expression(italic("V")[m]),
               expression(italic("V")[r]),
               expression(italic("V")[pe]),
               expression(italic("V")[a]),
               expression(italic("V")[m]),
               expression(italic("V")[s]),
               expression(italic("V")[r]))


# I actually want one of those nice forest plot
# estimate things, so how do I do it?

# this code is adapted from 
# https://github.com/nzcoops/blog_code/blob/master/forest_plot.Rmd

library(ggplot2)


png(filename = "Figure-variancecomponents20151117.png",
    width=400, height=700, units="px")

{p <- ggplot(figure.estimates,aes(x=estimates,y=labels)) + 
  theme(axis.line = element_line(colour = "black", size=1, lineend="butt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        axis.text=element_text(size=20, colour="black"),
        axis.title.x=element_text(size=25, vjust=0),
        axis.title.y=element_text(size=25, vjust=1.5),
        axis.ticks = element_line(colour = "black", size=1),
        panel.background = element_blank()) +
  geom_point(size=4, shape=19) +
  geom_errorbarh(aes(xmax = upperbounds, xmin = lowerbounds), height = 0, size=1) +
  scale_x_continuous(breaks = seq(0,1.5,0.25), labels = seq(0,1.5,0.25)) +
  scale_y_discrete(labels=rev(small.labels)) +
  geom_abline(intercept=5.5, slope=0, linetype=2) +
  geom_abline(intercept=9.5, slope=0, linetype=2) +
  labs(x="Variance estimate", y="") +
  ylab("    Activity               Exploration          Boldness")

p

}

dev.off()

# saved as a 400x700 pixel plot

# can do an eps version:


{p2 <- ggplot(figure.estimates,aes(x=estimates,y=labels)) + 
  theme(axis.line = element_line(colour = "black", size=0.3, lineend="butt"),
        plot.title = element_text(size = 10, hjust = 0, vjust = 2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.3),
        axis.text=element_text(size=8, colour="black"),
        axis.title.x=element_text(size=10, vjust=0),
        axis.title.y=element_text(size=10, vjust=1.5),
        axis.ticks = element_line(colour = "black", size=0.3),
        panel.background = element_blank()) +
  geom_point(size=2, shape=19) +
  geom_errorbarh(aes(xmax = upperbounds, xmin = lowerbounds), height = 0, size=0.3) +
  scale_x_continuous(breaks = seq(0,1.5,0.25), labels = seq(0,1.5,0.25)) +
  scale_y_discrete(labels=rev(small.labels)) +
  geom_abline(intercept=5.5, slope=0, linetype=2) +
  geom_abline(intercept=9.5, slope=0, linetype=2) +
  ggtitle("a)") +
  labs(x="Variance estimate", y="") +
  ylab("    Activity               Exploration           Boldness")
}
p2

ggsave(filename = "Figure-variancecomponents20151117.eps",
    width=3, height=6, units="in")



####################################################################



####################################################################
# I want a plot of proportions of variance for each of my
# traits 


# boldness
{boldstuff <- rbind(pin(boldanimalmincomp1, pe2bold~V1/(V1+V2+V3+V4)),
                   pin(boldanimalmincomp1, h2bold~V2/(V1+V2+V3+V4)),
                   pin(boldanimalmincomp1, m2bold~V3/(V1+V2+V3+V4)),
                   0)
}
boldstuff

# exploration
{exstuff <- rbind(pin(explorationanimal1, pe2tent~V1/(V1+V2+V3+V4)),
                 pin(explorationanimal1, h2tent~V2/(V1+V2+V3+V4)),
                 pin(explorationanimal1, m2tent~V3/(V1+V2+V3+V4)),
                 0)
}
exstuff

# arena
{arenastuff <- rbind(pin(arena.animal1, pe2arena~V1/(V1+V2+V3+V4+V5)),
                    pin(arena.animal1, h2arena~V2/(V1+V2+V3+V4+V5)),
                    pin(arena.animal1, m2arena~V3/(V1+V2+V3+V4+V5)),
                    pin(arena.animal1, s2arena~V4/(V1+V2+V3+V4+V5)))
}
arenastuff
plot(arenastuff$Estimate)

behaviourstuff1 <- cbind(boldstuff$Estimate,
                     exstuff$Estimate,
                     arenastuff$Estimate)


# data for second barplot:
# following Dochtermann et al 2015, I want to show this
# alongside a graph for how much of the personality i.e.
# the repeatability is explained by each variance component.
# So, this means removing the residual variance from each
# calculation:

{# boldness
boldstuff2 <- rbind(pin(boldanimalmincomp1, pe2bold~V1/(V1+V2+V3)),
                    pin(boldanimalmincomp1, h2bold~V2/(V1+V2+V3)),
                    pin(boldanimalmincomp1, m2bold~V3/(V1+V2+V3)),
                    0)

boldstuff2

# exploration
exstuff2 <- rbind(pin(explorationanimal1, pe2tent~V1/(V1+V2+V3)),
                  pin(explorationanimal1, h2tent~V2/(V1+V2+V3)),
                  pin(explorationanimal1, m2tent~V3/(V1+V2+V3)),
                  0)

exstuff2

# arena
arenastuff2 <- rbind(pin(arena.animal1, pe2arena~V1/(V1+V2+V3+V4)),
                     pin(arena.animal1, h2arena~V2/(V1+V2+V3+V4)),
                     pin(arena.animal1, m2arena~V3/(V1+V2+V3+V4)),
                     pin(arena.animal1, s2arena~V4/(V1+V2+V3+V4)))

arenastuff2
}
behaviourstuff2 <- cbind(boldstuff2$Estimate,
                         exstuff2$Estimate,
                         arenastuff2$Estimate)




# I need space for the legend, and I need to plot
# this graph alongside the h2 of personality graph


png(filename = "Figure-proportionsof-20151001.png",
    width=400, height=700, units="px")

{
par(mfrow=c(2,1),mar=c(4,5,1,0))

barplot(behaviourstuff1, ylim=c(0,0.4), xlim=c(0,5), 
        col=c("grey10", "grey40", "grey60", "white"),
        ylab="Proportion of variance",
        cex.axis=1.2, cex.lab=1.5, cex.names=1.2,
        las=2)

barplot(behaviourstuff1, ylim=c(0,0.4), xlim=c(0,5), 
        density=c(0,10,0,0), col="black", angle=c(0,45,0,135),
        cex.axis=1.2, cex.lab=1.5, cex.names=1.2,
        las=2,
        add=T)

mtext(c("Boldness","Exploration","Activity"), 
      side=1, line=c(2,1,3), at=c(1,2,3), cex=1.5, las=2)


####
# second barplot

par(mar=c(2,5,5,0))

barplot(behaviourstuff2, ylim=c(0,1), xlim=c(0,5), 
        col=c("grey10", "grey40", "grey60","white"),
        ylab="Proportion of personality",
        cex.axis=1.2, cex.lab=1.5, cex.names=1.2,
        las=2)

barplot(behaviourstuff2, ylim=c(0,1), xlim=c(0,5), 
        density=c(0,10,0,00), col="black", angle=c(0,45,0,135),
        cex.axis=1.2, cex.lab=1.5, cex.names=1.2,
        las=2,
        add=T)

legend(3.5, 1, cex=1.5,
       legend=c("s", "m","h2", "pe"),
       fill=c("white", "grey60", "grey40", "grey10"),
       bty=c("n"))

legend(3.5, 1, cex=1.5,
       legend=c("s", "m","h2", "pe"),
       density=c(0,0,10,0), angle=c(135,0,45,0),
       bty=c("n"))
}

dev.off()

# plot saved as a 400x700 plot


#########################################################
#########################################################

# 21st October 2015:
# re-doing the barplots as ggplot barplots and then plotting
# all my variance plots in a single panel:

# make my vectors of data in to data frames:

behaviourstuff3 <- data.frame(datapoints=c(boldstuff$Estimate,
                                           exstuff$Estimate,
                                           arenastuff$Estimate),
                              behaviour=rep(c("Boldness",
                                              "Exploration",
                                              "Activity"),
                                            each=4),
                              variances=rep(c("Vpe","Va","Vm","Vs"),
                                            times=3))

behaviourstuff3


behaviourstuff4 <- data.frame(datapoints=c(boldstuff2$Estimate,
                                           exstuff2$Estimate,
                                           arenastuff2$Estimate),
                              behaviour=rep(c("Boldness",
                                              "Exploration",
                                              "Activity"),
                                            each=4),
                              variances=rep(c("Vpe","Va","Vm","Vs"),
                                            times=3))

behaviourstuff4

# reorder behaviour to be plotted in order of data frame
behaviourstuff3$behaviour2 <- factor(behaviourstuff3$behaviour, 
                                     as.character(behaviourstuff3$behaviour))


behaviourstuff4$behaviour2 <- factor(behaviourstuff4$behaviour, 
                                     as.character(behaviourstuff4$behaviour))

# this leads to an error message, but gives the
# desired plot.



library(grid) # this is to use the units() function

bar.labels=c(expression(italic("V")[a]),
             expression(italic("V")[m]),
             expression(italic("V")[pe]),
             expression(italic("V")[s]))


# first, plot of proportions of variance:


bar1 <- ggplot(data=behaviourstuff3, aes(x=behaviour2, 
                                         y=datapoints, 
                                         fill=variances)) +
  theme(axis.line = element_line(colour = "black", size=0.3, lineend="butt"),
        axis.line.x = element_blank(),
        plot.title = element_text(size = 10, hjust=0, vjust=2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text=element_text(size=8, colour="black"),
        axis.text.x = element_text(angle=90),
        axis.title.x= element_blank(),
        axis.title.y=element_text(size=10, vjust=1.5),
        axis.ticks = element_line(colour = "black", size=0.3),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        legend.key = element_rect(colour = "black", size = 0.5),
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size=8, colour="black")) +
  ylab("Proportion of variance") +
  geom_bar(stat="identity", colour="black", size=0.3) +
  coord_cartesian(ylim = c(0, 0.4)) +
  scale_fill_manual(values=c("grey20", "grey50", "grey80", "white"),
                    labels=bar.labels) + 
  ggtitle("b)") +
  guides(fill=guide_legend(title=NULL,
                           override.aes = list(colour = NULL)))

# and proportions of personality

bar2 <- ggplot(data=behaviourstuff4, aes(x=behaviour2, 
                                         y=datapoints, 
                                         fill=variances)) +
  theme(axis.line = element_line(colour = "black", size=0.3, lineend="butt"),
        plot.title = element_text(size = 10, hjust=0, vjust=2),
        axis.line.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text=element_text(size=8, colour="black"),
        axis.text.x = element_text(angle=90),
        axis.title.x= element_blank(),
        axis.title.y=element_text(size=10, vjust=1.5),
        axis.ticks = element_line(colour = "black", size=0.3),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        legend.key = element_rect(colour = "black", size = 0.5),
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size=8, colour="black")) +
  ylab("Proportion of personality") +
  geom_bar(stat="identity", colour="black", size=0.3) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values=c("grey20", "grey50", "grey80", "white"),
                    labels=bar.labels) + 
  ggtitle("c)") +
  guides(fill=guide_legend(title=NULL,
                           override.aes = list(colour = NULL)))

bar1
bar2


# now I can arrange these and plot p:

# load up the multiplot function:

source("C:/Users/Issie/SkyDrive/RFunctions/multiplot-ggplot2-20151001.R")



# I can save this as an eps:
# size is chosen to roughly match JEB double column width

postscript(file="Figure2-allvarianceplots-20151118.eps",
           onefile=FALSE,width=4.33,height=4.33) 

multiplot(p2, bar1, bar2,
          layout=matrix(c(1,2,1,3), nrow=2, byrow=TRUE))

dev.off()


# or as a png to embed in the word document:

png(file="Figure2-allvarianceplots-20151118.png",
    width=4.33,height=4.33, units="in", res=96) 

multiplot(p2, bar1, bar2,
          layout=matrix(c(1,2,1,3), nrow=2, byrow=TRUE))

dev.off()


#########################################################
#########################################################


# this is script for a series of cross-plots showing how
# boldness, exploration, and nestling activity are correlated
# between individuals:

# first, make the data frame of information grouped within
# individuals:

str(tri$asrid)

tents1 <- aggregate(tri$fr, list(tri$asrid), FUN=min, na.rm=T)
tents2 <- aggregate(tri$fr, list(tri$asrid), FUN=mean, na.rm=T)
tents3 <- aggregate(tri$fr, list(tri$asrid), FUN=max, na.rm=T)

# sort out these 
tents1$x <- ifelse(tents1$x>500, NA, tents1$x)
head(tents1)
summary(tents1)

tents2$x <- ifelse(tents2$x=="NaN", NA, tents2$x)
head(tents2)
summary(tents2)

tents3$x <- ifelse(tents3$x<0, NA, tents3$x)
head(tents3)
summary(tents3)

arenas1 <- aggregate(tri$total, list(tri$asrid), FUN=min, na.rm=T)
arenas2 <- aggregate(tri$total, list(tri$asrid), FUN=mean, na.rm=T)
arenas3 <- aggregate(tri$total, list(tri$asrid), FUN=max, na.rm=T)

# sort out the weird bits
arenas1$x <- ifelse(arenas1$x>500, NA, arenas1$x)
head(arenas1)
summary(arenas1)

arenas2$x <- ifelse(arenas2$x=="NaN", NA, arenas2$x)
head(arenas2)
summary(arenas2)

arenas3$x <- ifelse(arenas3$x<0, NA, arenas3$x)
head(arenas3)
summary(arenas3)


bolds1 <- aggregate(tri$entrylatency, list(tri$asrid), FUN=min, na.rm=T)
bolds2 <- aggregate(tri$entrylatency, list(tri$asrid), FUN=mean, na.rm=T)
bolds3 <- aggregate(tri$entrylatency, list(tri$asrid), FUN=max, na.rm=T)

# dodgy bits
bolds1$x <- ifelse(bolds1$x>500, NA, bolds1$x)
head(bolds1)
summary(bolds1)

bolds2$x <- ifelse(bolds2$x=="NaN", NA, bolds2$x)
head(bolds2)
summary(bolds2)

bolds3$x <- ifelse(bolds3$x<0, NA, bolds3$x)
head(bolds3)
summary(bolds3)


cross1 <- cbind("asrid"=tents1$Group.1,
                "tmin"=tents1$x,
                "tmean"=tents2$x,
                "tmax"=tents3$x,
                "amin"=arenas1$x,
                "amean"=arenas2$x,
                "amax"=arenas3$x,
                "bmin"=bolds1$x,
                "bmean"=bolds2$x,
                "bmax"=bolds3$x)
head(cross1)
summary(cross1)

cross1 <- as.data.frame(cross1)
head(cross1)
summary(cross1)

######
# now plot three plots comparing the traits using this
# data:


png(filename = "Figure3-crossplot-20151119.png",
    width=166, height=55, units="mm", res=300)

par(mar=c(4,4,1,1), mfrow=c(1,3), mgp=c(2,0.5,0))


# boldness and exploration
plot(cross1$bmean, cross1$tmean,
     pch=16, cex=0.5,
     xlim=c(0, 50), ylim=c(0,170),
     xlab="Adult boldness (minutes to entry)",
     ylab="Adult exploration (total movements)",
     cex.axis=1,
     cex.lab=1)
arrows(cross1$bmean, cross1$tmean,
       cross1$bmean, cross1$tmin,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$bmean, cross1$tmean,
       cross1$bmean, cross1$tmax,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$bmean, cross1$tmean,
       cross1$bmin, cross1$tmean,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$bmean, cross1$tmean,
       cross1$bmax, cross1$tmean,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
points(cross1$bmean, cross1$tmean,
       pch=16, cex=0.5)




# arena and boldness
plot(cross1$amean, cross1$bmean,
     pch=16, cex=0.5,
     xlim=c(0, 83), ylim=c(0,50),
     xlab="Nestling activity (total squares)",
     ylab="Adult boldness (minutes to entry)",
     cex.axis=1,
     cex.lab=1)
arrows(cross1$amean, cross1$bmean,
       cross1$amean, cross1$bmin,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$amean, cross1$bmean,
       cross1$amean, cross1$bmax,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$amean, cross1$bmean,
       cross1$amin, cross1$bmean,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$amean, cross1$bmean,
       cross1$amax, cross1$bmean,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
points(cross1$amean, cross1$bmean,
       pch=16, cex=0.5)



# arena and tent
plot(cross1$amean, cross1$tmean,
     pch=16, cex=0.5,
     xlim=c(0, 83), ylim=c(0,170),
     xlab="Nestling activity (total squares)",
     ylab="Adult exploration (total movements)",
     cex.axis=1,
     cex.lab=1)
arrows(cross1$amean, cross1$tmean,
       cross1$amean, cross1$tmin,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$amean, cross1$tmean,
       cross1$amean, cross1$tmax,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$amean, cross1$tmean,
       cross1$amin, cross1$tmean,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$amean, cross1$tmean,
       cross1$amax, cross1$tmean,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
points(cross1$amean, cross1$tmean,
       pch=16, cex=0.5)



dev.off()

# panel saved as a 1500 by 500 pixel plot


# there is something strange with boldness and nestling
# activity.....



# as an eps:


postscript(file="Figure3-crossplot-20151119.eps",
           onefile=FALSE,width=6.5,height=2.17) 


par(mar=c(4,4,1,1), mfrow=c(1,3), mgp=c(2,0.5,0))


# boldness and exploration
plot(cross1$bmean, cross1$tmean,
     pch=16, cex=0.5,
     xlim=c(0, 50), ylim=c(0,170),
     xlab="Adult boldness (minutes to entry)",
     ylab="Adult exploration (total movements)",
     cex.axis=1,
     cex.lab=1)
arrows(cross1$bmean, cross1$tmean,
       cross1$bmean, cross1$tmin,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$bmean, cross1$tmean,
       cross1$bmean, cross1$tmax,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$bmean, cross1$tmean,
       cross1$bmin, cross1$tmean,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$bmean, cross1$tmean,
       cross1$bmax, cross1$tmean,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
points(cross1$bmean, cross1$tmean,
       pch=16, cex=0.5)




# arena and boldness
plot(cross1$amean, cross1$bmean,
     pch=16, cex=0.5,
     xlim=c(0, 83), ylim=c(0,50),
     xlab="Nestling activity (total squares)",
     ylab="Adult boldness (minutes to entry)",
     cex.axis=1,
     cex.lab=1)
arrows(cross1$amean, cross1$bmean,
       cross1$amean, cross1$bmin,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$amean, cross1$bmean,
       cross1$amean, cross1$bmax,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$amean, cross1$bmean,
       cross1$amin, cross1$bmean,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$amean, cross1$bmean,
       cross1$amax, cross1$bmean,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
points(cross1$amean, cross1$bmean,
       pch=16, cex=0.5)



# arena and tent
plot(cross1$amean, cross1$tmean,
     pch=16, cex=0.5,
     xlim=c(0, 83), ylim=c(0,170),
     xlab="Nestling activity (total squares)",
     ylab="Adult exploration (total movements)",
     cex.axis=1,
     cex.lab=1)
arrows(cross1$amean, cross1$tmean,
       cross1$amean, cross1$tmin,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$amean, cross1$tmean,
       cross1$amean, cross1$tmax,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$amean, cross1$tmean,
       cross1$amin, cross1$tmean,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
arrows(cross1$amean, cross1$tmean,
       cross1$amax, cross1$tmean,
       length=0.02, angle=90, lty=1,
       col="grey50", lwd=1)
points(cross1$amean, cross1$tmean,
       pch=16, cex=0.5)



dev.off()




##################################################
##################################################

# Figure 1 or so of the main text:

# one more thing to do is to plot my variables to 
# show the trait distributions before transformation.

# I would like to split the distributions in to male
# and female (for the boldness and exploration data) 
# and in to ages 10, 12, and 13 for the activity data

# day 13:

ar13 <- subset(ar1, ar1$age==3)
ar13


# saved as a 1500 x 500 pixel plot

# Figure 1:

png(filename = "Figure1-AllTraitHistograms-20151130.png",
    width=166, height=55, units="mm", res=300)

par(mar=c(3,3,1,1), mfrow=c(1,3), mgp=c(2,0.5,0))


# boldness:

{
hist(fboldmincomp2$entrylatency, xlim=c(0,50), ylim=c(0,50),
     breaks=seq(0,50,1), main="", xlab="Latency to enter (mintues)",
     cex.lab=1.2, cex.axis=1, col=rgb(0,0,0,0.7))

hist(mboldmincomp2$entrylatency, xlim=c(0,50), ylim=c(0,50),
     breaks=seq(0,50,1), main="", xlab="Latency to enter (mintues)",
     cex.lab=1.2, cex.axis=1, col=rgb(0.8,0.8,0.8,0.6), add=T)

legend(x=25, y=50, legend=c("Female", "Male"), 
       fill=c(rgb(0,0,0,0.7), rgb(0.8,0.8,0.8,0.6)), cex=1.2, 
       title="Boldness", bty="n")
}


# exploration:

{
hist(tent2.F$flights+tent2.F$x2r, xlim=c(0,200), ylim=c(0,80),
     breaks=seq(0,200,4), main="", xlab="Number of movements in tent",
     cex.lab=1.2, cex.axis=1, col=rgb(0,0,0,0.7))

hist(tent2.M$flights+tent2.M$x2r, xlim=c(0,200), ylim=c(0,80),
     breaks=seq(0,200,4), main="", xlab="Number of movements in tent",
     cex.lab=1.2, cex.axis=1, col=rgb(0.8,0.8,0.8,0.6), add=T)

legend(x=100, y=80, legend=c("Female", "Male"), 
       fill=c(rgb(0,0,0,0.7), rgb(0.8,0.8,0.8,0.6)), cex=1.2, 
       title="Exploration", bty="n")
}

# activity:

{
hist(ar10$total, xlim=c(0,100), ylim=c(0,500),
     breaks=seq(0,100,2), main="", xlab="Total squares entered",
     cex.lab=1.2, cex.axis=1, col=rgb(0,0,0,0.7))


hist(ar12$total, xlim=c(0,100), ylim=c(0,500),
     breaks=seq(0,100,2), main="", xlab="Total squares entered",
     cex.lab=1.2, cex.axis=1, col=rgb(0.8,0.8,0.8,0.6), add=T)


hist(ar13$total, xlim=c(0,100), ylim=c(0,500),
     breaks=seq(0,100,2), main="", xlab="Total squares entered",
     cex.lab=1.2, cex.axis=1, col=rgb(0,0,0,1), add=T)


legend(x=40, y=500, legend=c("day 10", "day 12", "day 13"), 
       fill=c(rgb(0,0,0,0.7), rgb(0.8,0.8,0.8,0.6), rgb(0,0,0,1)), 
       cex=1.2, title="Nestling Activity", bty="n")
}

dev.off()




# Figure 1 as an eps:
# use cairo_ps because this allows transparent
# eps colours:


cairo_ps(file="Figure1-AllTraitHistograms-20151130.eps",
           onefile=FALSE,width=6.5,height=2.17) 

{
par(mar=c(3,3,1,1), mfrow=c(1,3), mgp=c(2,0.5,0))


# boldness:


hist(fboldmincomp2$entrylatency, xlim=c(0,50), ylim=c(0,50),
     breaks=seq(0,50,1), main="", xlab="Latency to enter (mintues)",
     cex.lab=1.2, cex.axis=1, col=rgb(0,0,0,0.7))

hist(mboldmincomp2$entrylatency, xlim=c(0,50), ylim=c(0,50),
     breaks=seq(0,50,1), main="", xlab="Latency to enter (mintues)",
     cex.lab=1.2, cex.axis=1, col=rgb(0.8,0.8,0.8,0.6), add=T)

legend(x=25, y=50, legend=c("Female", "Male"), 
       fill=c(rgb(0,0,0,0.7), rgb(0.8,0.8,0.8,0.6)), cex=1.2, 
       title="Boldness", bty="n")



# exploration:

hist(tent2.F$flights+tent2.F$x2r, xlim=c(0,200), ylim=c(0,80),
     breaks=seq(0,200,4), main="", xlab="Number of movements in tent",
     cex.lab=1.2, cex.axis=1, col=rgb(0,0,0,0.7))

hist(tent2.M$flights+tent2.M$x2r, xlim=c(0,200), ylim=c(0,80),
     breaks=seq(0,200,4), main="", xlab="Number of movements in tent",
     cex.lab=1.2, cex.axis=1, col=rgb(0.8,0.8,0.8,0.6), add=T)

legend(x=100, y=80, legend=c("Female", "Male"), 
       fill=c(rgb(0,0,0,0.7), rgb(0.8,0.8,0.8,0.6)), cex=1.2, 
       title="Exploration", bty="n")


# activity:

hist(ar10$total, xlim=c(0,100), ylim=c(0,500),
     breaks=seq(0,100,2), main="", xlab="Total squares entered",
     cex.lab=1.2, cex.axis=1, col=rgb(0,0,0,0.7))


hist(ar12$total, xlim=c(0,100), ylim=c(0,500),
     breaks=seq(0,100,2), main="", xlab="Total squares entered",
     cex.lab=1.2, cex.axis=1, col=rgb(0.8,0.8,0.8,0.6), add=T)


hist(ar13$total, xlim=c(0,100), ylim=c(0,500),
     breaks=seq(0,100,2), main="", xlab="Total squares entered",
     cex.lab=1.2, cex.axis=1, col=rgb(0,0,0,1), add=T)


legend(x=40, y=500, legend=c("day 10", "day 12", "day 13"), 
       fill=c(rgb(0,0,0,0.7), rgb(0.8,0.8,0.8,0.6), rgb(0,0,0,1)), 
       cex=1.2, title="Nestling Activity", bty="n")
}

dev.off()


############################################################
############################################################

# now the plots for the supplement of whether my watches
# differ from those of Andres, and whether I differ from
# myself. Load up the data from the randomisation script
# (see the folder under personality-BRandAR about 
# the randomisation).

# what are the correlations like between me and Andres:

cor.test(ar.Andres$IWtotal, ar.Andres$total.sqs, 
         method="pearson")
cor.test(ar.Andres$IWtotal, ar.Andres$total.sqs, 
         method="kendall")
cor.test(ar.Andres$IWtotal, ar.Andres$total.sqs, 
         method="spearman")


# take the subset that were used for the randomisation:

ar.r <- ar1[-which(is.na(ar1$rand.total)),]

summary(ar.r)
length(ar.r[,1])

# what are the correlations like between me and myself

cor.test(ar.r$total, ar.r$rand.total, 
         method="pearson")
cor.test(ar.r$total, ar.r$rand.total, 
         method="kendall")
cor.test(ar.r$total, ar.r$rand.total, 
         method="spearman")


# plots for the supplement



# Figure S1

png(filename = "FigureS1-20151119.png",
    width=6.5,height=3.2, units="in", res=300)

par(mfrow=c(1,2), mar=c(3,4,2,2), mgp=c(1.5,0.5,0))

# S1a - how I differ from Andres
{
  plot(jitter(ar.Andres$IWtotal), jitter(ar.Andres$total.sqs), 
       xlab="Nestling activity by observer 1 (jittered)",
       ylab="Nestling activity\nby observer 2 (jittered)",
       pch=16, col=rgb(0,0,0,0.5),
       ylim=c(0,27), xlim=c(0,27),
       cex.lab=0.9, cex.axis=0.8, cex=0.5)
  
  mtext("a)", line=0.5, adj=0)
  
  lines(x=c(0,27),y=c(0,27), lty=2, lwd=1, col=rgb(0,0,0,0.5))
  
  cor.test(ar.Andres$IWtotal, ar.Andres$total.sqs, 
           method="pearson")
  
  text(x=10, y=24, 
       expression(paste(italic(r)==0.96,", ",italic(t)==38.6,", ")),
       cex=0.8)
  
  text(x=10, y=22, 
       expression(paste(italic(df)==142,", ",italic(p)<0.001)),
       cex=0.8)
  
}

# S1b - how I differ from myself after randomisation
{
  plot(jitter(ar.r$total), jitter(ar.r$rand.total), 
       xlab="Nestling activity by observer 1 (jittered)",
       ylab="Nestling activity\nfrom randomised videos (jittered)",
       pch=16, col=rgb(0,0,0,0.5),
       ylim=c(0,85), xlim=c(0,85),
       cex.lab=0.9, cex.axis=0.8, cex=0.5)
  
  mtext("b)", line=0.5, adj=0)
  
  lines(x=c(0,83),y=c(0,83), lty=2, lwd=1, col=rgb(0,0,0,0.5))
  
  cor.test(ar.r$total, ar.r$rand.total, method="pearson")
  
  text(x=30, y=75.5, 
       expression(paste(italic(r)==0.97,", ",italic(t)==71.0,", ")),
       cex=0.8)
  
  text(x=30, y=69, 
       expression(paste(italic(df)==268,", ",italic(p)<0.001)),
       cex=0.8)
  
}

dev.off()


# and the differences in repeatability to quote in the
# text:


# LMM gaussian in mcmcLMM
rands7 <- rpt.remlLMM(log(ar.r$total-0.5), groups=ar.r$asrid)
print(rands7)

rands8 <- rpt.remlLMM(log(ar.r$rand.total-0.5), groups=ar.r$asrid)
print(rands8)

# day 10 only:
table(ar.r$age)
table(ar1$age)

ar.r10 <- subset(ar.r, ar.r$age==1)


rands9 <- rpt.remlLMM(log(ar.r10$total-0.5), groups=ar.r10$asrid)
print(rands9)

rands10 <- rpt.remlLMM(log(ar.r10$rand.total-0.5), groups=ar.r10$asrid)
print(rands10)


# day 12:

ar.r12 <- subset(ar.r, ar.r$age==0)

length(ar.r12$birdid)
length(unique(ar.r12$birdid))


rands11 <- rpt.remlLMM(log(ar.r12$total-0.5), groups=ar.r12$asrid)
print(rands11)

rands12 <- rpt.remlLMM(log(ar.r12$rand.total-0.5), groups=ar.r12$asrid)
print(rands12)

############################################################
############################################################

# Supplementary information.




# Figure S2

library(lattice)

# this is from the example(wireframe) last two plots:
par.set <- list(axis.line = list(col = "transparent"),
                clip = list(panel = "off"))

top.left = wireframe(plot.power.bold~natv*h2, data=plot.power.bold,
                      scales=list(arrows=F, xlim=c(0,1), cex=0.8, distance=1.4),
                      xlab=list(label="Maternal\neffect variance",
                                cex=1),
                      ylab=list(label="Heritability",
                                cex=1),
                      zlab=list(label="Power - boldness",
                                cex=1, rot=90),
                      zlim=c(0:1),
                      par.settings = par.set,
                     zoom=1)

top.right = wireframe(plot.power.expl~natv*h2, data=plot.power.expl,
          scales=list(arrows=F, xlim=c(0,1), cex=0.8, distance=1.4),
          xlab=list(label="Maternal\neffect variance",
                    cex=1),
          ylab=list(label="Heritability",
                    cex=1),
          zlab=list(label="Power - exploration",
                    cex=1, rot=90),
          zlim=c(0:1),
          par.settings = par.set,
          zoom=1)

bottom.left = wireframe(plot.power.act~natv*h2, data=plot.power.act,
                      scales=list(arrows=F, xlim=c(0,1), cex=0.8, distance=1.4),
                      xlab=list(label="Maternal\neffect variance",
                                cex=1),
                      ylab=list(label="Heritability    ",
                                cex=1),
                      zlab=list(label="Power - nestling activity",
                                cex=1, rot=90),
                      zlim=c(0:1),
                      par.settings = par.set,
                      zoom=1)



# now for the plot of type 1 error (detecting 
# heritability where there is none to be detected)


# combine all these type 1 error plots in to 
# a single plot

# I have the script to do this as a normal plot so
# rather than re-write how this plot works I am going
# to save it as an object with recordPlot:

par(mar=c(5,4,2,2), mfrow=c(2,2), mgp=c(3,1,0))
plot.new()
plot.new()
plot.new()
plot(x=seq(0,0.3,0.1), 
     y=seq(0,0.03,0.01), 
     ylim=c(0,0.032),
     type="n", 
     cex.lab=0.9, cex.axis=0.8,
     xlab="Confounding variance",
     ylab="Type 1 error frequency")

lines(x=plot.power.bold[1:7,3], y=plot.power.bold[1:7,1],
      lwd=2, lty=1)
lines(x=plot.power.expl[1:7,3], y=plot.power.expl[1:7,1],
      lwd=2, lty=2)
lines(x=plot.power.act[1:7,3], y=plot.power.act[1:7,1],
      lwd=2, lty=3)

legend(0.1, 0.015,
       legend=c("Boldness", 
                "Exploration", 
                "Nestling activity"),
       lty=c(1,2,3), lwd=2, bty="n",
       cex=0.8)


bottom.right <- recordPlot()

# plot recorded. Now clear the plot space
plot.new()

# and plot the four together:



png(filename = "FigureS2-20151130.png",
    width=6.5,height=6.5, units="in", res=300)

print(bottom.right)
print(top.left, split=c(1,1,2,2), more=TRUE)
print(top.right, split=c(2,1,2,2), more=TRUE)
print(bottom.left, split=c(1,2,2,2), more=TRUE)

dev.off()


############################################################
############################################################

# reset everything back to normal:

par(mar=c(5,4,4,2), mfrow=c(1,31), mgp=c(3,1,0))

setwd("./masterdatasheets")