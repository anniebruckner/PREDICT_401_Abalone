# Data Analysis Assignment 02
# Andrea Bruckner
# PREDICT 401 Sec 56

# create the data frame of the abalone.csv
abalone <- read.table(file.path("/Users/annie/Desktop/PREDICT_401/DataAnalysis02/abalone.csv"))

# create the data frame of the mydata.csv
mydata <- read.table(file.path("/Users/annie/Desktop/PREDICT_401/DataAnalysis02/mydata.csv"))

# Step 1
# plot mydata
pdf("CharacteristicsMatrix.pdf")
plot(mydata[,2:8])
title(main="Correlations of Abalone Characteristics")
dev.off()

x <- mydata[,2:4]
cor(x,x,method=c("pearson")) # spatial - spatial; linear

y <- mydata[,5:8]
cor(x,y,method=c("spearman")) # spatial - weight; curvilinear

cor(y,y,method=c("pearson")) # weight - weight; linear

# Step 2
# matrix of boxplots showing SHUCK differentiated by CLASS and SEX
idxi <- mydata[,1]=="I"
idxm <- mydata[,1]=="M"
idxf <- mydata[,1]=="F"

pdf("ShuckBoxPlots.pdf")
par(mfrow=c(3,1))
boxplot(mydata$SHUCK[idxi] ~ mydata$CLASS[idxi], col = "darkseagreen", main = "Infant SHUCK", ylab = "SHUCK")
boxplot(mydata$SHUCK[idxm] ~ mydata$CLASS[idxm], col = "steelblue", main = "Male SHUCK", ylab = "SHUCK")
boxplot(mydata$SHUCK[idxf] ~ mydata$CLASS[idxf], col = "thistle", main = "Female SHUCK", ylab = "SHUCK")
par(mfrow=c(1,1))
dev.off()

# Step 3
# calculate the Pearson chi square statistic on 2x2 contingency tables
# which have the marginal totals

shuck <- factor(mydata$SHUCK > median(mydata$SHUCK), labels = c("below", "above"))
volume <- factor(mydata$VOLUME > median(mydata$VOLUME), labels = c("below", "above"))

shuck_volume <- addmargins(table(shuck,volume))
shuck_volume

# Now we're looking at a contingency table of our above | below vectors.
# We see an obvious pattern: individuals who are below the median volume
# are most often below the median shuck weight; individuals who are above
# the median volume are most often above the median shuck weight. We see
# 49 individuals (24 + 25) of our total 500, less than 10%, who don't follow this pattern.

my_function <- function(x) { # I'm assuming our 2 x 2 table with margins is called "x"
  # Expected values:
  e11 <- x[3,1]*x[1,3]/x[3,3] # 250 * 251 / 500
  e12 <- x[3,2]*x[1,3]/x[3,3] # 250 * 251 / 500
  e21 <- x[3,1]*x[2,3]/x[3,3] # 250 * 249 / 500
  e22 <- x[3,2]*x[2,3]/x[3,3] # 250 * 249 / 500
  chisqStat <- (x[1,1]-e11)^2/e11+(x[1,2]-e12)^2/e12+(x[2,1]-e21)^2/e21+(x[2,2]-e22)^2/e22
  return(chisqStat)
}
my_function(shuck_volume)

pchisq(323.2132, 1, lower.tail = FALSE) # will give the p-value given a quantile q

# Step 4
# Perform an analysis of variance with aov() on SHUCK using CLASS and SEX as
# the grouping variables. Assume equal variances.

anovaModel1 <- aov(SHUCK ~ CLASS*SEX, mydata)

# NOTE: the above is the same as "SHUCK~CLASS+SEX+CLASS*SEX" -
# R reads the "*" to mean include all variables and interaction term.
# Same as aov.shuck <- aov(SHUCK~CLASS+SEX+CLASS*SEX, mydata)

summary(anovaModel1)

anovaModel2 <- aov(SHUCK ~ CLASS+SEX, mydata)
summary(anovaModel2)

TukeyHSD(anovaModel2)

# Step 5
# install ggplot2
install.packages("ggplot2")
library(ggplot2)

# install gridExtra
install.packages("gridExtra")
library(gridExtra)

# form a scatterplot of SHUCK versus VOLUME
pdf(file="Plots.pdf",onefile = TRUE, width = 11, height = 8.5)
plot1<-ggplot(mydata, aes(x = VOLUME, y = SHUCK)) + geom_point(size=4, aes(color = CLASS)) +
theme_bw() + ggtitle("Abalone Shucked Weight (g) versus Volume (mm3)")

# form a scatterplot of their logarithms

L_SHUCK<-log(mydata$SHUCK)
L_VOLUME<-log(mydata$VOLUME)

#pdf(file="Plot2.pdf")
plot2<-ggplot(mydata, aes(x = L_VOLUME, y = L_SHUCK)) + geom_point(size=4, aes(color = CLASS)) +
  theme_bw() + ggtitle("Logarithms of Abalone Shucked Weight (g)\n versus Volume (mm3)")
grid.arrange(plot1,plot2,ncol=2)
dev.off()

# Step 6
# regress L_SHUCK
out<-lm(L_SHUCK~L_VOLUME+CLASS+SEX, mydata)
out
summary(out)

# Log transformation, and data transformation generally, will be covered in later courses.
# Log transformation is very commonly used for skewed or "widely" distributed data - i.e. data over > 1 order of magnitude.
# What we want to see in the scatterplots - logged versus not - is that the logged data doesn't show the same slight
# wedge-shape at increasing values; instead, we see the points - even at higher values - follow our linear relationship
# very closely.

# Step 7
# install moments
install.packages("moments")
library(moments)

# Perform an analysis of the residuals. If “out” is the regression object, use out$residuals and
# construct a histogram and QQ plot. Compute the skewness and kurtosis. The residuals should approximate
# a normal distribution. Describe the distribution of residuals. Use ggplot. Plot the residuals versus L_VOLUME
# coloring the data points by CLASS and a second time coloring the data points by SEX. Also use ggplot to present
# boxplots of the residuals differentiated by SEX and CLASS. How well does the regression model fit the data?

pdf(file="Hist.pdf")
hist(out$residuals, col = "burlywood", main = "Histogram of Residuals", xlab = "Residual")
dev.off()

pdf(file="QQ.pdf")
qqnorm(out$residuals, main="QQ Plot of Residuals")
qqline(out$residuals)
dev.off()
skewness(out$residuals)
kurtosis(out$residuals)

pdf("GGplots.pdf",onefile = TRUE, width = 11, height = 8.5)
ggclass<-ggplot(out, aes(x = L_VOLUME,y = out$residuals)) + geom_point(aes(color = CLASS),size=4) + labs(x = "L_VOLUME", y = "Residual")+
  ggtitle("Abalone Residuals versus \nLogarithm Volume per CLASS")+theme_bw()
ggsex<-ggplot(out, aes(x = L_VOLUME,y = out$residuals)) + geom_point(aes(color = SEX),size=4) + labs(x = "L_VOLUME", y = "Residual")+
  ggtitle("Abalone Residuals versus \nLogarithm Volume per SEX")+theme_bw()+
scale_color_manual(name="SEX", values=c("thistle", "darkseagreen", "steelblue"),
                   breaks=c("I", "M", "F"), labels=c("Infant", "Male", "Female"))
grid.arrange(ggclass,ggsex,ncol=2)
dev.off()

pdf("boxresiduals.pdf")
box1<-ggplot(mydata, aes(mydata$CLASS, out$residuals)) + 
  geom_boxplot() + theme_bw()+
  theme(legend.position = "none")+xlab("AGE CLASS")+ylab("Residual")+
  ggtitle("Abalone Residuals\n per Age Classification")

box2<-ggplot(mydata, aes(mydata$SEX, out$residuals)) + 
  geom_boxplot() + theme_bw()+
  theme(legend.position = "none")+xlab("SEX")+ylab("Residual")+
  ggtitle("Abalone Residuals\n per Sex")
grid.arrange(box1,box2,ncol=2)
dev.off()

# Step 8 for Infants
# indexes for I, M, and F already created for earlier part of assignment

max.v <- max(mydata$VOLUME)
min.v <- min(mydata$VOLUME)
delta <- (max.v - min.v)/100
prop.infants <- numeric(0)
volume.value <- numeric(0)
totali <- length(mydata[idxi,1]) # This value must be changed for adults.

for (k in 1:100)
{
  value <- min.v + k*delta
  volume.value[k] <- value
  prop.infants[k] <- sum(mydata$VOLUME[idxi] <= value)/totali
}

n.infants <- sum(prop.infants <= 0.5)
split.infants <- min.v + (n.infants + 0.5)*delta # This estimates the desired volume.
pdf("propi.pdf")
plot(volume.value, prop.infants, col = "orange", main = "Proportion of Infants Not Harvested",
     type = "l", lwd = 2)
abline(h=0.5)
abline(v = split.infants)
dev.off()

# If an abalone has a volume below a specified cutoff, it is not harvested. The vector
# prop.infants shows the impact of increasing the volume cutoff for harvesting. An
# increasing proportion of the infant population does not qualify for harvesting.
# This shows how to determine volume levels which "split" the population at
# any specified harvest proportion. For example using a 50% harvest of infants, the corresponding volume is:

# Step 8 for Adults

idxa <- mydata[,1]=="M" | mydata[,1]=="F"

prop.adults <- numeric(0)
volume.value <- numeric(0)
totala <- length(mydata[idxa,1]) # This value must be changed for adults.
for (k in 1:100)
{
  value <- min.v + k*delta
  volume.value[k] <- value
  prop.adults[k] <- sum(mydata$VOLUME[idxa] <= value)/totala
}

n.adults <- sum(prop.adults <= 0.5)
split.adults <- min.v + (n.adults + 0.5)*delta # This estimates the desired volume.
pdf("propa.pdf")
plot(volume.value, prop.adults, col = "orange", main = "Proportion of Adults Not Harvested",
     type = "l", lwd = 2)
abline(h=0.5)
abline(v = split.adults)
dev.off()

# It is essential that the males and females be combined into a single count as "adults"
# for computing the proportion for "adults". Part #9) will require plotting of infants versus adults.
# For this plotting to be accomplished, a "for loop", similar to the one above, must be used to compute
# the adult harvest proportions. It must use the same value for the constants min.v and delta. It must 
# also use the statement “for (k in 1:100)”. Otherwise, the resulting adult proportions cannot be directly 
# compared to the infant proportions.

# Step 9
# This part will address the determination of a volume or set of volumes which maximize the difference in
# percentages of adults and infants. To calculate this result, the proportions from #8) must be used.
# These proportions must be converted from "not harvested" proportions to "harvested" proportions by 
# subtracting (1-prop.infants) from (1-prop.adults). (The reason the proportion for infants drops sooner 
# than adults, is that infants are maturing and becoming adults with larger volumes.) From the plot 
# generated, select a range of values which have the potential to maximize the difference in harvest 
# proportions.

# Part (a)(3) in item (9) ask you to plot the difference of prop.adults and prop.infants over volume.value. 
# This is sort of a separate request from the harvest/not harvest, 1 - work. All we need to do is define 
# a new vector, like harvestdiff to be equal to prop.infants - prop.adults.The order, for this plot, is
# meant to be prop.infants - prop.adults.

# There is some unclear phrasing around the harvest/not harvest item. We have two "not harvest" vectors:
# prop.adults and prop.infants. We want to create two "harvest" vectors, and we do this by defining new 
# vectors to be equal to (1 - prop.adults) and (1 - prop.infants), respectively.

# I believe the only thing throwing your final "proportion of adults harvested under our no infants threshold" off
# was that you defined "totali" and "totala" vectors to be your total infant and total adult counts, but your for-loops
# called on "total" - which may have been the total infant count, just an earlier version that got defined but wasn't
# meant to be called and used in this case. Either way, i changed the loops to used the "totali" and "totala" vectors, 
# and I had the correct proportion returned. Additionally, i did modify "harvestdiff" to be prop.infants - prop.adults.

harvesti <- 1-prop.infants
harvesti
harvesta <- 1-prop.adults
harvesta

# harvested portions
harvestdiff <- prop.infants - prop.adults

# plot of (1-prop.infants) versus volume.value
pdf("1propi.pdf")
plot(volume.value, harvesti, col = "green", main = "Proportion of Infants Harvested to Volume",
     type = "l", lwd = 2)
abline(h=0.5)
abline(v = split.infants)
dev.off()

# plot of (1-prop.adults) versus volume.value
pdf("1propa.pdf")
plot(volume.value, harvesta, col = "green", main = "Proportion of Adults Harvested to Volume",
     type = "l", lwd = 2)
abline(h=0.5)
abline(v = split.adults)
dev.off()

# plot of the difference (prop.infants-prop.adults) versus volume.value
pdf("1propb.pdf")
plot(volume.value, harvestdiff, col = "green",
     main = "Proportion of Abalone Harvested to Volume", type = "l", lwd = 2)
abline(v = c(volume.value[which.max(harvestdiff) - 2],
             volume.value[which.max(harvestdiff) + 5]), lty = 2)
dev.off()

# To add only one vertical line at the maximum, replace the above abline() with:
# abline(v = volume.value[which.max(harvestdiff) ], lty = 2)

# plot ROC curve by plotting (1-prop.adults) versus (1-prop.infants)
# Can improve with labels, colors, etc.

pdf("roc.pdf")
plot(harvesti, harvesta, col = "green", main = "Proportion of Abalone Harvested to Volume",
     type = "l", lwd = 2)
dev.off()

# the below is the same as the above
# plot(1 - prop.infants, 1 - prop.adults, type = "l")

noInfants <- 1-prop.infants == 0 # logical index of whether 1 - prop.infants element equals zero
volume.value[noInfants] # this returns all the volumes for which 1 - prop.infants is equal to zero, we just want the smallest volume, so
min(volume.value[noInfants])

noAdults <- 1-prop.adults == 0 # logical index of whether 1 - prop.adults element equals zero
volume.value[noAdults] # this returns all the volumes for which 1 - prop.adults is equal to zero, we just want the smallest volume, so
min(volume.value[noAdults])

# To arrive at our threshold, consider our (1 - prop.infants) vector:
# This vector, includes the proportion of infants harvested at increasing volumes.
# Do remember that the volumes we're talking about come directly from our volume.value vector that we created.
# If you print and look at 1 - prop.infants, you'll see decreasing proportions until at element 48 we start seeing zeros;
# zeros for the rest of the vector. This is because the volume.value[48] is greater than all our infants:

volume.value[48]

#Looking down our vector of remaining zeros, we effectively have no one left to harvest.
# However, if we choose this volume as our harvest rule - don't harvest any individual below this volume -
# we'd avoid all infants.

# So, we want two values:

# What is the smallest volume in volume.value that doesn't see us
# harvest any proportion of infants?
min(volume.value[1 - prop.infants == 0])

# What proportion of adults are we harvesting at this volume?
max(1 - prop.adults[1 - prop.infants == 0])

# Alternatively,
(1 - prop.adults)[48]

# So, we've found a volume threshold that lets us harvest roughly 32.5% of adults while not harvesting any infants.


# Step 10
# Using the code supplied, find the largest volume which produces a zero harvest of abalone 
# in classes A1 and A2. This can be accomplished by substituting values into the code supplied. 
# The level of precision required can be achieved with values such as 0.036, 0.035, 0.034…
# More than that is unnecessary. Report your result and discuss how this compares to the volumes 
# identified from the plot of differences in harvesting proportions shown in #9).

cutoff <- 0.035 # Example volume cutoff value. Hint, a smaller volume cutoff works.

index.A1 <- (mydata$CLASS=="A1")
indexi <- index.A1 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A1) #[1] 0

index.A2 <- (mydata$CLASS=="A2")
indexi <- index.A2 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A2) #[1] 0

index.A3 <- (mydata[,10]=="A3")
indexi <- index.A3 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A3) #[1] 0.04545455

index.A4 <- (mydata[,10]=="A4")
indexi <- index.A4 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A4) #[1] 0.03296703

index.A5 <- (mydata[,10]=="A5")
indexi <- index.A5 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A5) #[1] 0.02857143

index.A6 <- (mydata[,10]=="A6")
indexi <- index.A6 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A6) #[1] 0.05714286

