# Data Analysis Assignment 01
# Andrea Bruckner
# PREDICT 401 Sec 56

# create the data frame of the abalone.csv
abalone <- read.table(file.path("/Users/annie/Desktop/PREDICT_401/DataAnalysis01/abalone.csv"))

# examine the structure of the data frame
str(abalone)

# look at the first few records of the data frame
head(abalone)

# look at the last few records of the data frame
tail(abalone)

# look at descriptive statistics 
summary(abalone)

# prior to random sampling, seed the random number generator 
# so that results will be reproducible
set.seed(123)

# select SRS of 500
SRS <- abalone[sample(1:nrow(abalone), 500),]

# write .csv
mydata.csv <- write.table(SRS, file = "/Users/annie/Desktop/PREDICT_401/DataAnalysis01/mydata.csv")

# create the data frame of the mydata.csv
mydata <- read.table(file.path("/Users/annie/Desktop/PREDICT_401/DataAnalysis01/mydata.csv"))

# examine the structure of the data frame
str(mydata)

# look at the first few records of the data frame
head(mydata)

# look at the last few records of the data frame
tail(mydata)

# look at descriptive statistics 
summary(mydata)

# plot mydata
pdf("CharacteristicsMatrix.pdf")
plot(mydata[,2:8])
title(main="Correlations of Abalone Characteristics")
dev.off()

# proportions of infant, female, and male in abalone.csv
p_infants<-sum(abalone[,1]=="I")/length(abalone[,1])
p_males<-sum(abalone[,1]=="M")/length(abalone[,1])
p_females<-sum(abalone[,1]=="F")/length(abalone[,1])

# proportions of infant, female, and male in mydata.csv
p_infants_md<-sum(mydata[,1]=="I")/length(mydata[,1])
p_males_md<-sum(mydata[,1]=="M")/length(mydata[,1])
p_females_md<-sum(mydata[,1]=="F")/length(mydata[,1])

# prop.test for abalone
prop.test(sum(abalone[,1]=="I"),4141, p = p_infants, alternative = c("two.sided"), conf.level = 0.95, correct = FALSE)
prop.test(sum(abalone[,1]=="M"),4141, p = p_males, alternative = c("two.sided"), conf.level = 0.95, correct = FALSE)
prop.test(sum(abalone[,1]=="F"),4141, p = p_females, alternative = c("two.sided"), conf.level = 0.95, correct = FALSE)

# prop.test for mydata
prop.test(sum(mydata[,1]=="I"),500, p = p_infants, alternative = c("two.sided"), conf.level = 0.95, correct = FALSE)
prop.test(sum(mydata[,1]=="M"),500, p = p_males, alternative = c("two.sided"), conf.level = 0.95, correct = FALSE)
prop.test(sum(mydata[,1]=="F"),500, p = p_females, alternative = c("two.sided"), conf.level = 0.95, correct = FALSE)

# indexes
indexi<-abalone$SEX=="I"
indexm<-abalone$SEX=="M"
indexf<-abalone$SEX=="F"

indexi_md<-mydata$SEX=="I"
indexm_md<-mydata$SEX=="M"
indexf_md<-mydata$SEX=="F"

# sum of indexes
sum(indexi)
sum(indexm)
sum(indexf)

sum(indexi_md)
sum(indexm_md)
sum(indexf_md)

# create VOLUME
volume<-c(mydata$LENGTH*mydata$DIAM*mydata$HEIGHT)

# add VOLUME variable to mydata
data.frame(mydata$VOLUME<-volume) # or mydata["VOLUME"]<-volume

# remove VOLUME using mydata$VOLUME<-NULL if necessary

# plot WHOLE v VOLUME
pdf("WholeVolume.pdf")
plot(WHOLE~VOLUME, mydata, ann=FALSE)
title(main="Whole Weight (g) versus Volume (mm^3) of Abalone", xlab = "VOLUME", ylab = "WHOLE WEIGHT")
dev.off()

# create DENSITY
density<-c(mydata$WHOLE/mydata$VOLUME)

# add DENSITY variable to mydata
data.frame(mydata$DENSITY<-density)

# create histograms
hist(mydata[mydata$SEX=="I",]$VOLUME)
hist(mydata[mydata$SEX=="I",]$WHOLE)
hist(mydata[mydata$SEX=="I",]$DENSITY)

hist(mydata[mydata$SEX=="M",]$VOLUME)
hist(mydata[mydata$SEX=="M",]$WHOLE)
hist(mydata[mydata$SEX=="M",]$DENSITY)

hist(mydata[mydata$SEX=="F",]$VOLUME)
hist(mydata[mydata$SEX=="F",]$WHOLE)
hist(mydata[mydata$SEX=="F",]$DENSITY)

# create histogram matrix
pdf("Histograms.pdf")
par(mfrow=c(3,3))
hist(mydata[mydata$SEX=="I",]$VOLUME, xlab="Volume", main="Volume (mm3)\n of Infant Abalone", col="darkseagreen")
hist(mydata[mydata$SEX=="M",]$VOLUME, xlab="Volume", main="Volume (mm3)\n of Male Abalone", col="steelblue")
hist(mydata[mydata$SEX=="F",]$VOLUME, xlab="Volume", main="Volume (mm3)\n of Female Abalone", col="thistle")

hist(mydata[mydata$SEX=="I",]$WHOLE, xlab="Whole Weight", main="Whole Weight (g)\n of Infant Abalone", col="darkseagreen")
hist(mydata[mydata$SEX=="M",]$WHOLE, xlab="Whole Weight", main="Whole Weight (g)\n of Male Abalone", col="steelblue")
hist(mydata[mydata$SEX=="F",]$WHOLE, xlab="Whole Weight", main="Whole Weight (g)\n of Female Abalone", col="thistle")

hist(mydata[mydata$SEX=="I",]$DENSITY, xlab="Density", main="Density (g/mm3)\n of Infant Abalone", col="darkseagreen")
hist(mydata[mydata$SEX=="M",]$DENSITY, xlab="Density", main="Density (g/mm3)\n of Male Abalone", col="steelblue")
hist(mydata[mydata$SEX=="F",]$DENSITY, xlab="Density", main="Density (g/mm3)\n of Female Abalone", col="thistle")
dev.off()

# create boxplots
boxplot(mydata[mydata$SEX=="I",]$VOLUME)
boxplot(mydata[mydata$SEX=="I",]$WHOLE)
boxplot(mydata[mydata$SEX=="I",]$DENSITY)

boxplot(mydata[mydata$SEX=="M",]$VOLUME)
boxplot(mydata[mydata$SEX=="M",]$WHOLE)
boxplot(mydata[mydata$SEX=="M",]$DENSITY)

boxplot(mydata[mydata$SEX=="F",]$VOLUME)
boxplot(mydata[mydata$SEX=="F",]$WHOLE)
boxplot(mydata[mydata$SEX=="F",]$DENSITY)

# create boxplots matrix
pdf("Boxplots.pdf")
par(mfrow=c(3,3))
boxplot(mydata[mydata$SEX=="I",]$VOLUME, col="darkseagreen")
title("Volume (mm3)\n of Infant Abalone",xlab="",ylab="VOLUME")
boxplot(mydata[mydata$SEX=="M",]$VOLUME, col="steelblue")
title("Volume (mm3)\n of Male Abalone",xlab="",ylab="VOLUME")
boxplot(mydata[mydata$SEX=="F",]$VOLUME, col="thistle")
title("Volume (mm3)\n of Female Abalone",xlab="",ylab="VOLUME")
boxplot(mydata[mydata$SEX=="I",]$WHOLE, col="darkseagreen")
title("Whole Weight (g)\n of Infant Abalone",xlab="",ylab="WHOLE")
boxplot(mydata[mydata$SEX=="M",]$WHOLE, col="steelblue")
title("Whole Weight (g)\n of Male Abalone",xlab="",ylab="WHOLE")
boxplot(mydata[mydata$SEX=="F",]$WHOLE, col="thistle")
title("Whole Weight (g)\n of Female Abalone",xlab="",ylab="WHOLE")
boxplot(mydata[mydata$SEX=="I",]$DENSITY, col="darkseagreen")
title("Density (g/mm3)\n of Infant Abalone",xlab="",ylab="DENSITY")
boxplot(mydata[mydata$SEX=="M",]$DENSITY, col="steelblue")
title("Density (g/mm3)\n of Male Abalone",xlab="",ylab="DENSITY")
boxplot(mydata[mydata$SEX=="F",]$DENSITY, col="thistle")
title("Density (g/mm3)\n of Female Abalone",xlab="",ylab="DENSITY")
dev.off()

# create qq plots
pdf("QQPlots.pdf")
par(mfrow=c(3,1))
qqnorm(mydata[mydata$SEX=="I",]$DENSITY, main="Density (g/mm3) of Infant Abalone")
qqline(mydata[mydata$SEX=="I",]$DENSITY)
qqnorm(mydata[mydata$SEX=="M",]$DENSITY, main="Density (g/mm3) of Male Abalone")
qqline(mydata[mydata$SEX=="M",]$DENSITY)
qqnorm(mydata[mydata$SEX=="F",]$DENSITY, main="Density (g/mm3)  of Female Abalone")
qqline(mydata[mydata$SEX=="F",]$DENSITY)
dev.off()

# install ggplot2
install.packages("ggplot2")
library(ggplot2)

# install gridExtra
install.packages("gridExtra")
library(gridExtra)

# create ggplots
pdf(file="GGPlots.pdf", onefile = TRUE, width = 11, height = 8.5)
ggwhole<-ggplot(mydata, aes(x = RINGS, y = WHOLE)) + ylab("WHOLE WEIGHT") + geom_point(size=4, aes(color = SEX))+
  scale_color_manual(values=c("thistle", "darkseagreen", "steelblue"),
                     breaks=c("I", "M", "F"), labels=c("Infant", "Male", "Female"))+theme_bw()+
  ggtitle("Abalone Whole Weight (g)\n and Shell Ring Count")

ggvolume<-ggplot(mydata, aes(x = RINGS, y = VOLUME)) + geom_point(size=4, aes(color = SEX))+
  scale_color_manual(values=c("thistle", "darkseagreen", "steelblue"),
                     breaks=c("I", "M", "F"), labels=c("Infant", "Male", "Female"))+theme_bw()+
  ggtitle("Abalone Volume (mm3)\n and Shell Ring Count")
  
ggdensity<-ggplot(mydata, aes(x = RINGS, y = DENSITY)) + geom_point(size=4,  aes(color = SEX))+
  scale_color_manual(values=c("thistle", "darkseagreen", "steelblue"),
                     breaks=c("I", "M", "F"), labels=c("Infant", "Male", "Female"))+theme_bw()+
  ggtitle("Abalone Density (g/mm3)\n and Shell Ring Count")
grid.arrange(ggwhole,ggvolume,ggdensity,ncol=3)
dev.off()

# compute count for each CLASS level

x<-table(mydata$SEX,mydata$CLASS)
out<-as.data.frame(x)
colnames(out)<-c("sex","class","count")

# compute proportion for each CLASS level

# original table
# x<-table(mydata$SEX,mydata$CLASS)
# out<-as.data.frame(x)
# colnames(out)<-c("sex","class","count")

# create y data frame
y <- data.frame(table(mydata$CLASS))

# matches the CLASS count in our "y" data frame to the CLASS in out$class and defines out$length as
# equal to "Freq" value of the relevant CLASS
out$length <- y[match(out$class, y$Var1), "Freq"]

# defines the proportion as the count per SEX-CLASS pair over the total count per relevant CLASS
out$proportion <- out$count / out$length

# show table
out

# plot count and proportion

pdf(file="ClassCountProp.pdf",onefile = TRUE, width = 11, height = 8.5)
count<-ggplot(data=out,aes(x=class,y=count,group=sex,colour=sex))+geom_line()+
geom_point(size=4)+ggtitle("Sample Counts of Different Sexes \n per Age Classification")+
  scale_color_manual(name="SEX", values=c("thistle", "darkseagreen", "steelblue"),
                     breaks=c("I", "M", "F"), labels=c("Infant", "Male", "Female"))+theme_bw()+
  xlab("AGE CLASS")+ylab("COUNT")

proportion<-ggplot(data=out,aes(x=class,y=proportion,group=sex,colour=sex))+geom_line()+
  geom_point(size=4)+ggtitle("Sample Proportions of Different Sexes \n per Age Classification")+
  scale_color_manual(name="SEX", values=c("thistle","darkseagreen","steelblue"),
                     breaks=c("I", "M", "F"), labels=c("Infant", "Male", "Female"))+theme_bw()+
  xlab("AGE CLASS")+ylab("PROPORTION")
grid.arrange(count,proportion,ncol=2)
dev.off()


# create side-by-side boxplots

boxvol<-ggplot(mydata, aes(mydata$CLASS, mydata$VOLUME)) + 
  geom_boxplot(aes(fill = mydata$CLASS)) + theme_bw()+
  theme(legend.position = "none")+xlab("AGE CLASS")+ylab("VOLUME")+
  ggtitle("Abalone Volume (mm3) \n per Age Classification")+
  scale_fill_manual(values=c("lemonchiffon","lavenderblush3","honeydew","burlywood","lightblue","ivory"))

boxwhole<-ggplot(mydata, aes(mydata$CLASS, mydata$WHOLE)) + 
  geom_boxplot(aes(fill = mydata$CLASS)) + theme_bw()+
  theme(legend.position = "none")+xlab("AGE CLASS")+ylab("WHOLE WEIGHT")+
  ggtitle("Abalone Whole Weight (g) \n per Age Classification")+
  scale_fill_manual(values = c("lemonchiffon","lavenderblush3","honeydew","burlywood","lightblue","ivory"))

# place boxplots side-by-side
pdf("BoxVolBoxWhole.pdf")
grid.arrange(boxvol,boxwhole,ncol=2)
dev.off()

# aggregate for WHOLE
pdf(file="Aggregate.pdf",onefile = TRUE, width = 11, height = 8.5)
out<-aggregate(WHOLE~SEX+CLASS,data=mydata,mean)
agwhole<-ggplot(data=out,aes(x=CLASS,y=WHOLE,group=SEX,colour=SEX))+
  geom_line()+geom_point(size=4)+theme_bw()+
  ggtitle("Abalone Mean Whole Weight (g) \n versus Age Classification per Sex")+
scale_color_manual(name="SEX", values=c("thistle", "darkseagreen", "steelblue"),
                   breaks=c("I", "M", "F"), labels=c("Infant", "Male", "Female"))+
  xlab("AGE CLASS")+ylab("WHOLE WEIGHT")

# aggregate for DENSITY
out<-aggregate(DENSITY~SEX+CLASS,data=mydata,mean)
agdensity<-ggplot(data=out,aes(x=CLASS,y=DENSITY,group=SEX,colour=SEX))+
  geom_line()+geom_point(size=4)+theme_bw()+
  ggtitle("Abalone Mean Density (g/mm3) \n versus Age Classification per Sex")+
  scale_color_manual(name="SEX", values=c("thistle", "darkseagreen", "steelblue"),
                     breaks=c("I", "M", "F"), labels=c("Infant", "Male", "Female"))+
  xlab("AGE CLASS")+ylab("DENSITY")
grid.arrange(agwhole,agdensity,ncol=2)
dev.off()

# End
