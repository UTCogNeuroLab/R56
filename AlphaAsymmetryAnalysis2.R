###Alpha Analyses### 
#2018 Alex Alario#
#please let me know if you have any questions! alexalario96@gmail.com

##Creating depression severity bins based on BDI severity cutoffs##
library(ggplot2)
library(ggthemes)

#Before this analysis I had created a dataset with all of the behavioral scores/eye tracking scores/etc
#because I was looking at day 1 and day 2, each of my columns end in either a 1 or a 2
data = read.csv("~/Documents/R56/new final mean activity day1.csv", header = TRUE, sep = ",",na = c("-","99999","NA"))

#creating subsets based on BDI scores
minimal = subset(data, data$pre_bdi_total<=13& data$mdd_past_meets == 1)
mild = subset(data, data$pre_bdi_total>=14 & data$pre_bdi_total<=19 & data$mdd_past_meets == 1)
moderate = subset(data, data$pre_bdi_total>=20 & data$pre_bdi_total<=28 & data$mdd_past_meets == 1)
severe = subset(data, data$pre_bdi_total>=29 & data$pre_bdi_total<=63 & data$mdd_past_meets == 1)

#using previous subset to make another subset with severity score and whether or not there is a history of depression
notdepressedandhistory = subset(data, data$pre_bdi_total<20 & data$mdd_past_meets ==1)
depressedandhistory = subset(data, data$pre_bdi_total>=20 & data$mdd_past_meets ==1)
notdepressednot=subset(data, data$pre_bdi_total<20 & data$mdd_past_meets ==0)
depressednot=subset(data, data$pre_bdi_total>=20 & data$mdd_past_meets ==0)
onlyhistory = subset(data, data$mdd_past_meets == 1)
nothistory = subset(data, data$mdd_past_meets ==0)

#can also subset based on current depression and severity score
notdepressedandhistory = subset(data, data$pre_bdi_total<20 & data$mdd_past_meets ==1)
depressedandhistory = subset(data, data$pre_bdi_total>=20 & data$mdd_past_meets ==1)
notdepressednot=subset(data, data$pre_bdi_total<20 & data$mdd_past_meets ==0)
depressednot=subset(data, data$pre_bdi_total>=20 & data$mdd_past_meets ==0)
onlyhistory = subset(data, data$mdd_past_meets == 1)
nothistory = subset(data, data$mdd_past_meets ==0)

#makes notched boxplot/violin plot from above subsets
#replace the name after the $ with names to plot
boxplot(minimal$f87CSD1, mild$f87CSD1, moderate$f87CSD1, severe$f87CSD1, range = 1, notch=TRUE, boxwex = .5, col=(c("lightgreen", "pink", "lightblue", "purple")), 
        main="BDI and F87 CSD",names = c("Minimal", "Mild", "Moderate", "Severe"))

#running T tests between the severity groups
t.test(minimal$midCSD1,mild$midCSD1)
t.test(minimal$midCSD1,moderate$midCSD1)
t.test(minimal$midCSD1,severe$midCSD1)
t.test(mild$midCSD1,moderate$midCSD1)
t.test(mild$midCSD1,severe$midCSD1)
t.test(moderate$midCSD1,severe$midCSD1)

t.test(minimal$midAVG1,mild$midAVG1)
t.test(minimal$midAVG1,moderate$midAVG1)
t.test(minimal$midAVG1,severe$midAVG1)
t.test(mild$midAVG1,moderate$midAVG1)
t.test(mild$midAVG1,severe$midAVG1)
t.test(moderate$midAVG1,severe$midAVG1)

#anova
fit <- aov(mdd_past_meets ~ pre_bdi_total, data=data)
summary(fit)



##Correlations between Alpha scores and negative attention bias in people with a history of depression##

#creating subsets of history of depression (can also do current depression) so that you only have values from people
#   in the subset
withpast1 <- subset(data, data$mdd_past_meets == "1")
withoutpast1<- subset(data, data$mdd_past_meets == "0")
#writes into meaningless file
write.csv(withpast1, "faawithhistoryofdep.csv")
write.csv(withoutpast1, "faawithouthistoryofdep.csv")
#reads meaningless file
a1=read.csv("~/faawithhistoryofdep.csv")
b1=read.csv("~/faawithouthistoryofdep.csv")

#plots alpha and attention bias measure in people with a history of depressin (based on previous subsets) and gives correlation value
#can replace with whatever measures
plot(data$pre_bdi_total,data$midAVG1,las=1,ylab="Eyes Open Midfrontal Day 1",
     xlab="Attention Bias to Sad Faces (variation gaze bias)",col="black",pch=18)
abline(lm(data$midAVG1~data$pre_bdi_total))
cor.test(data$pre_bdi_total,data$midAVG1)
