
# Journal of Fish Biology
# River lamprey (Lampetra fluviatilis) spawn preferentially on the best substrate for egg retention
# Script by L. Daupagne & M. Dhamelincourt
# Data available on INRAE dataverse, GitHub

library(readxl)
library(writexl)
library(lubridate)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape)
library(ggthemes)
library(ggpubr)
library(corrplot)
library(devtools)
library(MASS)
library(RColorBrewer)
library(gridExtra)
library(tidyverse)
library(gridExtra)
library(scales)
library(AER)


setwd("C:/Users/ldaupagne/Desktop/Th?se/Egg drift")

#################################
#### Formatting matings data ####
#################################

data_matings<- read_excel("Data_matings.xlsx") 
data_matings$hour1<-format(as.POSIXct(data_matings$date_start,format="%Y-%m-%d %H:%M:%S"), format = "%H")
data_matings$hour1<-as.numeric(as.character(data_matings$hour1))

test<-data_matings
test$count<-1
test<-test[,c(6,23,24)]
test<- ddply(test, .(Lieu,hour1), summarize, sum = sum(count))
test$D_N<-0

for (i in 1:nrow(test)) {
  if (test[i,2] < 8) {
    test[i,4]<-"N"
  }
  if (test[i,2] > 19) {
    test[i,4]<-"N"
  }
  if (test[i,4] == 0) {
    test[i,4]<-"D"
  }   
}

blup<-test
blup$hour1<-NULL
blup<- ddply(blup, .(Lieu,D_N), summarize, sum = sum(sum))

glop<-as.table(rbind(c(392, 3147), c(123,3153)))
dimnames(glop) <- list(time=c("D","N"), lieu=c("fin","grossier"))
(test <- chisq.test(glop)) # affichage des r?sultats du test

test$statistic #: la statistique du Chi2.
test$parameter #: le nombre de degr?s de libert?s.
test$p.value #: la p-value.
test$observed #: la matrice observ?e de d?part.
test$expected #: la matrice attendue sous l'hypoth?se nulle d'absence de biais.

ggplot(test, aes(x=Lieu, y=sum, colour=D_N, fill=D_N))+
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("grey", "black")))+ scale_fill_manual(values=c("grey", "black"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+ 
  ylab("Number of matings")+ 
  theme_classic() 

# First, we first applied a GLM assuming a poisson distribution (count data)
mod1 <- glm(sum ~ D_N * Lieu, family = "poisson",data=test)
summary(mod1)
dispersiontest(mod1) # overdispersion, so we assumed a "quasipoisson" distribution

mod2 <- glm(sum ~ D_N * Lieu, family = "quasipoisson",data=test)
summary(mod2) # overdispersion, so we assumed a "negative binomial" distribution

Final_model <- glm.nb(sum ~ D_N * Lieu, data=test)
summary(stepAIC(Final_model)) # we reduced our initial through a model selection procedure based on the minimization of Akaike Information Criterion 
summary(Final_model) 

#Plot number of matings according to substrat each day
ggplot(test, aes(x=hour1,y=sum,fill=Lieu,colour=Lieu,label=sum))+ 
  scale_x_continuous(breaks = seq(0,23, by = 1))+
  geom_col(position="stack",size=0.3)+
  scale_colour_manual(values = c("black","black"))+
  scale_fill_manual(values = c("white","grey30"))+
  ylab(label="Number of matings")+
  xlab(label="")+
  ggtitle("(a)")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.y=element_text(size=10,face = "bold"),axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(face = "bold"))



# As the nets were daily lifted  at 4 p.m., if a mating occured after that time it was considered to have taken place the following day
for (i in 1:nrow(data_matings)) {
  if (data_matings[i,23] < 16) {
  }else {
    data_matings[i,18]<-(data_matings[i,18])+1
  }
}

data_matings$hour<-rescale(data_matings$hour, from = c(9, 30), to = c(1, 22)) # We rescale the season from day 1 to day 22

#################################
##### Formatting eggs data ######
#################################

oeufs<- read_excel("Data_eggs.xlsx",sheet = "Feuil1") 
oeufs$Jour<-format(as.Date(oeufs$Jour,format="%Y-%m-%d"), format = "%d")
oeufs$Jour<-as.numeric(oeufs$Jour)
oeufs<-oeufs[c(1:44),] # we do not select the eggs lifted after the last mating (2 days following the end) 
oeufs$Jour<-rescale(oeufs$Jour, from = c(9, 30), to = c(1, 22))


#Plot number of matings according to substrat each day
Fig1a<-ggplot(data_matings, aes(x=hour,fill=Lieu,colour=Lieu))+ 
  scale_x_continuous(breaks = seq(1,22, by = 1))+
  ylim(0,1700)+
  geom_bar(position="stack",size=0.3)+
  scale_colour_manual(values = c("black","black"))+
  scale_fill_manual(values = c("white","grey30"))+
  ylab(label="Number of matings")+
  xlab(label="")+
  ggtitle("(a)")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.y=element_text(size=10,face = "bold"),axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(face = "bold"))


#Plot number of eggs according to substrat each day
Fig1b<-ggplot(oeufs, aes(x=Jour,y=Oeufs_idk,fill=Substrat,colour=Substrat,label=Oeufs_idk))+ 
  scale_x_continuous(breaks = seq(1,22, by = 1))+
  ylim(0,1700)+
  geom_col(position="stack",size=0.3)+
  ylab(label="Number of drifting eggs")+
  scale_colour_manual(values = c("black","black"))+
  scale_fill_manual(values = c("white","grey30"))+
  xlab(label="Day of the experiment")+
  ggtitle("(b)")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.y=element_text(size=10,face = "bold"),axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(face = "bold"))

svg("Figure1.svg")
ggarrange(Fig1a,Fig1b,nrow=2,ncol=1)
dev.off()


########################################
##### Formatting statistical data ######
########################################

names(oeufs)[names(oeufs) == "Jour"] <- "hour"
names(oeufs)[names(oeufs) == "Substrat"] <- "Lieu"
test<-data.frame(with(data_matings, table(Lieu, hour)))
test<-merge(test,oeufs[,c(1,4:5)],by=c("Lieu","hour"))
test_fin<-test[which(test$Lieu=="fin"),]
test_fin<-arrange(test_fin,hour)
test_grossier<-test[which(test$Lieu=="grossier"),]
test_grossier<-arrange(test_grossier,hour)

# We create num column with cumulated number of matings for each substrate
test_fin$Freq_acc <- cumsum(test_fin$Freq)
test_grossier$Freq_acc <- cumsum(test_grossier$Freq)

# We create num column with cumulated number of matings on the last 12 days for each substrate
test_fin$Freq_acc1 <- rollsumr(test_fin$Freq, k = 12, fill = NA)
test_grossier$Freq_acc1 <- rollsumr(test_grossier$Freq, k = 12, fill = NA)

# For the first 12 days, I select the cumulated number of matings
# Otherwise, I select the cumulated number of matings on the last 12 days
for (i in 1:nrow(test_fin)){
  if (is.na(test_fin[i,6])){
    test_fin[i,6]<-test_fin[i,5]
  }else{}
}

for (i in 1:nrow(test_grossier)){
  if (is.na(test_grossier[i,6])){
    test_grossier[i,6]<-test_grossier[i,5]
  }else{}
}

#################################
###### Statistical analysis #####
#################################

nrow(data_matings) # To know the number of mating acts
nrow(data_matings[which(data_matings$Lieu=="fin"),])  # on fine substrate
nrow(data_matings[which(data_matings$Lieu=="grossier"),])  # on coarse substrate
mean(test[which(test$Lieu=="fin"),c(3)]) # mean number of matings per day on fine substrate
mean(test[which(test$Lieu=="grossier"),c(3)])  # mean number of matings per day on coarse substrate
sd(test[which(test$Lieu=="fin"),c(3)])  # sd number of matings per day on fine substrate
sd(test[which(test$Lieu=="grossier"),c(3)])  # sd number of matings per day on coarse substrate

# To test if the overall number of matings is different according to the substrate type
Model_anova<-aov(Freq~Lieu, data=test)
summary(Model_anova) #***
shapiro.test(residuals(Model_anova)) # Shapiro not satisfying so switch to non-parametric kruskal-wallis test
kruskal.test(Freq~Lieu, data=test) 

# plot this result
test<-bind_rows(test_grossier,test_fin)
ggplot(test, aes(x=Lieu, y=Freq,fill=Lieu,color=Lieu)) +
  geom_point(shape=21,size=2,stroke=1)+
  geom_boxplot(outlier.shape=NA,alpha=0.5, position=position_dodge(width=0.8),fatten=NULL)+
  scale_colour_manual(values=c(c("black","black")))+
  scale_fill_manual(values=c("white","#8c8c8c"))+
  stat_summary(fun=median,geom="errorbar",aes(ymax=..y..,ymin=..y..),width=0.75, size=1.5, linetype="solid", position=position_dodge(width=0.8))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(size=10))+
  xlab("Lieu")+
  ylab("Number of mating acts")

# To know how many individuals mated on each substrate
# and min/max number of matings on each type and for each sex
sum_sex<-data_matings[,c(6,18,19,20)]
sum_sex$count<-1
sum_mal<- ddply(sum_sex, .(Lieu,Subject2), summarize, sum = sum(count))
nrow(sum_mal[which(sum_mal$Lieu=="fin"),]);nrow(sum_mal[which(sum_mal$Lieu=="grossier"),])
min(sum_mal[which(sum_mal$Lieu=="fin"),c(3)]);max(sum_mal[which(sum_mal$Lieu=="fin"),c(3)])
min(sum_mal[which(sum_mal$Lieu=="grossier"),c(3)]);max(sum_mal[which(sum_mal$Lieu=="grossier"),c(3)])
sum_fem<- ddply(sum_sex, .(Lieu,Subject), summarize, sum = sum(count))
nrow(sum_fem[which(sum_fem$Lieu=="fin"),]);nrow(sum_fem[which(sum_fem$Lieu=="grossier"),])
min(sum_fem[which(sum_fem$Lieu=="fin"),c(3)]);max(sum_fem[which(sum_fem$Lieu=="fin"),c(3)])
min(sum_fem[which(sum_fem$Lieu=="grossier"),c(3)]);max(sum_fem[which(sum_fem$Lieu=="grossier"),c(3)])


# Show on which substrate individuals mated first (initial preference for either substrate)
f <- aggregate(date_start~Subject, FUN=min,data=data_matings)
f<-merge(f,data_matings[,c(6,16,19)],by=c("date_start","Subject"))
f<-f[!duplicated(f), ]
binom.test(6, 20, 0.5) # in females, no preferences
m <- aggregate(date_start~Subject2, FUN=min,data=data_matings)
m<-merge(m,data_matings[,c(6,16,20)],by=c("date_start","Subject2"))
m<-m[!duplicated(m), ]
binom.test(10, 15, 0.5) # in males, no preferences
binom.test(16,35,0.5) # in both, no preferences

# Results eggs
sum(oeufs[,c(5)])
sum(oeufs[which(oeufs$Lieu=="fin"),c(5)]);sum(oeufs[which(oeufs$Lieu=="grossier"),c(5)])   
oeufs<-data.frame(oeufs)
mean(oeufs[which(oeufs$Lieu=="fin"),c(5)]);mean(oeufs[which(oeufs$Lieu=="grossier"),c(5)])
sd(oeufs[which(oeufs$Lieu=="fin"),c(5)]);sd(oeufs[which(oeufs$Lieu=="grossier"),c(5)])

# To test if the number of eggs caught downstream on a given day depends on:
# 1) substrate type (Lieu)
# 2) the number of spawning acts performed on it on that day (Freq)
# 3) the cumulated number of spawning acts performed on it over the last twelve days (Freq_acc1)

# First, we first applied a GLM assuming a poisson distribution (count data)
mod1 <- glm(Oeufs_idk ~ Lieu * Freq_acc1 * Freq, family = "poisson",data=test)
summary(mod1)
dispersiontest(mod1) # overdispersion, so we assumed a "quasipoisson" distribution

mod2 <- glm(Oeufs_idk ~ Lieu * Freq_acc1 * Freq, family = "quasipoisson",data=test)
summary(mod2) # overdispersion, so we assumed a "negative binomial" distribution

Final_model <- glm.nb(Oeufs_idk ~ Lieu * Freq_acc1 * Freq, data=test)
summary(stepAIC(Final_model)) # we reduced our initial through a model selection procedure based on the minimization of Akaike Information Criterion 



