'''
Jake Holley
10/31/19
Analysis of Lettuce Growth Data from Greenhouse 141a
'''
library(lme4)
library(lmerTest)
library(emmeans)
library(plyr)
library(ggplot2)
library(plotrix)
library(scales)

#Read in CSV file
plantdata<-read.csv('C:/Users/jacob/Documents/PhDData/141a/Sept2019Lettuce/141a2019to2020HarvestData.csv')
light<-read.csv('C:/Users/jacob/Documents/PhDData/141a/Sept2019Lettuce/LightMaps/fullpar.csv')
DLI<-read.csv('C:/Users/jacob/Documents/PhDData/141a/Sept2019Lettuce/EnviData/FinalOrganizedPAR/FullDLI.csv')
suphrs<-read.csv('C:/Users/jacob/Documents/PhDData/141a/Sept2019Lettuce/hrslight.csv')
NightAvg<-read.csv('C:/Users/jacob/Documents/PhDData/141a/Sept2019Lettuce/NightAvg.csv')
hplc<-read.csv('C:/Users/jacob/Documents/PhDData/141a/Sept2019Lettuce')
antho<-read.csv('C:/Users/jacob/Documents/PhDData/141a/Sept2019Lettuce')
n1<-length(plantdata$Replicate)
n2<-length(light$Replicate)
n3<-length(DLI$Harvest)
n4<-length(suphrs$Date)
n5<-length(NightAvg$Treatment)

#Add Data from Supplemental Light Maps
for (x in 1:n1){
  for (a in 1:n2){
    if (plantdata$Replicate[x] == light$Replicate[a] & plantdata$Treatment[x] == light$Treatment[a] & plantdata$Raft[x] == light$Raft[a]& plantdata$Number[x] == light$Number[a]){
      plantdata$PAR[x] <- light$PAR[a] }

  }
}

#Add DLI Data
for (x in 1:n1){
  for (a in 1:n3){
    if (plantdata$Date[x] == DLI$Harvest[a] & plantdata$Treatment[x] == DLI$Treatment[a]){
      plantdata$DLI[x] <- DLI$DLI[a] }

  }
}

#add hours of light
for (x in 1:n1){
  for (a in 1:n4){
    if (plantdata$Date[x] == suphrs$Date[a]){
      plantdata$Hrsfix[x] <- suphrs$Hours[a] }

  }
}

#Night Average
for (x in 1:n1){
  for (a in 1:n4){
    if (plantdata$Treatment[x] == NightAvg$Treatment[a] & plantdata$Replicate[x] == NightAvg$Replicate[a]){
      plantdata$nightavg[x] <- NightAvg$NightAverage[a] }

  }
}

plantdata$DLIsun<-plantdata$DLI-(plantdata$nightavg*plantdata$Hrsfix*3600/1000000)
plantdata$DLIfix<-plantdata$PAR*3600*plantdata$Hrsfix/1000000
plantdata$trueDLI<-plantdata$DLIfix+plantdata$DLIsun

md.1 <- lmer(FreshWeight ~ Treatment + Raft + Number + Variety + trueDLI + Spectrum + Replicate + (1|Treatment)
             + (1|Treatment:Raft) + (1|Treatment:Raft:Number), data=plantdata)
summary(md.1)
anova(md.1)

emmeans(md.1, pairwise ~ Spectrum|Variety)
