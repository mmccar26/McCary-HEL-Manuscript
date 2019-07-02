### McCary et al. 2019 manuscript 
##Experiment 2 to test midge and arthropods effects on plant biomass
##1 July 2019

#library
## Load relevant R packages
library(ggplot2)
library(plyr)
library(car)
library(lmerTest)
library(dplyr)
library(Rmisc)
library(tidyverse)

#=====import data======
#relative pathname
plants <- file.path(".", "Experiment 2", "Data", "Plant Biomass Data.csv")
print(plants)

#import tibble
plt<-read_csv(plants)

###=========test response variables against treatments=========

##Mass
mod1<-aov(log(Mass)~Midges*Arthropods, data = plt)
anova(mod1)

#Posthoc mass
TukeyHSD(mod1)

##Height
mod2<-aov(log(Height)~Midges*Arthropods, data = plt)
anova(mod2)

#Posthoc height
TukeyHSD(mod2)


##=======Figures to plot according to response variables======
##To rename to change order of appearance in ggplot2
plt<-
  plt%>%
  mutate(Arthropods = revalue(Arthropods, c("Arthropods" = "A2", "No Arthropods" = "A1"))
  )

#Mass
#Plot to see data, average means and SE using base r
mass<-summarySE(plt, measurevar = "Mass", groupvars=c("Midges", "Arthropods"))
mass

##barplot
ggplot(mass, aes(x=Midges, y=Mass, fill = factor(Arthropods, levels=c("A1", "A2")))) + 
  geom_bar(position=position_dodge(),stat = "identity", colour="black",size=.3)+ 
  geom_errorbar(aes(ymin=Mass-se, ymax=Mass+se), size=1, width=0,position=position_dodge(.9)) +
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim=c(0,15))+
  scale_x_discrete(limits=c("Control", "Carcasses", "Slurry"))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values=c("white", "grey"))+
  theme(axis.text = element_text(size=12, face="bold", colour = "black"),
        axis.line = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size =1),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(colour = "black", size = 1),
        axis.ticks.length = unit(.2, "cm"),
        panel.background = element_rect(fill= "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Height
#Plot to see data, means and SE
height<-summarySE(plt, measurevar="Height", groupvars=c("Midges", "Arthropods"))
height

#Barplot
ggplot(height, aes(x=Midges, y=Height, fill = Arthropods)) + 
  geom_bar(position=position_dodge(),stat = "identity", colour="black",size=.3)+ 
  geom_errorbar(aes(ymin=Height-se, ymax=Height+se), size=1, width=0,position=position_dodge(.9)) +
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim=c(0,15))+
  scale_x_discrete(limits=c("Control", "Carcasses", "Slurry"))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values=c("white", "grey"))+
  theme(axis.text = element_text(size=12, face="bold", colour = "black"),
        axis.line = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size =1),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(colour = "black", size = 1),
        axis.ticks.length = unit(.2, "cm"),
        panel.background = element_rect(fill= "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

