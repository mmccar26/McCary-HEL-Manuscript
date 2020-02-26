### McCary et al. 2020 manuscript 
##Experiment 2 to test midge effects on plant biomass

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

##Height
t.test(log(Height)~Treatment, data = plt)

##Mass
t.test(log(Mass)~Treatment, data = plt)

##=======Figures to plot according to response variables======

#Mass
#To see summary statistics
summarySE(plt, measurevar = "Mass", groupvars=c("Treatment"))

##barplot
##Plot data
ggplot(plt, aes(x=Treatment, y=Mass, fill = Treatment))+ 
  geom_boxplot(lwd = 1, width=0.5)+
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim=c(0,15))+
  scale_x_discrete(limits=c("Control", "Midges added"))+
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
#Plot to see summary statistics
summarySE(plt, measurevar="Height", groupvars=c("Treatment"))

#Barplot
ggplot(plt, aes(x=Treatment, y=Height, fill = Treatment))+ 
  geom_boxplot(lwd = 1, width=0.5)+
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim=c(0,15))+
  scale_x_discrete(limits=c("Control", "Midges added"))+
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
