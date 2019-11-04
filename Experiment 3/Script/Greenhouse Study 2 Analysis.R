### McCary et al. 2019 manuscript 
##Experiment 3 to test midge effects on plant biomass

#library
## Load relevant R packages
library(ggplot2)
library(plyr)
library(car)
library(lmerTest)
library(dplyr)
library(Rmisc)
library(tidyverse)
library(emmeans)
library(Rmisc)

#=====import data======
#relative pathname
plants <- file.path(".", "Experiment 3", "Data", "Greenhouse Study 2 Data.csv")
print(plants)

#import tibble
plt<-read_csv(plants)

###=========test response variables against treatments=========

##biomass
mod1<-lmer(Biomass.culm ~ Treatment + (1|Block), data = plt)
anova(mod1, ddf = "Kenward-Roger")
emmeans(mod1, list(pairwise ~ Treatment), adjust = "tukey")

##nitrate
mod2<-lmer(NO3 ~ Treatment + (1|Block), data = plt)
anova(mod2, ddf = "Kenward-Roger")
emmeans(mod2, list(pairwise ~ Treatment), adjust = "tukey")

##ammonium
mod3<-lmer(NH4 ~ Treatment + (1|Block), data = plt)
anova(mod3, ddf = "Kenward-Roger")
emmeans(mod3, list(pairwise ~ Treatment), adjust = "tukey")

##total N
mod4<-lmer(Total.N ~ Treatment + (1|Block), data = plt)
anova(mod4, ddf = "Kenward-Roger")
emmeans(mod4, list(pairwise ~ Treatment), adjust = "tukey")

##=======Figures to plot according to response variables======

#Mass
#To see summary statistics
summarySE(plt, measurevar = "Biomass.culm", groupvars=c("Treatment"))

##barplot
##biomass
ggplot(plt, aes(x=Treatment, y=Biomass.culm, fill = Treatment))+ 
  geom_boxplot(lwd = 1, width=0.5)+
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim=c(0,18))+
  scale_fill_manual(values=c("white", "lightgrey", "gray45"))+
  theme(axis.text = element_text(size=12, face="bold", colour = "black"),
        axis.line = element_line(colour = "black", size = 1),
        axis.line.x = element_line(colour = "black", size =1),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size =1),
        panel.background = element_rect(fill= "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1))

##KNO3
summarySE(plt, measurevar = "NO3", groupvars=c("Treatment"))

ggplot(plt, aes(x=Treatment, y=NO3, fill = Treatment))+ 
  geom_boxplot(lwd = 1, width=0.5)+
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim=c(0,300))+
  scale_fill_manual(values=c("white", "lightgrey", "gray45"))+
  theme(axis.text = element_text(size=12, face="bold", colour = "black"),
        axis.line = element_line(colour = "black", size = 1),
        axis.line.x = element_line(colour = "black", size =1),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size =1),
        panel.background = element_rect(fill= "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1))

##NH4
summarySE(plt, measurevar = "NH4", groupvars=c("Treatment"))

ggplot(plt, aes(x=Treatment, y=NH4, fill = Treatment))+ 
  geom_boxplot(lwd = 1, width=0.5)+
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim=c(0,40))+
  scale_fill_manual(values=c("white", "lightgrey", "gray45"))+
  theme(axis.text = element_text(size=12, face="bold", colour = "black"),
        axis.line = element_line(colour = "black", size = 1),
        axis.line.x = element_line(colour = "black", size =1),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size =1),
        panel.background = element_rect(fill= "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1))

##Total N
summarySE(plt, measurevar = "Total.N", groupvars=c("Treatment"))

ggplot(plt, aes(x=Treatment, y=Total.N, fill = Treatment))+ 
  geom_boxplot(lwd = 1, width=0.5)+
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim=c(0,300))+
  scale_fill_manual(values=c("white", "lightgrey", "gray45"))+
  theme(axis.text = element_text(size=12, face="bold", colour = "black"),
        axis.line = element_line(colour = "black", size = 1),
        axis.line.x = element_line(colour = "black", size =1),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size =1),
        panel.background = element_rect(fill= "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1))

