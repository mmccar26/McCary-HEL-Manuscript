##McCary et al. 2020
##Analysis on decomposition for Experiment 4
##Negative Exponential decay LMM

#library
## Load relevant R packages
library(ggplot2)
library(lme4)
library(car)
library(lmerTest)
library(dplyr)
library(Rmisc)
library(broom)
library(tidyverse)

#=====import data======
#relative pathname
respiration <- file.path(".", "Experiment 4", "Data", "CO2 Respiration Data.csv")
print(respiration)

#import tibble
resp<-read_csv(respiration)

#First log transform run number before running LMM and change Day into a factor
resp<-
  resp%>%
    mutate(run = log(run), Day = as.character(Day)
           )

#=======LMM to test for differences in CO2 rates==================
mod1<-lmer(CO2_flux ~ run + treatment + run:treatment + (1|plot), data = resp)
anova(mod1, ddf= "Kenward-Roger")
summary(mod1, ddf = "Kenward-Roger")

##residual vs fitted plot
plot(fitted(mod1), resid(mod1), xlab = "Fitted Residuals", ylab = "Residuals") 
abline(h=0)

##qqplot
qqPlot(resid(mod1), xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", line = "robust", col.lines = "black", grid = FALSE)

#Extract slopes from model
lsmeansLT(mod1, test.effs = "log(run):treatment", method.grad= 'simple', ddf = "Kenward-Roger")

#====Plots of the data================

##Line graphs
##First summarize data according to treatment
tot.resp<-summarySE(resp, measurevar="CO2_flux", groupvars=c("treatment", "Day"))

##ggplot line graph with error bars
ggplot(tot.resp, aes(x=Day, y=CO2_flux, group=treatment, colour=treatment)) + 
  geom_line(aes(linetype=treatment), size = 1) +
  geom_point(aes(shape=treatment), size = 3)+
  geom_errorbar(aes(ymin=CO2_flux-se, ymax=CO2_flux+se), size=1, width=0) +
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim=c(0,2000))+
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual(values=c("black", "black"))+
  scale_shape_manual(values=c(15, 16))+
  scale_x_discrete(limits = c("0", "3", "5", "10", "17", "29"))+
  theme_bw() +
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
