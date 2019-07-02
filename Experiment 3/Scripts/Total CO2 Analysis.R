##McCary et al. 2019
##Analysis of total C respired for Experiment 3
#February 28, 2019

#load appropriate libraries
library(ggplot2)
library(Rmisc)

#=====import data======
#relative pathname
tot.CO2 <- file.path(".", "Experiment 3", "Data", "Total CO2 Data.csv")
print(tot.CO2)

#import tibble
CO2<-read_csv(tot.CO2)

##T-test to test for total CO2 respired
t.test(CO2_flux ~ treatment, data = CO2)

##Summarize data for ggplot
CO<-summarySE(CO2, measurevar="CO2_flux", groupvars=c("treatment"))

##To rename for the press treatment to midge addition
tot.c<-
  CO%>%
  mutate(treatment = revalue(treatment, c("Press" = "Midges added"))
  )

#Plot data
ggplot(tot.c, aes(x=treatment, y=CO2_flux, fill = treatment)) + 
  geom_bar(position=position_dodge(),stat = "identity", colour="black",size=.3)+ 
  geom_errorbar(aes(ymin=CO2_flux-se, ymax=CO2_flux+se), size=1, width=0,position=position_dodge(.9)) +
  xlab(NULL) +
  ylab(NULL) +
  #coord_cartesian(ylim=c(0, 5000))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values=c("white", "gray"))+
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


