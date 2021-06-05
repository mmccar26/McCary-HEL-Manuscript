##McCary et al. Oikos
##Analysis on PLFA for Experiment 3

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
library(gridExtra)

#=====import data======
#relative pathname
plfa <- file.path(".", "Experiment 3", "Data", "PLFA Biomass Data.csv")
print(plfa)

#import tibble
fa<-read_csv(plfa)%>%
  filter(Treatment %in% c("Press", "Control"))

##T-test for major functional groups
t.test(Fungi~ Treatment, data = fa)
t.test(Bacteria~ Treatment, data = fa)
t.test(Gm_negative~ Treatment, data = fa)
t.test(Gm_positive~ Treatment, data = fa)
t.test(Total~ Treatment, data = fa)

##To rename for the press treatment to midge addition
fa.c<-
  fa%>%
  mutate(Treatment = revalue(Treatment, c("Press" = "Midges added"))
  )

#Plot data
fun<-ggplot(fa.c, aes(x=Treatment, y=Fungi, fill = Treatment)) + 
  geom_boxplot(lwd = 1, width=0.5)+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(limits=c("Control", "Midges added"))+
  scale_fill_manual(values=c("white", "grey"))+
  theme_bw() +
  ggtitle("(e) Total fungi")+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"),
        axis.text.y = element_text(size=12, face="bold", colour = "black"),
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

#Plot data
bac<-ggplot(fa.c, aes(x=Treatment, y=Bacteria, fill = Treatment)) + 
  geom_boxplot(lwd = 1, width=0.5)+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(limits=c("Control", "Midges added"))+
  scale_fill_manual(values=c("white", "grey"))+
  theme_bw() +
  ggtitle("(b) Total bacteria")+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"),
        axis.text.y = element_text(size=12, face="bold", colour = "black"),
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

bac.n<-ggplot(fa.c, aes(x=Treatment, y=Gm_negative, fill = Treatment)) + 
  geom_boxplot(lwd = 1, width=0.5)+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(limits=c("Control", "Midges added"))+
  scale_fill_manual(values=c("white", "grey"))+
  theme_bw() +
  ggtitle("(c) Total gram- bacteria")+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"),
        axis.text.y = element_text(size=12, face="bold", colour = "black"),
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

bac.p<-ggplot(fa.c, aes(x=Treatment, y=Gm_positive, fill = Treatment)) + 
  geom_boxplot(lwd = 1, width=0.5)+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(limits=c("Control", "Midges added"))+
  scale_fill_manual(values=c("white", "grey"))+
  theme_bw() +
  ggtitle("(d) Total gram+ bacteria")+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"),
        axis.text.y = element_text(size=12, face="bold", colour = "black"),
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

tot<-ggplot(fa.c, aes(x=Treatment, y=Total, fill = Treatment)) + 
  geom_boxplot(lwd = 1, width=0.5)+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(limits=c("Control", "Midges added"))+
  scale_fill_manual(values=c("white", "grey"))+
  theme_bw() +
  ggtitle("(a) Total microbial biomass")+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"),
        axis.text.y = element_text(size=12, face="bold", colour = "black"),
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

#plot all figures together
grid.arrange(tot, bac, bac.n, bac.p, fun, ncol = 2)

