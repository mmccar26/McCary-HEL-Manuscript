### McCary et al. 2019 manuscript 
##Analysis of HEL dcomposition according to midge addition and insecticides
##Negative Exponential decay
##1 July 2019

#library
## Load relevant R packages
library(ggplot2)
library(lme4)
library(car)
library(lmerTest)
library(dplyr)
library(Rmisc)
library(scales)
library(tidyverse)
library(plyr)

#=====import data======
#relative pathname
decomposition <- file.path(".", "Experiment 1", "Data", "Decomposition Data.csv")
print(decomposition)

#import tibble
decomp<-read_csv(decomposition)

# Estimate decay parameters using a linear model
model.0 <- lm(log(Premain) ~ Month, data=decomp)  
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

##Store estimates in a list
start <- list(alpha = alpha.0, beta = beta.0)
start

##Run model to calculate and split out slopes
lit<- with(decomp, split(decomp, list(Plot=Plot)))

#Function to calculate coefficients
coefLM <- function(x) {
  coef(nls(Premain ~ alpha * exp(beta*Month), data = x, start = start))[2]
}

#Store coefficients
coefs <- sapply(lit, coefLM)
head(coefs)

##Place into dataframe
out <- unique(decomp[, c("Plot")])
out1 <- transform(out, Slope = sapply(lit, coefLM))

##Combine all
#to organize the slopes according to plot and treatment 
design<-
  decomp%>%
  filter(Month == "0")

#merge the two dataframes
total <- merge(out1, design, by="Plot")

#to put "K" into days
total<-
  total%>%
  mutate(Slope_days = Slope/409) #409 days of the experiment

##=====Linear mixed models to test decomposition responses==============
##LMM
mod1<-lmer(Slope_days~Midges*Arthropods + (1|Block), data = total)
anova(mod1, ddf = "Kenward-Roger")

##evaluate model fit
##residual vs fitted plot
plot(fitted(mod1), resid(mod1), xlab = "Fitted Residuals", ylab = "Residuals") #residuals vs fitted
abline(h=0)

##qqplot
qqPlot(resid(mod1), xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", line = "robust", col.lines = "black", grid = FALSE)

##============Plot data for manuscript==========
#re-organize and summarize data (Rmisc package to plot data)

#Plot to see summary statistics
decomp1<-summarySE(decomp, measurevar="Premain", groupvars=c("Midges", "Arthropods", "Treatment", "Month"))

##Barplot
#Plot to see data
art<-summarySE(total, measurevar="Slope_days", groupvars=c("Midges", "Arthropods", "Treatment"))
art

##Remove scientific notation
point <- format_format(big.mark = " ", decimal.mark = ".", scientific = FALSE)

#Change to appropriate labels
art<-
  art%>%
  mutate(Trt= revalue(Midges, c("Midges" = "Midges Added")))%>%
        mutate(Art = revalue(Arthropods, c("Control" = "Arthropods", "Detritivore Removal" = "Arthropod Reduction")))
  
#Set up to jitter error bars
pd <- position_dodge(width = 0.05)

##Plot data
ggplot(art, aes(x=Trt, y=-Slope_days, group = Art, colour = Art)) + 
  scale_linetype_manual(values = c("dashed", "solid"))+
  geom_errorbar(aes(ymin=-Slope_days-se, ymax=-Slope_days+se), size=1, width=0,     position = pd) +
  geom_line(aes(linetype=Art), size = 1.5, position = pd) +
  geom_point(aes(shape = Art), size = 5, fill = "white", position = pd)+
  scale_x_discrete(limits=c("No Midges", "Midges Added"))+
  scale_y_continuous(labels = point)+
  scale_color_manual(values=c("black", "black"))+
  scale_shape_manual(values=c(16, 22))+
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text = element_text(colour= "black", face = "bold", size = 13),
        axis.line = element_line(colour = "black", size = .3),
        axis.line.x = element_line(colour = "black", size =.3),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size =.3),
        panel.background = element_rect(fill= "white"),
        legend.position = "none",
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5))
