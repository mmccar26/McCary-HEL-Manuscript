### McCary et al. 2019 manuscript 
##Analysis of HEL dcomposition according to midge addition 
##Negative Exponential decay

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
mod1<-lmer(Slope_days~Treatment + (1|Block), data = total)
anova(mod1, ddf = "Kenward-Roger")

##evaluate model fit
##residual vs fitted plot
plot(fitted(mod1), resid(mod1), xlab = "Fitted Residuals", ylab = "Residuals") #residuals vs fitted
abline(h=0)

##qqplot
qqPlot(resid(mod1), xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", line = "robust", col.lines = "black", grid = FALSE)

##============Plot data for manuscript==========
#re-organize and summarize data (Rmisc package to plot data)

#to see summary statistics
summarySE(total, measurevar="Slope_days", groupvars=c("Treatment"))

##Remove scientific notation
point <- format_format(big.mark = " ", decimal.mark = ".", scientific = FALSE)

#Set up to jitter error bars
pd <- position_dodge(width = 0.05)

##Plot data
ggplot(total, aes(x=Treatment, y=-Slope_days, fill = Treatment))+ 
 geom_boxplot(lwd = 1, width=0.5)+
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim=c(0,0.0013))+
  scale_x_discrete(limits=c("Control", "Midges added"))+
  scale_y_continuous(expand = c(0,0), labels = point)+
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