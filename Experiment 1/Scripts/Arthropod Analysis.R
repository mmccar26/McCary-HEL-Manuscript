### McCary et al. 2019 manuscript 
##Analysis of HEL arthropod densities according to insecticide treatment
## 1 July 2019

## Load relevant R packages
library(ggplot2)
library(plyr)
library(tidyverse)

#=====import data======
#relative pathname
arthropods <- file.path(".", "Experiment 1", "Data", "Arthropod Data.csv")
print(arthropods)

#import tibble
art.all<-read_csv(arthropods)

##Group by treatment and then average across years
art<-
  art.all %>%
  drop_na()%>%
  group_by(Plot, Midges, Insecticides) %>%
  summarise_each(funs(mean),-Year, -Date)

glimpse(art)

#=====plot data and evaluation of outliers======
##Plot 4 looks like an outlier
ggplot(art, aes(x=Plot, y=Total, Label = Plot))+
  geom_point() +
  geom_text(aes(label=Plot),hjust=0, vjust=0)

##check for outlier (+/- 2.5 SD of mean)
##scale function to convert to z scores
art.z<- scale(art$Total, center = TRUE, scale = TRUE)
view(art.z)
  
##to check for outlier (+/- 2.5 SE)
art$SD <- ifelse(art.z < 2.75, "below", "above") ##general cutoff for outliers

##Plot 4 looks like the outlier
##to remove the outlier plot
art1<-
  art%>%
  filter(Plot != "4")

#=====One-tailed t-test for all and individual taxa======
#insecticide treatments expected to have fewer arthropods

#arthropod total density
t.test(log(Total+1) ~ Insecticides, alternative="less", data = art1) 

##mites
t.test(log(Mites+1) ~ Insecticides, alternative="less", data = art1)

#Spiders
t.test(log(Spiders+1) ~ Insecticides, alternative="less", data = art1)

#Beetles
t.test(log(Beetles+1) ~ Insecticides, alternative="less", data = art1)

#Collembola
t.test(log(Springtails+1) ~ Insecticides, alternative="less", data = art1)

#Stern
t.test(log(Stern+1) ~ Insecticides, alternative="less", data = art1)

#thrips
t.test(log(Thyanosoptera+1) ~ Insecticides, alternative="less", data = art1)

#=====Calculation for Bonferroni Corrections======

#build dataframe with p-values from t-test
taxa<-c("Mites", "Spiders", "Beetles", "Springtails", "Stern", "Thysanoptera")
p<- c(0.079, 0.00000491, 0.0297, 0.3421, 0.000001983, 0.00002802)
b_cor<-data.frame(taxa, p)

#order p-values accordingly
Data <- b_cor[order(b_cor$p),]

#direct calculation of Bonferroni Corrections
Data$Bonferroni <- 
  p.adjust(Data$p, 
           method = "bonferroni")
