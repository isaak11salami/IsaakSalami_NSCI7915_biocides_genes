#We've done an ANOVA and got pretty muc what we expected. Now, let's
#do a GLMM and see if that's better (will be great if it works, because
#our data doesn't meet a lot of the ANOVA assumptions)
getwd()
setwd("C:/Users/isaak/OneDrive/Documents/Uni/2025/Session 1/Scientific Analysis_NSCI7915/Assessments/DataProject/data/raw")

#Start by loading up the required libraries
library(lme4)
install.packages('lmerTest')
library(lmerTest)
install.packages('MuMIn')
library(MuMIn)

#Import out data
library(readr)
ports <- read_csv("transporters_lm.csv")
View(ports)
ports

#Now let's set up the GLMM
#The lmer requires a random effect term. However, in our instance we
#only have fixed effects. We want to know the logFC for a specific 
#set of genes for a specific biocide. Therefore, we must use lm rather
#than lmer
biocide.ports <- lm(logFC~biocide, data = ports)

summary(biocide.ports)
plot(biocide.ports)



anova(biocide.ports)

