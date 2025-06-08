#Importing data
library(readr)
library(car)

#let's set the working directory

setwd("/Users/isaak/OneDrive/Documents/Uni/2025/Session 1/Scientific Analysis_NSCI7915/Assessments/DataProject/data/raw")

pumpBAL_data<- read_csv("pumps_BAL062.csv")
View(pumpBAL_data)

#Let's start by running an ANOVA to look at differences between "groups"

gmod <- aov(logFC~group, data=pumpBAL_data)
gmod

#Check Assumptions
#1) Homogeneity of Variance, via...
#Levene Test
leveneTest(gmod)
#Plot Residuals against Fitted values
plot(gmod)

#2) Normality of Variance, via...
#Q-Q plot
plot(gmod)

anova(gmod)

gmod_tukey <- TukeyHSD(gmod)
gmod_tukey
gm_tukey <- as.data.frame(gmod_tukey$group)
gm_tukey$comparison <- rownames(gm_tukey)
gm_tukey <- gm_tukey[c("comparison", "lwr", "upr", "p adj")]
gmod_sigcompare <- gm_tukey[gm_tukey$`p adj` < 0.05, ]
gmod_sigcompare

