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

#Now let's do an interaction model
balbio <- aov(logFC~group*biocide, data=pumpBAL_data)

#Check Assumptions
#1) Homogeneity of Variance, via...
#Levene Test
leveneTest(balbio)
#Passes the Levene's Test!

#Plot Residuals against Fitted values
plot(balbio)

#2) Normality of Variance, via...
#Q-Q plot
plot(balbio)

anova(balbio)

#TukeyHSD to look at comparisons between the interaction groups (aidded by QwenAI)
pumpBAL_data$interaction <- interaction(pumpBAL_data$group, pumpBAL_data$biocide, sep = "-")
balbioint <- aov(logFC ~ interaction, data = pumpBAL_data)
balbioint_tukey <- TukeyHSD(balbioint)
balbioint_tukey
bbi_tukey <- as.data.frame(balbioint_tukey$interaction)
bbi_tukey$comparison <- rownames(bbi_tukey)
bbi_tukey <- bbi_tukey[c("comparison", "lwr", "upr", "p adj")]
bbi_sigcompare <- im_tukey[im_tukey$`p adj` < 0.05, ]
bbi_sigcompare
write.csv(as.data.frame(bbi_sigcompare), 
          "bbi_tukey_sig.csv", row.names = FALSE)

