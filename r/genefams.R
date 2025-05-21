#Importing data
library(readr)

#let's set the working directory

setwd("/Users/isaak/OneDrive/Documents/Uni/2025/Session 1/Scientific Analysis_NSCI7915/Assessments/DataProject/data/raw")

pumpdata<- read_csv("rnd_pumps.csv")
View(pumpdata)

biomod <- aov(logFC~biocide, data=pumpdata)
summary(biomod)
anova(biomod)

fammod <- aov(logFC~family, data=pumpdata)
summary(fammod)
anova(fammod)
TukeyHSD(fammod)

biofam <- aov(logFC~family*biocide, data=pumpdata)
anova(biofam)

#Maybe don't bother with the TukeyHSD?
tukey_biofam <- TukeyHSD(biofam)
print(tukey_biofam)
write.csv(tukey_biofam, "tukey_hsd_biofam.csv", row.names = FALSE)

#Can we plot a factor analysis plot for gene fam too?

#Barplots > make barplots for each gene family (somehow)
