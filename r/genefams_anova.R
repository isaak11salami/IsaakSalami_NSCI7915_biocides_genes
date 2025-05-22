#Importing data
library(readr)

#let's set the working directory

setwd("/Users/isaak/OneDrive/Documents/Uni/2025/Session 1/Scientific Analysis_NSCI7915/Assessments/DataProject/data/raw")

pumpdata<- read_csv("abc_pumps.csv")
View(pumpdata)

biomod <- aov(logFC~biocide, data=pumpdata)
anova(biomod)
biomod_tukey <- TukeyHSD(biomod)
print(biomod_tukey)

#Reworking the TukeyHSD as a .csv-ready dataframe (aided by Qwen AI)

# Extract Tukey HSD comparisons (for the "biocide" term)
bm_tukey <- as.data.frame(biomod_tukey$biocide)

# Add comparison labels as a new column (optional)
bm_tukey$comparison <- rownames(bm_tukey)

# Reorder columns if desired
bm_tukey <- bm_tukey[c("comparison", "lwr", "upr", "p adj")]

#There is a lot of comparisons happening here. Let's filter to return
#only those with a p.adj < 0.05 (code from QwenAI)
biomod_sigcompare <- bm_tukey[bm_tukey$`p adj` < 0.05, ]
biomod_sigcompare

#Now let's look at logFC differences between our ABC transporters
fammod <- aov(logFC~family, data=pumpdata)
anova(fammod)

fammod_tukey <- TukeyHSD(fammod)
fammod_tukey
fm_tukey <- as.data.frame(fammod_tukey$family)
fm_tukey$comparison <- rownames(fm_tukey)
fm_tukey <- fm_tukey[c("comparison", "lwr", "upr", "p adj")]
fammod_sigcompare <- fm_tukey[fm_tukey$`p adj` < 0.05, ]
fammod_sigcompare


#Now let's look for interaction between ABC pumps and biocide (do the 
#biocides impact certain pumps in a unique manner?)
biofam <- aov(logFC~family*biocide, data=pumpdata)
anova(biofam)

#TukeyHSD to look at comparisons between the interaction groups (aidded by QwenAI)
pumpdata$interaction <- interaction(pumpdata$family, pumpdata$biocide, sep = "-")
interactionmod <- aov(logFC ~ interaction, data = pumpdata)
interactionmod_tukey <- TukeyHSD(interactionmod)
interactionmod_tukey
im_tukey <- as.data.frame(interactionmod_tukey$interaction)
im_tukey$comparison <- rownames(im_tukey)
im_tukey <- im_tukey[c("comparison", "lwr", "upr", "p adj")]
interactionmod_sigcompare <- im_tukey[im_tukey$`p adj` < 0.05, ]
interactionmod_sigcompare
write.csv(as.data.frame(interactionmod_sigcompare), 
          "interaction_tukey_sig.csv", row.names = FALSE)

#Okay, this is a little hard to interpret. Looks like we have a lot of
#significant differences. Let's try visually representing these differences
#in a barplot (new file tho)


