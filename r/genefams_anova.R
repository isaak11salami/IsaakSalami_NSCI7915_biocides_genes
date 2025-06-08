#Importing data
library(readr)

#let's set the working directory

setwd("/Users/isaak/OneDrive/Documents/Uni/2025/Session 1/Scientific Analysis_NSCI7915/Assessments/DataProject/data/raw")

pumpdata<- read_csv("tps.csv")
View(pumpdata)

#Effect of biocides
biomod <- aov(logFC~biocide, data=pumpdata)

#Check Assumptions
#1) Homogeneity of Variance, via...
#Levene Test
library(car)
leveneTest(biomod)

#Plot Residuals against Fitted values
#2) Normality of Variance, via...
#Q-Q plot
plot(biomod)
#Not equal variances, but that is to be expected due to the different
#genes in the family. Let's proceed with the analysis anyway

anova(biomod)

#which groups differ?
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

write.csv(as.data.frame(biomod_sigcompare), 
         "bmsig_tukey_sig.csv", row.names = FALSE)

#Now let's look at logFC differences between our ABC transporters
fammod <- aov(logFC~family, data=pumpdata)

#Check Assumptions
#1) Homogeneity of Variance, via...
#Levene Test
leveneTest(fammod)
#Plot Residuals against Fitted values
plot(fammod)

#2) Normality of Variance, via...
#Q-Q plot
plot(fammod)

anova(fammod)

fammod_tukey <- TukeyHSD(fammod)
fammod_tukey
fm_tukey <- as.data.frame(fammod_tukey$family)
fm_tukey$comparison <- rownames(fm_tukey)
fm_tukey <- fm_tukey[c("comparison", "lwr", "upr", "p adj")]
fammod_sigcompare <- fm_tukey[fm_tukey$`p adj` < 0.05, ]
fammod_sigcompare

fm_tukey



citation()

#Can we fit our data to a GLMM? This would require both
#Fixed Effects and Random Effects. What effects do we have?
#Biocides are fixed. We care both about the difference between
#each biocide, AS WELL AS the SPECIFIC logFC value of each
#biocide. So, this effect is FIXED
#Gene family is... not super sure. Typically, if objects
#within each cluster vary, we say the effect is random.
#If they don't vary, then the effect is fixed. If gene
#families are our clusters, and the gene_name is our 
#object, then yes we do have variance within clusters.
#However, it feels weird to say that "yep gene family
#is a random effect" as the genes in each family are 
#indeed specified (because the gene family is inherently
#defined by its constituent genes). For example, the adeIJK
#family only has genes adeI, J and K, and can ONLY have those
#three genes. It does not have genes adeA, B or C. Nor does
#it have the znu or ttg2 genes. So, the effect MUST be fixed,
#right? If we had a dataset where trio's of genes were randomly
#assigned to a gene family, then yeah maybe it would be random.
#However, gene_name and gene_family are inherently linked. So
#GLMM won't work in this case... right???
#We can do a comparison of models, one where we consider
#gene family as fixed and one where we consider it as random
#and see what happens? (maybe idk if this will actually work)

#Start by installing packages
library(lme4) 
install.packages('lmerTest') 
library(lmerTest) 
install.packages('MuMIn') 
library(MuMIn) 

#Let's set up a random effects model and see what happens

m = lmer(logFC~biocide + (biocide|family), data=pumpdata)
m
plot(m)
summary(m)
ranef(m) 
rand(m) 
r.squaredGLMM(m) 

#We use the (biocide|family) term to indicate that while the
#intercept of family is random, the slope (which is dependent
#on biocide) is also random. Basically, this is the INTERACTION
#term between biocide and family, which assumes that biocides will
#interact with each gene family in a unique manner. We know this 
#to be true conceptually, because... Wait, we can't look at an 
#interaction in this way. Basically, we just have 4 subjects (gene families)
#10 treatments (biocides) and the resultant logFC in gene family
#expression. You can't really treat gene families as a factor just because
#it CONTAINS stuff. The things that it contains are the genes. However, 
#we need to treat these genes like they're individual measurements
#(like trials 1,2 and 3) to calculate our mean (which is gene family)
#So, in this instance it's a little disingenuous to try and treat gene family
#as some kind of grouping factor. 

#So basically no, we CANNOT use GLMM because we only have 
#one effect, and that effect is FIXED.

