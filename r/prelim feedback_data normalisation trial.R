#You're very right that the logFC data aren't normally distributed and you make a very
#convincing case. Usually, one would try a different transform. I'm not sure what the logFC
#distributions truly are, but perhaps they are something close to the Student's t distribution
#with just a few degrees of freedom:
  x = rt(10000,2)
plot(qnorm(1:10000 / 10001),sort(x))
#Take a look at the pattern of outliers on the ends.

#It's possible to normalise a t distribution by leveraging pt and then qnorm, asssuming you
#know the degrees of freedom:
x = qnorm(pt(rt(10000,2),2))
plot(qnorm(1:10000 / 10001),sort(x))

#There may be some other relevant distribution. It's not the gamma distribution or the Cauchy
#distribution, and unlikely to be exponential

#Let's set up the linear model for our data
#Importing data
library(readr)
setwd("/Users/isaak/OneDrive/Documents/Uni/2025/Session 1/Scientific Analysis_NSCI7915/Assessments/DataProject/data/raw")
pumpdata<- read_csv("abc_pumps.csv")
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

anova(biomod)

#Now let's try normalizing our data using John's method, using 
#df=9
n = qnorm(pt(biomod,9),9)
plot(qnorm(biomod),sort(n))

# Rank the logFC values
ranks <- rank(pumpdata$logFC)
ranks

# Convert ranks to percentiles
percentiles <- ranks / (nrow(pumpdata) + 1)

# Apply inverse normal transformation
normalized_logFC <- qnorm(percentiles)

# Replace logFC in your dataset with normalized version
pumpdata$normalized_logFC <- normalized_logFC

# Now re-run ANOVA with normalized data
biomod_normalized <- aov(normalized_logFC ~ biocide, data = pumpdata)
summary(biomod_normalized)

plot(biomod_normalized)

#Okay so the data looks more normal. Does this affect our barplots
#tho?

#let's start by just re-rerunning a simple ABC barplot 

#SetUp Fonts and establish Times New Roman as the global font
library(extrafont)
fonts()
par(family = "Times New Roman")



library(dplyr)


Biocide_ID <- c("AgNO3","BZK","CHL","CRL","CTAB","EtOH","GLU","HClO","PVPi","TRC")

#adeABE 

#Start by filtering for adeA data
adeA_data <- pumpdata %>%
  filter(gene_name == "adeA")

adeA_data
adeA_vector <- as.numeric(adeA_data[[4]])
adeA_vector

#Filtering for adeB data
adeB_data <- pumpdata %>%
  filter(gene_name == "adeB")

adeB_data
adeB_vector <- as.numeric(adeB_data[[4]])
adeB_vector

#Filtering for adeC data
adeC_data <- pumpdata %>%
  filter(gene_name == "adeC")

adeC_data
adeC_vector <- as.numeric(adeC_data[[4]])
adeC_vector

ABC <- cbind(adeA_vector,
             adeB_vector,
             adeC_vector)
ABC <- t(ABC)
ABC

ABC.bar=barplot(ABC,beside=T,names.arg=Biocide_ID, 
                col= c("maroon", "orchid", "magenta"),
                xlab= "Biocide",ylab= "logFC",ylim=c(-7,3), width=0.3, 
                cex.names=0.8, main="Response of adeABC genes")
legend("bottomright",c("adeA","adeB","adeC"),
       fill=c("maroon", "orchid", "magenta"),cex=0.5)
box()

#Now let's re-run using the normalised logFC values

adeA_vector <- as.numeric(adeA_data[[5]])
adeA_vector

#Filtering for adeB data
adeB_data <- pumpdata %>%
  filter(gene_name == "adeB")

adeB_data
adeB_vector <- as.numeric(adeB_data[[5]])
adeB_vector

#Filtering for adeC data
adeC_data <- pumpdata %>%
  filter(gene_name == "adeC")

adeC_data
adeC_vector <- as.numeric(adeC_data[[5]])
adeC_vector

ABC <- cbind(adeA_vector,
             adeB_vector,
             adeC_vector)
ABC <- t(ABC)
ABC

ABC.bar=barplot(ABC,beside=T,names.arg=Biocide_ID, 
                col= c("maroon", "orchid", "magenta"),
                xlab= "Biocide",ylab= "NORMALISED logFC",ylim=c(-3,1.5), width=0.3, 
                cex.names=0.8, main="Response of adeABC genes")
legend("bottomright",c("adeA","adeB","adeC"),
       fill=c("maroon", "orchid", "magenta"),cex=0.5)
box()

#The barplots are different (minute differences, but changes in logFC
#direction and magnitude are hard to ignore)
