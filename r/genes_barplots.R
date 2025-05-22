#SetUp Fonts and establish Times New Roman as the global font
library(extrafont)
fonts()
par(family = "Times New Roman")

#Importing data
library(readr)

#let's set the working directory

setwd("/Users/isaak/OneDrive/Documents/Uni/2025/Session 1/Scientific Analysis_NSCI7915/Assessments/DataProject/data/raw")

library(dplyr)

pumpdata<- read_csv("abc_pumps.csv")
View(pumpdata)

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

#Now let's do the same for the adeIJK genes

adeI_data <- pumpdata %>%
  filter(gene_name == "adeI")

adeI_data
adeI_vector <- as.numeric(adeI_data[[4]])
adeI_vector

#Filtering for adeB data
adeJ_data <- pumpdata %>%
  filter(gene_name == "adeJ")

adeJ_data
adeJ_vector <- as.numeric(adeJ_data[[4]])
adeJ_vector

#Filtering for adeC data
adeK_data <- pumpdata %>%
  filter(gene_name == "adeK")

adeK_data
adeK_vector <- as.numeric(adeK_data[[4]])
adeK_vector

IJK <- cbind(adeI_vector,
             adeJ_vector,
             adeK_vector)
IJK <- t(IJK)
IJK

IJK.bar=barplot(IJK,beside=T,names.arg=Biocide_ID, 
                col= c("skyblue", "aquamarine4", "turquoise"),
                xlab= "Biocide",ylab= "logFC",ylim=c(-7,3), width=0.3, 
                cex.names=0.8, main="Response of adeIJK genes")
legend("bottomright",c("adeI","adeJ","adeK"),
       fill=c("skyblue", "aquamarine4", "turquoise"),cex=0.5)
box()

#znuABC

#znuA
znuA_data <- pumpdata %>%
  filter(gene_name == "znuA")

znuA_data
znuA_vector <- as.numeric(znuA_data[[4]])
znuA_vector

#Filtering for znuB data
znuB_data <- pumpdata %>%
  filter(gene_name == "znuB")

znuB_data
znuB_vector <- as.numeric(znuB_data[[4]])
znuB_vector

#Filtering for znuC data
znuC_data <- pumpdata %>%
  filter(gene_name == "znuC")

znuC_data
znuC_vector <- as.numeric(znuC_data[[4]])
znuC_vector

znu <- cbind(znuA_vector,
             znuB_vector,
             znuC_vector)
znu <- t(znu)
znu

znu.bar=barplot(znu,beside=T,names.arg=Biocide_ID, 
                col= c("seagreen3", "palegreen", "springgreen2"),
                xlab= "Biocide",ylab= "logFC",ylim=c(-1.5,3), width=0.3, 
                cex.names=0.8, main="Response of znuABC genes")
legend("bottomright",c("znuA","znuB","znuC"),
       fill=c("seagreen3", "palegreen", "springgreen2"),cex=0.5)
box()

#ttg2ABC

#ttg2A
ttg2A_data <- pumpdata %>%
  filter(gene_name == "ttg2A")

ttg2A_data
ttg2A_vector <- as.numeric(ttg2A_data[[4]])
ttg2A_vector

#Filtering for ttg2B data
ttg2B_data <- pumpdata %>%
  filter(gene_name == "ttg2B")

ttg2B_data
ttg2B_vector <- as.numeric(ttg2B_data[[4]])
ttg2B_vector

#Filtering for ttg2C data
ttg2C_data <- pumpdata %>%
  filter(gene_name == "ttg2C")

ttg2C_data
ttg2C_vector <- as.numeric(ttg2C_data[[4]])
ttg2C_vector

ttg2 <- cbind(ttg2A_vector,
             ttg2B_vector,
             ttg2C_vector)
ttg2 <- t(ttg2)
ttg2

ttg2.bar=barplot(ttg2,beside=T,names.arg=Biocide_ID, 
                col= c("coral1", "chocolate1", "tan2"),
                xlab= "Biocide",ylab= "logFC",ylim=c(-1.5,1.5), width=0.3, 
                cex.names=0.8, main="Response of ttg2ABC genes")
legend("bottomright",c("ttg2A","ttg2B","ttg2C"),
       fill=c("coral1", "chocolate1", "tan2"),cex=0.5)
box()



