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

newgenes <- read_csv("BAL062_genes.csv")
View(newgenes)

#Filter out data for each gene

#Start by filtering for BAL062_00181 data
bal181_data <- newgenes %>%
  filter(gene_name == "BAL062_00181")
bal181_data

bal181_data
bal181_vector <- as.numeric(bal181_data[[3]])
bal181_vector

#BAL062_01982
bal1982_data <- newgenes %>%
  filter(gene_name == "BAL062_01982")
bal1982_data

bal1982_data
bal1982_vector <- as.numeric(bal1982_data[[3]])
bal1982_vector

#BAL00181 was clustered with the ttg2 genes. So, let's make a quick clustered bar plot and see what's up

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

#mlaC (reference gene known to be associated wirh ttg2)
mlaC_data <- newgenes %>%
  filter(gene_name == "mlaC")

mlaC_data
mlaC_vector <- as.numeric(mlaC_data[[3]])
mlaC_vector

#bind it all togther
ttg2xtra <- cbind(ttg2A_vector,
              ttg2B_vector,
              ttg2C_vector,
              mlaC_vector,
              bal181_vector)
ttg2xtra <- t(ttg2xtra)
ttg2xtra

#plot
ttg2xtra.bar=barplot(ttg2xtra,beside=T,names.arg=Biocide_ID, 
                 col= c("coral1", "chocolate1", "tan2","orange","yellow"),
                 xlab= "Biocide",ylab= "logFC",ylim=c(-1.5,1.5), width=0.3, 
                 cex.names=0.8, main="Does BAL062_00181 fit with ttg2ABC?")
legend("bottomright",c("ttg2A","ttg2B","ttg2C","mlaC","BAL062_00181"),
       fill=c("coral1", "chocolate1", "tan2","orange","yellow"),cex=0.5)
box()

#Now test hwo well BAL062_01982 matches with the znu genes. Use zur as the 
#reference "associated" gene
znuA_data <- pumpdata %>%
  filter(gene_name == "znuA")

znuA_data
znuA_vector <- as.numeric(znuA_data[[4]])
znuA_vector


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

#Filter for zur reference
zur_data <- newgenes %>%
  filter(gene_name == "zur")

zur_data
zur_vector <- as.numeric(zur_data[[3]])
zur_vector

#Filter for BAL062_01982
bal1982_data <- newgenes %>%
  filter(gene_name == "BAL062_01982")

bal1982_data
bal1982_vector <- as.numeric(bal1982_data[[3]])
bal1982_vector

#bind it all together
znuxtra <- cbind(znuA_vector,
                  znuB_vector,
                  znuC_vector,
                  zur_vector,
                  bal1982_vector)
znuxtra <- t(znuxtra)
znuxtra

#plot
znuxtra.bar=barplot(znuxtra,beside=T,names.arg=Biocide_ID, 
                     col= c("seagreen3", "palegreen", "springgreen2", "chartreuse","green1"),
                     xlab= "Biocide",ylab= "logFC",ylim=c(-1.5,3.5), width=0.3, 
                     cex.names=0.8, main="Does BAL062_01982 fit with znuABC?")
legend("bottomright",c("znuA","znuB","znuC","zur","BAL062_01982"),
       fill=c("seagreen3", "palegreen", "springgreen2", "chartreuse","green1"),cex=0.5)
box()

#Does BAL062_01982 look similar to adeABC

#bind it all together
ABCxtra <- cbind(adeA_vector,
                 adeB_vector,
                 adeC_vector,
                 zur_vector,
                 bal1982_vector)
ABCxtra <- t(ABCxtra)
ABCxtra

#plot
ABCxtra.bar=barplot(ABCxtra,beside=T,names.arg=Biocide_ID, 
                    col= c("maroon", "orchid", "magenta","chartreuse","green1"),
                    xlab= "Biocide",ylab= "logFC",ylim=c(-7,3.5), width=0.3, 
                    cex.names=0.8, main="Do zur and BAL062_01982 fit with adeABC?")
legend("bottomright",c("adeA","adeB","adeC","zur","BAL062_01982"),
       fill=c("maroon", "orchid", "magenta","chartreuse","green1"),cex=0.5)
box()

#Does BAL062_01982 look similar to adeIJK

#bind it all together
IJKxtra <- cbind(adeI_vector,
                 adeJ_vector,
                 adeK_vector,
                 zur_vector,
                 bal1982_vector)
IJKxtra <- t(IJKxtra)
IJKxtra

#plot
IJKxtra.bar=barplot(IJKxtra,beside=T,names.arg=Biocide_ID, 
                    col= c("skyblue", "aquamarine4", "turquoise","chartreuse","green1"),
                    xlab= "Biocide",ylab= "logFC",ylim=c(-7,3.5), width=0.3, 
                    cex.names=0.8, main="Do zur and BAL062_01982 fit with adeIJK?")
legend("bottomright",c("adeI","adeJ","adeK","zur","BAL062_01982"),
       fill=c("skyblue", "aquamarine4", "turquoise","chartreuse","green1"),cex=0.5)
box()

#Does BAL062_01982 look similar to ttg2ABC?

#bind it all together
ttg2xtra <- cbind(ttg2A_vector,
                 ttg2B_vector,
                 ttg2C_vector,
                 zur_vector,
                 bal1982_vector)
ttg2xtra <- t(ttg2xtra)
ttg2xtra

#plot
ttg2xtra.bar=barplot(ttg2xtra,beside=T,names.arg=Biocide_ID, 
                    col= c("coral1", "chocolate1", "tan2","chartreuse","green1"),
                    xlab= "Biocide",ylab= "logFC",ylim=c(-1.5,3.5), width=0.3, 
                    cex.names=0.8, main="Do zur and BAL062_01982 fit with ttg2ABC?")
legend("bottomright",c("ttg2A","ttg2B","ttg2C","zur","BAL062_01982"),
       fill=c("coral1", "chocolate1", "tan2","chartreuse","green1"),cex=0.5)
box()

#Now let's look at BAL062_00181

#Does BAL062_00181 look similar to adeABC

#bind it all together
ABCxtra2 <- cbind(adeA_vector,
                 adeB_vector,
                 adeC_vector,
                 mlaC_vector,
                 bal181_vector)
ABCxtra2 <- t(ABCxtra2)
ABCxtra2

#plot
ABCxtra2.bar=barplot(ABCxtra2,beside=T,names.arg=Biocide_ID, 
                    col= c("maroon", "orchid", "magenta","orange","yellow"),
                    xlab= "Biocide",ylab= "logFC",ylim=c(-7,2), width=0.3, 
                    cex.names=0.8, main="Do mlaC and BAL062_00181 fit with adeABC?")
legend("bottomright",c("adeA","adeB","adeC","mlaC","BAL062_00181"),
       fill=c("maroon", "orchid", "magenta","orange","yellow"),cex=0.5)
box()

#Does BAL062_00181 look similar to adeIJK

#bind it all together
IJKxtra2 <- cbind(adeI_vector,
                 adeJ_vector,
                 adeK_vector,
                 mlaC_vector,
                 bal181_vector)
IJKxtra2 <- t(IJKxtra2)
IJKxtra2

#plot
IJKxtra2.bar=barplot(IJKxtra2,beside=T,names.arg=Biocide_ID, 
                    col= c("skyblue", "aquamarine4", "turquoise","orange","yellow"),
                    xlab= "Biocide",ylab= "logFC",ylim=c(-7,2), width=0.3, 
                    cex.names=0.8, main="Do mlaC and BAL062_00181 fit with adeIJK?")
legend("bottomright",c("adeI","adeJ","adeK","mlaC","BAL062_00181"),
       fill=c("skyblue", "aquamarine4", "turquoise","orange","yellow"),cex=0.5)
box()

#Does BAL062_00181 look similar to znu?

#bind it all together
znuxtra2 <- cbind(znuA_vector,
                 znuB_vector,
                 znuC_vector,
                 mlaC_vector,
                 bal181_vector)
znuxtra2 <- t(znuxtra2)
znuxtra2

#plot
znuxtra2.bar=barplot(znuxtra2,beside=T,names.arg=Biocide_ID, 
                    col= c("seagreen3", "palegreen", "springgreen2","orange","yellow"),
                    xlab= "Biocide",ylab= "logFC",ylim=c(-1.5,3), width=0.3, 
                    cex.names=0.8, main="Do mlaC and BAL062_00181  fit with znuABC?")
legend("bottomright",c("znuA","znuB","znuC","mlaC","BAL062_00181"),
       fill=c("seagreen3", "palegreen", "springgreen2","orange","yellow"),cex=0.5)
box()

