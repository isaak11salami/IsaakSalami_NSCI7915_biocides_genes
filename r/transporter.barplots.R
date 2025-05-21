#Importing data
library(readr)

#let's set the working directory

setwd("/Users/isaak/OneDrive/Documents/Uni/2025/Session 1/Scientific Analysis_NSCI7915/Assessments/DataProject/data/raw")

exdata <- read_csv("tps_simple.csv")
View(exdata)

#AgNO3 barplot
exdata_sorted <- exdata[order(exdata$AgNO3), ]

# Make sorted bar plot
barplot(height = exdata_sorted$AgNO3,names.arg = exdata_sorted$gene_name,
        las = 2,cex.names = 0.6,main = "AgNO3",
        ylab = "logFC",
        xlab = "gene",
        col = "green")

#BZK barplot
exdata_sorted <- exdata[order(exdata$BZK), ]

# Make sorted bar plot
barplot(height = exdata_sorted$BZK,names.arg = exdata_sorted$gene_name,
        las = 2,cex.names = 0.6,main = "BZK",
        ylab = "logFC",
        xlab = "gene",
        col = "green")

#CTAB barplot
exdata_sorted <- exdata[order(exdata$CTAB), ]

# Make sorted bar plot
barplot(height = exdata_sorted$CTAB,names.arg = exdata_sorted$gene_name,
        las = 2,cex.names = 0.6,main = "CTAB",
        ylab = "logFC",
        xlab = "gene",
        col = "green")

#CHL barplot
exdata_sorted <- exdata[order(exdata$CHL), ]

# Make sorted bar plot
barplot(height = exdata_sorted$CHL,names.arg = exdata_sorted$gene_name,
        las = 2,cex.names = 0.6,main = "CHL",
        ylab = "logFC",
        xlab = "gene",
        col = "green")

#TRC barplot
exdata_sorted <- exdata[order(exdata$TRC), ]

# Make sorted bar plot
barplot(height = exdata_sorted$TRC,names.arg = exdata_sorted$gene_name,
        las = 2,cex.names = 0.6,main = "TRC",
        ylab = "logFC",
        xlab = "gene",
        col = "green")

#CRL barplot
exdata_sorted <- exdata[order(exdata$CRL), ]

# Make sorted bar plot
barplot(height = exdata_sorted$CRL,names.arg = exdata_sorted$gene_name,
        las = 2,cex.names = 0.6,main = "CRL",
        ylab = "logFC",
        xlab = "gene",
        col = "green")

#PvPi barplot
exdata_sorted <- exdata[order(exdata$PVPi), ]

# Make sorted bar plot
barplot(height = exdata_sorted$PVPi,names.arg = exdata_sorted$gene_name,
        las = 2,cex.names = 0.6,main = "PvPi",
        ylab = "logFC",
        xlab = "gene",
        col = "green")

#HClO barplot
exdata_sorted <- exdata[order(exdata$HClO), ]

# Make sorted bar plot
barplot(height = exdata_sorted$HClO,names.arg = exdata_sorted$gene_name,
        las = 2,cex.names = 0.6,main = "HClO",
        ylab = "logFC",
        xlab = "gene",
        col = "green")

#GLU barplot
exdata_sorted <- exdata[order(exdata$GLU), ]

# Make sorted bar plot
barplot(height = exdata_sorted$GLU,names.arg = exdata_sorted$gene_name,
        las = 2,cex.names = 0.6,main = "GLU",
        ylab = "logFC",
        xlab = "gene",
        col = "green")

#EtOH barplot
exdata_sorted <- exdata[order(exdata$EtOH), ]

# Make sorted bar plot
barplot(height = exdata_sorted$EtOH,names.arg = exdata_sorted$gene_name,
        las = 2,cex.names = 0.6,main = "ETOH",
        ylab = "logFC",
        xlab = "gene",
        col = "green")















