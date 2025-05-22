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

#Following code was QwenAI

# Filter for the adeABC family
adeABC_data <- pumpdata %>%
  filter(family == "adeABC")

# Calculate average logFC for each biocide


adeABC <- summarise(group_by(filter(pumpdata, family == "adeABC"), 
                             biocide), avg_adeABC_logFC = mean(logFC))
#Let's calculate the mean and standard dev for the adeABC family (QwenAI)
adeABC <- summarise(
  group_by(
    filter(pumpdata, family == "adeABC"),
    biocide
  ),
  avg_adeABC_logFC = mean(logFC),
  sd_adeABC_logFC = sd(logFC),
  adeABC_n = n()
)
adeABC
adeABC.means <- (adeABC$avg_adeABC_logFC)
adeABC.means
adeABC.sd <- (adeABC$sd_adeABC_logFC)
adeABC.sd
adeABC.n <- (adeABC$adeABC_n)
adeABC.n

#logFC means of teh adeABC family
adeABC

adeABC_vector <- as.numeric(adeABC[[2]])
adeABC_vector

Biocide_ID <- c("AgNO3","BZK","CHL","CRL","CTAB","EtOH","GLU","HClO","PVPi","TRC")
adeABC_bar=barplot(adeABC_vector,beside=T,names.arg=Biocide_ID,col= c("maroon3"),
                   xlab= "Biocide",
                   ylab= "logFC",ylim=c(-12,4), width=0.2,
                   cex.names=0.8,main="adeABC")
box()

#Making 95% CI for the barplot
#We already have the means, but we also need the standard error
adeABC.ses <- (adeABC.sd)/sqrt(adeABC.n)
adeABC.ses
#Now Calculate 95%CI
adeABC.lowerci <- adeABC.means - adeABC.ses*qt(0.025,df=adeABC.n-1)
adeABC.lowerci
adeABC.upperci <- adeABC.means + adeABC.ses*qt(0.025,df=adeABC.n-1)
adeABC.upperci

arrows(x0 = adeABC_bar,y0 = adeABC.lowerci,x1 = adeABC_bar,
       y1 = adeABC.upperci, angle = 90, code = 3, length = 0.1)

#Now repeat for adeIJK, znuABC and ttg2ABC

#adeIJK
# Filter for the adeIJK family
adeIJK_data <- pumpdata %>%
  filter(family == "adeIJK")

# Calculate average logFC for each biocide

adeIJK <- summarise(group_by(filter(pumpdata, family == "adeIJK"), 
                             biocide), avg_adeIJK_logFC = mean(logFC))
#Let's calculate the mean and standard dev for the adeIJK family (QwenAI)
adeIJK <- summarise(
  group_by(
    filter(pumpdata, family == "adeIJK"),
    biocide
  ),
  avg_adeIJK_logFC = mean(logFC),
  sd_adeIJK_logFC = sd(logFC),
  adeIJK_n = n()
)
adeIJK
adeIJK.means <- (adeIJK$avg_adeIJK_logFC)
adeIJK.means
adeIJK.sd <- (adeIJK$sd_adeIJK_logFC)
adeIJK.sd
adeIJK.n <- (adeIJK$adeIJK_n)
adeIJK.n

#logFC means of the adeIJK family
adeIJK

adeIJK_vector <- as.numeric(adeIJK[[2]])
adeIJK_vector


adeIJK_bar=barplot(adeIJK_vector,beside=T,names.arg=Biocide_ID,col= c("aquamarine"),
                   xlab= "Biocide",
                   ylab= "logFC",ylim=c(-10,3), width=0.2,
                   cex.names=0.8,main="adeIJK")
box()

#Making 95% CI for the barplot
#We already have the means, but we also need the standard error
adeIJK.ses <- (adeIJK.sd)/sqrt(adeIJK.n)
adeIJK.ses
#Now Calculate 95%CI
adeIJK.lowerci <- adeIJK.means - adeIJK.ses*qt(0.025,df=adeIJK.n-1)
adeIJK.lowerci
adeIJK.upperci <- adeIJK.means + adeIJK.ses*qt(0.025,df=adeIJK.n-1)
adeIJK.upperci

arrows(x0 = adeIJK_bar,y0 = adeIJK.lowerci,x1 = adeIJK_bar,
       y1 = adeIJK.upperci, angle = 90, code = 3, length = 0.1)

#znuABC
# Filter for the znuABC family
znuABC_data <- pumpdata %>%
  filter(family == "znuABC")

# Calculate average logFC for each biocide

znuABC <- summarise(group_by(filter(pumpdata, family == "znuABC"), 
                             biocide), avg_znuABC_logFC = mean(logFC))
#Let's calculate the mean and standard dev for the znuABC family (QwenAI)
znuABC <- summarise(
  group_by(
    filter(pumpdata, family == "znuABC"),
    biocide
  ),
  avg_znuABC_logFC = mean(logFC),
  sd_znuABC_logFC = sd(logFC),
  znuABC_n = n()
)
znuABC
znuABC.means <- (znuABC$avg_znuABC_logFC)
znuABC.means
znuABC.sd <- (znuABC$sd_znuABC_logFC)
znuABC.sd
znuABC.n <- (znuABC$znuABC_n)
znuABC.n

#logFC means of the znuABC family
znuABC

znuABC_vector <- as.numeric(znuABC[[2]])
znuABC_vector


znuABC_bar=barplot(znuABC_vector,beside=T,names.arg=Biocide_ID,col= c("darkseagreen1"),
                   xlab= "Biocide",
                   ylab= "logFC",ylim=c(-1.5,4), width=0.2,
                   cex.names=0.8,main="znuABC")
box()

#Making 95% CI for the barplot
#We already have the means, but we also need the standard error
znuABC.ses <- (znuABC.sd)/sqrt(znuABC.n)
znuABC.ses
#Now Calculate 95%CI
znuABC.lowerci <- znuABC.means - znuABC.ses*qt(0.025,df=znuABC.n-1)
znuABC.lowerci
znuABC.upperci <- znuABC.means + znuABC.ses*qt(0.025,df=znuABC.n-1)
znuABC.upperci

arrows(x0 = znuABC_bar,y0 = znuABC.lowerci,x1 = znuABC_bar,
       y1 = znuABC.upperci, angle = 90, code = 3, length = 0.1)

#ttg2ABC
# Filter for the ttg2ABC family
ttg2ABC_data <- pumpdata %>%
  filter(family == "ttg2ABC")

# Calculate average logFC for each biocide

ttg2ABC <- summarise(group_by(filter(pumpdata, family == "ttg2ABC"), 
                             biocide), avg_ttg2ABC_logFC = mean(logFC))
#Let's calculate the mean and standard dev for the ttg2ABC family (QwenAI)
ttg2ABC <- summarise(
  group_by(
    filter(pumpdata, family == "ttg2ABC"),
    biocide
  ),
  avg_ttg2ABC_logFC = mean(logFC),
  sd_ttg2ABC_logFC = sd(logFC),
  ttg2ABC_n = n()
)
ttg2ABC
ttg2ABC.means <- (ttg2ABC$avg_ttg2ABC_logFC)
ttg2ABC.means
ttg2ABC.sd <- (ttg2ABC$sd_ttg2ABC_logFC)
ttg2ABC.sd
ttg2ABC.n <- (ttg2ABC$ttg2ABC_n)
ttg2ABC.n

#logFC means of the znuABC family
ttg2ABC

ttg2ABC_vector <- as.numeric(ttg2ABC[[2]])
ttg2ABC_vector


ttg2ABC_bar=barplot(ttg2ABC_vector,beside=T,names.arg=Biocide_ID,col= c("coral"),
                   xlab= "Biocide",
                   ylab= "logFC",ylim=c(-2,1.5), width=0.2,
                   cex.names=0.8,main="ttg2ABC")
box()

#Making 95% CI for the barplot
#We already have the means, but we also need the standard error
ttg2ABC.ses <- (ttg2ABC.sd)/sqrt(ttg2ABC.n)
ttg2ABC.ses
#Now Calculate 95%CI
ttg2ABC.lowerci <- ttg2ABC.means - ttg2ABC.ses*qt(0.025,df=ttg2ABC.n-1)
ttg2ABC.lowerci
ttg2ABC.upperci <- ttg2ABC.means + ttg2ABC.ses*qt(0.025,df=ttg2ABC.n-1)
ttg2ABC.upperci

arrows(x0 = ttg2ABC_bar,y0 = ttg2ABC.lowerci,x1 = ttg2ABC_bar,
       y1 = ttg2ABC.upperci, angle = 90, code = 3, length = 0.1)

#OKay cool. Now, let's do a clutered bar plot
#Bind our four sets of data into the one matrix
pump.means <- cbind(
  adeABC = adeABC_vector,
  adeIJK = adeIJK_vector,
  znuABC = znuABC_vector,
  ttg2ABC = ttg2ABC_vector
)
pump.means = t(pump.means)
pump.means

pump.bar=barplot(pump.means,beside=T,names.arg=Biocide_ID, 
                  col= c("maroon3", "aquamarine", "darkseagreen1", "coral"),
              xlab= "Biocide", 
              ylab= "logFC",ylim=c(-11,4), width=0.3, 
              cex.names=0.8, main="Impact of Biocides on ABC pumps")


legend("bottomright",c("adeABC","adeIJK","znuABC","ttg2ABC"),
       fill=c("maroon3", "aquamarine", "darkseagreen1", "coral"),cex=0.5)

#Now add error bars
adeABC.pos <- pump.bar[c(1,5,9,13,17,21,25,29,33,37)]
arrows(x0 = adeABC.pos,y0 = adeABC.lowerci,x1 = adeABC.pos,
       y1 = adeABC.upperci, angle = 90, code = 3, length = 0.05)

adeIJK.pos <- pump.bar[c(2,6,10,14,18,22,26,30,34,38)]
arrows(x0 = adeIJK.pos,y0 = adeIJK.lowerci,x1 = adeIJK.pos,
       y1 = adeIJK.upperci, angle = 90, code = 3, length = 0.05)

znuABC.pos <- pump.bar[c(3,7,11,15,19,23,27,31,35,39)]
arrows(x0 = znuABC.pos,y0 = znuABC.lowerci,x1 = znuABC.pos,
       y1 = znuABC.upperci, angle = 90, code = 3, length = 0.05)

ttg2ABC.pos <- pump.bar[c(4,8,12,16,20,24,28,32,36,40)]
arrows(x0 = ttg2ABC.pos,y0 = ttg2ABC.lowerci,x1 = ttg2ABC.pos,
       y1 = ttg2ABC.upperci, angle = 90, code = 3, length = 0.05)




box()





