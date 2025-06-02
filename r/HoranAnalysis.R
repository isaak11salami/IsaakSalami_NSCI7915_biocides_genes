#Testing out Horan's Parallel analysis as an alternative to PCA scree plot

#Start by formatting the data using the same method employed in prep for
#princom

#Importing data
library(readr)

#let's set the working directory

setwd("/Users/isaak/OneDrive/Documents/Uni/2025/Session 1/Scientific Analysis_NSCI7915/Assessments/DataProject/data/raw")

tps <- read_csv("transporters.csv")
View(tps)

#Will try to process this data using the same method that John walked me
#through in the "General Analysis" file

#Formatting data
tps = as.data.frame(tps)
#Isolate our gene names from the 3rd column, starting at the 3rd row and
#continuing donward
genename=tps[3:nrow(tps),3]
genename
#This cuts out the first two columns of tps

tps=tps[,1:ncol(tps) %% 3==2]
#Okay. So, what did this just do?
#From Qwen 2.5-Max: Subsets the columns of the tps data frame based on 
#a condition involving the modulo operator (%%). Specifically: 1:ncol(tps) 
#generates a sequence of integers from 1 to the total number of columns in 
#tps. %% 3 == 2 checks which column indices satisfy the condition "remainder 
#when divided by 3 equals 2."Only the columns whose indices meet this 
#condition are retained.
#So, we are isolating every 3rd column (the ones with the logFC values, which
#we care about)

headers=tps[,]
tps

tps=tps[,-1]
tps=tps[-(1:2),]
#This removes the first column and first two rowns of unnecessary data (which aren't logFC values)
#from the dataframe

tps=matrix(as.numeric(unlist(tps)),nrow(tps),ncol(tps))
head(tps)

#Plot a Dendrogram to get a sense of the data
plot(hclust(dist(tps)))
#Okay, so we have two distinct clusters yet again

h=hclust(dist(t(tps)))
plot(h)
h

#Fixing up the headers
headers
headers=as.character(headers)
is.character(headers)

#OK so the data is formatted. Now, let's start by installing the
#required package

install.packages("paran")
library("paran")

#Now let's do our ananlysis

paran(tps, cfa=TRUE, graph=TRUE, color=TRUE, col=c("black","red","blue"))

#Okay so based on the output, we should retain 5 factors rather than 4...

plot(factanal(tps,factors=5,scores='regression')$scores,cex=0)
text(factanal(tps,factors=5,scores='regression')$scores,labels=genename,xpd=NA)
factor=factanal(tps,factors=5,scores='regression')
factor

factor$loadings

factor$scores
plot(factor$scores, cex=0)
text(factor$scores, cex=0.5, labels=genename)

#By using 5 factors rather than 4, our FA plot of scores is completely messed
#up. We still get groupings for znu and ttg2, but adeABC/IJK are muddles with
#other genes. Moreover, adeR and adeS are now associated with ttg2 and
#znu, respectively. This still kind of works with me current
#hypothesis and data, but not exactly perfect.
#Might be worth doing a bit of research into parallel analysis vs
#princomp/scree plot 

plot(factanal(tps,factors=4,scores='regression')$scores,cex=0)
text(factanal(tps,factors=4,scores='regression')$scores,labels=genename,xpd=NA)
factor=factanal(tps,factors=4,scores='regression')
factor

factor$loadings

factor$scores
plot(factor$scores, cex=0)
text(factor$scores, cex=0.5, labels=genename)
