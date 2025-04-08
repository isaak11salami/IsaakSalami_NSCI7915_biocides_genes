#Importing data
library(readr)
cells <- read_csv("cells.csv")
View(cells)

#Formatting data
cells = as.data.frame(cells)
genename=cells[3:nrow(cells),3]
genename
cells=cells[,1:ncol(cells) %% 3==2]
headers=cells[,]
headers
cells=cells[,-1]
cells=cells[-(1:2),]
cells=matrix(as.numeric(unlist(cells)),nrow(cells),ncol(cells))
head(cells)
plot(hclust(dist(cells)))
#Two clusters. RHS has a group toward the bottom
plot
h=hclust(dist(t(cells)))
plot(h)
h
headers

?hclust
headers=as.character(headers)
is.character(headers)

rowSums(cells)
princomp(cells)
#Skree plot to show PCA
plot(princomp(cells)$sdev)
plot(princomp(cells)$loadings,cex=0)
text(princomp(cells)$loadings,labels=headers,xpd=NA)
#Let's look at scores instead of loadings
plot(princomp(cells)$scores,cex=0)
text(princomp(cells)$scores,labels=genename,xpd=NA)

plot(factanal(cells,factors=2,scores='regression')$scores,cex=0)
text(factanal(cells,factors=2,scores='regression')$scores,labels=genename,xpd=NA)
factor=factanal(cells,factors=4,scores='regression')
factor

factor$loadings

factor$scores
plot(factor$scores, cex=0)
text(factor$scores, cex=0.5, labels=genename)

is.numeric(cells)
is.matrix(cells)
sum(cells)
t(cells)

headers
colMeans(cells)
cells[,4]
sort(cells[,4])


