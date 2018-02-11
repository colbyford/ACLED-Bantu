####################################
# Dimensionality Reduction Script  #
# for Aligned Molecular Sequences  #
#         By: Colby Ford           #
####################################


## Sequence Alignment Comparison
#install.packages("AlignStat")
library(AlignStat)

comp <- compare_alignments(ref, com, SP = TRUE, CS = TRUE)

## Nonmetric MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

library(MASS)
d <- dist(mydata) # euclidean distances between the rows
fit <- isoMDS(d, k=2) # k is the number of dim
fit # view results

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
  main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(mydata), cex=.7) 

## Laplacian Eigenmaps
#install.packages("dimRed")
dat <- loadDataSet("3D S Curve")
leim <- LaplacianEigenmaps()
emb <- leim@fun(dat, leim@stdpars)
plot(emb@data@data)
