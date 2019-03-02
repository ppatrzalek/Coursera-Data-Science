# Lesson 1: Hierarchical Clustering
set.seed(123)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

# We choose the closest points and for them we create new single point for 
# next step of clustering - we start with n points (here 12) and we will finish
# with 1 single point

options(locatorBell = FALSE)
pnt <- identify(x, y, plot = F)
points(x[pnt], y[pnt], pch = 19, cex = 2, col = "red")

# Distance data.frame
dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)

# Hierarchical Clustering - hclust
hClustering <- hclust(distxy)
plot(hClustering)


# Prettier dendrogram

mypclust <- function(hclust, lab = hclust$labels, lab.col = rep(1, length(hclust$labels)), hang = 0.1, ...){
            y <- rep(hclust$height, 2)
            x <- as.numeric(hclust$merge)
            y <- y[which(x < 0)]
            x <- x[which(x < 0)]
            x <- abs(x)
            y <- y[order(x)]
            x <- x[order(x)]
            plot(hclust, labels = FALSE, hang = hang, ...)
            text(x = x, y = y[hclust$order] - (max(hclust$height)*hang), labels = lab[hclust$order],
                 col = lab.col[hclust$order], srt = 90, adj = c(1, 0.05), xpd = NA, ...)
}

mypclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))

# Heatmap
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)

# Lesson 2: K-Means Clustering
set.seed(12234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

DataFrame <- data.frame(x = x, y = y)
kmeansObj <- kmeans(DataFrame, centers = 3)
names(kmeansObj)
kmeansObj$centers

par(mar = c(0.2, 0.2, 0.2, 0.2))
plot(DataFrame$x, DataFrame$y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3)

# Hetmap
set.seed(1234)
dataMatrix <- as.matrix(DataFrame)[sample(1:12),]
kmeansObj2 <- kmeans(dataMatrix, centers = 2)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[,nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[,order(kmeansObj$cluster)], yaxt = "n")

set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

par(mar = rep(0.2, 4))
heatmap(dataMatrix)

# What if we add the pattern ?
set.seed(678910)
for(i in 1:40) {
  # flip a coin
  coinFlip <- rbinom(1, size = 1, prob = 0.5)
  # if coin is heads add a common pattern to that row
  if (coinFlip) {
   dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 3), each = 5) 
  }
}

par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

par(mar = rep(0.2, 4))
heatmap(dataMatrix)

# Patterns in rows ad columns
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(1, 3))
image(t(dataMatrix)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column Mean", ylab = "Column", pch = 19)

# Components of the SVD - v and u

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$u[,1], 40:1, xlab = "Row", ylab = "First left singular vector", pch = 19)
plot(svd1$v[,1],  xlab = "Column", ylab = "First right singular vector", pch = 19)

# Components of the SVD - Variance explained
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19)

# Relationship to principal component
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[,1], svd1$v[,1], pch = 19, xlab = "Principal Component 1", ylab = "Right Singular Vector 1")
abline(c(0, 1))

# Components of the SVD - variance explained
constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0, 1), each = 5)}
svd1 <- svd(constantMatrix)
par(mfrow = c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19)

# Missing value
dataMatrix2 <- dataMatrixOrdered

dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA
svd1 <- svd(scale(dataMatrix2))

# Lesson 3: Working with colors
pal <- colorRamp(c("red", "blue"))
pal(1)
pal(0)
pal(0.5)
pal(seq(0, 1, len = 10))

pal <- colorRampPalette(c("red", "yellow"))
pal(2)
pal(10)
