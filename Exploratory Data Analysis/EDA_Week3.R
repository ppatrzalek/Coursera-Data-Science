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
