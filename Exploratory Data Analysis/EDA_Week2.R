# Lesson 1 (lattice)

library(lattice)
library(datasets)

# Simple scatterplot
xyplot(Ozone ~ Wind, data = airquality)

# Convert 'Month'  to a factor variable
airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5, 1))

# Lattice plotting syste
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <-  x + f - f * y + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))

xyplot(x ~ y | f, panel = function(x, y, ...){
  panel.xyplot(x, y, ...)
  panel.abline(h = median(y), lty = 2)
}) 

xyplot(x ~ y | f, panel = function(x, y, ...){
  panel.xyplot(x, y, ...)
  panel.lmline(x, y, col = 2)
}) 

# Lesson 2 (ggplot2)


# Quiz
library(nlme)
library(lattice)
xyplot(weight ~ Time | Diet, BodyWeight)

library(datasets)
library(ggplot2)

data(airquality)
qplot(Wind, Ozone, data = airquality, facets = . ~ factor(Month))

airquality = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)
library(ggplot2movies)
qplot(votes, rating, data = movies) + geom_smooth()

qplot(votes, rating, data = movies) + stats_smooth("loess")
