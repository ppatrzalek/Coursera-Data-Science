# Lesson 2: Plotting
library(datasets)
data(cars)
with(cars, plot(speed, dist))

library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))

library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

# Base plotting system
library(datasets)
hist(airquality$Ozone)

airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")
# Lesson 3: Graphics devices
library(datasets)
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithfull Geyser data")
dev.copy(png, file = "geyserplot.png")
dev.copy2pdf(file = "geyserplot.pdf")
dev.off()

