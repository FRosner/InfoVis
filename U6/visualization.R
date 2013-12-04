rm(list = ls())

cars = read.csv("04cars.csv")
cars$InvoicePrice = as.numeric(as.character(cars$InvoicePrice))
cars$SuggestedRetailPrice = as.numeric(as.character(cars$SuggestedRetailPrice))
cars$EngineSize = as.numeric(as.character(cars$EngineSize))
cars$NumCylinders = as.numeric(as.character(cars$NumCylinders))
cars$HP = as.numeric(as.character(cars$HP))
cars$HMPG = as.numeric(as.character(cars$HMPG))
cars$CMPG = as.numeric(as.character(cars$CMPG))
cars$Weight = as.numeric(as.character(cars$Weight))
cars$Length = as.numeric(as.character(cars$Length))
cars$Width = as.numeric(as.character(cars$Width))
cars = data.frame(cars$InvoicePrice, cars$SuggestedRetailPrice, cars$EngineSize, cars$NumCylinders, cars$HP, cars$HMPG, cars$CMPG, cars$Weight, cars$Length, cars$Width)
cars = cars[complete.cases(cars),]

source("starcoord.R")
source("mmnorm.R") 
source("circledraw.R")
source("radviz2d.R")

starcoord(cars)
radviz2d(cars)

# Star Coordinate Plots

starcoord(iris,class = T)
starcoord(cbind(bulls[,-1], as.factor(bulls[,1])),class = T)
dev.copy2pdf(file = "Rplots/bulls-starcoord.pdf")

# Radial visualization Plots


radviz2d(iris)

dev.copy2pdf(file = "Rplots/iris-radviz.pdf")
