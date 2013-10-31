rm(list = ls())

library(e1071)
library(ggplot2)

cars = read.csv("04cars.csv")
cars$Length = as.numeric(cars$Length)
cars$VehicleName = as.character(cars$VehicleName)

cars = cars[cars$Pickup == 0,] # Pickups are missing length attribute
Types = rep("Not Minivan",nrow(cars))
Types[cars$SUV == 1] = "Not Minivan"
Types[cars$SportsCar == 1] = "Not Minivan"
Types[cars$Wagon == 1] = "Not Minivan"
Types[cars$Minivan == 1] = "Minivan"
cars$Type = Types

qplot(cars$InvoicePrice, cars$Length, colour=cars$Type)