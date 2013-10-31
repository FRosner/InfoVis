rm(list = ls())

library(e1071)
library(ggplot2)

cars = read.csv("04cars.csv")
cars$Length = as.numeric(cars$Length)
cars$VehicleName = as.character(cars$VehicleName)
cars$CMPG = as.numeric(cars$CMPG)

cars = cars[cars$Pickup == 0,] # Pickups are missing length attribute
Types = rep("Not Minivan",nrow(cars))
Types[cars$SUV == 1] = "Not Minivan"
Types[cars$SportsCar == 1] = "Not Minivan"
Types[cars$Wagon == 1] = "Not Minivan"
Types[cars$Minivan == 1] = "Minivan"
cars$Type = Types

MinivanAvg = aggregate(cars$CMPG, by=list(cars$Type), FUN=mean)$x[1]
Ecos = rep("Not Minivan", nrow(cars))
Ecos[(cars$Minivan == 1) & (cars$CMPG < MinivanAvg)] = "Not Eco"
Ecos[cars$Minivan == 1 & cars$CMPG >= MinivanAvg] = "Eco"
cars$Eco = Ecos
cars$Type[cars$Eco == "Eco"] = "Eco Minivan"

svg("EcoMinivans.svg", width=7, height=5)
p = ggplot(cars, aes(x = InvoicePrice, y=Length, colour=Type)) + geom_point() + theme_bw()
p + scale_colour_manual(name="Car Type",labels=c("Eco Minivan", "Normal Minivan", "No Minivan"), values=c("green", "blue", "grey"))
dev.off()
