rm(list = ls())

library(e1071)
library(ggplot2)

cars = read.csv("04cars.csv" )
cars$Length = as.numeric(as.character(cars$Length))
cars$VehicleName = as.character(cars$VehicleName)
cars$CMPG = as.numeric(as.character(cars$CMPG))

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

svg("EcoMinivans_plot.svg", width=9, height=5)
p = ggplot(cars, aes(x = InvoicePrice, y=Length, colour=Type)) + geom_point() + theme_bw()
p = p + scale_x_continuous(breaks=seq(0,200000,10000)) + scale_y_continuous(breaks=seq(0,250,10))
p + scale_colour_manual(name="Minivan Type",labels=c("Eco", "Normal", "No Minivan"), values=c("limegreen", "lightpink2", "grey80"))
dev.off()
