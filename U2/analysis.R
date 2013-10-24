rm(list = ls())

cars = read.csv("04cars.csv")
cars$VehicleName = as.character(cars$VehicleName)
VehicleBrand = strsplit(cars$VehicleName, " ")
VehicleBrand = unlist(lapply(VehicleBrand, FUN=function(x) {return(x[1])}))
cars = data.frame(VehicleBrand, cars)
cars$VehicleBrand[cars$VehicleBrand=="Mazda3"] = "Mazda"
cars$VehicleBrand[cars$VehicleBrand=="Mazda6"] = "Mazda"