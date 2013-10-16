rm(list = ls())

rawCarNames = read.table("04cars_cars.txt", sep="\t")
rawCarData = read.table("04cars_data.txt")
cars = data.frame(rawCarNames, rawCarData)
names(cars) = c("VehicleName", "SportsCar", "SUV", "Wagon", "Minivan", "Pickup", "AWD", "RWD", "SuggestedRetailPrice", "InvoicePrice", "EngineSize", "NumCylinders", "HP", "CMPG", "HMPG", "Weight", "WheelBase", "Length", "Width")
write.csv(cars, file="04cars.csv", row.names=FALSE, quote=FALSE)