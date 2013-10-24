rm(list = ls())

cars = read.csv("04cars.csv")
cars$VehicleName = as.character(cars$VehicleName)
cars$CMPG = as.numeric(cars$CMPG)
cars$HMPG = as.numeric(cars$HMPG)

VehicleBrand = strsplit(cars$VehicleName, " ")
VehicleBrand = unlist(lapply(VehicleBrand, FUN=function(x) {return(x[1])}))
VehicleBrand = as.character(VehicleBrand)
VehicleBrand[VehicleBrand=="Mazda3"] = "Mazda"
VehicleBrand[VehicleBrand=="Mazda6"] = "Mazda"
VehicleBrand[VehicleBrand=="Land"] = "Land Rover"
cars = data.frame(VehicleBrand, cars)

brandTable = table(cars$VehicleBrand)
rareBrands = names(which(brandTable==1))
cars = cars[!(cars$VehicleBrand %in% rareBrands),]

avg_CMPG = aggregate(cars$CMPG, by=list(cars$VehicleBrand), FUN=mean)
var_CMPG = aggregate(cars$CMPG, by=list(cars$VehicleBrand), FUN=var)
var_CMPG$x = sqrt(var_CMPG$x)
avg_HMPG = aggregate(cars$HMPG, by=list(cars$VehicleBrand), FUN=mean)
var_HMPG = aggregate(cars$HMPG, by=list(cars$VehicleBrand), FUN=var)
var_HMPG$x = sqrt(var_HMPG$x)
mpg_stat = data.frame(avg_CMPG,var_CMPG$x, avg_HMPG$x, var_HMPG$x)
names(mpg_stat) = c("VehicleBrand", "CMPG_Mean", "CMPG_Stdv", "HMPG_Mean", "HMPG_Stdv")
write.csv2(mpg_stat, "mpg_stat.csv", row.names=FALSE)