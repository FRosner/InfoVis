rm(list = ls())

error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

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
cars_norare = cars[!(cars$VehicleBrand %in% rareBrands),]

avg_CMPG = aggregate(cars_norare$CMPG, by=list(cars_norare$VehicleBrand), FUN=mean)
var_CMPG = aggregate(cars_norare$CMPG, by=list(cars_norare$VehicleBrand), FUN=var)
var_CMPG$x = sqrt(var_CMPG$x)
avg_HMPG = aggregate(cars_norare$HMPG, by=list(cars_norare$VehicleBrand), FUN=mean)
var_HMPG = aggregate(cars_norare$HMPG, by=list(cars_norare$VehicleBrand), FUN=var)
var_HMPG$x = sqrt(var_HMPG$x)
mpg_stat_norare = data.frame(avg_CMPG,var_CMPG$x, avg_HMPG$x, var_HMPG$x)
names(mpg_stat_norare) = c("VehicleBrand", "CMPG_Mean", "CMPG_Stdv", "HMPG_Mean", "HMPG_Stdv")

mpg_stat_norare = mpg_stat_norare[order(mpg_stat_norare$CMPG_Mean, decreasing=TRUE),]
write.csv2(mpg_stat_norare, "mpg_stat_norare.csv", row.names=FALSE)

brandTable = table(cars$VehicleBrand)
frequentBrands = names(sort(brandTable, decreasing=TRUE)[1:10])
cars_top10 = cars[(cars$VehicleBrand %in% frequentBrands),]

avg_CMPG = aggregate(cars_top10$CMPG, by=list(cars_top10$VehicleBrand), FUN=mean)
var_CMPG = aggregate(cars_top10$CMPG, by=list(cars_top10$VehicleBrand), FUN=var)
var_CMPG$x = sqrt(var_CMPG$x)
avg_HMPG = aggregate(cars_top10$HMPG, by=list(cars_top10$VehicleBrand), FUN=mean)
var_HMPG = aggregate(cars_top10$HMPG, by=list(cars_top10$VehicleBrand), FUN=var)
var_HMPG$x = sqrt(var_HMPG$x)
mpg_stat_top10 = data.frame(avg_CMPG,var_CMPG$x, avg_HMPG$x, var_HMPG$x)
names(mpg_stat_top10) = c("VehicleBrand", "CMPG_Mean", "CMPG_Stdv", "HMPG_Mean", "HMPG_Stdv")

mpg_stat_top10 = mpg_stat_top10[order(mpg_stat_top10$CMPG_Mean, decreasing=TRUE),]
pdf("fuel-consumption.pdf", width=7, height=5)
mpg_top10_plot = barplot(mpg_stat_top10$CMPG_Mean, names.arg=mpg_stat_top10$VehicleBrand, las=3, ylim=c(0,25), xlab="Vehicle Brand", ylab="City MPG", main="Fuel Consumption of different Vehicle Brands")
error.bar(mpg_top10_plot, mpg_stat_top10$CMPG_Mean, mpg_stat_top10$CMPG_Stdv)
dev.off()