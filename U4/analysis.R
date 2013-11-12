rm(list = ls())

cars = read.csv("04cars_data.csv")
cars$Vehicle.Name = as.character(cars$Vehicle.Name)
cars$Len = as.numeric(as.character(cars$Len))
cars$Dealer.Cost = as.numeric(as.character(cars$Dealer.Cost))
cars$City.MPG = as.numeric(as.character(cars$City.MPG))
cars$Hwy.MPG = as.numeric(as.character(cars$Hwy.MPG))

svg("length_vs_dealerCost.svg")
plot(cars$Len, cars$Dealer.Cost)
dev.off()

cars$City.GPHM = 1/cars$City.MPG*100
cars$Hwy.GPHM = 1/cars$Hwy.MPG*100

sports.cars = cars[cars$Sports.Car==1,]
svg("sports_cityGPHM_quantiles.svg")
plot((1:length(sports.cars$City.GPHM) - 1)/(length(sports.cars$City.GPHM) - 1), sort(sports.cars$City.GPHM), type="l", main = "Quantiles for the Sports Cars\nCity GPHM", xlab = "Sample Fraction", ylab = "Sample Quantile")
dev.off()
svg("sports_highwayGPHM_quantiles.svg")
plot((1:length(sports.cars$Hwy.GPHM) - 1)/(length(sports.cars$Hwy.GPHM) - 1), sort(sports.cars$Hwy.GPHM), type="l", main = "Quantiles for the Sports Cars\nCity GPHM", xlab = "Sample Fraction", ylab = "Sample Quantile")
dev.off()

library(lattice)
sedan.cars = cars[cars$Small.Sporty..Compact.Large.Sedan == 1,]
suv.cars = cars[cars$SUV == 1,]
wagon.cars = cars[cars$Wagon == 1,]
minivan.cars = cars[cars$Minivan == 1,]
pickup.cars = cars[cars$Pickup == 1,]

qqplot(sports.cars$City.GPHM, sedan.cars$City.GPHM)
qqplot(sports.cars$City.GPHM, suv.cars$City.GPHM)
qqplot(sports.cars$City.GPHM, wagon.cars$City.GPHM)
qqplot(sports.cars$City.GPHM, minivan.cars$City.GPHM)
qqplot(sports.cars$City.GPHM, pickup.cars$City.GPHM)

qqplot(sports.cars$Hwy.GPHM, sedan.cars$Hwy.GPHM)
qqplot(sports.cars$Hwy.GPHM, suv.cars$Hwy.GPHM)
qqplot(sports.cars$Hwy.GPHM, wagon.cars$Hwy.GPHM)
qqplot(sports.cars$Hwy.GPHM, minivan.cars$Hwy.GPHM)
qqplot(sports.cars$Hwy.GPHM, pickup.cars$Hwy.GPHM)

qqnorm(sports.cars$City.GPHM)
qqline(sports.cars$City.GPHM)
qqnorm(sports.cars$Hwy.GPHM)
qqline(sports.cars$Hwy.GPHM)