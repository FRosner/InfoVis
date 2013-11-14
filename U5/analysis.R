rm(list = ls())

library(lattice)

dji = read.csv("djia-100.txt", colClasses=c("character", "numeric"))
dji = dji[which(dji$YYMMDD == "900102"):which(dji$YYMMDD == "941230"),]
dji$YYMMDD = as.Date(dji$YYMMD, "%y%m%d")

for (aspect in c(banking(1:nrow(dji), dji$closing.value), 0.5, 1, 2)) {
  svg(paste("dow_", aspect, ".svg", sep=""))
  print(xyplot(dji$closing.value ~ dji$YYMMDD, aspect=aspect, type="l", main="Dow Jones Closing Value\n 1990 - 1995", ylab="Closing Value ($)", xlab="Year"))
  dev.off()
}
