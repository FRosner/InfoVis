rm(list = ls())

library(lattice)

dji = read.csv("djia-100.txt", colClasses=c("character", "numeric"))
dji = dji[which(dji$YYMMDD == "890103"):which(dji$YYMMDD == "941230"),]
dji$YYMMDD = as.Date(dji$YYMMD, "%y%m%d")

pdf("cm_dji.pdf", width=10, height=5)
plot.new()
dji.relative = sapply(dji$closing.value, FUN=function(x) {return(x/dji$closing.value[1])})*100
dji.relative.range = max(dji.relative) - min(dji.relative)
dji.relative.min = min(dji.relative)
colors = cm.colors(ceiling(dji.relative.range), alpha = 1)
border = par("fg")
#border = NA
currentDay = 1;
for (year in 1:4) {
  year.xleft = (year - 1)/4
  year.xright = (year)/4
  year.ytop = 0
  year.ybottom = 1
  for (quart in 1:4) {
    quart.xleft = year.xleft + (quart - 1)/4*(year.xright-year.xleft)
    quart.xright = year.xleft + (quart)/4*(year.xright-year.xleft)
    quart.ytop = year.ytop
    quart.ybottom = year.ybottom
    rect(xleft=quart.xleft, xright=quart.xright, ytop=quart.ytop, ybottom=quart.ybottom, border=border)
    for (month in 1:3) {
      month.xleft = quart.xleft
      month.xright = quart.xright
      month.ytop = (month - 1)/3
      month.ybottom = (month)/3
      rect(xleft=month.xleft, xright=month.xright, ytop=month.ytop, ybottom=month.ybottom, border=border)
      for (week in 1:4) {
        week.xleft = month.xleft
        week.xright = month.xright
        week.ytop = month.ytop + (week - 1)/4*(month.ybottom-month.ytop)
        week.ybottom = month.ytop + (week)/4*(month.ybottom-month.ytop)
        for (day in 1:7) {
          day.xleft = week.xleft + (day - 1)/7*(week.xright-week.xleft)
          day.xright = week.xleft + (day)/7*(week.xright-week.xleft)
          day.ytop = week.ytop
          day.ybottom = week.ybottom
          rect(xleft=day.xleft, xright=day.xright, ytop=day.ytop, ybottom=day.ybottom, col=colors[ceiling(dji.relative[currentDay]-dji.relative.min)], border=NA)
          currentDay = currentDay + 1
        }
        rect(xleft=week.xleft, xright=week.xright, ytop=week.ytop, ybottom=week.ybottom, border=border)
      }
    }
  }
}
dev.off()