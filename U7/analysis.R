rm(list = ls())

library(MASS)
library(lattice)
library(ggplot2)
source("ggcorplot.R")

cereals = read.table("cereal.csv", colClasses=c("character", "character", "factor", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "factor", "numeric", "numeric", "numeric", "numeric"))
names(cereals) = c("name", "manufacturer", "type", "calories", "protein", "fat", "sodium", "fiber", "carbo", "sugar", "shelf", "potassium", "vitamins", "weight", "cups")

cerealsVis = c(4, 5, 6, 7, 8, 9, 10, 12, 14)

write.table(cor(cereals[,cerealsVis]), "cor.txt")

svg("parcoord.svg", width=10, height=5)
parcoord(cereals[,cerealsVis])
dev.off()

svg("scatter.svg", width=10, height=10)
splom(~cereals[,cerealsVis], pty=19)
dev.off()