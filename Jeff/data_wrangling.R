library(pacman)
p_load(skimr,tidyverse, corrplot, MASS)

kobe = read.csv("./modelingKobeData.csv", header=T, sep=",", strip.white=T, stringsAsFactors = F, na.strings=c(""))

