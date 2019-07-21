library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
daten <- read_csv("beehive_small.csv")
View(daten)
ggplot(daten)+geom_histogram(aes(ticks))

# convert ticks to timestamp
tmp<-as.POSIXct(daten$ticks, origin="1970-1-1")

# verteilung anschauen
ggplot(daten)+geom_histogram(aes(weight))
ggplot(daten)+geom_bar(aes(temp1))

ggplot(daten)+geom_point(aes(x=timestamp,y=weight))


# zusammenhang rausfinden
cor(daten[,c("weight", "temp1", "temp2", "hum1", "ticks")])

ggplot(daten)+geom_point(aes(x=ticks,y=temp1, col=as.factor(weight)))
#as.factor(weight) # disketre variable