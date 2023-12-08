nbaplayerdata<-read.csv("C:\\NCSU\\Statistics\\ST558\\Final Project\\all_seasons.csv")
str(nbaplayerdata)

library(ggplot2)
library(dplyr)

ggplot (nbaplayerdata, aes (x=player_name, y=pts))+
  geom_bar(stat = "identity")
ggplot (nbaplayerdata, aes (x=age, y=pts))+
  geom_bar(stat = "identity")
ggplot (nbaplayerdata, aes (x=age, y=reb))+
  geom_bar(stat = "identity")
ggplot(nbaplayerdata,aes(x=reb,y=pts))+
  geom_point()+
  geom_smooth(method = lm)+
  facet_wrap(~season)
