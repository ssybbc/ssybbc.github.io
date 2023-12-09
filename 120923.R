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
ggplot (nbaplayerdata, aes(x=ast, y=pts))+
  geom_point()+
  geom_smooth(method = lm)+
  facet_wrap(~season)
ggplot (nbaplayerdata, aes(x=player_height, y=player_weight))+
  geom_point()+
  geom_smooth(method = lm)+
  facet_wrap(~team_abbreviation)
summary(nbaplayerdata)

# Let's start with exploratory data analysis for just one season, then we will come back at all seasons to do prediction work.

nbaplayerdata2022<-nbaplayerdata%>%filter (season == "2022-23")
str(nbaplayerdata2022)
nbaplayerdata2022%>%count(country)
nbaplayerdata2022%>%count(college,sort=TRUE)
nbaplayerdata2022%>%count(age,sort=TRUE)

# Think of a scatter plot, where y axis is the number of points (pts) per game a player scored during a particular season, for the y axis, the positive direction is the number of rebound (reb) per game, and the negative direction is the number of assists (ast) per game. It is possible to plot 3 variables on a 2D plot just because there is no negative value. For a data point, we could also use the size of the point as the number of games played (gp) and the color as the team (team_abbreviation). It would be a lot of fun to see the outliers in a plot (stars) and how all players in a single team aggregate and compare with other teams. If avaiable, we could also set the shape of the points to the age group of the players.

nbaplayerdata2022$age_group <- as.factor (ifelse(nbaplayerdata2022$age<24, "19-23",
                                                 ifelse (nbaplayerdata2022$age<29, "24-28",
                                                         ifelse (nbaplayerdata2022$age<34, "29-33", "34 and up")
                                                        )
                                                 )
                                          )
str(nbaplayerdata2022$age_group)


ggplot(nbaplayerdata2022, aes(y = pts, shape = age_group, color = team_abbreviation, size = gp, alpha = 0.9))+
  geom_point (aes(x =reb))+
  geom_point (aes (x = -ast))

ggplot(nbaplayerdata2022, aes(y = pts, shape = age_group, color = team_abbreviation, size = gp , position = "gitter", alpha = 0.75))+
  geom_point (aes(x =reb))+
  geom_point (aes (x = -ast))+
  scale_color_brewer(palette = "Dark2")

# There are too many teams for the color to display. Create another variable "team_division" to make things simpler and divide the scatter plots into six different ones.

nbaplayerdata2022$team_division <- as.factor (ifelse (nbaplayerdata2022$team_abbreviation %in% c ("BOS", "NYK", "PHI", "TOR"), "Atlantic",
                                                      ifelse (nbaplayerdata2022$team_abbreviation %in% c ("CHI", "CLE", "DET", "IND", "MIL"), "Central",
                                                              ifelse (nbaplayerdata2022$team_abbreviation %in% c ("ATL", "CHA", "MIA", "ORL", "WAS"), "Southeast",
                                                                      ifelse (nbaplayerdata2022$team_abbreviation %in% c ("DEN", "MIN", "OKC", "POR", "UTA"), "Northwest",
                                                                              ifelse (nbaplayerdata2022$team_abbreviation %in% c ("GSW", "LAC", "LAL", "PHX", "SAS"), "Pacific", "Southwest"
                                                                             ))))))
str(nbaplayerdata2022$team_division)

nbaplayer2022atl<-filter (nbaplayerdata2022, team_division == "Atlantic")
nbaplayer2022ctr<-filter (nbaplayerdata2022, team_division == "Central")
nbaplayer2022ste<-filter (nbaplayerdata2022, team_division == "Southeast")
nbaplayer2022ntw<-filter (nbaplayerdata2022, team_division == "Northwest")
nbaplayer2022pac<-filter (nbaplayerdata2022, team_division == "Pacific")
nbaplayer2022stw<-filter (nbaplayerdata2022, team_division == "Southwest")

library (gridExtra)

atlplot <- ggplot(nbaplayer2022atl, aes(y = pts, color = age_group, size = gp , position = "gitter", alpha = 0.75))+
  geom_point (aes(x =reb))+
  geom_point (aes (x = -ast))

ctrplot <- ggplot(nbaplayer2022ctr, aes(y = pts, color = age_group, size = gp , position = "gitter", alpha = 0.75))+
  geom_point (aes(x =reb))+
  geom_point (aes (x = -ast))

steplot <- ggplot(nbaplayer2022ste, aes(y = pts, color = age_group, size = gp , position = "gitter", alpha = 0.75))+
  geom_point (aes(x =reb))+
  geom_point (aes (x = -ast))

ntwplot <- ggplot(nbaplayer2022ntw, aes(y = pts, color = age_group, size = gp , position = "gitter", alpha = 0.75))+
  geom_point (aes(x =reb))+
  geom_point (aes (x = -ast))

pacplot <- ggplot(nbaplayer2022pac, aes(y = pts, color = age_group, size = gp , position = "gitter", alpha = 0.75))+
  geom_point (aes(x =reb))+
  geom_point (aes (x = -ast))

stwplot <- ggplot(nbaplayer2022stw, aes(y = pts, color = age_group, size = gp , position = "gitter", alpha = 0.75))+
  geom_point (aes(x =reb))+
  geom_point (aes (x = -ast))

grid.arrange(atlplot, ctrplot, steplot, ntwplot, pacplot, stwplot,ncol=2)

# Another way to do it is through faceting 

ggplot(nbaplayerdata2022, aes(y = pts, color = age_group, size = gp , position = "gitter", alpha = 0.75))+
  geom_point (aes(x =reb))+
  geom_point (aes (x = -ast))+
  facet_wrap (~team_division)

ggplot(nbaplayerdata2022, aes(y = pts, color = age_group, size = gp , alpha = 0.75))+
  geom_point (aes(x =reb))+
  geom_point (aes (x = -ast))+
  geom_text (aes (label = player_name), check_overlap = TRUE)+
  facet_wrap (~team_abbreviation)

ggplot(nbaplayerdata2022, aes(y = pts, color = age_group, size = gp , alpha = 0.75))+
  geom_point (aes (x= reb))+
  geom_point (aes (x= -ast))+
  geom_text (aes (x= reb, y =pts, label = player_name), check_overlap = TRUE, size = 3, color = "black")+
  facet_wrap (~team_abbreviation)


# There are some other data yet to explore, like net_rating, oreb_pct, dreb_pct, usg_pct, ts_pct, etc. To see the correlation between this interesting stats, decided to do a correlation plot.

library (corrplot)
correlation <- cor (select(nbaplayerdata2022, player_height, player_weight, gp, pts, reb, ast, net_rating, oreb_pct, dreb_pct, usg_pct, ts_pct, ast_pct), method = "spearman")
corrplot (correlation, method = "color", order = "hclust", addrect =2, tl.cex = 0.6, cl.cex =0.5)

# There is an interesting negative correlation between player's height, weight and assistance numbers. I decided to go over that to check the condition in different divisions.
# Make a plot demonstrating the distribution of height and weight of players in a single year and later make a widget to show the changes over the years, same ideas as before.

ggplot (nbaplayerdata2022, aes (x=player_height))+
  geom_density (adjust =0.5, alpha = 0.5, aes (fill = team_abbreviation), position = "stack")+
  facet_wrap(~team_division)
  

ggplot (nbaplayerdata2022, aes (x=player_weight))+
  geom_density (adjust =0.5, alpha = 0.5, aes (fill = team_abbreviation), position = "stack")+
  facet_wrap(~team_division)

# In the app, we could let user to choose which variables they wanted to see in both scatter plot and the density plots.

