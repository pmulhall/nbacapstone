if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(plyr)

###### Data Loading Section #######

# Load Games
games <- fread(text = gsub("::", "\t", readLines("games.csv")),na.strings=c("", "NA"))
games <- na.omit(games) # remove rows with NA data
games <- as.data.frame(games) %>%
  mutate(PTS_total = PTS_home + PTS_away) %>% # add total points scored
  mutate(PTS_diff = PTS_home - PTS_away) %>% # Add points difference between teams
  mutate(HOME_TEAM_ID = as.factor(HOME_TEAM_ID),
         VISITOR_TEAM_ID = as.factor(VISITOR_TEAM_ID),
         HOME_TEAM_WINS = as.factor(HOME_TEAM_WINS),
         SEASON = as.factor(SEASON))

###### Validation Set Section #######
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = games$PTS_total, times = 1, p = 0.1, list = FALSE)
gamedata <- games[-test_index,]
validation <- games[test_index,]


###### Data Visualization   ###########
ggplot(games, aes(x=FG_PCT_home, y=FG_PCT_away, color=as.factor(HOME_TEAM_WINS))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

ggplot(games, aes(x=SEASON, y=PTS_home, color=as.factor(HOME_TEAM_WINS))) +
  geom_point()


ggplot(games, aes(x=FG_PCT_home, y=FG_PCT_away, color=as.factor(HOME_TEAM_WINS))) +
  geom_boxplot()

table(games$HOME_TEAM_ID, games$HOME_TEAM_WINS)

ggplot(games, aes(y=FG_PCT_home, x=HOME_TEAM_ID)) +
  geom_boxplot()

ggplot(games, aes(y=PTS_diff, x=HOME_TEAM_ID)) +
  geom_boxplot()

Ptsbyyear <- as_tibble(aggregate(x = games$PTS_home, by = list(games$SEASON), FUN = "mean")) %>%
  rename(Group.1 = Season, x = Points)

###### Model 1: Linear Regression #########

HomePointsModel = lm(PTS_home ~ FG_PCT_home + FT_PCT_home + FG3_PCT_home + AST_home + REB_home + FG_PCT_away + FT_PCT_away + FG3_PCT_away + HOME_TEAM_ID + VISITOR_TEAM_ID + SEASON + AST_away + REB_away, data=games)
summary(HomePointsModel)

PointDiffModel = lm(PTS_diff ~ FG_PCT_home + FT_PCT_home + FG3_PCT_home + AST_home + REB_home + FG_PCT_away + FT_PCT_away + FG3_PCT_away + HOME_TEAM_ID + VISITOR_TEAM_ID + SEASON + AST_away + REB_away, data=games)
summary(PointDiffModel)

glm_fit <- games %>% 
  mutate(y = as.numeric(HOME_TEAM_WINS == "WIN")) %>%
  glm(y ~ FG_PCT_home + FT_PCT_home + FG3_PCT_home + AST_home + REB_home + FG_PCT_away + FT_PCT_away + FG3_PCT_away + HOME_TEAM_ID + VISITOR_TEAM_ID + SEASON + AST_away + REB_away, data=., family = "binomial")
summary(glm_fit)
p_hat_logit <- predict(glm_fit, newdata = validation, type = "response")

###### Model 2: Advanced #########

low_factors <- seq(50, 70, 1) #test for lambda value
high_factors <- seq(140, 160, 1) #test for lambda value

rmses <- sapply(high_factors, function(l){
  
  adv_games<-games[!(games$PTS_home>l),]
  
  HomePointsModel = lm(PTS_home ~ FG_PCT_home + FT_PCT_home + FG3_PCT_home + AST_home + REB_home + FG_PCT_away + FT_PCT_away + FG3_PCT_away + HOME_TEAM_ID + VISITOR_TEAM_ID + SEASON + AST_away + REB_away, data=adv_games)
  
  advpredictedscores <- predict(HomePointsModel, newdata = validation, type = "response")
  test <- data.frame(Points = as.integer(advpredictedscores))
  
  #HomePointsModel_rmse <- RMSE(validation$PTS_home,test$Points)
  
  return(RMSE(test$Points, validation$PTS_home))
})
plot(high_factors, rmses)

## Test results based on regression algorithm
rmse_results<- add_row(rmse_results, method = "Remove Outliers", RMSE = min(rmses))
rmse_results

