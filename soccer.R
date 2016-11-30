# from  https://www.r-bloggers.com/using-sqlite-in-r/
library(DBI)
library(RSQLite)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library(MASS)
library(nlme)
setwd("/Users/axlmart/Desktop/653/Folder")
# connect to sqlite file
soccer = dbConnect(SQLite(), dbname = "database.sqlite")

# get list of all tables
alltables = dbListTables(soccer)

# get tables as data.frame
countries = dbGetQuery( soccer, 'select * from Country' )
leagues = dbGetQuery( soccer, 'select * from League' )
matches = dbGetQuery( soccer, 'select * from Match' )
players = dbGetQuery( soccer, 'select * from Player' )
player_attr = dbGetQuery( soccer, 'select * from Player_Attributes' )
teams = dbGetQuery( soccer, 'select * from Team' )
team_attr = dbGetQuery( soccer, 'select * from Team_Attributes' )

#select only variables we care about
match_variables = c("id", "country_id", "league_id", "season", "stage", "date", "match_api_id", "home_team_api_id", 
                    "away_team_api_id",  "home_team_goal", "away_team_goal", "B365H", "B365D", "B365A")
match_subset <- subset(matches, select = match_variables)

#just take Spain matches for now
spain_matches <- match_subset[match_subset$country_id == 21518, ]

# Merge in long team name for better understanding
home <- subset(teams, select = c("team_api_id", "team_long_name"))
colnames(home) <- c( "home_team_api_id", " home_team_long_name")
away <- subset(teams, select = c("team_api_id", "team_long_name"))
colnames(away) <- c( "away_team_api_id", "away_team_long_name")
spain1 <- merge(spain_matches, home)
spain <- merge(spain1, away)
spain$date <- abbreviate(spain$date,minlength = 9)
spain = spain[order(as.Date(spain$date, format="%Y-%m-%d") ) ,] # Ordered the games by date


passChances_home = numeric(nrow(spain))
crossChances_home = numeric(nrow(spain))
shotChances_home = numeric(nrow(spain))
passChances_away = numeric(nrow(spain))
crossChances_away = numeric(nrow(spain))
shotChances_away = numeric(nrow(spain))
outcome = numeric(nrow(spain))

for ( i in 1:nrow(spain)) {
  
  #get outcome
  if(spain$home_team_goal[i] > spain$away_team_goal[i]) {outcome[i] = 0}
  if (spain$home_team_goal[i] == spain$away_team_goal[i]) {outcome[i] = 1}
  if(spain$home_team_goal[i] < spain$away_team_goal[i]) {outcome[i] =2}
  
  #get last chance creation from passes, crosses and shots value:
  #home
  passChances_temp_home = team_attr[team_attr$team_api_id == spain$home_team_api_id[i],]
  passChances_temp_home = passChances_temp_home[order(as.Date(passChances_temp_home$date, format="%Y-%m-%d") ) ,] # Ordered the stats by date
  passChances_home[i] = passChances_temp_home$chanceCreationPassing[nrow(passChances_temp_home)]
  crossChances_home[i] = passChances_temp_home$chanceCreationCrossing[nrow(passChances_temp_home)]
  shotChances_home[i] = passChances_temp_home$chanceCreationShooting[nrow(passChances_temp_home)]
  #away
  passChances_temp_away = team_attr[team_attr$team_api_id == spain$away_team_api_id[i],]
  passChances_temp_away = passChances_temp_away[order(as.Date(passChances_temp_away$date, format="%Y-%m-%d") ) ,] # Ordered the stats by date
  passChances_away[i] = passChances_temp_away$chanceCreationPassing[nrow(passChances_temp_away)]
  crossChances_away[i] = passChances_temp_away$chanceCreationCrossing[nrow(passChances_temp_home)]
  shotChances_away[i] = passChances_temp_away$chanceCreationShooting[nrow(passChances_temp_home)]
}
spain = as.data.frame(cbind(spain, outcome,passChances_home,passChances_away, crossChances_home, crossChances_away, shotChances_home, shotChances_away ))
View(spain)


### Trying to get wanted info on current form of teams + previous meeting between them.

#team_id <- unique(spain$home_team_api_id)

prevG = c()
cur_form_home = c()
cur_form_away = c()
for (i in 1:nrow(spain)) {
  
  cur_form_home[i] = 0
  cur_form_away[i] = 0 
  home_team = spain$home_team_api_id[i]
  away_team = spain$away_team_api_id[i]
  
  temp = spain[1:i,]
  
  # First current form for home team :
  temp_home = as.data.frame( rbind( temp[temp$away_team_api_id == home_team,], temp[temp$home_team_api_id == home_team,] ))
  temp_home = temp_home[order(as.Date(temp_home$date, format="%Y-%m-%d") ) ,]
  if (nrow (temp_home) > 1) {
    if(nrow (temp_home) > 5) {
      for (k in (nrow(temp_home)-1) : (nrow(temp_home)-5) ) {
        
        if (temp_home$home_team_api_id[k] == home_team) {
          if (temp_home$outcome[k] == 0 ) (cur_form_home[i] = cur_form_home[i]+3)
          if (temp_home$outcome[k] == 1 ) (cur_form_home[i] = cur_form_home[i]+1)
        }
        if (temp_home$away_team_api_id[k] == home_team) {
          if (temp_home$outcome[k] == 2 ) (cur_form_home[i] = cur_form_home[i]+3)
          if (temp_home$outcome[k] == 1 ) (cur_form_home[i] = cur_form_home[i]+1)
        }
        #print(paste("match " ,i ,"   k = ", k , "    " ,  cur_form_home[i]) )
      }
      cur_form_home[i] = cur_form_home[i]/5
      #print(paste("Final ! match " ,i ,"   ",  cur_form_home[i] , '\n') )
    }
    
    else {
      for (k in nrow (temp_home)-1 : 1 ) {
        
        if (temp_home$home_team_api_id[k] == home_team) {
          if (temp_home$outcome[k] == 0 ) (cur_form_home[i] = cur_form_home[i]+3)
          if (temp_home$outcome[k] == 1 ) (cur_form_home[i] = cur_form_home[i]+1)
        }
        if (temp_home$away_team_api_id[k] == home_team) {
          if (temp_home$outcome[k] == 2 ) (cur_form_home[i] = cur_form_home[i]+3)
          if (temp_home$outcome[k] == 1 ) (cur_form_home[i] = cur_form_home[i]+1)
        }
        #print(paste("match " ,i ,"   ",  cur_form_home[i]) )
      }
      cur_form_home[i] = cur_form_home[i] /(nrow(temp_home)-1)
      #print(paste("Final ! match " ,i ,"   ",  cur_form_home[i] , '\n') )
    }
  }
  
  #Similarly for the away team :
  
  temp_away = as.data.frame( rbind( temp[temp$away_team_api_id == away_team,], temp[temp$home_team_api_id == away_team,] ))
  temp_away = temp_away[order(as.Date(temp_away$date, format="%Y-%m-%d") ) ,]
  if (nrow (temp_away) > 1) {
    if(nrow (temp_away) > 5) {
      for (k in (nrow(temp_away)-1) : (nrow(temp_away)-5) ) {
        
        if (temp_away$home_team_api_id[k] == away_team) {
          if (temp_away$outcome[k] == 0 ) (cur_form_away[i] = cur_form_away[i]+3)
          if (temp_away$outcome[k] == 1 ) (cur_form_away[i] = cur_form_away[i]+1)
        }
        if (temp_away$away_team_api_id[k] == away_team) {
          if (temp_away$outcome[k] == 2 ) (cur_form_away[i] = cur_form_away[i]+3)
          if (temp_away$outcome[k] == 1 ) (cur_form_away[i] = cur_form_away[i]+1)
        }
        #print(paste("match " ,i ,"   k = ", k , "    " ,  cur_form_away[i]) )
      }
      cur_form_away[i] = cur_form_away[i]/5
      #print(paste("Final ! match " ,i ,"   ",  cur_form_away[i] , '\n') )
    }
    
    else {
      for (k in nrow (temp_away)-1 : 1 ) {
        
        if (temp_away$home_team_api_id[k] == away_team) {
          if (temp_away$outcome[k] == 0 ) (cur_form_away[i] = cur_form_away[i]+3)
          if (temp_away$outcome[k] == 1 ) (cur_form_away[i] = cur_form_away[i]+1)
        }
        if (temp_away$away_team_api_id[k] == away_team) {
          if (temp_away$outcome[k] == 2 ) (cur_form_away[i] = cur_form_away[i]+3)
          if (temp_away$outcome[k] == 1 ) (cur_form_away[i] = cur_form_away[i]+1)
        }
        #print(paste("match " ,i ,"   ",  cur_form_away[i]) )
      }
      cur_form_away[i] = cur_form_away[i] /(nrow(temp_away)-1)
      #print(paste("Final ! match " ,i ,"   ",  cur_form_away[i] , '\n') )
    }
  }
  
  # It would be nice to retrieve the outcome of the previous encounter between the two teams
  prevG[i] = -1 
  found =0 
  j=i-1
  while (found == 0 && j > 1) {
    
    if( (spain$away_team_api_id[j] == away_team && spain$home_team_api_id[j] == home_team) || (spain$away_team_api_id[j] == home_team && spain$home_team_api_id[j] == away_team) ) {
      if(spain$outcome[j] ==1 || (spain$away_team_api_id[j] == away_team && spain$home_team_api_id[j] == home_team)){
      prevG[i] = spain$outcome[j]}
      if (spain$away_team_api_id[j] == home_team && spain$home_team_api_id[j] == away_team) {
        if (spain$outcome[j] == 0 ){
          prevG[i] = 2
        }
        if (spain$outcome[j] == 2 ){
          prevG[i] = 0
        }
      }
      found = 1
    }
    j=j-1
  }
  
}

spain_cur = as.data.frame(cbind(spain , cur_form_home,cur_form_away,prevG))
colnames(spain_cur)= c(colnames(spain), "cur_form_home", "cur_form_away","prevG")
spain = spain_cur
# Set for current forms of teams and prev game









### Desperate attempts to make models


# Trying to incorporate random effects

# Separate pb in two parts as described in the pdf : Home win / draw and Home win / Away win
# Fit both models using GLMMPQL and then solve for prob using the equations of multinomial logistic regression

#Make table with only wins and draws

#spain_reduced <- spain[spain$prevG != -1,]
spain_reduced <- spain 
draws <- spain_reduced[spain_reduced$outcome != 2 ,]
away <- spain_reduced[spain_reduced$outcome != 1 , ]


# Fit using only current form

draw_fit <- glmmPQL(outcome ~ cur_form_home + cur_form_away, random = ~ 1 | match_api_id, family = binomial, data= draws)
summary(draw_fit)
away_fit <- glmmPQL(as.factor(outcome) ~ cur_form_home + cur_form_away, random = ~ 1 | match_api_id, family = binomial, data = away)
summary(away_fit)


# Solving for probs
draw_coef = as.numeric(summary(draw_fit)$tTable[,1])
away_coef = as.numeric(summary(away_fit)$tTable[,1])

PY0 = c()
PY1 = c()
PY2 = c()

for ( i in 1:nrow(spain_reduced)){
denom = 1 + exp(draw_coef[1] + draw_coef[2]*spain_reduced$cur_form_home[i] + draw_coef[3]*spain_reduced$cur_form_away[i] ) + exp(away_coef[1] + away_coef[2]*spain_reduced$cur_form_home[i] + away_coef[3]*spain_reduced$cur_form_away[i])
PY0[i] = 1 /denom
PY1[i] = exp(draw_coef[1] + draw_coef[2]*spain_reduced$cur_form_home[i] + draw_coef[3]*spain_reduced$cur_form_away[i] )/denom
PY2[i] = exp(away_coef[1] + away_coef[2]*spain_reduced$cur_form_home[i] + away_coef[3]*spain_reduced$cur_form_away[i] )/denom
}

spain_cur_red = as.data.frame(cbind(spain_reduced , PY0, PY1 , PY2))
colnames(spain_cur_red)= c(colnames(spain_reduced), "Home_win_P", "Draw_P","Away_win_P")
spain_reduced = spain_cur_red

### Check how many outcomes we could have predicted right if we bet on the most probable outcome found

bets = c()
for ( i in 1:length(PY0)) {
  P = c(PY0[i], PY1[i], PY2[i])
  best = which.is.max(P)
  if (best == 1 ) {bets [i] = 0 }
  if (best == 2 ) {bets [i] = 1 }
  if (best == 3 ) {bets [i] = 2 }
}

counter = 0 

for ( i in 1:length(PY0)){
  if (bets[i] == spain_reduced$outcome[i]) {counter = counter +1}
}
counter/length(PY0) # 51% 


# Let's do the same thing without the random effects

# Fit using only current form

draw_fit <- glm(outcome ~ cur_form_home + cur_form_away, family = binomial, data= draws)
summary(draw_fit)
away_fit <- glm(as.factor(outcome) ~ cur_form_home + cur_form_away, family = binomial, data = away)
summary(away_fit)


# Solving for probs
draw_coef = as.numeric(summary(draw_fit)$coefficients[,1])
away_coef = as.numeric(summary(away_fit)$coefficients[,1])

PY0 = c()
PY1 = c()
PY2 = c()

for ( i in 1:nrow(spain_reduced)){
  denom = 1 + exp(draw_coef[1] + draw_coef[2]*spain_reduced$cur_form_home[i] + draw_coef[3]*spain_reduced$cur_form_away[i] ) + exp(away_coef[1] + away_coef[2]*spain_reduced$cur_form_home[i] + away_coef[3]*spain_reduced$cur_form_away[i])
  PY0[i] = 1 /denom
  PY1[i] = exp(draw_coef[1] + draw_coef[2]*spain_reduced$cur_form_home[i] + draw_coef[3]*spain_reduced$cur_form_away[i] )/denom
  PY2[i] = exp(away_coef[1] + away_coef[2]*spain_reduced$cur_form_home[i] + away_coef[3]*spain_reduced$cur_form_away[i] )/denom
}

spain_cur_red = as.data.frame(cbind(spain_reduced , PY0, PY1 , PY2))
colnames(spain_cur_red)= c(colnames(spain_reduced), "Home_win_P", "Draw_P","Away_win_P")
spain_reduced = spain_cur_red

### Check how many outcomes we could have predicted right if we bet on the most probable outcome found

bets = c()
for ( i in 1:length(PY0)) {
  P = c(PY0[i], PY1[i], PY2[i])
  best = which.is.max(P)
  if (best == 1 ) {bets [i] = 0 }
  if (best == 2 ) {bets [i] = 1 }
  if (best == 3 ) {bets [i] = 2 }
}

counter = 0 

for ( i in 1:length(PY0)){
  if (bets[i] == spain_reduced$outcome[i]) {counter = counter +1}
}
counter/length(PY0) # 51% 



########

# let's try to build a more complete model (using only significant coeffs)
spain_reduced <- spain[spain$prevG != -1,]
spain_reduced <- spain_reduced[complete.cases(spain_reduced),]
draws <- spain_reduced[spain_reduced$outcome != 2 ,]
away <- spain_reduced[spain_reduced$outcome != 1 , ]


draw_fit <- glmmPQL(outcome ~ cur_form_home + cur_form_away + shotChances_home + shotChances_away, random = ~ 1 | match_api_id, family = binomial, data= draws)
summary(draw_fit)
away_fit <- glmmPQL(as.factor(outcome) ~ cur_form_home + cur_form_away + shotChances_home + shotChances_away , random = ~ 1 | match_api_id, family = binomial, data = away)
summary(away_fit)


# Solving for probs
draw_coef = as.numeric(summary(draw_fit)$tTable[,1])
away_coef = as.numeric(summary(away_fit)$tTable[,1])

PY0 = c()
PY1 = c()
PY2 = c()

for ( i in 1:nrow(spain_reduced)){
  denom = 1 + exp(draw_coef[1] + draw_coef[2]*spain_reduced$cur_form_home[i] + draw_coef[3]*spain_reduced$cur_form_away[i] +draw_coef[4]*spain_reduced$shotChances_home[i] + draw_coef[5]*spain_reduced$shotChances_away[i]  ) + exp(away_coef[1] + away_coef[2]*spain_reduced$cur_form_home[i] + away_coef[3]*spain_reduced$cur_form_away[i] + away_coef[4]*spain_reduced$shotChances_home[i] + away_coef[5]*spain_reduced$shotChances_away[i])
  PY0[i] = 1 /denom
  PY1[i] = exp(draw_coef[1] + draw_coef[2]*spain_reduced$cur_form_home[i] + draw_coef[3]*spain_reduced$cur_form_away[i] + draw_coef[4]*spain_reduced$shotChances_home[i] + draw_coef[5]*spain_reduced$shotChances_away[i] )/denom
  PY2[i] = exp(away_coef[1] + away_coef[2]*spain_reduced$cur_form_home[i] + away_coef[3]*spain_reduced$cur_form_away[i] + away_coef[4]*spain_reduced$shotChances_home[i] + away_coef[5]*spain_reduced$shotChances_away[i] )/denom
}

spain_cur_red = as.data.frame(cbind(spain_reduced , PY0, PY1 , PY2))
colnames(spain_cur_red)= c(colnames(spain_reduced), "Home_win_P", "Draw_P","Away_win_P")
spain_reduced = spain_cur_red

### Check how many outcomes we could have predicted right if we bet on the most probable outcome found

bets = c()
for ( i in 1:length(PY0)) {
  P = c(PY0[i], PY1[i], PY2[i])
  best = which.is.max(P)
  if (best == 1 ) {bets [i] = 0 }
  if (best == 2 ) {bets [i] = 1 }
  if (best == 3 ) {bets [i] = 2 }
}

counter = 0 

for ( i in 1:length(PY0)){
  if (bets[i] == spain_reduced$outcome[i]) {counter = counter +1}
}
counter/length(PY0) # 51% 



#############

# let's try to build a more complete model (using only significant coeffs)
spain_reduced <- spain[spain$prevG != -1,]
spain_reduced <- spain_reduced[complete.cases(spain_reduced),]
draws <- spain_reduced[spain_reduced$outcome != 2 ,]
away <- spain_reduced[spain_reduced$outcome != 1 , ]


draw_fit <- glmmPQL(outcome ~ cur_form_home + cur_form_away + shotChances_home , random = ~ 1 | match_api_id, family = binomial, data= draws)
summary(draw_fit)
away_fit <- glmmPQL(as.factor(outcome) ~ cur_form_home + cur_form_away + shotChances_away, random = ~ 1 | match_api_id, family = binomial, data = away)
summary(away_fit)


# Solving for probs
draw_coef = as.numeric(summary(draw_fit)$tTable[,1])
away_coef = as.numeric(summary(away_fit)$tTable[,1])

PY0 = c()
PY1 = c()
PY2 = c()

for ( i in 1:nrow(spain_reduced)){
  denom = 1 + exp(draw_coef[1] + draw_coef[2]*spain_reduced$cur_form_home[i] + draw_coef[3]*spain_reduced$cur_form_away[i] +draw_coef[4]*spain_reduced$shotChances_home[i] ) + exp(away_coef[1] + away_coef[2]*spain_reduced$cur_form_home[i] + away_coef[3]*spain_reduced$cur_form_away[i] +  away_coef[4]*spain_reduced$shotChances_away[i])
  PY0[i] = 1 /denom
  PY1[i] = exp(draw_coef[1] + draw_coef[2]*spain_reduced$cur_form_home[i] + draw_coef[3]*spain_reduced$cur_form_away[i] + draw_coef[4]*spain_reduced$shotChances_home[i]  )/denom
  PY2[i] = exp(away_coef[1] + away_coef[2]*spain_reduced$cur_form_home[i] + away_coef[3]*spain_reduced$cur_form_away[i] + away_coef[4]*spain_reduced$shotChances_away[i] )/denom
}

spain_cur_red = as.data.frame(cbind(spain_reduced , PY0, PY1 , PY2))
colnames(spain_cur_red)= c(colnames(spain_reduced), "Home_win_P", "Draw_P","Away_win_P")
spain_reduced = spain_cur_red

### Check how many outcomes we could have predicted right if we bet on the most probable outcome found

bets = c()
for ( i in 1:length(PY0)) {
  P = c(PY0[i], PY1[i], PY2[i])
  best = which.is.max(P)
  if (best == 1 ) {bets [i] = 0 }
  if (best == 2 ) {bets [i] = 1 }
  if (best == 3 ) {bets [i] = 2 }
}

counter = 0 

for ( i in 1:length(PY0)){
  if (bets[i] == spain_reduced$outcome[i]) {counter = counter +1}
}
counter/length(PY0) # 51% 











####

draw_fit <- glmmPQL(outcome ~ cur_form_home + cur_form_away + as.factor(prevG) + as.factor(prevG)*cur_form_home + as.factor(prevG)*cur_form_away, random = ~ 1 | match_api_id, family = binomial, data= draws)
summary(draw_fit)
away_fit <- glmmPQL(as.factor(outcome) ~ cur_form_home + cur_form_away, random = ~ 1 | match_api_id, family = binomial, data = away)
summary(away_fit)

##




# Dummy
Real_Vil = as.data.frame(matrix(ncol=ncol(spain)))

k = 1
for ( i in 1:nrow(spain)) {
  
  if( (spain$away_team_api_id[i] == 8633 && spain$home_team_api_id[i] == 10205) || (spain$away_team_api_id[i] == 10205 && spain$home_team_api_id[i] == 8633) ) {
    Real_Vil[k,] = spain[i,]
    k=k+1}
}
colnames(Real_Vil)= colnames(spain)
View(Real_Vil)

Real_Vil$outcome <- relevel(as.factor(Real_Vil$outcome), ref = "0")
Real_test <- multinom(as.factor(Real_Vil$outcome) ~ Real_Vil$B365D)
summary(Real_test)
















####################

binary_spain = spain[spain$outcome != 2,]
View(binary_spain)

mylogit <- glm(formula = binary_spain$outcome ~ binary_spain$B365H , family = "binomial")
mylogit <- glm(formula = spain$outcome ~ spain$B365H , family = "quasibinomial")

spain$outcome <- relevel(as.factor(spain$outcome), ref = "0")
test <- multinom(spain$outcome ~ binary_spain$B365H)

# Dummy test

outcome = c(0,1,0,1,2)
PrevGame = c(1,0,1,0,1)
betsHome = c(1.9,1.7,2,1.8,2.3)
CurHome = c(4,3,5,2,1)
CurAway = c(1,3,2,3,4)

table1 = as.data.frame(cbind(outcome,PrevGame,betsHome,CurHome, CurAway))

table1$outcome <- relevel(as.factor(table1$outcome), ref = "0")
test1 <- multinom(as.factor(table1$outcome) ~ table1$PrevGame + table1$betsHome + table1$CurHome + table1$CurAway)
summary(test)
