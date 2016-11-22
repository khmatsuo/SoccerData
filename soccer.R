# from  https://www.r-bloggers.com/using-sqlite-in-r/
library(DBI)
library(RSQLite)


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

# Merge in long team name for better understanding, reorder variables
home <- subset(teams, select = c("team_api_id", "team_long_name"))
colnames(home) <- c( "home_team_api_id", " home_team_long_name")
away <- subset(teams, select = c("team_api_id", "team_long_name"))
colnames(away) <- c( "away_team_api_id", "away_team_long_name")
spain1 <- merge(spain_matches, home)
spain <- merge(spain1, away)
spain <- spain[c(2,15,1,16,3:14)]
spain <- spain[order(spain$home_team_api_id, spain$away_team_api_id),]
