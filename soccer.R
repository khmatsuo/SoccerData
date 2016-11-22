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

match_variables = c("id", "country_id", "league_id", "season", "stage", "date", "match_api_id", "home_team_api_id", 
                    "away_team_api_id",  "home_team_goal", "away_team_goal", "B365H", "B365D", "B365A")

match_subset <- subset(matches, select = match_variables)

spain <- subset(matches, country_id = 21518)