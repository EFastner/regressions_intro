library(RPostgreSQL)
library(scales)
library(readr)
library(plyr)
library(dplyr)

#YOU WILL NEED A db.conn VARIABLE SET TO nhl_pbp POSTGRES DATABASE

#Get existing summary of game results
df.game_summaries <- 
  dbGetQuery(db.conn, "SELECT * FROM game_results WHERE season = '20162017' OR season = '20172018';")

#Get PbP data to summarize more results
df.raw_pbp <- 
  dbGetQuery(db.conn, "SELECT * FROM corsica_pbp WHERE season = '20162017' OR season = '20172018';")

#Create vector containing all corsi events
corsi_events <- c("SHOT", "MISS", "BLOCK", "GOAL")

#Group PbP by season, game_id, home_team, and away_team. Create some summarized values to analyze
df.pbp_corsi <- 
  df.raw_pbp %>%
  group_by(season, game_id, home_team, away_team) %>%
  summarise(home_shots = sum(event_type == "SHOT" & event_team == home_team),
            away_shots = sum(event_type == "SHOT" & event_team == away_team),
            home_corsi = sum(event_type %in% corsi_events & event_team == home_team),
            away_corsi = sum(event_type %in% corsi_events & event_team == away_team),
            home_corsi_5v5 = sum(event_type %in% corsi_events & event_team == home_team & game_strength_state == "5v5"),
            away_corsi_5v5 = sum(event_type %in% corsi_events & event_team == away_team & game_strength_state == "5v5"),            
            home_goals = sum(event_type == "GOAL" & event_team == home_team & game_period <= 4),
            away_goals = sum(event_type == "GOAL" & event_team == away_team & game_period <= 4))

#Combine df.game_summaries and df.pbp_corsi            
df.combined_game_summaries <- 
  merge(x = df.game_summaries, 
        y = df.pbp_corsi, 
        by = c("season", "game_id", "home_team", "away_team")) %>%
  mutate(home_points_entering = as.integer(home_point_total) - as.integer(home_points),
         away_points_entering = as.integer(away_point_total) - as.integer(away_points),
         home_points_possible = (as.integer(home_game_num) - 1) * 2,
         away_points_possible = (as.integer(away_game_num) - 1) * 2,
         home_points_percent = ifelse(home_points_possible > 0, percent(home_points_entering/home_points_possible), NA),
         away_points_percent = ifelse(away_points_possible > 0, percent(away_points_entering/away_points_possible), NA)) %>%
  select(season,
         game_id,
         game_date,
         session,
         game_period,
         home_team, away_team,
         home_score, away_score,
         home_shots,away_shots,
         home_corsi, away_corsi,
         home_corsi_5v5, away_corsi_5v5,
         home_goals, away_goals,
         home_game_num, away_game_num,
         home_points_entering, away_points_entering,
         home_points_possible, away_points_possible,
         home_points_percent, away_points_percent)
  
write_csv(df.combined_game_summaries, "~/HG_Mentorship/regression_intro/summarized_games.csv")

