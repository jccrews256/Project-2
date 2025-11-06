#This basic script subsets the original dataset from Kaggle and produces
#data files small enough to be pushed to the GitHub repository

#Reading in data and filtering to passes and runs in the 2018-2019 season
play_data<-read_csv("NFL Play by Play 2009-2018 (v5).csv") |>
  filter(play_type %in% c("pass","run"),game_date >= ymd("2018-06-01")) |>
  select(game_id,game_date,home_team,away_team,posteam,qtr,time,down,ydstogo,yrdln,total_home_score,
         total_away_score,score_differential,desc,play_type,yards_gained,wp,wpa,fumble_lost,interception)

#Reading in dataset a second time for win probability time series plot
play_data2<-read_csv("NFL Play by Play 2009-2018 (v5).csv") |>
  filter(game_date >= ymd("2018-06-01")) |>
  select(home_team,away_team,home_wp,away_wp,qtr,time,desc,wp,game_seconds_remaining,game_id,game_date,
         total_away_score,total_home_score)

#Writing the datasets to csv's for the app
write_csv(play_data,"nfl_plays/2018_2019_rp_plays.csv")
write_csv(play_data2,"nfl_plays/2018_2019_all_plays.csv")