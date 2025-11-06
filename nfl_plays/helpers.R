#Vector of team abbreviations with corresponding full names
nfl_teams<-c(
  "Atlanta Falcons" = "ATL",
  "Philadelphia Eagles" = "PHI",
  "Baltimore Ravens" = "BAL",
  "Buffalo Bills" = "BUF",
  "Jacksonville Jaguars" = "JAX",
  "New York Giants" = "NYG",
  "New Orleans Saints" = "NO",
  "Tampa Bay Buccaneers" = "TB",
  "New England Patriots" = "NE",
  "Houston Texans" = "HOU",
  "Minnesota Vikings" = "MIN",
  "San Francisco 49ers" = "SF",
  "Tennessee Titans" = "TEN",
  "Miami Dolphins" = "MIA",
  "Cincinnati Bengals" = "CIN",
  "Indianapolis Colts" = "IND",
  "Pittsburgh Steelers" = "PIT",
  "Cleveland Browns" = "CLE",
  "Los Angeles Chargers" = "LAC",
  "Kansas City Chiefs" = "KC",
  "Denver Broncos" = "DEN",
  "Seattle Seahawks" = "SEA",
  "Dallas Cowboys" = "DAL",
  "Carolina Panthers" = "CAR",
  "Washington Commanders" = "WAS",
  "Arizona Cardinals" = "ARI",
  "Green Bay Packers" = "GB",
  "Chicago Bears" = "CHI",
  "New York Jets" = "NYJ",
  "Detroit Lions" = "DET",
  "Oakland Raiders" = "OAK",
  "Los Angeles Rams" = "LA"
)

#Vector of numeric variables with corresponding assigned labels
num_vars<-c(
  "Offense's Win Probability Before Play" = "wp",
  "Offense's Win Probability Added on Play" = "wpa",
  "Score Differential Before Play (Offense minus Defense)" = "score_differential",
  "Yards Gained on Play" = "yards_gained"
)

#Vector of primary categorical variables with corresponding assigned labels
primary_cat_vars<-c(
  "Play Type" = "play_type",
  "Turnover?" = "turnover",
  "Winning or Tied?" = "winning"
)

#Vector of secondary (grouping) categorical variables with corresponding assigned labels
secondary_cat_vars<-c(
  "Down" = "down",
  "Quarter" = "qtr",
  primary_cat_vars
)

#Vector of grouping variables with corresponding assigned labels
grouping_vars<-c(
  "Play Type" = "play_type",
  "Down" = "down",
  "Quarter" = "qtr"
)

#Vector of all variables in main dataset wtih corresponding assigned labels
all_vars<-c(
  "Game ID Number" = "game_id",
  "Game Date" = "game_date",
  "Home Team" = "home_team",
  "Away Team" = "away_team",
  "Team with Possession/on Offense" = "posteam",
  "Quarter" = "qtr",
  "Game Clock" = "time",
  "Down" = "down",
  "Yards to Go" = "ydstogo",
  "Yardline" = "yrdln",
  "Home Team Score" = "total_home_score",
  "Away Team Score" = "total_away_score",
  "Score Differential Before Play (Offense minus Defense)" = "score_differential",
  "Play Description" = "desc",
  "Play Type" = "play_type",
  "Yards Gained on Play" = "yards_gained",
  "Win Probability Before the Play" = "wp",
  "Win Probability Added on the Play" = "wpa",
  "Fumble Lost on Play?" = "fumble_lost",
  "Interception on Play?" = "interception",
  "Turnover?" = "turnover",
  "Winning (or Tied)?" = "winning"
)

#Function generating kernel density plot with no grouping variable
no_group_density<-function(data,numvar) {
  g<-ggplot(data=data,aes(x=!!sym(numvar)))+geom_density(fill="navy",alpha=0.8)+
    theme_light(base_size = 16, base_family = "Helvetica Neue")+
    #Applying custom theming
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold",color = "#ffffff",size=19),
      axis.title.y = element_text(face="bold",color = "#ffffff",size = 16),
      axis.title.x = element_text(face="bold",color = "#ffffff",size = 16),
      plot.background = element_rect(fill = "#2f2f2f"),
      panel.background = element_rect(fill = "#2f2f2f"),
      legend.text = element_text(color="#ffffff",face="bold"),
      legend.background= element_rect(fill = NA, color = NA),
      axis.text = element_text(color = "#ffffff", size = 16,face="bold")
    )+labs(title=paste0("Kernel Density Plot for ",names(num_vars)[num_vars==numvar]),y="Density",x=names(num_vars)[num_vars==numvar])
  
  g
}

#Function generating kernel density plot WITH grouping variable
grouped_density<-function(data,numvar,groupvar) {
  colors<-c("navy","darkred","darkgreen","darkorange","darkgrey")
  colors_subset<-colors[1:length(unique(data[[groupvar]]))]
  g<-ggplot(data=data,aes(x=!!sym(numvar),fill=!!sym(groupvar)))+geom_density(alpha=0.3)+
    theme_light(base_size = 16, base_family = "Helvetica Neue")+scale_fill_manual(values=colors_subset)+
    #Applying custom theming
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold",color = "#ffffff",size=19),
      axis.title.y = element_text(face="bold",color = "#ffffff",size = 16),
      axis.title.x = element_text(face="bold",color = "#ffffff",size = 16),
      plot.background = element_rect(fill = "#2f2f2f"),
      panel.background = element_rect(fill = "#2f2f2f"),
      legend.text = element_text(color="#ffffff",face="bold"),
      legend.background= element_rect(fill = NA, color = NA),
      axis.text = element_text(color = "#ffffff", size = 16,face="bold")
    )+labs(title=paste0("Kernel Density Plot for ",names(num_vars)[num_vars==numvar]," by ",names(grouping_vars)[grouping_vars==groupvar]),y="Density",x=names(num_vars)[num_vars==numvar],fill=NULL)
  
  g
}
