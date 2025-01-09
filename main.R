# Load necessary libraries
library(httr)
library(jsonlite)
library(gamlss)
library(DBI)
library(RSQLite)
library(dplyr)
library(readxl)
library(crayon)




# this dataset is for the 2023-2024 season
Last_NBA_SEASON <- read_excel("Last_NBA_SEASON.xlsx")
Last_NBA_SEASON <-  Last_NBA_SEASON %>%
  filter(rowSums(is.na(.)) != ncol(.))

Last_NBA_SEASON <- Last_NBA_SEASON %>%
  rename(Visitor = `Visitor/Neutral`, 
         Home = `Home/Neutral`, 
         VisitorPoints = `PTS...4`, 
         HomePoints = `PTS...6`)

Last_NBA_SEASON <-  Last_NBA_SEASON %>%
  filter(rowSums(is.na(.)) != ncol(.))
Last_NBA_SEASON <- Last_NBA_SEASON %>%
  select(Visitor, Home, VisitorPoints,HomePoints)
Last_NBA_SEASON$HomeScoreDif <- Last_NBA_SEASON$HomePoints - Last_NBA_SEASON$VisitorPoints

gamlss_model <- gamlss(
  (HomeScoreDif>0) ~ Home + Visitor,
  family = BI,  # Binomial distribution
  data = Last_NBA_SEASON
)


# Function to fetch odds from the Odds API
get_odds <- function(api_key, sport, region = "us", market = "h2h") {
  base_url <- "https://api.the-odds-api.com/v4/sports/"
  
  # Build the request URL
  request_url <- paste0(base_url, sport, "/odds/?apiKey=", api_key, "&regions=", region, "&markets=", market)
  
  # Send GET request to the API
  response <- GET(request_url)
  
  # Check if the request was successful
  if (status_code(response) != 200) {
    stop("Failed to fetch data from Odds API: ", status_code(response))
  }
  
  # Parse the response as JSON
  odds_data <- content(response, as = "text")
  odds_data <- fromJSON(odds_data, flatten = TRUE)
  
  return(odds_data)
}



# Example usage
api_key <- Sys.getenv("MY_API_KEY")
sport <- "basketball_nba" 

# Fetch and print the odds
odds <- get_odds(api_key, sport)
#print(odds)

if ("bookmakers" %in% names(odds)) {
  # Access and print bookmakers' odds for this event
  for (j in seq_along(odds$bookmakers)) {
    bookmaker <- odds$bookmakers[[j]] 
    
    # Loop through each market for the bookmaker
    for (i in seq_along(bookmaker$markets)) {
      market <- bookmaker$markets[[i]]  # Access each market
      title <- bookmaker$title[i] 
      cat("Bookmaker: ",title, "\n")
      cat("Date:", market$last_update, "\n")
      
      # Loop through each outcome within the market
      for (outcome in market$outcomes) {
        for (outcome in market$outcomes) {
          
          new_game <- data.frame(
            Home = odds$home_team[j],
            Visitor = odds$away_team[j]
          )
          predicted_prob <- predict(gamlss_model, newdata = new_game, type = "response")
          cat(odds$home_team[j]," have ", predicted_prob*100 ,"% chance to beat ",odds$away_team[j],"\n" )
          
          # Print the calculated price as 1/price * 100
          if( 1 / outcome$price[outcome$name == odds$home_team[j]] < predicted_prob){
            cat(green("Betting odds Home Team ",odds$home_team[j]," : ", 1 / outcome$price[outcome$name == odds$home_team[j]] * 100, "\n"))
          }
          else{
            cat(red("Betting odds Home Team ",odds$home_team[j]," : ", 1 / outcome$price[outcome$name == odds$home_team[j]] * 100, "\n"))
          }
          if( (1 / outcome$price[outcome$name == odds$away_team[j]]) < (1-predicted_prob)){
            cat(green("Betting odds Away Team ",odds$away_team[j]," : ", 1 / outcome$price[outcome$name == odds$away_team[j]] * 100, "\n\n"))
          }
          else{
            cat(red("Betting odds Away Team ",odds$away_team[j]," : ", 1 / outcome$price[outcome$name == odds$away_team[j]] * 100, "\n\n"))
          }
        }
      }
    }
    cat("\n")
  }
} else {
  cat("No bookmakers found in the first event.\n")
}



