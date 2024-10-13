# Load necessary libraries
library(httr)
library(jsonlite)
library(shiny)
library(shinythemes)
library(gamlss)
library(DBI)
library(RSQLite)
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
  for (bookmaker in odds$bookmakers) {
    
    # Loop through each market for the bookmaker
    for (i in seq_along(bookmaker$markets)) {
      market <- bookmaker$markets[[i]]  # Access each market
      title <- bookmaker$title[i] 
      cat("Bookmaker: ",title, "\n")
      cat("Date:", market$last_update, "\n")
      
      # Loop through each outcome within the market
      for (outcome in market$outcomes) {
        for (outcome in market$outcomes) {
          cat("Team/Outcome:", outcome$name, "\n")
          cat("Odds:", outcome$price, "\n")
          
          # Print the calculated price as 1/price * 100
          cat("Calculated Price:", 1 / outcome$price * 100, "\n\n")
        }
      }
    }
    cat("\n")
  }
} else {
  cat("No bookmakers found in the first event.\n")
}

