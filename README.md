# NBA Machine Learning
## Blog post
https://kitotheron.wixsite.com/kitotheron/post/nba-game-outcome-analyser
## Evaluate potential bets from training a model trained from data over the 2023/2024 nba season
## Instructions
Main code is contained inside the main.R file. The training data I used is called Last_NBA_SEASON.xlsx.
## Language: R 
Model Trained using the Gamlss library. I trained the model by inputting; which teams were playing, who had home court advantage and what the score difference was at the end. I did this using a binomial distribution to predict the odds one team would beat another team. I then formatted the calls retrieved from the odds-api to compare the win % different bet makers were assigning different games to conclude if these bets were considered undervalued by my model. 
