# SoccerData

The goal of our project is to generate a predictive model capable to give out the probability of outcomes for soccer games in european leagues. In order to do so we found a large data set of previous games (from 2008 to 2016) for each league, along with the overall tactical attributes of teams, the skills of the players on each team (taken from the well known EA SPORTS FIFA 2016 game) and finally the odds made by bookmakers for all previous games.

It is important to note here that each team will face each other twice a season (typically running from August to May of the following solar year), this added to the fact that most player transfers occur during the summer we can take the double confrontation between each team during a single season to be the data at a single time point. Our hope is that given the past confrontation between each team (where using splines we will make recent matchups have more weight than more distant confrontations in the past) along with the current form of the team (results over the past 5 games), bookeys betting odds, players available on that day, overall tactics matchup and taking in account home side advantage, we will be able to predict regularly the winner of the games at end that week end.

Due to the possibility of 3 outcomes for each game (home wins, draw, away wins) we will use a multinomial logistic regression to get the probability of each outcome setting "home victory" as the reference group (for simplicity due to the advantages that the home team carries). We first must attribute a score to each possible outcoume, we thus specify the following scoring model :

Score (Xi, k) = Xi' * Beta_k

which is the score associated with assigning game i to oucome k (home win = 0, draw = 1, away win = 2 is simplyt a categorical variable).

We will be running a simple binary logistic regression around a pivot value (as mentinonned previously home victory) :

ln ( P(Yi = 1)/P(Yi = 0) ) = Xi' * Beta_1
ln ( P(Yi = 2)/P(Yi = 0) ) = Xi' * Beta_2

Solving will yield :

P(Yi = 1) = P(Yi = 0) * exp (Xi' * Beta_1)
P(Yi = 2) = P(Yi = 0) * exp (Xi' * Beta_2)

As these outcome probabilities must sum to 1 we therefore have :

P(Yi = 0) = 1/(1 + sum(exp(Xi' * Beta_k)) )

giving :

P (Yi = j) = Xi' * Beta_j / (1 + sum(exp(Xi' * Beta_k)) )
For all other outcomes.

