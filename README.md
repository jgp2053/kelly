# Abstract

The Kelly criterion is a long-term betting strategy that outputs a fixed fraction of a bankroll to wager given the probability of winning the bet and the bet’s payoff odds. The corresponding “Kelly Bet” maximizes the expected logarithm of the bettor’s bankroll (Kelly, 1956). This betting strategy has been extended to many scenarios, including bets with non- binary outcomes (Smoczynski and Tomkins, 2010), asset optimization in the stock market (Thorp and Kassouf, 1968; Nekrasov, 2014; MacLean et al., 2011a), and sports betting (Thorp, 1997). However, many modern betting scenarios restrict the bettor to discrete bet amounts. For example, in daily fantasy sports (DFS), bettors may only enter contests with fixed entry fees (e.g. $1, $5, $25, or $100). What happens when a bettor cannot submit a Kelly Bet because it is not an allowed bet amount? We provide a framework for determining the “Discrete Kelly Bet”, given arbitrary allowed discrete bet amounts, probability of winning, and payoff odds. By isolating one variable (e.g. probability of winning the bet) and holding the others constant (e.g. the odds of winning the bet and the bettor’s bankroll), we can define optimal regions for each bet amount. Our simulation studies demonstrate that the Discrete Kelly Bet affords many of the same desirable properties (MacLean et al., 2011b) as the Kelly criterion.

# Repository Information

This repository contains the code used to generate the data and figures used in the paper:

+ utility_curves.R and intersection_plots/ contain illustrative examples used during the theoretical portion of the paper.
+ simulation_study.R and simulation_plots/ show the effectiveness of the proposed "Discrete Kelly" betting strategy.
+ intersection.R is an implementation of Newton's method that can be used for varying bankroll, for which no closed form is available. It is included in the paper's appendix. intersection_points.csv is an example output of intersection.R.