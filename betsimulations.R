library(tidyverse)
library(magrittr)
library(directlabels)

# Expected Logarithmic Growth
EV <- function(odds, probability, bankroll, bet){
  # TODO: fix logic for floating point errors
  adjusted_fraction = pmin(bet / bankroll, 1)
  return(
    probability * log(1 + (odds * adjusted_fraction)) + 
    (1 - probability) * log(1 - adjusted_fraction)
    )
}

# Find the two values in haystack that 'bookend' needle
find_bookends <- function(needle, haystack){
  # If there are no values in haystack, return 0
  if(length(haystack) == 0){
    return(0)
  }
  # If there is only one value in haystack, return that value
  if(length(haystack) == 1){
    return(haystack[1])
  }
  # If all of the values in haystack are bigger than needle, return the max value
  else if((maximum <- max(haystack)) < needle){
    return(maximum)
  }
  # If all of the values in haystack are smaller than needle, return the min value
  else if((minimum <- min(haystack)) > needle){
    return(minimum)
  }
  # Otherwise, find the bookend values
  else {
    lower <- max(haystack[haystack <= needle])
    higher <- min(haystack[haystack >= needle])
    return(c(lower, higher)) 
  }
}

# TODO: fix issue with floating point storage
precision <- .000001

# Betting strategy: pick a random bet out of the set of discrete options
random_bet <- function(odds = NA, probability = NA, bankroll, possible_bets){
  allowed_bets <- possible_bets[possible_bets <= (bankroll + precision)]
  if(length(allowed_bets) == 0){
    return(0)
  }
  else if(length(allowed_bets) == 1){
    return(allowed_bets)
  }
  else{
    return(sample(allowed_bets, 1))
  }
}

# Betting strategy: pick the highest bet from the discrete options that can be afforded
max_bet <- function(odds = NA, probability = NA, bankroll, possible_bets){
  allowed_bets <- possible_bets[possible_bets <= (bankroll + precision)]
  if(length(allowed_bets) > 0){
    return(max(allowed_bets))
  }
  else{
    return(0)
  }
}

# Betting strategy: bet the kelly bet, regardless of the discrete options
kelly_bet <- function(odds, probability, bankroll, possible_bets) {
  return(((probability * (odds + 1) - 1) / odds) * bankroll)
}

# Betting strategy: use suggested methodology to make kelly bet
best_fixed_bet <- function(odds, probability, bankroll, possible_bets){
  allowed_bets <- possible_bets[possible_bets <= (bankroll + precision)]
  if(length(allowed_bets) > 0){
    best_bets <- find_bookends(kelly_bet(odds, probability, bankroll), allowed_bets)
    return(best_bets[which.max(EV(odds, probability, bankroll, best_bets))])
  }
  else{
    return(0)
  }
}

# Betting strategy: select randomly from the bets that bookend the kelly bet
best_two_bets <- function(odds, probability, bankroll, possible_bets){
  allowed_bets <- possible_bets[possible_bets <= (bankroll + precision)]
  if(length(allowed_bets) > 0){
    best_bets <- find_bookends(kelly_bet(odds, probability, bankroll), allowed_bets)
    return(best_bets[sample(length(best_bets), 1)])
    }
  else{
    return(0)
  }
}

# Betting strategy: select the bookend bet closest to the kelly bet
closest_bookend_bet <- function(odds, probability, bankroll, possible_bets){
  allowed_bets <- possible_bets[possible_bets <= (bankroll + precision)]
  if(length(allowed_bets) > 0){
    k_bet <- kelly_bet(odds, probability, bankroll, allowed_bets)
    best_bets <- find_bookends(k_bet, allowed_bets)
    return(best_bets[which(abs(best_bets - k_bet) == min(abs(best_bets - k_bet)))])
  }
  else{
    return(0)
  }
}

# Simulate a run and output the bankrolls associated with various betting strategies
bet_sim <- function(initial_bankroll, odds, probability, possible_bets, num_bets){
  # Determine wins and losses
  bet_result = c(NA, rbinom(num_bets, 1, probability))

  # Initialize bankroll vectors
  kelly_bankroll = integer(length(num_bets))
  discrete_kelly_bankroll = integer(length(num_bets))
  random_bookend_bankroll = integer(length(num_bets))
  closest_bookend_bankroll = integer(length(num_bets))
  random_discrete_bankroll = integer(length(num_bets))
  max_discrete_bankroll = integer(length(num_bets))
  
  # Set the initial bankrolls
  kelly_bankroll[1] = initial_bankroll
  discrete_kelly_bankroll[1] = initial_bankroll
  closest_bookend_bankroll[1] = initial_bankroll
  random_bookend_bankroll[1] = initial_bankroll
  random_discrete_bankroll[1] = initial_bankroll
  max_discrete_bankroll[1] = initial_bankroll

  # check if the discrete kelly and closest bookend strategies differ
  diff_bet = integer(length(num_bets))
  diff_bet[1] = NA
  
  # Bet num_bets times
  for (i in 2:(num_bets + 1)){
    
    # if the result is a win, the bankroll increases by odds * the bet
    # otherwise it decreases by the bet
    result_multiplier <- ifelse(bet_result[i], odds, -1)
    
    kelly_bankroll[i] <- 
      kelly_bankroll[i-1] + (
        result_multiplier *
        kelly_bet(odds, probability, kelly_bankroll[i-1], possible_bets)
      )
    
    discrete_kelly_bankroll[i] <- 
      discrete_kelly_bankroll[i-1] + (
        result_multiplier *
        best_fixed_bet(odds, probability, discrete_kelly_bankroll[i-1], possible_bets)
      )
    
    closest_bookend_bankroll[i] <- 
      closest_bookend_bankroll[i-1] + (
        result_multiplier *
        closest_bookend_bet(odds, probability, closest_bookend_bankroll[i-1], possible_bets)
      )
    
    diff_bet[i] <- 
      near(
        best_fixed_bet(odds, probability, discrete_kelly_bankroll[i-1], possible_bets),
        closest_bookend_bet(odds, probability, discrete_kelly_bankroll[i-1], possible_bets)
      )
    
    random_bookend_bankroll[i] <- 
      random_bookend_bankroll[i-1] + (
        result_multiplier *
        best_two_bets(odds, probability, random_bookend_bankroll[i-1], possible_bets)
      )
    
    random_discrete_bankroll[i] <-
      random_discrete_bankroll[i-1] + (
        result_multiplier *
        random_bet(odds, probability, random_discrete_bankroll[i-1], possible_bets)
      )
    max_discrete_bankroll[i] <-
      max_discrete_bankroll[i-1] + (
        result_multiplier *
        max_bet(odds, probability, max_discrete_bankroll[i-1], possible_bets)
      )
    }
  return (
    tibble(
      time = 0:num_bets, 
      bet_result, 
      kelly_bankroll, 
      discrete_kelly_bankroll, 
      closest_bookend_bankroll,
      random_bookend_bankroll,
      random_discrete_bankroll,
      max_discrete_bankroll,
      diff_bet
    )
  )
}

# Simulate a series of runs and save all data
multiple_sim <- function(initial_bankroll, odds, probability, possible_bets, num_bets, num_sims){
  replicate(
    n = num_sims, 
    expr = bet_sim(initial_bankroll, odds, probability, possible_bets, num_bets),
    simplify = FALSE
  ) %>%
    # bind the simulations together
    # (we use the identity function since nothing needs to be done to the dfs)
    map_dfr(identity, .id = "sim_no")
}

set.seed(0)

# multiple_run <- multiple_sim(
#   initial_bankroll = 100, 
#   odds = 1, 
#   probability = .525,
#   # allow option for no bet
#   possible_bets = c(0, 2^seq(0, 1023)),
#   num_bets = 10^4,
#   num_sims = 10^4
# )

# save(multiple_run, file = 'data/10K_runs.Rdata')
load('data/10K_runs.Rdata')


multiple_run %<>% select(-c(max_discrete_bankroll, random_discrete_bankroll))

clean_strategy_name <- function(name){
  case_when(
    grepl('discrete_kelly', name) ~ 'Discrete Kelly',
    grepl('closest_bookend', name) ~ 'Closest Bookend',
    grepl('random_bookend', name) ~ 'Random Bookend',
    grepl('random_discrete', name) ~ 'Random',
    grepl('max_discrete', name) ~ 'Maximum',
    grepl('kelly', name) ~ 'Theoretical Kelly'
  ) %>%
    as_factor()
}

strategy_is_kelly <- function(strategy){
  ifelse(strategy == 'Theoretical Kelly', 'dashed', 'solid')
}

# Define colors for lines in graphs to follow
# Colors are persistent across graphs
my_colors <- RColorBrewer::brewer.pal(6, "Dark2")
names(my_colors) <- colnames(multiple_run) %>%
  as_tibble() %>%
  filter(grepl('bankroll', value)) %>%
  mutate(value = clean_strategy_name(value)) %>%
  .$value

# Define common style for graphs
kelly_plot_style <- function(chart_type){
  if(chart_type == 'line'){
    list(
      theme_bw(),
      scale_colour_manual(
        name = "Bet Strategy",
        values = my_colors,
        guide = "none"
        ),
      geom_dl(
        method = list(
          'last.bumpup', 
          dl.trans(x = x + .2), 
          cex = .85,
          hjust = 0
          )
        ),
      scale_x_continuous(expand = expansion(mult = c(0, .35))) 
    )
  }
  else if(chart_type == 'bar'){
    list(
      theme_bw(),
      scale_fill_manual(name = "Bet Strategy", values = my_colors, guide = "none")
    )
  }
}

# median bankroll at each point in time
medians <- multiple_run %>%
  group_by(time) %>%
  summarise(discrete_kelly_bankroll = median(discrete_kelly_bankroll),
            random_bookend_bankroll = median(random_bookend_bankroll),
            closest_bookend_bankroll = median(closest_bookend_bankroll),
            kelly_bankroll = median(kelly_bankroll)) %>%
  gather(key = 'strategy', value = 'bankroll', -time) %>%
  # clean up column value for graph
  mutate(
    strategy = clean_strategy_name(strategy),
    is_kelly = strategy_is_kelly(strategy)
  )

graph_medians <- ggplot(
  data = medians,
  mapping = aes(
    x = time, 
    y = bankroll, 
    color = strategy, 
    label = strategy
    )
) +
  kelly_plot_style('line') +
  geom_line() +
  ylab("Median bankroll") +
  xlab("Time") +
  scale_y_continuous(
    trans='log10',
    expand = expansion(mult = c(0, .1))
    )

ggsave(
  file = 'medians.png', 
  plot = graph_medians, 
  width = 6, 
  height = 4, 
  path = 'simulation_plots'
)

# mean bankroll at each point in time
means <- multiple_run %>%
  group_by(time) %>%
  summarise(discrete_kelly_bankroll = mean(discrete_kelly_bankroll),
            random_bookend_bankroll = mean(random_bookend_bankroll),
            closest_bookend_bankroll = mean(closest_bookend_bankroll),
            kelly_bankroll = mean(kelly_bankroll)) %>%
  gather(key = 'strategy', value = 'bankroll', -time) %>%
  # clean up column value for graph
  mutate(
    strategy = clean_strategy_name(strategy),
    is_kelly = strategy_is_kelly(strategy)
  )
  
graph_means <- ggplot(
  data = means,
  mapping = aes(
    x = time, 
    y = bankroll, 
    color = strategy, 
    label = strategy
    )
) +
  kelly_plot_style('line') +
  geom_line() +
  ylab("Mean bankroll") +
  xlab("Time") +
  scale_y_continuous(trans='log10', expand = expansion(mult = c(0, .1)))

ggsave(
  file = 'means.png', 
  plot = graph_means, 
  width = 6, 
  height = 4, 
  path = 'simulation_plots'
)

# see how often strategies are tied with or ahead of kelly
percent_win <- multiple_run %>%
  select(-c(diff_bet, bet_result)) %>%
  pivot_longer(
    cols = ends_with('bankroll') & ! matches('discrete_kelly_bankroll'),
    names_to = 'strategy',
    values_to = 'strategy_bankroll'
  ) %>%
  mutate(
    beats_discrete_kelly = strategy_bankroll > discrete_kelly_bankroll,
    ties_discrete_kelly = strategy_bankroll == discrete_kelly_bankroll
  ) %>%
  group_by(time, strategy) %>%
  summarise(
    beats_discrete_kelly_count = sum(beats_discrete_kelly),
    ties_discrete_kelly_count = sum(ties_discrete_kelly),
    total_count = n()
  ) %>%
  mutate(beats_or_ties_discrete_kelly_count = beats_discrete_kelly_count + ties_discrete_kelly_count) %>%
  # clean up column value for graph
  mutate(
    strategy = clean_strategy_name(strategy),
    is_kelly = strategy_is_kelly(strategy)
  )

graph_beat_or_tie <- ggplot(
  data = percent_win, 
  aes(
    x = time, 
    y = beats_or_ties_discrete_kelly_count / total_count, 
    color = strategy,
    label = strategy
  )
) +
  kelly_plot_style('line') +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  ylab("% of Runs Beating or Tying Discrete Kelly") +
  xlab("Time")

ggsave(
  file = 'beat_or_tie.png', 
  plot = graph_beat_or_tie, 
  width = 6, 
  height = 4, 
  path = 'simulation_plots'
)

graph_beats <- ggplot(
  data = percent_win, 
  aes(
    x = time, 
    y = beats_discrete_kelly_count / total_count, 
    color = strategy,
    label = strategy
  )
) +
  kelly_plot_style('line') +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  ylab("% of Runs Beating Discrete Kelly") +
  xlab("Time")

ggsave(
  file = 'beats.png', 
  plot = graph_beats, 
  width = 6, 
  height = 4, 
  path = 'simulation_plots'
)

gone_broke <- multiple_run %>%
  select(-c(diff_bet, bet_result)) %>%
  filter(time >= (max(time) - 1)) %>% # last and next to last betting cycle
  pivot_longer(
    cols = ends_with('bankroll'),
    names_to = 'strategy',
    values_to = 'strategy_bankroll'
  ) %>%
  group_by(strategy, sim_no) %>%
  summarise(
    run_gone_broke = 
      min(strategy_bankroll) == 0 || # bankroll hit zero
      length(unique(strategy_bankroll)) == 1
      ) %>%
  # clean up column value for graph
  mutate(
    strategy = clean_strategy_name(strategy),
    is_kelly = strategy_is_kelly(strategy)
    )

# if we want all the strategies, need to lessen text size
graph_gone_broke <- gone_broke %>%
  group_by(strategy) %>%
  summarise(times_gone_broke = sum(run_gone_broke) / n()) %>%
  ggplot(
    aes(
      x = reorder(strategy, -times_gone_broke),
      y = times_gone_broke,
      fill = strategy
      )
    ) +
  kelly_plot_style('bar') +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  ylab("% of Runs Gone Broke") +
  xlab("Betting Strategy")

ggsave(
  file = 'gone_broke.png', 
  plot = graph_gone_broke, 
  width = 6, 
  height = 4, 
  path = 'simulation_plots'
)

gone_broke %>%
  group_by(strategy) %>%
  summarise(times_gone_broke = sum(run_gone_broke) / n()) %>%
  write_csv(path = 'data/gone_broke.csv')
