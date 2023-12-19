### FUNCTIONS FOR THE SIMULATION ###

#Generates a classic deck of 52 cards.
generate_complete_deck <- function() {
  ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")
  suits <- c("s", "h", "d", "c")
  return(paste(rep(ranks, each = length(suits)), rep(suits, times = length(ranks)), sep = ""))
}


find_winning_hand <- function(player_combinations, player_ranks) {
  max_rank_index <- which.max(player_ranks)
  winning_combination <- player_combinations[max_rank_index, ]
  return(list(winner = max_rank_index, rank = player_ranks[max_rank_index], combination = winning_combination))
}

#Function used to generate the complete deck and player's hands (depending on the number of players)
generate_hands_and_deck <- function(num_players) {
  complete_deck <- generate_complete_deck()
  hands <- list()
  for (i in 1:num_players) {
    hands[[paste0("player", i, "_hand")]] <- sample(complete_deck, 4, replace = FALSE)
  }
  
  #Gives the remaining deck after the player's cards have been dealt, ensuring no cards are repeated.
  remaining_deck <- setdiff(complete_deck, unlist(hands))
  communal_deck <- sample(remaining_deck, 5, replace = FALSE)
  return(list(hands = hands, communal_deck = communal_deck))
}

#A function that generates all possible hands, for all 4 players.
#In Omaha, all players must finish the game with a hand that includes 3 of the communal cards, and 2 of their original cards.

generate_combinations <- function(player_hand, communal_deck) {
  player_combinations <- combn(player_hand, 2, simplify = TRUE) #Keeping two original cards from the hand
  communal_combinations <- combn(communal_deck, 3, simplify = TRUE) #Keeping three  cards from the communal deck
  combinations <- matrix(NA, nrow = ncol(player_combinations) * ncol(communal_combinations), ncol = 5) #Creates all possible combinations
  
  count <- 1
  for (i in 1:ncol(player_combinations)) {
    for (j in 1:ncol(communal_combinations)) {
      combinations[count, ] <- c(player_combinations[, i], communal_combinations[, j])
      count <- count + 1
    }
  }
  
  return(combinations)
}

#The function that is in charge of the ranking system of the cards.

rank_mapping <- c("2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7,"8" = 8, "9" = 9, "10" = 10, "J" = 11, "Q" = 12, "K" = 13, "A" = 14)

evaluate_hand <- function(cards) {
  ranks <- substr(cards, 1, nchar(cards) - 1)
  suits <- substr(cards, nchar(cards), nchar(cards))
  numeric_ranks <- rank_mapping[ranks]
  sorted_ranks <- sort(numeric_ranks, decreasing = TRUE)
  rank_counts <- table(sorted_ranks)
  max_count <- max(rank_counts)
  
  if (all(c("A", "K", "Q", "J", "10") %in% ranks) && length(unique(suits)) == 1) {
    return(10)  # Royal flush
  }
  
  if (length(unique(suits)) == 1 && length(unique(sorted_ranks)) == 5 && max(sorted_ranks) - min(sorted_ranks) == 4) {
    return(9 + max(sorted_ranks) / 100)  # Straight flush, + highest card in decimal form
  }
  
  if (max_count == 4) {
    return(8 + max(table(sorted_ranks) / 100) + max(sorted_ranks) / 10000)  # Four of a kind in decimal form + highest card
  } else if (max_count == 3) {
    if (length(unique(sorted_ranks)) == 2) {
      return(7 + max(sorted_ranks) / 100 + min(sorted_ranks) / 1000)  # Full house in decimal form
    } else {
      three_ranks <- names(rank_counts[rank_counts == 3])
      return(4 + max((rank_mapping[three_ranks]) / 100) + max(sorted_ranks) / 10000)  # Three of a kind in decimal form, with the three of a kind value + highest card
    }
  } else if (max_count == 2) {
    if (length(unique(sorted_ranks)) == 3) {
      pair_ranks <- names(rank_counts[rank_counts == 2]) #Two pairs
      return(3 + max(rank_mapping[pair_ranks]) / 100 + min(rank_mapping[pair_ranks]) / 10000 + max(sorted_ranks)/1000000) #Two pairs + highest value pair + lowest value pair + highest card
    } else {
      return(2 + max(table(sorted_ranks) / 100) + max(sorted_ranks)/10000)  # One pair in decimal form + highest card
    }
  }
  
  if (length(unique(sorted_ranks)) == 5 && max(sorted_ranks) - min(sorted_ranks) == 4) {
    return(5 + max(sorted_ranks) / 100)  # Straight in decimal form, including highest rank
  }
  
  if (length(unique(suits)) == 1) {
    return(6 + max(sorted_ranks) / 100)  # Flush in decimal form, including highest rank
  }
  
  return(1 + max(sorted_ranks) / 100)  # High card + value of the high card in decimal form
}


### RUNNING A SIMULATION ###
num_players <- 4 ##Input the number of players in the game, from 3-10.
hands_and_deck <- generate_hands_and_deck(num_players) # Generates the appropriate amount of hands according to the number of players
hands <- hands_and_deck$hands #Hands is JUST the players hands, excluding the communal deck.
communal_deck <- hands_and_deck$communal_deck #Grabs the communal deck from hands_and_deck
player_combinations <- list() #Generates the combinations
player_ranks <- list() #Generates the list of ranks for all combinations

for (i in 1:num_players) {
  player_combinations[[i]] <- generate_combinations(hands[[paste0("player", i, "_hand")]], communal_deck)
  player_ranks[[i]] <- apply(player_combinations[[i]], 1, evaluate_hand)
}

winner_info <- list()
for (i in 1:num_players) {
  winner_info[[i]] <- find_winning_hand(player_combinations[[i]], player_ranks[[i]])
  cat("Player", i, "'s best hand:", winner_info[[i]]$combination, "with a rank of", winner_info[[i]]$rank, "\n")
}

max_rank <- -Inf
max_rank_player <- 0

# Loop through each player's winning information
for (i in 1:num_players) {
  # Check if the current player has a higher rank than the current maximum
  if (winner_info[[i]]$rank > max_rank) {
    max_rank <- winner_info[[i]]$rank
    max_rank_player <- i
  }
}

# Print the result
cat("Player", max_rank_player, "has the highest rank of", max_rank, "\n")

#### SIMULATING THE PROBABILITY OF WINNING, KNOWING EVERYONE'S HANDS & THE COMMUNAL DECK ###

run_simulations <- function(num_simulations, num_players) {
  wins <- rep(0, num_players)  # Initialize a vector to store the number of wins for each player
  total_simulations <- 0
  
  for (s in 1:num_simulations) {
    # Generate hands and deck
    hands_and_deck <- generate_hands_and_deck(num_players)
    hands <- hands_and_deck$hands
    communal_deck <- hands_and_deck$communal_deck
    
    player_combinations <- list()
    player_ranks <- list()
    
    for (i in 1:num_players) {
      player_combinations[[i]] <- generate_combinations(hands[[paste0("player", i, "_hand")]], communal_deck)
      player_ranks[[i]] <- apply(player_combinations[[i]], 1, evaluate_hand)
    }
    
    winner_info <- list()
    max_rank <- -Inf
    max_rank_players <- integer(0)
    
    # Loop through each player's winning information
    for (i in 1:num_players) {
      winner_info[[i]] <- find_winning_hand(player_combinations[[i]], player_ranks[[i]])
      # cat("Player", i, "'s best hand:", winner_info[[i]]$combination, "with a rank of", winner_info[[i]]$rank, "\n")
      
      # Check if the current player has a higher rank than the current maximum
      if (winner_info[[i]]$rank > max_rank) {
        max_rank <- winner_info[[i]]$rank
        max_rank_players <- i
      } else if (winner_info[[i]]$rank == max_rank) {
        # If two players have the same rank, reset the list
        max_rank_players <- integer(0)
      }
    }
    
    # If only one player has the highest rank, increment the win count for that player
    if (length(max_rank_players) == 1) {
      wins[max_rank_players] <- wins[max_rank_players] + 1
      total_simulations <- total_simulations + 1
    }
  }
  
  # Calculate the percentage of wins for each player
  win_percentages <- wins / total_simulations
  result <- data.frame(Player = 1:num_players, Win_Percentage = win_percentages)
  cat("Simulation Results (excluding ties):\n")
  print(result)
  cat("Total simulations without ties:", total_simulations, "\n")
}

# Set the number of simulations
num_simulations <- 1000

# Set the number of players
num_players <-3

# Run the simulations
run_simulations(num_simulations, num_players)

## PLOTTING THE PROBABILITY DISTRIBUTION ##

# Install and load ggplot2 if not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)

# Your 8 probability values
probability_values <- c(0.3357664, 0.2534161, 0.2078086, 0.1763959, 0.1543287, 0.1347882, 0.1126005, 0.10975610)

# Number of players from 3 to 10
num_players <- 3:10

# Create a data frame for plotting
plot_data <- data.frame(Players = num_players, Probability = probability_values)

# Plotting
ggplot(plot_data, aes(x = Players, y = Probability)) +
  geom_line() +
  geom_point() +
  labs(title = "Probability of Winning vs. Number of Players",
       x = "Number of Players",
       y = "Probability") +
  scale_x_continuous(breaks = num_players) +  # Set tick marks for each player count
  theme_minimal()

