# Probability of winning a point when serving
p1_serve_win <- 0.55  # player 1
p2_serve_win <- 0.4   # player 2

# Simulate 1000 games
n_games <- 1000

# variables to get number of wins (question a)
rule_a_wins <- 0
rule_b_wins <- 0

for (i in 1:n_games) {
  # Service rule A
  p1_score <- 0
  p2_score <- 0
  for (j in 1:3) {
    if (j %% 2 != 0) {  # player 1 serves
      if (runif(1) <= p1_serve_win) {  # player 1 wins the point and serves again
        p1_score <- p1_score + 1
      } else {  # player 2 wins the point and serves next
        p2_score <- p2_score + 1
      }
    } else {  # player 2 serves
      if (runif(1) <= p2_serve_win) {  # player 2 wins the point and serves again
        p2_score <- p2_score + 1
      } else {  # player 1 wins the point and serves next
        p1_score <- p1_score + 1
      }
    }
    # Check if a player has won the game (question a)
    if (p1_score == 2) {
      rule_a_wins <- rule_a_wins + 1
      break
    } else if (p2_score == 2) {
      break
    }
  }
  
  # Service rule B
  p1_score <- 0
  p2_score <- 0
  for (j in 1:2) {
    if (j == 1) {  # player 1 serves
      if (runif(1) <= p1_serve_win) {  # player 1 wins the point and serves again
        p1_score <- p1_score + 1
      } else {  # player 2 wins the point and serves next
        p2_score <- p2_score + 1
      }
    } else {  # player 2 serves
      if (runif(1) <= p2_serve_win) {  # player 2 wins the point and serves again
        p2_score <- p2_score + 1
      } else {  # player 1 wins the point and wins the game
        p1_score <- p1_score + 1
      }
    }
    # Check if a player has won the game
    if (p1_score == 2) {
      rule_b_wins <- rule_b_wins + 1
      break
    } else if (p2_score == 2) {
      break
    }
  }
}

# Estimate winning probabilities (question a)
p_rule_a <- rule_a_wins / n_games
p_rule_b <- rule_b_wins / n_games



# Print results (question a)
cat("Winning probability of Player 1 under service rule A:", round(p_rule_a, 3), "\n")
cat("Winning probability of Player 1 under service rule B:", round(p_rule_b, 3))


# question a explanation

# This program simulates a game of tennis between two players (Player 1 and Player 2) with two different rules for serving.
# The rules are called Service Rule A and Service Rule B. Under Service Rule A, if Player 1 serves, they have a 55% chance of winning a point.
# If Player 2 serves, they have a 40% chance of winning a point. The game is played Best-of-3, meaning the first player to win 2 points wins the game.
# 
# The program simulates 1000 games of tennis, with each game played under both Service Rule A and Service Rule B. For each game played, the program records
# which player wins, and then calculates the proportion of games won by Player 1 under each rule.
# 
# Finally, the program prints out the results, showing the estimated probability of Player 1 winning a
# game of tennis under each service rule.


