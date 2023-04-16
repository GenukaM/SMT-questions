# Probability of winning a point when serving
p1_serve_win <- 0.55  # player 1
p2_serve_win <- 0.4   # player 2

# Simulate 1000 games
n_games <- 1000
rule_a_lengths <- c()
rule_b_lengths <- c()

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
    # Check if a player has won the game
    if (p1_score == 2) {
      rule_a_lengths <- c(rule_a_lengths, j)
      break
    } else if (p2_score == 2) {
      rule_a_lengths <- c(rule_a_lengths, j)
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
      rule_b_lengths <- c(rule_b_lengths, j)
      break
    } else if (p2_score == 2) {
      rule_b_lengths <- c(rule_b_lengths, j)
      break
    }
  }
}

# Estimate expected game lengths
expected_rule_a <- mean(rule_a_lengths)
expected_rule_b <- mean(rule_b_lengths)

# Print results
cat("Expected length of a game under service rule A:", round(expected_rule_a, 2), "\n")
cat("Expected length of a game under service rule B:", round(expected_rule_b, 2))
