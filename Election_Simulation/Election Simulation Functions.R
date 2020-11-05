library(tidyverse)

#for positive and negative odds (American system), calculates implied probability
convert_odds <- function(odds) {
  if(odds < 0) {
    odds <- abs(odds)
    prob <- odds/(odds+100)
  }
  else{
    prob <- 100/(odds+100)
  }
  prob
}
win_pred <- function(state_odds) {
  wins <- vector("numeric", length(state_odds))
  for(i in 1:length(state_odds)) {
    wins[i] <- sample(c(1, 0), size = 1, prob = c(state_odds[i], 1-state_odds[i]))
  }
  names(wins) <- state_names
  wins
  
}
score_predic <- function(results) {
  if(results["pa"] ==1) {
    biden_elec <- biden_elec+pa_elec
  }
  if(results["ga"] ==1) {
    biden_elec <- biden_elec+ga_elec
  }
  if(results["mi"] ==1) {
    biden_elec <- biden_elec+mi_elec
  }
  if(results["wi"] ==1) {
    biden_elec <- biden_elec+wi_elec
  }
  if(results["az"] == 1) {
    biden_elec <- biden_elec+az_elec
  }
  if(results["pa"] == 0) {
    trump_elec <- trump_elec+pa_elec
  }
  if(results["az"] == 0) {
    trump_elec <- trump_elec+az_elec
  }
  if(results["ga"] == 0) {
    trump_elec <- trump_elec+ga_elec
  }
  if(results["mi"] == 0) {
    trump_elec <- trump_elec+mi_elec
  }
  if(results["wi"] == 0) {
    trump_elec <- trump_elec+wi_elec
  }
  final <- c(biden_elec, trump_elec)
  final 
}

determine_winner <- function(final) {
  winner <- vector("character", nrow(final))
  for(i in 1:nrow(final)) {
    iter <- paste("Scoring Iteration No.", i)
    print(iter, quote = F)
    if(final$biden_points[i] >= 270 & final$trump_points[i] <270) {
      
      winner[i] <- "Biden Wins"
    }
    else if(final$biden_points[i] < 270 & final$trump_points[i] >= 270) {
      winner[i] <- "Trump Wins"
    }
    else{
      winner[i] <- "Tie"
    }
  }
  winner
  
}



game_election <- function(iter) {
  results <- vector("list", length(iter))
  score <- vector("list", length(iter))
  for(i in 1:iter) {
    results[[i]] <- win_pred(swing_odds)
    score[[i]] <- score_predic(results[[i]])
    game <- paste(i, "iteration")
    print(game, quote = F)
    
  }
  print.noquote("Preparing Results")
  results <- as.data.frame(purrr::reduce(results, rbind), stringsasFactors = F, row.names = F)
  print.noquote("Scoring Result")
  score <- as.data.frame(purrr::reduce(score, rbind), stringsasFactors = F, row.names = F)
  print.noquote("Finalizing Scores")
  final <- cbind(results, score)
  names(final) <- c(state_names, "biden_points", "trump_points")
  print.noquote("Determining Winner")
  winner <- determine_winner(final)
  
  print.noquote("Preparing Final Output")
  final <- cbind(final, winner) 
  names(final) <- c(state_names, "biden_points", "trump_points", "Winner")
  final
  
  
}
