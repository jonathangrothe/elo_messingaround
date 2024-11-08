#Goal: Elo for sprinters in tdf 2024


#step 1: get dataset with all proper sprinters in tdf and all their sprint results

library(readr)
headtohead <- read_csv("tdf_headtohead.csv")
giro_h2h <-read_csv("giro_headtohead.csv")

#step 2: do the elo process

num_riders <- function(rider_vector){
  return (length(rider_vector))
}

#finds the expected score of a rider based on their
ex_score <- function(elo_vector, rider){
  sum <- 0 
  rider_elo <- elo_vector[rider]
  num_riders <- num_riders(elo_vector)
  for (i in 1:num_riders){
    if(i != rider){
      h2hscore <- 1/(1+10^((elo_vector[i]-rider_elo)/400))
      sum = sum + h2hscore
    }
  }
  ex_score <- (2*sum)/(num_riders*(num_riders-1))
  return(ex_score)
}

#setting up the values for each place (can change scaling)
score_vector <- function(num){
  score_values <- vector(length=num)
  for (i in 1:num){
    score <- 2*(num-i)/(num*(num-1))
    score_values[i]<-score
  }
  return (score_values)
}

update <- function(scoring_vector, elo_vector, stage_vector, rider){
  actual_result <- scoring_vector[stage_vector[rider]]
  expected_result <- ex_score(elo_vector, rider)
  updated_elo = elo_vector[rider] + 20*(num_riders(elo_vector)-1)*(actual_result-expected_result)
  return (updated_elo)
}

elo <- function(df, scoring_vector){
  rider_vector <- unlist(df[1])
  elo_vector <- unlist(df[ncol(df)])
  num_riders <- num_riders(elo_vector)
  for (x in 2:(ncol(df)-1)){
    updated_elo <- vector(length=num_riders)
    stage <- unlist(df[x])
    for (i in 1:num_riders){
      rider_elo <- update(scoring_vector, elo_vector, stage, i)
      updated_elo[i] <- rider_elo
    }
    elo_vector <- updated_elo
  }
  return (updated_elo)
}

scores_14 <- score_vector(14)

#CURRENTLY GIRO GIVES SUM OF 78, NOT OK
scores_12 <- score_vector(12)

rider_elo_tdf <- elo(headtohead,scores_14)
rider_elo_giro <- elo(giro_h2h,scores_12)

rider_elos <- data.frame(headtohead$...1,rider_elo)





