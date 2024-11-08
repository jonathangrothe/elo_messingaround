#Goal: Elo for sprinters in tdf 2024


#step 1: get dataset with all proper sprinters in tdf and all their sprint results

library(readr)
headtohead <- read_csv("headtohead.csv")

#step 2: do the elo process


#a function which calculates the expected score of a rider based on elo

# rider_vector <- function(df){
#   return (unlist(df[1]))
# }


num_riders <- function(rider_vector){
  return (length(rider_vector))
}

 
# elo_vector <- function(df){
#   return (unlist(df[ncol(df)]))
# }

# stage_vector <- function(df,stage){
#   return (unlist(df[stage+1]))
# }

ex_score <- function(elo_vector, rider){
  sum <- 0 
  rider_elo <- elo_vector[rider]
  for (i in 1:14){
    h2hscore <- 1/(1+10^((elo_vector[i]-rider_elo)/400))
    sum = sum + h2hscore
  }
  ex_score <- sum/91
  return(ex_score)
}

#setting up the values for each place (can change scaling)
score_values <- vector(length=14)
for (i in 1:14){
  score <- (14-i)/91
  score_values[i]<-score
}

update <- function(scoring_vector, elo_vector, stage_vector, rider){
  actual_result <- scoring_vector[stage_vector[rider]]
  expected_result <- ex_score(elo_vector, rider)
  updated_elo = elo_vector[rider] + 20*13*(actual_result-expected_result)
  return (updated_elo)
}

elo <- function(df, scoring_vector){
  rider_vector <- unlist(df[1])
  elo_vector <- unlist(df[ncol(df)])
  for (x in 2:(ncol(df)-1)){
    updated_elo <- vector(length=14)
    stage <- unlist(df[x])
    for (i in 1:14){
      rider_elo <- update(scoring_vector, elo_vector, stage, i)
      updated_elo[i] <- rider_elo
    }
    elo_vector <- updated_elo
  }
  return (updated_elo)
}


rider_elo <- elo(headtohead,score_values)

rider_elos <- data.frame(headtohead$...1,rider_elo)





