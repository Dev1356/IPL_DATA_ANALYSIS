##### test data of 2023 IPL
data2=read.csv("C:\\Users\\Shiv\\Documents\\Final_Year_Project\\IPL_2023\\match_info_data.csv")
View(test_data)
data2$result_type=ifelse(data2$result_type==0,"chased","defended")

c12=data2[,c(1,10)]
test_data=cbind(c12,data2[,(2:ncol(data2))])
test_data=test_data[,-11]

c98=test_data[,c(9,8)]
test_data=cbind(test_data[,1:7],c98,test_data[,10:ncol(test_data)])


# Feature extraction
freq_table1 <- table(test_data$winner)

test_data$winning_prop <- (freq_table1/(table(test_data$team1)+table(test_data$team2)))[test_data$winner]

test_data$tossWin_given_Winner <- ifelse(test_data$toss_winner==test_data$winner,1,0)



# Calculate winning percentage with each umpire
test_data$winning_percentage_with_umpire1 <- NA
test_data$winning_percentage_with_umpire2 <- NA
test_data$winning_percentage_with_tv_umpire <- NA

for (ump in unique(test_data$umpire1)) {
  for (team in unique(test_data$winner)) {
    matches_won_with_umpire <- sum(test_data$winner[test_data$umpire1 == ump] == team)
    umpire_matches <- sum(test_data$umpire1 == ump)
    test_data$winning_percentage_with_umpire1[test_data$umpire1 == ump & test_data$winner == team] <- matches_won_with_umpire / umpire_matches
  }
}

for (ump in unique(test_data$umpire2)) {
  for (team in unique(test_data$winner)) {
    matches_won_with_umpire <- sum(test_data$winner[test_data$umpire2 == ump] == team)
    umpire_matches <- sum(test_data$umpire2 == ump)
    test_data$winning_percentage_with_umpire2[test_data$umpire2 == ump & test_data$winner == team] <- matches_won_with_umpire / umpire_matches
  }
}

for (ump in unique(test_data$tv_umpire)) {
  for (team in unique(test_data$winner)) {
    matches_won_with_umpire <- sum(test_data$winner[test_data$tv_umpire == ump] == team)
    umpire_matches <- sum(test_data$tv_umpire == ump)
    test_data$winning_percentage_with_tv_umpire[test_data$tv_umpire == ump & test_data$winner == team] <- matches_won_with_umpire / umpire_matches
  }
}

# Calculate conditional probabilities
test_data$conditional_probability <- NA

for (umpire in unique(test_data$tv_umpire)) {
  for (team in unique(test_data$winner)) {
    for (outcome in unique(test_data$result)) {
      matches_with_conditions <- sum(test_data$tv_umpire == umpire & test_data$winner == team & test_data$result == outcome)
      matches_with_umpire <- sum(test_data$tv_umpire == umpire)
      conditional_probability <-matches_with_conditions / matches_with_umpire
      test_data$conditional_probability[test_data$tv_umpire == umpire & test_data$winner == team & test_data$result == outcome] <- conditional_probability
    }
  }
}

View(test_data)
test_data=test_data[,-(11:13)]
new_test_data=rbind(t2,test_data)


