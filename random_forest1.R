# Load required libraries
library(randomForest)

# Read data
data <- read.csv("C:\\Users\\Shiv\\Documents\\Final_Year_Project\\ipl data 2\\ipl_match_info_data.csv")
names(data)


# Get unique values of player_of_match
unique(data$player_of_match)

# Get column names
names(data)

# Remove unwanted columns
d1 <- data[,-c(1,2,3,10,16,14,17,18,19,22,24,25,26,27)]



# Modify team names
col_modified <- c("team1","team2","toss_winner","winner")

for(i in col_modified){
  d1[[i]][d1[[i]]=="Kings XI Punjab"] <- "Punjab Kings"
  d1[[i]][d1[[i]]=="Deccan Chargers"] <- "Sunrisers Hyderabad"
  d1[[i]][d1[[i]]=="Gujarat Lions"] <- "Gujarat Titans"
  d1[[i]][d1[[i]]=="Delhi Daredevils"] <- "Delhi Capitals"
}

# Remove old teams
for(i in col_modified){
  d1 <- d1[!(d1[[i]]=="Rising Pune Supergiants"),]
  d1 <- d1[!(d1[[i]]=="Rising Pune Supergiant"),]
  d1 <- d1[!(d1[[i]]=="Kochi Tuskers Kerala"),]
  d1 <- d1[!(d1[[i]]=="Pune Warriors"),]
}

data1=d1

# Feature extraction
freq_table <- table(data1$winner)
probabilities <- freq_table[-11]/(table(data1$team1)+table(data1$team2))

data1 <- data1[data1$winner!= "tie",]

data1$winning_prop <- probabilities[data1$winner]

data1$tossWin_given_Winner <- ifelse(data1$toss_winner==data1$winner,1,0)

# Calculate total matches
total_matches <- nrow(data1)

# Calculate matches won by each team
matches_won_by_team <- table(data1$winner)

# Calculate winning percentage with each umpire
data1$winning_percentage_with_umpire1 <- NA
data1$winning_percentage_with_umpire2 <- NA
data1$winning_percentage_with_tv_umpire <- NA

for (ump in unique(data1$umpire1)) {
  for (team in unique(data1$winner)) {
    matches_won_with_umpire <- sum(data1$winner[data1$umpire1 == ump] == team)
    umpire_matches <- sum(data1$umpire1 == ump)
    data1$winning_percentage_with_umpire1[data1$umpire1 == ump & data1$winner == team] <- matches_won_with_umpire / umpire_matches
  }
}

for (ump in unique(data1$umpire2)) {
  for (team in unique(data1$winner)) {
    matches_won_with_umpire <- sum(data1$winner[data1$umpire2 == ump] == team)
    umpire_matches <- sum(data1$umpire2 == ump)
    data1$winning_percentage_with_umpire2[data1$umpire2 == ump & data1$winner == team] <- matches_won_with_umpire / umpire_matches
  }
}

for (ump in unique(data1$tv_umpire)) {
  for (team in unique(data1$winner)) {
    matches_won_with_umpire <- sum(data1$winner[data1$tv_umpire == ump] == team)
    umpire_matches <- sum(data1$tv_umpire == ump)
    data1$winning_percentage_with_tv_umpire[data1$tv_umpire == ump & data1$winner == team] <- matches_won_with_umpire / umpire_matches
  }
}

# Calculate conditional probabilities
data1$conditional_probability <- NA

for (umpire in unique(data1$tv_umpire)) {
  for (team in unique(data1$winner)) {
    for (outcome in unique(data1$result)) {
      matches_with_conditions <- sum(data1$tv_umpire == umpire & data1$winner == team & data1$result == outcome)
      matches_with_umpire <- sum(data1$tv_umpire == umpire)
      conditional_probability <-matches_with_conditions / matches_with_umpire
      data1$conditional_probability[data1$tv_umpire == umpire & data1$winner == team & data1$result == outcome] <- conditional_probability
    }
  }
}

# Filter data
filtered_data <- data1[,-c(11,12,13)]
names(filtered_data)
View(filtered_data)
t2=filtered_data[656:nrow(filtered_data),]
View(t2)
filtered_data=filtered_data[-(656:nrow(filtered_data)),]

# Fit random forest model using the training data
model <- randomForest(as.factor(winner)~.,data=filtered_data,mtry=3,ntree = 1000)
model
# Make predictions on the test data
predictions <- predict(model,t2)
result=data.frame(t2$winner,predictions)

# Compute accuracy table
accuracy_table <- table(Actual =t2$winner , Predicted = predictions)
accuracy <- sum(diag(accuracy_table)) / sum(accuracy_table)  # Compute accuracy

# Print accuracy table and overall accuracy
print("Accuracy Table:")
print(accuracy_table)
print(paste("Overall Accuracy:", round(accuracy, 2)))
varImpPlot(model)

