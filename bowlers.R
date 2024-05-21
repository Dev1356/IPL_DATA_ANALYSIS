library(readxl)
bowlers=read_xlsx("C:\\Users\\Shiv\\Documents\\Final_Year_Project\\data by praju\\bowlers_data.xlsx")
bowlers=na.omit(bowlers)
dim(bowlers)
players=read_xlsx("C:\\Users\\Shiv\\Documents\\Final_Year_Project\\data by praju\\archive\\IPL2023 Data (1).xlsx")

# Load required library
library(stringdist)

# Initialize bowlers$Team with NA values
bowlers$Team <- NA

# Loop through players and assign teams to bowlers$Team based on fuzzy matching of player names
for(i in 1:nrow(players)){
  player <- players$Player[i]
  team <- players$Team[i]
  
  # Convert player names to lowercase
  player_lower <- tolower(player)
  
  # Find approximate matches using string distance
  distances <- stringdist::stringdist(tolower(bowlers$Bowler), player_lower, method = "jw")
  
  # Threshold for considering a match (you may need to adjust this value)
  threshold <- 0.15
  
  # Assign team names to matching rows
  bowlers$Team[distances <= threshold] <- team
}

# Remove rows where team is still NA
bowlers <- bowlers[complete.cases(bowlers$Team), ]

dim(bowlers)








# Load required library
library(stringdist)

# Initialize bowlers$Role with NA values
bowlers$Role <- NA

# Loop through players and assign roles to bowlers$Role based on fuzzy matching of player names
for(i in 1:nrow(players)){
  player <- players$Player[i]
  role <- players$Role[i]
  
  # Convert player names to lowercase
  player_lower <- tolower(player)
  
  # Find approximate matches using string distance
  distances <- stringdist::stringdist(tolower(bowlers$Bowler), player_lower, method = "jw")
  
  # Threshold for considering a match (you may need to adjust this value)
  threshold <- 0.15
  
  # Assign roles to matching rows
  bowlers$Role[distances <= threshold] <- role
}


############# Usefull Data
bowlers1=read_xlsx("C:\\Users\\Shiv\\Documents\\Final_Year_Project\\data by praju\\bowlers_data_dev.xlsx")
View(bowlers1)
BOWLERS=bowlers1[,-7]


# Create a new dataframe to store aggregated statistics
aggregate_stats <- data.frame(
  Bowler = character(),  # Bowler name
  Total_O = numeric(),   # Total overs bowled
  Total_R = numeric(),   # Total runs conceded
  Total_W = numeric(),   # Total wickets taken
  Average_Econ = numeric(),  # Average economy rate
  Total_Dots = numeric(),    # Total dot balls bowled
  Team = character(),    # Team name
  Role = character(),    # Role in the team
  stringsAsFactors = FALSE
)

# Trim player names to remove leading and trailing spaces
BOWLERS$Bowler <- trimws(BOWLERS$Bowler)

# Loop through unique players
unique_players <- unique(BOWLERS$Bowler)
for (player in unique_players) {
  # Filter data for the current player
  player_data <- BOWLERS[BOWLERS$Bowler == player, ]
  
  # Calculate aggregated statistics
  total_O <- ceiling(sum(player_data$O))  # Round up the sum of overs
  total_R <- sum(player_data$R)
  total_W <- sum(player_data$W)
  average_Econ <- mean(player_data$Econ)
  total_Dots <- sum(player_data$Dots)
  team <- unique(player_data$Team)
  role <- unique(player_data$Role)
  
  # Add aggregated statistics to the aggregate_stats dataframe
  aggregate_stats <- rbind(aggregate_stats, 
                           data.frame(Bowler = player,
                                      Total_O = total_O,
                                      Total_R = total_R,
                                      Total_W = total_W,
                                      Average_Econ = average_Econ,
                                      Total_Dots = total_Dots,
                                      Team = team,
                                      Role = role))
}

# Remove duplicate observations
aggregate_stats <- aggregate_stats[!duplicated(aggregate_stats$Bowler), ]

# Print the resulting dataframe
print(aggregate_stats)
dim(aggregate_stats)
bowlers_stats=aggregate_stats



