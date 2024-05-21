library(readxl)
batsman=read_xlsx("C:\\Users\\Shiv\\Documents\\Final_Year_Project\\data by praju\\batsman_data.xlsx")
batsman=batsman[,-2]
players=read_xlsx("C:\\Users\\Shiv\\Documents\\Final_Year_Project\\data by praju\\archive\\IPL2023 Data (1).xlsx")

names(players)
batsman[,1]

# Load required library
library(stringr)

# Remove characters in parentheses from the Batter column
batsman$Batter <- str_replace(batsman$Batter, "\\s*\\(.*?\\)", "")
batsman$SR[batsman$SR == "-"] <- 0

# Load required library
library(stringdist)

# Initialize batsman$Team with NA values
batsman$Team <- NA

# Loop through players and assign teams to batsman$Team based on fuzzy matching of player names
for(i in 1:nrow(players)){
  player <- players$Player[i]
  team <- players$Team[i]
  
  # Convert player names to lowercase
  player_lower <- tolower(player)
  
  # Find approximate matches using string distance
  distances <- stringdist::stringdist(tolower(batsman$Batter), player_lower, method = "jw")
  
  # Threshold for considering a match (you may need to adjust this value)
  threshold <- 0.1
  
  # Assign team names to matching rows
  batsman$Team[distances <= threshold] <- team
}

# Remove rows where team is still NA
batsman <- batsman[complete.cases(batsman$Team), ]


dim(batsman)



# Load required library
library(stringdist)

# Initialize batsman$Role with NA values
batsman$Role <- NA

# Loop through players and assign roles to batsman$Role based on fuzzy matching of player names
for(i in 1:nrow(players)){
  player <- players$Player[i]
  role <- players$Role[i]
  
  # Convert player names to lowercase
  player_lower <- tolower(player)
  
  # Find approximate matches using string distance
  distances <- stringdist::stringdist(tolower(batsman$Batter), player_lower, method = "jw")
  
  # Threshold for considering a match (you may need to adjust this value)
  threshold <- 0.1
  
  # Assign roles to matching rows
  batsman$Role[distances <= threshold] <- role
}

# Remove rows where role is still NA
batsman <- batsman[complete.cases(batsman$Role), ]


############# Usefull Data
batsman1=read_xlsx("C:\\Users\\Shiv\\Documents\\Final_Year_Project\\data by praju\\batsman_data_dev.xlsx")

BATSMAN=batsman1[,-7]

# Create a new dataframe to store aggregated statistics
aggregate_stats1 <- data.frame(
  Batter = character(),    # Batter name
  Total_R = numeric(),     # Total runs scored
  Total_B = numeric(),     # Total balls faced
  Total_4s = numeric(),    # Total 4s hit
  Total_6s = numeric(),    # Total 6s hit
  SR = numeric(),          # Strike rate
  Team = character(),      # Team name
  Role = character(),      # Role in the team
  stringsAsFactors = FALSE
)


# Loop through unique players
unique_players <- unique(BATSMAN$Batter)
for (player in unique_players) {
  # Filter data for the current player
  player_data <- BATSMAN[BATSMAN$Batter == player, ]
  
  # Calculate aggregated statistics
  total_R <- sum(player_data$R)
  total_B <- sum(player_data$B)
  total_4s <- sum(player_data$`4s`)
  total_6s <- sum(player_data$`6s`)
  
  # Calculate strike rate if total balls faced is greater than zero, else set to zero
  if (total_B > 0) {
    SR <- sum(player_data$R) / sum(player_data$B) * 100
  } else {
    SR <- 0
  }
  
  team <- unique(player_data$Team)
  role <- unique(player_data$Role)
  
  # Add aggregated statistics to the aggregate_stats dataframe
  aggregate_stats1 <- rbind(aggregate_stats1, 
                           data.frame(Batter = player,
                                      Total_R = total_R,
                                      Total_B = total_B,
                                      Total_4s = total_4s,
                                      Total_6s = total_6s,
                                      SR = SR,
                                      Team = team,
                                      Role = role))
}

# Remove duplicate observations
aggregate_stats1 <- aggregate_stats1[!duplicated(aggregate_stats1$Batter), ]

# Print the resulting dataframe
print(aggregate_stats1)

Batsman_stats=aggregate_stats1
Batsman_stats$Role[trimws(tolower(Batsman_stats$Batter)) == "ishan kishan"] <- "Wicket-Keeper"
