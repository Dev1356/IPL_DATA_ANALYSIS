Batsman_stats$Role <- gsub("Batsmen", "Batsman", Batsman_stats$Role)
Batsman_stats$Role <- gsub("Batsman/Wicket-keeper", "Wicket-Keeper", Batsman_stats$Role)
View(bowlers_stats)
bat=Batsman_stats[Batsman_stats$Role=="Batsman",]
View(bat)

unique(bowlers_stats$Role)
bowlers_stats$Role <- gsub("Batsmen", "Batsman", bowlers_stats$Role)

bowlers_stats$Role <- gsub("Batsman/Wicket-keeper", "Wicket-Keeper", bowlers_stats$Role)

ball=bowlers_stats[bowlers_stats$Role=="Bowler",]
View(ball)


player_bowlers=read_xlsx("C:\\Users\\Shiv\\Documents\\Final_Year_Project\\data by praju\\ball.xlsx")
player_batsman=bat
View(player_bowlers)

Batsman_stats$Role <- gsub("Wicket-keeper", "Wicket-Keeper", Batsman_stats$Role)
Keeper=Batsman_stats[Batsman_stats$Role=="Wicket-Keeper",]
Keeper
unique(Keeper$Team)


all_rounders_batsman <- Batsman_stats[Batsman_stats$Role == "All-Rounder", ]
all_rounders_bowlers <- bowlers_stats[bowlers_stats$Role == "All-Rounder", ]

################# Batsman  clustering

# Load necessary library
if (!require("cluster")) install.packages("cluster", dependencies = TRUE)
library(cluster)

# Function to perform clustering for players from two teams playing against each other
perform_match_clustering_batsman <- function(team1_name, team2_name, player_data) {
  # Subset data for the specified teams
  team1_data <- player_data[player_data$Team == team1_name, ]
  team2_data <- player_data[player_data$Team == team2_name, ]
  
  # Check if both teams have players
  if (nrow(team1_data) == 0 | nrow(team2_data) == 0) {
    cat("No players found for", team1_name, "and/or", team2_name, "\n")
    return(NULL)
  }
  
  # Combine data for both teams
  combined_data <- rbind(team1_data, team2_data)
  
  # Set player names as row names
  rownames(combined_data) <- combined_data$Batter
  
  # Select relevant columns for clustering
  clustering_data <- combined_data[, c("Total_R", "Total_B", "Total_4s", "Total_6s", "SR")]
  
  # Determine the number of clusters (maximum of 3 or number of players / 2)
  k <- min(3, nrow(clustering_data) %/% 2)
  
  # Perform K-means clustering
  set.seed(123) # for reproducibility
  km <- kmeans(clustering_data, centers = k)
  
  # Add cluster labels to original data
  combined_data$Cluster <- km$cluster
  
  # Create separate tables for each cluster
  cluster_tables <- lapply(unique(km$cluster), function(cluster_num) {
    cluster_data <- combined_data[combined_data$Cluster == cluster_num, ]
    return(cluster_data)
  })
  
  # Print tables for each cluster
  cat("Match Clustering for Teams:", team1_name, "vs", team2_name, "\n")
  for (i in seq_along(cluster_tables)) {
    cat("Cluster", i, ":\n")
    print(cluster_tables[[i]][, c("Total_R", "Total_B", "Total_4s", "Total_6s", "SR", "Cluster","Team","Role")])
    cat("\n")
  }
  
  # Call function to select top batsmen from clusters
  select_top_batsmen_teams(cluster_tables, team1_name, team2_name)
}

# Function to select top 4 batsmen from clustering results for specified teams
select_top_batsmen_teams <- function(cluster_tables, team1_name, team2_name) {
  # Combine all cluster tables into one dataframe
  all_players <- do.call(rbind, cluster_tables)
  
  # Filter players for specified teams
  team1_players <- all_players[all_players$Team == team1_name, ]
  team2_players <- all_players[all_players$Team == team2_name, ]
  
  # Sort players by Total_R (total runs) in descending order for each team
  sorted_team1 <- team1_players[order(-team1_players$Total_R), ]
  sorted_team2 <- team2_players[order(-team2_players$Total_R), ]
  
  # Select top 2 batsmen from each team
  top_batsmen <- rbind(sorted_team1[1:2, ], sorted_team2[1:2, ])
  
  return(top_batsmen)
}

# Assuming you have the team names for the match and player data
team1 <- "MI"
team2 <- "CSK"

# Assuming you have the player data named player_batsman

# Apply clustering for the match and select top batsmen
top_batsmen_teams <- perform_match_clustering_batsman(team1, team2, player_batsman)

# Print the top 4 batsmen
print(top_batsmen_teams[, c("Total_R", "Total_B", "Total_4s", "Total_6s", "SR", "Cluster", "Team", "Role")])


####################### Clustering for Bowlers 
# Load necessary library
if (!require("cluster")) install.packages("cluster", dependencies = TRUE)
library(cluster)

# Function to perform clustering for bowlers from two teams playing against each other
perform_match_clustering_bowlers <- function(team1_name, team2_name, bowler_data) {
  # Subset data for the specified teams
  team1_bowlers <- bowler_data[bowler_data$Team == team1_name, ]
  team2_bowlers <- bowler_data[bowler_data$Team == team2_name, ]
  
  # Check if both teams have bowlers
  if (nrow(team1_bowlers) == 0 | nrow(team2_bowlers) == 0) {
    cat("No bowlers found for", team1_name, "and/or", team2_name, "\n")
    return(NULL)
  }
  
  # Combine data for both teams
  combined_bowlers_data <- rbind(team1_bowlers, team2_bowlers)
  
  # Select relevant columns for clustering
  clustering_bowlers_data <- combined_bowlers_data[, c("Total_O", "Total_R", "Total_W", "Average_Econ", "Total_Dots")]
  
  # Determine the number of clusters (maximum of 4 or number of bowlers / 2)
  k <- min(4, nrow(clustering_bowlers_data) %/% 2)
  
  # Perform K-means clustering
  set.seed(123) # for reproducibility
  km <- kmeans(clustering_bowlers_data, centers = k)
  
  # Add cluster labels to original data
  combined_bowlers_data$Cluster <- km$cluster
  
  # Create separate tables for each cluster
  cluster_tables <- lapply(unique(km$cluster), function(cluster_num) {
    cluster_bowlers_data <- combined_bowlers_data[combined_bowlers_data$Cluster == cluster_num, ]
    return(cluster_bowlers_data)
  })
  
  # Print tables for each cluster
  cat("Match Clustering for Bowlers from Teams:", team1_name, "vs", team2_name, "\n")
  for (i in seq_along(cluster_tables)) {
    cat("Cluster", i, ":\n")
    print(cluster_tables[[i]][, c("Bowler", "Total_O", "Total_R", "Total_W", "Average_Econ", "Total_Dots", "Cluster", "Team", "Role")])
    cat("\n")
  }
  
  # Call function to select top bowlers from clusters
  select_top_bowlers_teams(cluster_tables, team1_name, team2_name)
}

# Function to select top 4 bowlers from clustering results for specified teams
select_top_bowlers_teams <- function(cluster_tables, team1_name, team2_name) {
  # Combine all cluster tables into one dataframe
  all_bowlers <- do.call(rbind, cluster_tables)
  
  # Filter bowlers for specified teams
  team1_bowlers <- all_bowlers[all_bowlers$Team == team1_name, ]
  team2_bowlers <- all_bowlers[all_bowlers$Team == team2_name, ]
  
  # Sort bowlers by Total_W (total wickets) in descending order for each team
  sorted_team1 <- team1_bowlers[order(-team1_bowlers$Total_W), ]
  sorted_team2 <- team2_bowlers[order(-team2_bowlers$Total_W), ]
  
  # Select top 2 bowlers from each team
  top_bowlers <- rbind(sorted_team1[1:2, ], sorted_team2[1:2, ])
  
  return(top_bowlers)
}

# Assuming you have the team names for the match and player data
team1 <- "MI"
team2 <- "CSK"

# Assuming you have the player data named player_bowlers

# Apply clustering for the match and select top bowlers
top_bowlers_teams <- perform_match_clustering_bowlers(team1, team2, player_bowlers)

# Print the top 4 bowlers
print(top_bowlers_teams[, c("Bowler", "Total_O", "Total_R", "Total_W", "Average_Econ", "Total_Dots", "Team", "Role")])


                     
   

############################ Wicket_Keeper

# Function to select top wicket keeper from clustering results for specified teams
select_top_keeper_teams <- function(cluster_tables, team1_name, team2_name) {
  # Combine all cluster tables into one dataframe
  all_keepers <- do.call(rbind, cluster_tables)
  
  # Filter keepers for specified teams
  team1_keepers <- all_keepers[all_keepers$Team == team1_name, ]
  team2_keepers <- all_keepers[all_keepers$Team == team2_name, ]
  
  # Combine data for both teams
  all_keepers <- rbind(team1_keepers, team2_keepers)
  
  # Select the top wicket keeper based on Total_R (total runs)
  top_keeper <- all_keepers[which.max(all_keepers$Total_R), ]
  
  return(top_keeper)
}

# Load necessary library
if (!require("cluster")) install.packages("cluster", dependencies = TRUE)
library(cluster)

# Function to perform clustering for keepers from two teams playing against each other
perform_match_clustering_keepers <- function(team1_name, team2_name, keeper_data) {
  # Subset data for the specified teams
  team1_keepers <- keeper_data[keeper_data$Team == team1_name, ]
  team2_keepers <- keeper_data[keeper_data$Team == team2_name, ]
  
  # Check if both teams have keepers
  if (nrow(team1_keepers) == 0 | nrow(team2_keepers) == 0) {
    cat("No keepers found for", team1_name, "and/or", team2_name, "\n")
    return(NULL)
  }
  
  # Combine data for both teams
  combined_keepers_data <- rbind(team1_keepers, team2_keepers)
  
  # Select relevant columns for clustering
  clustering_keepers_data <- combined_keepers_data[, c("Total_R", "Total_B", "Total_4s", "Total_6s", "SR")]
  
  # Determine the number of clusters 
  num_keepers <- nrow(clustering_keepers_data)
  k <- ifelse(num_keepers < 3, 2, min(3, num_keepers %/% 2))
  
  
  # Perform K-means clustering
  set.seed(123) # for reproducibility
  km <- kmeans(clustering_keepers_data, centers = k)
  
  # Add cluster labels to original data
  combined_keepers_data$Cluster <- km$cluster
  
  # Create separate tables for each cluster
  cluster_tables <- lapply(unique(km$cluster), function(cluster_num) {
    cluster_keepers_data <- combined_keepers_data[combined_keepers_data$Cluster == cluster_num, ]
    return(cluster_keepers_data)
  })
  
  # Print tables for each cluster
  cat("Match Clustering for Keepers from Teams:", team1_name, "vs", team2_name, "\n")
  for (i in seq_along(cluster_tables)) {
    cat("Cluster", i, ":\n")
    print(cluster_tables[[i]][, c("Batter", "Total_R", "Total_B", "Total_4s", "Total_6s", "SR", "Cluster", "Team", "Role")])
    cat("\n")
  }
  
  # Call function to select top wicket keeper from clusters
  select_top_keeper_teams(cluster_tables, team1_name, team2_name)
}

# Assuming you have the team names for the match and player data
team1 <- "MI"
team2 <- "CSK"

# Assuming you have the player data named Keeper

# Apply clustering for the match and select top wicket keeper
top_keeper <- perform_match_clustering_keepers(team1, team2, Keeper)

# Print the top wicket keeper
print(top_keeper[, c("Batter", "Total_R", "Total_B", "Total_4s", "Total_6s", "SR", "Team", "Role")])




############ clustering all-rounder Bastman
# Function to select top all-rounder batsmen from clustering results for specified teams
select_top_all_rounder_batsmen_teams <- function(cluster_batsmen_tables, team1_name, team2_name) {
  # Combine all cluster tables into one dataframe
  all_batsmen <- do.call(rbind, cluster_batsmen_tables)
  
  # Filter batsmen for specified teams
  team1_batsmen <- all_batsmen[all_batsmen$Team == team1_name, ]
  team2_batsmen <- all_batsmen[all_batsmen$Team == team2_name, ]
  
  # Combine data for both teams
  all_batsmen <- rbind(team1_batsmen, team2_batsmen)
  
  # Select the top 3 all-rounder batsmen based on Total_R (total runs)
  top_3_batsmen <- all_batsmen[order(-all_batsmen$Total_R), ][1:3, ]
  
  return(top_3_batsmen)
}

# Load necessary library
if (!require("cluster")) install.packages("cluster", dependencies = TRUE)
library(cluster)

# Function to perform clustering for all-rounder batsmen from two teams playing against each other
perform_match_clustering_all_rounder_batsmen <- function(team1_name, team2_name, all_rounder_batsmen_data) {
  # Subset data for the specified teams
  team1_batsmen <- all_rounder_batsmen_data[all_rounder_batsmen_data$Team == team1_name, ]
  team2_batsmen <- all_rounder_batsmen_data[all_rounder_batsmen_data$Team == team2_name, ]
  
  # Check if both teams have all-rounder batsmen
  if (nrow(team1_batsmen) == 0 | nrow(team2_batsmen) == 0) {
    cat("No all-rounder batsmen found for", team1_name, "and/or", team2_name, "\n")
    return(NULL)
  }
  
  # Combine data for both teams
  combined_batsmen_data <- rbind(team1_batsmen, team2_batsmen)
  
  # Select relevant columns for clustering
  clustering_batsmen_data <- combined_batsmen_data[, c("Total_R", "Total_B", "Total_4s", "Total_6s", "SR")]
  
  # Determine the number of clusters
  num_batsmen <- nrow(clustering_batsmen_data)
  k <- ifelse(num_batsmen < 3, 1, min(3, num_batsmen %/% 2))
  
  # Perform K-means clustering
  set.seed(123) # for reproducibility
  km <- kmeans(clustering_batsmen_data, centers = k)
  
  # Add cluster labels to original data
  combined_batsmen_data$Cluster <- km$cluster
  
  # Create separate tables for each cluster
  cluster_batsmen_tables <- lapply(unique(km$cluster), function(cluster_num) {
    cluster_batsmen_data <- combined_batsmen_data[combined_batsmen_data$Cluster == cluster_num, ]
    return(cluster_batsmen_data)
  })
  
  # Print tables for each cluster
  cat("Match Clustering for All-Rounder Batsmen from Teams:", team1_name, "vs", team2_name, "\n")
  for (i in seq_along(cluster_batsmen_tables)) {
    cat("Cluster", i, ":\n")
    print(cluster_batsmen_tables[[i]][, c("Batter", "Total_R", "Total_B", "Total_4s", "Total_6s", "SR", "Cluster", "Team", "Role")])
    cat("\n")
  }
  
  # Call function to select top 3 all-rounder batsmen from clusters
  select_top_all_rounder_batsmen_teams(cluster_batsmen_tables, team1_name, team2_name)
}

# Assuming you have the team names for the match and player data
team1 <- "MI"
team2 <- "CSK"

# Assuming you have the player data named all_rounders_batsman

# Apply clustering for the match and select top 3 all-rounder batsmen
top_3_all_rounder_batsmen <- perform_match_clustering_all_rounder_batsmen(team1, team2, all_rounders_batsman)

# Print the top 3 all-rounder batsmen
print(top_3_all_rounder_batsmen[, c("Batter", "Total_R", "Total_B", "Total_4s", "Total_6s", "SR", "Team", "Role")])



############################################ All-rounder Bowlers


# Function to select top all-rounder bowlers from clustering results for specified teams
select_top_all_rounder_bowlers_teams <- function(cluster_bowlers_tables, team1_name, team2_name) {
  # Combine all cluster tables into one dataframe
  all_bowlers <- do.call(rbind, cluster_bowlers_tables)
  
  # Filter bowlers for specified teams
  team1_bowlers <- all_bowlers[all_bowlers$Team == team1_name, ]
  team2_bowlers <- all_bowlers[all_bowlers$Team == team2_name, ]
  
  # Combine data for both teams
  all_bowlers <- rbind(team1_bowlers, team2_bowlers)
  
  # Select the top 3 all-rounder bowlers based on Total_W (total wickets)
  top_3_bowlers <- all_bowlers[order(-all_bowlers$Total_W), ][1:3, ]
  
  return(top_3_bowlers)
}

# Function to perform clustering for all-rounder bowlers from two teams playing against each other
perform_match_clustering_all_rounder_bowlers <- function(team1_name, team2_name, all_rounder_bowlers_data) {
  # Subset data for the specified teams
  team1_bowlers <- all_rounder_bowlers_data[all_rounder_bowlers_data$Team == team1_name, ]
  team2_bowlers <- all_rounder_bowlers_data[all_rounder_bowlers_data$Team == team2_name, ]
  
  # Check if both teams have all-rounder bowlers
  if (nrow(team1_bowlers) == 0 | nrow(team2_bowlers) == 0) {
    cat("No all-rounder bowlers found for", team1_name, "and/or", team2_name, "\n")
    return(NULL)
  }
  
  # Combine data for both teams
  combined_bowlers_data <- rbind(team1_bowlers, team2_bowlers)
  
  # Select relevant columns for clustering
  clustering_bowlers_data <- combined_bowlers_data[, c("Total_O", "Total_R", "Total_W", "Average_Econ", "Total_Dots")]
  
  # Determine the number of clusters
  num_bowlers <- nrow(clustering_bowlers_data)
  k <- ifelse(num_bowlers < 3, 1, min(3, num_bowlers %/% 2))
  
  # Perform K-means clustering
  set.seed(123) # for reproducibility
  km <- kmeans(clustering_bowlers_data, centers = k)
  
  # Add cluster labels to original data
  combined_bowlers_data$Cluster <- km$cluster
  
  # Create separate tables for each cluster
  cluster_bowlers_tables <- lapply(unique(km$cluster), function(cluster_num) {
    cluster_bowlers_data <- combined_bowlers_data[combined_bowlers_data$Cluster == cluster_num, ]
    return(cluster_bowlers_data)
  })
  
  # Print tables for each cluster
  cat("Match Clustering for All-Rounder Bowlers from Teams:", team1_name, "vs", team2_name, "\n")
  for (i in seq_along(cluster_bowlers_tables)) {
    cat("Cluster", i, ":\n")
    print(cluster_bowlers_tables[[i]][, c("Bowler", "Total_O", "Total_R", "Total_W", "Average_Econ", "Total_Dots", "Cluster", "Team", "Role")])
    cat("\n")
  }
  
  # Call function to select top 3 all-rounder bowlers from clusters
  select_top_all_rounder_bowlers_teams(cluster_bowlers_tables, team1_name, team2_name)
}

# Assuming you have the team names for the match and player data
team1 <- "MI"
team2 <- "CSK"

# Assuming you have the player data named all_rounders_bowlers

# Apply clustering for the match and select top 3 all-rounder bowlers
top_3_all_rounder_bowlers <- perform_match_clustering_all_rounder_bowlers(team1, team2, all_rounders_bowlers)

# Print the top 3 all-rounder bowlers
print(top_3_all_rounder_bowlers[, c("Bowler", "Total_O", "Total_R", "Total_W", "Average_Econ", "Total_Dots", "Team", "Role")])




# Print the top 4 batsmen
top_batsmen_teams <- perform_match_clustering_batsman(all_teams[i], all_teams[i+1], player_batsman)
print(top_batsmen_teams[, c("Total_R", "Total_B", "Total_4s", "Total_6s", "SR", "Cluster", "Team", "Role")])


# Print the top 4 bowlers
top_bowlers_teams <- perform_match_clustering_bowlers(all_teams[i], all_teams[i+1], player_bowlers)
print(top_bowlers_teams[, c("Bowler", "Total_O", "Total_R", "Total_W", "Average_Econ", "Total_Dots", "Team", "Role")])


# Print the top wicket keeper
top_keeper <- perform_match_clustering_keepers(all_teams[i], all_teams[i+1], Keeper)
print(top_keeper[, c("Batter", "Total_R", "Total_B", "Total_4s", "Total_6s", "SR", "Team", "Role")])



# Print the top 3 all-rounder batsmen
top_3_all_rounder_batsmen <- perform_match_clustering_all_rounder_batsmen(all_teams[i], all_teams[i+1], all_rounders_batsman)

print(top_3_all_rounder_batsmen[, c("Batter", "Total_R", "Total_B", "Total_4s", "Total_6s", "SR", "Team", "Role")])


# Print the top 3 all-rounder bowlers
top_3_all_rounder_bowlers <- perform_match_clustering_all_rounder_bowlers(all_teams[i], all_teams[i+1], all_rounders_bowlers)

print(top_3_all_rounder_bowlers[, c("Bowler", "Total_O", "Total_R", "Total_W", "Average_Econ", "Total_Dots", "Team", "Role")])














################################## images 
library(ggplot2)
library(gridExtra)
library(cowplot)

# Function to save a table as a PNG image
save_table_image <- function(df, file_name, title) {
  if (nrow(df) > 0) {
    table_plot <- ggplot() +
      theme_void() +
      annotation_custom(tableGrob(df, rows = NULL, theme = ttheme_minimal()), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      ggtitle(title)
    
    ggsave(file_name, plot = table_plot, width = 8, height = 4)
  } else {
    message("Data frame is empty. Skipping: ", file_name)
  }
}

# Sample teams list
all_teams <- c("CSK", "PBKS", "LSG", "DC", "SRH", "RCB", "MI", "KKR", "GT", "RR")

for (i in 1:(length(all_teams) - 1)) {
  team1 <- all_teams[i]
  team2 <- all_teams[i + 1]
  
  # Perform clustering and get top players
  top_batsmen_teams <- perform_match_clustering_batsman(team1, team2, player_batsman)
  top_bowlers_teams <- perform_match_clustering_bowlers(team1, team2, player_bowlers)
  top_keeper <- perform_match_clustering_keepers(team1, team2, Keeper)
  top_3_all_rounder_batsmen <- perform_match_clustering_all_rounder_batsmen(team1, team2, all_rounders_batsman)
  top_3_all_rounder_bowlers <- perform_match_clustering_all_rounder_bowlers(team1, team2, all_rounders_bowlers)
  
  # Ensure data frames
  top_batsmen_teams <- as.data.frame(top_batsmen_teams)
  top_bowlers_teams <- as.data.frame(top_bowlers_teams)
  top_keeper <- as.data.frame(top_keeper)
  top_3_all_rounder_batsmen <- as.data.frame(top_3_all_rounder_batsmen)
  top_3_all_rounder_bowlers <- as.data.frame(top_3_all_rounder_bowlers)
  
  # Check and print column names for debugging
  cat("Columns in top_batsmen_teams:", colnames(top_batsmen_teams), "\n")
  cat("Columns in top_bowlers_teams:", colnames(top_bowlers_teams), "\n")
  cat("Columns in top_keeper:", colnames(top_keeper), "\n")
  cat("Columns in top_3_all_rounder_batsmen:", colnames(top_3_all_rounder_batsmen), "\n")
  cat("Columns in top_3_all_rounder_bowlers:", colnames(top_3_all_rounder_bowlers), "\n")
  
  # Save table images
  save_table_image(top_batsmen_teams, paste0("top_batsmen_", team1, "_vs_", team2, ".png"), paste("Top 4 Batsmen for", team1, "vs", team2))
  save_table_image(top_bowlers_teams, paste0("top_bowlers_", team1, "_vs_", team2, ".png"), paste("Top 4 Bowlers for", team1, "vs", team2))
  save_table_image(top_keeper, paste0("top_keeper_", team1, "_vs_", team2, ".png"), paste("Top Wicket Keeper for", team1, "vs", team2))
  save_table_image(top_3_all_rounder_batsmen, paste0("top_3_all_rounder_batsmen_", team1, "_vs_", team2, ".png"), paste("Top 3 All-Rounder Batsmen for", team1, "vs", team2))
  save_table_image(top_3_all_rounder_bowlers, paste0("top_3_all_rounder_bowlers_", team1, "_vs_", team2, ".png"), paste("Top 3 All-Rounder Bowlers for", team1, "vs", team2))
}
