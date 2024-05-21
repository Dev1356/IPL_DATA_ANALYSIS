library(topsis)
library(ggplot2)

# Load the dataset
data <- readxl::read_xlsx("C:\\Users\\Shiv\\Documents\\Final_Year_Project\\records_players.xlsx")
names(data)
data=data.frame(data)
is.data.frame(data)
# Define weights for batsmen, bowlers, all-rounders, wicket-keepers, and best fielders separately
batsman_weights <- c(
  runs = 0.3,
  batting_avg = 0.1,
  batting_strike_rate = 0.2,
  boundaries_percent = 0.1,
  matches=0.1,
  balls_faced=0.2
)

bowler_weights <- c(
  wickets = 0.3,
  bowling_avg = 0.3,
  bowling_economy = 0.2,
  bowling_strike_rate = 0.1,
  matches=0.1
)

allrounder_weights <- c(
  runs = 0.07,
  wickets = 0.13,
  batting_avg = 0.1,
  bowling_avg = 0.1,
  batting_strike_rate = 0.2,
  bowling_economy = 0.2,
  boundaries_percent=0.2
)

wicketkeeper_weights <- c(
  runs = 0.2,
  batting_avg = 0.1,
  batting_strike_rate = 0.1,
  catches = 0.2,
  stumpings = 0.3,
  matches=0.1
)

fielder_weights <- c(
  catches = 0.3,
  matches= 0.3,
  runs=0.2,
  wickets=0.2
)

i1=c("+","+","+","+","+","+")
i2=c("+","-","-","-","+")
i3=c("+","+","+","-","+","-","+")
i4=c("+","+","+","+","+","+")
i5=c("+","+","+","+")


# Perform TOPSIS analysis for batsmen
d1=as.matrix(data[, c("runs", "batting_avg", "batting_strike_rate", "boundaries_percent","matches","balls_faced")])
batsman_topsis <- topsis(d1, batsman_weights,i1)
batsman_topsis$Player_Type <- data[,"player"]
View(batsman_topsis)

# Perform TOPSIS analysis for bowlers
d2=as.matrix(data[, c("wickets", "bowling_avg", "bowling_economy", "bowling_strike_rate","matches")])
bowler_topsis <- topsis(d2, bowler_weights,i2)
bowler_topsis$Player_Type <-data[,"player"]
View(bowler_topsis)

# Perform TOPSIS analysis for all-rounders
d3=as.matrix(data[, c("runs", "wickets", "batting_avg", "bowling_avg", "batting_strike_rate", "bowling_economy","boundaries_percent")])
allrounder_topsis <- topsis(d3, allrounder_weights,i3)
allrounder_topsis$Player_Type <- data[,"player"]
View(allrounder_topsis)

# Perform TOPSIS analysis for wicket-keepers
d4=as.matrix(data[, c("runs", "batting_avg", "batting_strike_rate", "catches", "stumpings","matches")])
wicketkeeper_topsis <- topsis(d4, wicketkeeper_weights,i4)
wicketkeeper_topsis$Player_Type <- data[,"player"]
View(wicketkeeper_topsis)

# Perform TOPSIS analysis for best fielders
d5=as.matrix(data[, c("catches", "matches", "runs", "wickets")])
fielder_topsis <- topsis(d5, fielder_weights,i5)
fielder_topsis$Player_Type <- data[,"player"]
View(fielder_topsis)

# Sort the results by TOPSIS score for each category
batsman_sorted <- batsman_topsis[order(batsman_topsis$rank, decreasing = F), ]
bowler_sorted <- bowler_topsis[order(bowler_topsis$rank, decreasing = F), ]
allrounder_sorted <- allrounder_topsis[order(allrounder_topsis$rank, decreasing = F), ]
wicketkeeper_sorted <- wicketkeeper_topsis[order(wicketkeeper_topsis$rank, decreasing = F), ]
fielder_sorted <- fielder_topsis[order(fielder_topsis$rank, decreasing = F), ]

# Extract the top 10 players for each category
top_batsmen <- batsman_sorted[1:10, c("Player_Type", "rank")]
top_bowlers <- bowler_sorted[1:10, c("Player_Type", "rank")]
top_allrounders <- allrounder_sorted[1:10, c("Player_Type", "rank")]
top_wicketkeepers <- wicketkeeper_sorted[1:10, c("Player_Type", "rank")]
top_fielders <- fielder_sorted[1:10, c("Player_Type", "rank")]

library(writexl)
write_xlsx()
# Create tables
library(officer)
library(flextable)

# Function to create flextable
create_flextable <- function(data) {
  ft <- flextable(data)
  ft <- flextable::set_table_properties(ft, layout = "autofit")
  return(ft)
}

# Create flextables for each category

ft_batsmen <- create_flextable(top_batsmen)
ft_bowlers <- create_flextable(top_bowlers)
ft_allrounders <- create_flextable(top_allrounders)
ft_wicketkeepers <- create_flextable(top_wicketkeepers)
ft_fielders <- create_flextable(top_fielders)

# Create a Word document
doc <- read_docx()
doc <- doc %>% 
  body_add_par("Top 10 Batsmen:", style = "heading 1") %>% 
  body_add_flextable(ft_batsmen) %>% 
  body_add_par("Top 10 Bowlers:", style = "heading 1") %>% 
  body_add_flextable(ft_bowlers) %>% 
  body_add_par("Top 10 All-rounders:", style = "heading 1") %>% 
  body_add_flextable(ft_allrounders) %>% 
  body_add_par("Top 10 Wicket-keepers:", style = "heading 1") %>% 
  body_add_flextable(ft_wicketkeepers) %>% 
  body_add_par("Top 10 Fielders:", style = "heading 1") %>% 
  body_add_flextable(ft_fielders)

# Save the Word document
print(doc, target = "Top_Players.docx")


# Ensure each subset is a data frame
top_batsmen <- as.data.frame(top_batsmen)
top_bowlers <- as.data.frame(top_bowlers)
top_allrounders <- as.data.frame(top_allrounders)
top_wicketkeepers <- as.data.frame(top_wicketkeepers)
top_fielders <- as.data.frame(top_fielders)

# Create LaTeX tables
library(xtable)

# Function to create LaTeX table from a data frame
create_latex_table <- function(data, caption) {
  print(xtable(data, caption = caption), include.rownames = FALSE, comment = FALSE)
}

# Open a connection to write to a .tex file
sink("Top_Players_Tables.tex")

# Write LaTeX tables for each category
cat("\\section*{Top 10 Batsmen}\n")
create_latex_table(top_batsmen, "Top 10 Batsmen")

cat("\\section*{Top 10 Bowlers}\n")
create_latex_table(top_bowlers, "Top 10 Bowlers")

cat("\\section*{Top 10 All-rounders}\n")
create_latex_table(top_allrounders, "Top 10 All-rounders")

cat("\\section*{Top 10 Wicket-keepers}\n")
create_latex_table(top_wicketkeepers, "Top 10 Wicket-keepers")

cat("\\section*{Top 10 Fielders}\n")
create_latex_table(top_fielders, "Top 10 Fielders")

# Close the connection
sink()


