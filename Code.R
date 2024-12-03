# INTRODUCTION----
# Clear Environment
rm(list = ls())

# Setting working directory
setwd(choose.dir())

# Display numbers in natural form
options(scipen=999)

# Set seed
set.seed(467)

# Install packages & Load libraries
install.packages(c("ggpubr", "ranger", "Rborist", "gridGraphics", "SHAPforxgboost", "DALEX", "pdp", "DALEXtra", "DiagrammeR", "NeuralNetTools"))
library(parallel)
library(neuralnet)
library(foreach)
library(NeuralNetTools)
library(nnet)
library(DiagrammeR)
library(DALEX)
library(pdp)
library(ROCR)
library(pROC)
library(DALEXtra)
library(reshape2)
library(doParallel)
library(cowplot)
library(gridGraphics)
library(tidyverse)
library(gridExtra)
library(grid)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(dplyr)
library(readxl)
library(mice)
library(ggpubr)
library(caret)
library(viridis)
library(stats)
library(glmnet)
library(car)
library(ModelMetrics)
library(randomForest)
library(ranger)
library(Rborist)
library(xgboost)
library(SHAPforxgboost)
library(iml)


# FUNCTIONS----
# Function to check for missing values and white spaces in each column
missing_values <- function(data) {
  
  # Replace missing values with NA
  data[data == "" | data == " "] <- NA
  
  # Number of NA values in each column
  na_values <- colSums(is.na(data))
  
  # Number of white space values in each column
  whitespace_values <- sapply(data, function(x) sum(grepl("^\\s*$", x)))
  
  # Determine data types of each column
  data_types <- sapply(data, function(x) class(x)[1])
  
  # Combine results into a data frame
  quality_summary <- data.frame(
    naValues = na_values,
    Data_Type = data_types,
    Whitespaces = whitespace_values
  )
  
  # Add TOTAL column
  quality_summary$Missing <- quality_summary$naValues + quality_summary$Whitespaces
  
  # Filter rows where TOTAL column value is not equal to 0
  quality_summary <- quality_summary[quality_summary$Missing != 0, ]
  
  if (nrow(quality_summary) == 0) {
    smiley <- "\U263A"  # Unicode character for smiley face
    return(paste0("There are no missing values", " ", smiley))
  }
  
  # Sort the table based on the TOTAL column in descending order
  quality_summary <- quality_summary[order(-quality_summary$Missing), ]
  output <- subset(quality_summary, select = -c(naValues, Whitespaces))
  return(output)
}

# Define a custom function to replace accented characters with ASCII encoding
replace_accented_chars <- function(text) {
  cleaned_text <- iconv(text, to = "ASCII//TRANSLIT")
  return(cleaned_text)
}

# Function to determine franchise based on title
assign_franchise <- function(name) {
  for (franchise in names(franchises)) {
    keywords <- franchises[[franchise]]
    for (keyword in keywords) {
      if (grepl(keyword, name, ignore.case = TRUE)) {
        return(franchise)
      }
    }
  }
  return(NA)
}


count_occurrences <- function(data, column_name) {
  # Check if the column exists in the data frame
  if (!column_name %in% colnames(data)) {
    stop("Column does not exist in the data frame.")
  }
  
  # Count the occurrences of each unique value in the specified column
  counts <- table(data[[column_name]])
  
  # Convert the table to a data frame
  counts_df <- as.data.frame(counts)
  
  # Rename the column
  colnames(counts_df) <- c("Value", "Count")
  
  # Sort the data frame by Count in descending order
  counts_df <- counts_df[order(-counts_df$Count), ]
  
  # Return the sorted data frame
  return(counts_df)
}

# Function to impute missing values in Rating column based on Name or Genre
impute_rating <- function(data) {
  # For each missing Rating value
  for (i in which(is.na(data$Rating))) {
    # Get the name and genre of the game
    name <- data$Name[i]
    genre <- data$Genre[i]
    
    # Find all rows with the same name or genre
    similar_games <- data[data$Name == name | data$Genre == genre, ]
    
    # Exclude the current row (to avoid using the same game's rating)
    similar_games <- similar_games[-i, ]
    
    # If there are similar games with ratings
    if (nrow(similar_games) > 0 && any(!is.na(similar_games$Rating))) {
      # Impute the mode of ratings of similar games
      data$Rating[i] <- names(which.max(table(similar_games$Rating)))
    } else {
      # If no similar games with ratings, impute with the overall mode of ratings
      data$Rating[i] <- names(which.max(table(data$Rating)))
    }
  }
  return(data)
}

# Function to perform min-max scaling
min_max_scale <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# DATA UNDERSTANDING----

# Read the data
df <- read.csv("Video_Games_Data.csv", header = TRUE)

# Check structure of the data
str(df)

# View summary of the data
summary(df)
head(df)

# Understanding all variables

unique(df$Name)
df$Name <- replace_accented_chars(df$Name)
summary(df$Name)
class(df$Name)

unique(df$Platform)
summary(df$Platform)
class(df$Platform)

unique(df$Year_of_Release)
df$Year_of_Release <- na_if(df$Year_of_Release, "N/A")
summary(df$Year_of_Release)
class(df$Year_of_Release)

unique(df$Genre)
summary(df$Genre)
class(df$Genre)

unique(df$Publisher)
summary(df$Publisher)
class(df$Publisher)

summary(df$NA_Sales)

summary(df$EU_Sales)

summary(df$JP_Sales)

summary(df$Other_Sales)

summary(df$Global_Sales)

summary(df$Critic_Score)

summary(df$Critic_Count)

unique(df$User_Score)
df$User_Score <- ifelse(df$User_Score == "tbd", NA, df$User_Score)
df$User_Score <- as.numeric(df$User_Score)
summary(df$User_Score)
df <- df %>% filter(User_Score > 0 | is.na(User_Score)) # Filter out scores <=0

summary(df$User_Count)

unique(df$Developer)
summary(df$Developer)
class(df$Developer)

unique(df$Rating)
df <- df %>%
  mutate(Rating = ifelse(Rating == "K-A", "E", Rating))
summary(df$Rating)
class(df$Rating)

# Summary of the dataset
summary(df)

# Check for missing values, blanks and white spaces
missing_summary <- missing_values(df)
missing_summary
missing_summary$Percentage <- round(((missing_summary$Missing / nrow(df)) * 100),2)

# Create a horizontal bar plot of variables with missing values
ggplot(missing_summary, aes(x = Missing, y = reorder(row.names(missing_summary), Missing))) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  geom_text(aes(label = ifelse(Missing > 0, 
                               paste0(Missing, " (", Percentage, "%)"), 
                               ""),
                hjust = ifelse(Percentage < 25, -0.1, 1.1),
                #color = ifelse(Percentage < 25, "black", "white")
                ), size = 3) + 
  labs(title = "Variables with Count of Missing Values",
       x = "Total Missing Values",
       y = "Variable") +
  theme_minimal() +
  coord_cartesian(clip = "off")

# Check for duplicates in dataset
duplicates <- df[duplicated(df),]
duplicates

# Create a copy of the data
data <- df

# Impute missing values for user & critc scores with mean
data$User_Score[is.na(data$User_Score)] <- mean(data$User_Score, na.rm = TRUE)
data$User_Count[is.na(data$User_Count)] <- mean(data$User_Count, na.rm = TRUE)
data$Critic_Score[is.na(data$Critic_Score)] <- mean(data$Critic_Score, na.rm = TRUE)
data$Critic_Count[is.na(data$Critic_Count)] <- mean(data$Critic_Count, na.rm = TRUE)
str(data)
missing_values(data)

# Check for correlation of numeric variables
numeric_data <- data[, sapply(data, is.numeric)]
missing_values(numeric_data)
ggcorrplot(cor(numeric_data)) + 
  labs(title = "Correlation Plot of Numerical Variables") + 
  theme(plot.title = element_text(hjust = 0, size = 10, face = "bold")) + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, limits = c(-1, 1), name = "Correlation")


# save file
write.csv(df, file = "Data_Understanding.csv", row.names = FALSE)

# HANDLING MISSING VALUES----

# Read data
df <- read.csv("Data_Understanding.csv", header = TRUE)
df[df == "" | df == " "] <- NA
missing_values(df)
str(df)

# Handling missing values in Name & Genre by removing the rows as percentage is less
df <- df[complete.cases(df$Name), ]
df <- df[complete.cases(df$Genre), ]
missing_values(df)

# Replace missing Developer with Unknown
df <- df %>% mutate(Developer = ifelse(is.na(Developer),"Unknown", Developer))
missing_values(df)

# Impute year of release manually from https://www.vgchartz.com/games/games.php
str(df)
df$Year_of_Release <- as.numeric(df$Year_of_Release)

# Read the Excel file containing the missing release years
missing_years <- read_excel("Release_Year.xlsx")
str(missing_years)

# Ensure column names match between both datasets
colnames(missing_years)
colnames(df)

# Merge the data frames based on index
merged_df <- merge(df, missing_years[, c("Index", "Year_of_Release")], by = "Index", all.x = TRUE)
merged_df$Year_of_Release.x[is.na(merged_df$Year_of_Release.x)] <- merged_df$Year_of_Release.y[is.na(merged_df$Year_of_Release.x)]
merged_df <- merged_df[, -which(names(merged_df) == "Year_of_Release.y")]
names(merged_df)[names(merged_df) == "Year_of_Release.x"] <- "Year_of_Release"
missing_values(merged_df)

# Impute missing values in Rating
df1 <- impute_rating(merged_df)
df2 <- merged_df %>% mutate(Rating = ifelse(is.na(Rating),"Unknown", Rating))
missing_values(df1)
missing_values(df2)

ggplot(df1, aes(x = Critic_Score)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Critic Score", x = "Critic Score", y = "Frequency") +
  theme_minimal()


# save file
write.csv(df1, file = "Cat_Imputation_1.csv", row.names = FALSE)
write.csv(df2, file = "Cat_Imputation_2.csv", row.names = FALSE)

# Perform multiple imputation on Numeric variables on df1 dataset
str(df1)
datami <- mice(subset(df1, select = c('Year_of_Release', 'NA_Sales', 'EU_Sales',
                                            'JP_Sales', 'Other_Sales', 'Global_Sales',
                                            'Critic_Score', 'Critic_Count', 'User_Score', 'User_Count')), 
               method = "cart", m = 5, maxit = 30, seed = 467)

# Pooling imputed datasets
data_mi <- complete(datami)
missing_values(data_mi)

summary(df2$Critic_Score)
summary(data_mi$Critic_Score)

summary(df2$Critic_Count)
summary(data_mi$Critic_Count)

summary(df2$User_Score)
summary(data_mi$User_Score)

summary(df2$User_Count)
summary(data_mi$User_Count)

# Creating imputed dataframe
df_mi <- cbind(df1[!names(df1) %in% names(data_mi)], data_mi)
missing_values(df_mi)

# Perform imputation on Numeric variables on df2 dataset by adding dummy columns
df2$Critic_Missing <- ifelse(is.na(df2$Critic_Score), 1, 0)
df2$User_Missing <- ifelse(is.na(df2$User_Score), 1, 0)
missing_values(df2)

# save file
write.csv(df_mi, file = "Imputed_Data_1.csv", row.names = FALSE)
write.csv(df2, file = "Imputed_Data_2.csv", row.names = FALSE)

# NEW COLUMNS----
df1 <- read.csv("Imputed_Data_1.csv", header = TRUE)
df2 <- read.csv("Imputed_Data_2.csv", header = TRUE)

## FRANCHISE----
# Define franchises and their associated keywords
franchises <- list("Age of Empires" = c("Age of Empires"), "Animal Crossing" = c("Animal Crossing"), "Batman" = c("Batman"), "Battlefield" = c("Battlefield"),   "Bejeweled" = c("Bejeweled"), "BioShock" = c("BioShock"), "Borderlands" = c("Borderlands"), "Brain Age" = c("Brain Age"),   "Call of Duty" = c("Call of Duty"), "Castlevania" = c("Castlevania"), "Civilization" = c("Civilization"), "Command & Conquer" = c("Command & Conquer"),   "Counter-Strike" = c("Counter-Strike"), "Crash Bandicoot" = c("Crash Bandicoot"), "Dark Souls" = c("Dark Souls"), "Devil May Cry" = c("Devil May Cry"),   "Diablo" = c("Diablo"), "Dragon Ball" = c("Dragon Ball"), "Dragon Quest" = c("Dragon Quest"), "Dying Light" = c("Dying Light"),   "Dynasty Warriors" = c("Dynasty Warriors"), "The Elder Scrolls" = c("The Elder Scrolls"), "Fallout" = c("Fallout"), "Far Cry" = c("Far Cry"),   "FIFA" = c("FIFA"), "Final Fantasy" = c("Final Fantasy"), "Football Manager" = c("Football Manager"), "Frogger" = c("Frogger"),   "Gears of War" = c("Gears of War"), "God of War" = c("God of War"), "Gran Turismo" = c("Gran Turismo"), "Grand Theft Auto" = c("Grand Theft Auto"),   "Guitar Hero" = c("Guitar Hero"), "Gundam" = c("Gundam"), "Half-Life" = c("Half-Life"), "Halo" = c("Halo"), "Horizon" = c("Horizon"),   "Imagine" = c("Imagine"), "James Bond" = c("James Bond", "007:", "GoldenEye"), "J.B. Harold" = c("J.B. Harold"), "Just Dance" = c("Just Dance"),   "Kingdom Hearts" = c("Kingdom Hearts"), "Kirby" = c("Kirby"), "The Last of Us" = c("The Last of Us"), "The Legend of Zelda" = c("The Legend of Zelda"),   "Lego" = c("Lego"), "Lemmings" = c("Lemmings"), "Madden NFL" = c("Madden NFL"), "Mario" = c("Mario"), "Mass Effect" = c("Mass Effect"),   "Medal of Honor" = c("Medal of Honor"), "Mega Man" = c("Mega Man"), "Megami Tensei" = c("Megami Tensei"), "Metal Gear" = c("Metal Gear"),   "Metroid" = c("Metroid"),   "Microsoft Flight Simulator" = c("Microsoft Flight Simulator"),   "Minecraft" = c("Minecraft"),   "Monster Hunter" = c("Monster Hunter"),   "Mortal Kombat" = c("Mortal Kombat"),   "Mystery Dungeon" = c("Mystery Dungeon"),   "NBA 2K" = c("NBA 2K"),   "NBA Live" = c("NBA Live"),   "Need for Speed" = c("Need for Speed", "NFS"),   "Nintendogs" = c("Nintendogs"),   "The Oregon Trail" = c("The Oregon Trail"),   "Pac-Man" = c("Pac-Man"),   "Petz" = c("Petz"),   "Pokemon" = c("Pokemon"),   "Power Pros" = c("Power Pros"),   "Prince of Persia" = c("Prince of Persia"),   "Ratchet & Clank" = c("Ratchet & Clank"),   "Rayman" = c("Rayman"),   "Red Dead" = c("Red Dead"),   "Resident Evil" = c("Resident Evil"),   "Saints Row" = c("Saints Row"),   "SimCity" = c("SimCity"),   "Simple" = c("Simple"),   "The Sims" = c("The Sims"),   "SingStar" = c("SingStar"),   "Sonic the Hedgehog" = c("Sonic the Hedgehog"),   "Spider-Man" = c("Spider-Man"),   "Splatoon" = c("Splatoon"),   "SpongeBob SquarePants" = c("SpongeBob SquarePants"),   "Spyro" = c("Spyro"),   "Star Wars" = c("Star Wars"),   "Street Fighter" = c("Street Fighter"),   "Super Robot Wars" = c("Super Robot Wars"),   "Super Smash Bros." = c("Super Smash Bros."),   "Tales" = c("Tales"),   "Tekken" = c("Tekken"),   "Tetris" = c("Tetris"),   "Tom Clancy's" = c("Tom Clancy's"),   "Tomb Raider" = c("Tomb Raider"),   "Tony Hawk's" = c("Tony Hawk's"),   "Total War" = c("Total War"),   "Uncharted" = c("Uncharted"),   "The Walking Dead" = c("The Walking Dead"),   "Watch Dogs" = c("Watch Dogs"),   "Wii" = c("Wii"),   "The Witcher" = c("The Witcher"),   "Worms" = c("Worms"),   "WWE 2K" = c("WWE 2K"),   "Yu-Gi-Oh!" = c("Yu-Gi-Oh!"),   "1942" = c("1942"),   ".hack" = c(".hack/"),   "1080° Snowboarding" = c("1080° Snowboarding"),   "3D Ultra Minigolf" = c("3D Ultra Minigolf"),   "3-D Ultra Pinball" = c("3-D Ultra Pinball"),   "7th Dragon" = c("7th Dragon"),   "A Boy and His Blob" = c("A Boy and His Blob"),   "Ace Attorney" = c("Ace Attorney"),   "Ace Combat" = c("Ace Combat"),   "ActRaiser" = c("ActRaiser"),   "Adventure Island" = c("Adventure Island"),   
                   "Adventures of Lolo" = c("Adventures of Lolo"),   "Aero Fighters" = c("Aero Fighters"),   "Aero the Acro-Bat" = c("Aero the Acro-Bat"),   "After Burner" = c("After Burner"),   "Age of Wonders" = c("Age of Wonders"),   "Airforce Delta" = c("Airforce Delta"),   "Aleste" = c("Aleste"),   "Alex Kidd" = c("Alex Kidd"),   "Alien Breed" = c("Alien Breed"),   "Alien Syndrome" = c("Alien Syndrome"),   "Alone in the Dark" = c("Alone in the Dark"),   "Alpine Racer" = c("Alpine Racer"),   "Altered Beast" = c("Altered Beast"),   "Alundra" = c("Alundra"),   "American McGee's Alice" = c("American McGee's Alice"),   "America's Army" = c("America's Army"),   "Amnesia" = c("Amnesia"),   "Amped" = c("Amped"),   "Angry Birds" = c("Angry Birds"),   "Anno" = c("Anno"),   "Anomaly" = c("Anomaly"),   "Another Century's Episode" = c("Another Century's Episode"),   "Another Code" = c("Another Code"),   "Ape Escape" = c("Ape Escape"),   "Ar Tonelico" = c("Ar Tonelico"),   "Arc the Lad" = c("Arc the Lad"),   "Arkanoid" = c("Arkanoid"),   "ARMA" = c("ARMA"),   "Armored Core" = c("Armored Core"),   "Army Men" = c("Army Men"),   "Army of Two" = c("Army of Two"),   "Art Academy" = c("Art Academy"),   "Asheron's Call" = c("Asheron's Call"),   "Asphalt" = c("Asphalt"),   "Assassin's Creed" = c("Assassin's Creed"),   "Assault Heroes" = c("Assault Heroes"),   "Asteroids" = c("Asteroids"),   "Astro" = c("Astro"),   "Atelier" = c("Atelier"),   "ATV Offroad Fury" = c("ATV Offroad Fury"),   "Audiosurf" = c("Audiosurf"),   "Avadon" = c("Avadon"),   "Babylonian Castle Saga" = c("Babylonian Castle Saga"),   "Backyard Sports" = c("Backyard Sports"),   "Baldur's Gate" = c("Baldur's Gate"),   "Bangai-O" = c("Bangai-O"),   "Banjo-Kazooie" = c("Banjo-Kazooie"),   "Baraduke" = c("Baraduke"),   "Bard's Tale" = c("Bard's Tale"),   "Baseball Stars" = c("Baseball Stars"),   "Bases Loaded" = c("Bases Loaded"),   "Batman: Arkham" = c("Batman: Arkham"),   "Battle Arena Toshinden" = c("Battle Arena Toshinden"),   "Battle Gear" = c("Battle Gear"),   "Battle Isle" = c("Battle Isle"),   "Battlestations" = c("Battlestations"),   "Battletoads" = c("Battletoads"),   "Battlezone" = c("Battlezone"),   "Bayonetta" = c("Bayonetta"),   "Beat Hazard" = c("Beat Hazard"),   "Beatmania" = c("Beatmania"),   "Bendy" = c("Bendy"),   "Big Brain Academy" = c("Big Brain Academy"),   "Bionic Commando" = c("Bionic Commando"),   "Bit.Trip" = c("Bit.Trip"),   "Black & White" = c("Black & White"),   "Blaster Master" = c("Blaster Master"),   "BlazBlue" = c("BlazBlue"),   "Blazing Angels" = c("Blazing Angels"),   "Blinx" = c("Blinx"),   "Blitz: The League" = c("Blitz: The League"),   "Blood" = c("Blood"),   "Blood Bowl" = c("Blood Bowl"),   "BloodRayne" = c("BloodRayne"),   "Bloody Roar" = c("Bloody Roar"),   "Blue Dragon" = c("Blue Dragon"),   "Bobby Carrot" = c("Bobby Carrot"),   "Boktai" = c("Boktai"),   "Boku no Natsuyasumi" = c("Boku no Natsuyasumi"),   "Bomb Jack" = c("Bomb Jack"),   "Bomberman" = c("Bomberman"),   "Bonk" = c("Bonk"),   "Boom Blox" = c("Boom Blox"),   "Border Break" = c("Border Break"),   "Bosconian" = c("Bosconian"),   "Boulder Dash" = c("Boulder Dash"),   "BoxBoy!" = c("BoxBoy!"),   "Bravely" = c("Bravely"),   "Breakout" = c("Breakout"),   "Breath of Fire" = c("Breath of Fire"),   "Broken Sword" = c("Broken Sword"),   "Brothers in Arms" = c("Brothers in Arms"),   "Bubble Bobble" = c("Bubble Bobble"),   "Bubsy" = c("Bubsy"),   "Budget Cuts" = c("Budget Cuts"),   "Burgertime" = c("Burgertime"),   "Burnout" = c("Burnout"),   "Bus Simulator" = c("Bus Simulator"),   "Bushido Blade" = c("Bushido Blade"),   "Bust a Groove" = c("Bust a Groove"),   "Buster Bros." = c("Buster Bros."),   "Buzz!" = c("Buzz!"),   "Cabela's" = c("Cabela's"),   "Call of Juarez" = c("Call of Juarez"),   "Candy Crush" = c("Candy Crush"),   "Cannon Fodder" = c("Cannon Fodder"),   "Capcom Vs. SNK" = c("Capcom Vs. SNK"),   "Car Mechanic Simulator" = c("Car Mechanic Simulator"),   
                   "Carmageddon" = c("Carmageddon"),   "Carmen Sandiego" = c("Carmen Sandiego"),   "Carnival Games" = c("Carnival Games"),   "Castle Shikigami" = c("Castle Shikigami"),   "Centipede" = c("Centipede"),   "Championship Manager" = c("Championship Manager"),   "Chaos Rings" = c("Chaos Rings"),   "Chase H.Q." = c("Chase H.Q."),   "Chessmaster" = c("Chessmaster"),   "Chibi-Robo!" = c("Chibi-Robo!"),   "Chivalry" = c("Chivalry"),   "Choplifter" = c("Choplifter"),   "Chrono" = c("Chrono"),   "City Connection" = c("City Connection"),   "ClayFighter" = c("ClayFighter"),   "Clicker Heroes" = c("Clicker Heroes"),   "Clock Tower" = c("Clock Tower"),   "Clockwork Knight" = c("Clockwork Knight"),   "Close Combat" = c("Close Combat"),   "Clubhouse Games" = c("Clubhouse Games"),   "Colin McRae Rally" = c("Colin McRae Rally"),   "Colony Wars" = c("Colony Wars"),   "Columns" = c("Columns"),   "Combat Mission" = c("Combat Mission"),   "Combat Wings" = c("Combat Wings"),   "Commander Keen" = c("Commander Keen"),   "Commandos" = c("Commandos"),   "Company of Heroes" = c("Company of Heroes"),   "Condemned" = c("Condemned"),   "Conker" = c("Conker"),   "Contra" = c("Contra"),   "Cooking Mama" = c("Cooking Mama"),   "Cool Boarders" = c("Cool Boarders"),   "Corpse Party" = c("Corpse Party"),   "Cotton" = c("Cotton"),   "Crackdown" = c("Crackdown"),   "Crazy Castle" = c("Crazy Castle"),   "Crazy Chicken / Moorhuhn" = c("Crazy Chicken / Moorhuhn"),   "Crazy Taxi" = c("Crazy Taxi"),   "Creatures" = c("Creatures"),   "Crimson Skies" = c("Crimson Skies"),   "Croc" = c("Croc"),   "Cruis'n" = c("Cruis'n"),   "Crusader Kings" = c("Crusader Kings"),   "Crush Pinball" = c("Crush Pinball"),   "Crysis" = c("Crysis"),   "Custom Robo" = c("Custom Robo"),   "Cut the Rope" = c("Cut the Rope"),   "Cyber Sled" = c("Cyber Sled"),   "Dance Central" = c("Dance Central"),   "Dance Dance Revolution" = c("Dance Dance Revolution"),   "Danganronpa" = c("Danganronpa"),   "Darius" = c("Darius"),   "Dark Cloud" = c("Dark Cloud"),   "Dark Seed" = c("Dark Seed"),   "Darkstalkers" = c("Darkstalkers"),   "Daytona USA" = c("Daytona USA"),   "DC Comics" = c("DC Comics"),   "de Blob" = c("de Blob"),   "Dead Frontier" = c("Dead Frontier"),   "Dead Island" = c("Dead Island"),   "Dead or Alive" = c("Dead or Alive"),   "Dead Rising" = c("Dead Rising"),   "Dead Space" = c("Dead Space"),   "Dead to Rights" = c("Dead to Rights"),   "Deadly Premonition" = c("Deadly Premonition"),   "Deathsmiles" = c("Deathsmiles"),   "DeathSpank" = c("DeathSpank"),   "Deer Hunter" = c("Deer Hunter"),   "Def Jam" = c("Def Jam"),   "Defender" = c("Defender"),   "Defense Grid" = c("Defense Grid"),   "Delta Force" = c("Delta Force"),   "Densha de Go!" = c("Densha de Go!"),   "Descent" = c("Descent"),   "Desperados" = c("Desperados"),   "Destroy All Humans!" = c("Destroy All Humans!"),   "Deus Ex" = c("Deus Ex"),   "Devil Children" = c("Devil Children"),   "Dies irae" = c("Dies irae"),   "Dig Dug" = c("Dig Dug"),   "Dino Crisis" = c("Dino Crisis"),   "Disaster Report" = c("Disaster Report"),   "Disgaea" = c("Disgaea"),   "Dishonored" = c("Dishonored"),   "Disney Infinity" = c("Disney Infinity"),   "Divinity" = c("Divinity"),   "Dizzy" = c("Dizzy"),   "Donkey Kong" = c("Donkey Kong"),   "DonPachi" = c("DonPachi"),   "Doom" = c("Doom"),   "Dota" = c("Dota"),   "Double Dragon" = c("Double Dragon"),   "Dragon Age" = c("Dragon Age"),   "Dragon Buster" = c("Dragon Buster"),   "Dragon Force" = c("Dragon Force"),   "Dragon Slayer" = c("Dragon Slayer"),   "Dragon Spirit" = c("Dragon Spirit"),   "Dragon's Lair" = c("Dragon's Lair"),   "Drakengard" = c("Drakengard"),   "Drawn to Life" = c("Drawn to Life"),   "Dream Chronicles" = c("Dream Chronicles"),   "Driver" = c("Driver"),   "Duke Nukem" = c("Duke Nukem"),   "Dungeon Defenders" = c("Dungeon Defenders"),   "Dungeon Explorer" = c("Dungeon Explorer"),   "Dungeon Keeper " = c("Dungeon Keeper "),   "Dungeon Siege" = c("Dungeon Siege"), 
                   "Earth Defense Force" = c("Earth Defense Force"),   "Earthworm Jim" = c("Earthworm Jim"),   "Ecco the Dolphin" = c("Ecco the Dolphin"),   "El Dorado Gate" = c("El Dorado Gate"),   "Elevator Action" = c("Elevator Action"),   "Empire Earth" = c("Empire Earth"),   "Eternal Champions" = c("Eternal Champions"),   "Etrian Odyssey" = c("Etrian Odyssey"),   "Europa Universalis" = c("Europa Universalis"),   "EverQuest" = c("EverQuest"),   "Everybody's Golf" = c("Everybody's Golf"),   "Evil Genius" = c("Evil Genius"),   "eXceed" = c("eXceed"),   "Excite" = c("Excite"),   "Exerion" = c("Exerion"),   "Exit" = c("Exit"),   "EyeToy" = c("EyeToy"),   "F.E.A.R." = c("F.E.A.R."),   "F1 Circus" = c("F1 Circus"),   "Fable" = c("Fable"),   "Famicom Grand Prix" = c("Famicom Grand Prix"),   "Famicom Tantei Club" = c("Famicom Tantei Club"),   "Family Game Night" = c("Family Game Night"),   "Family Party" = c("Family Party"),   "Family Stadium" = c("Family Stadium"),   "Fantasy Zone" = c("Fantasy Zone"),   "Far East of Eden" = c("Far East of Eden"),   "Farming Simulator" = c("Farming Simulator"),   "Fat Princess" = c("Fat Princess"),   "Fatal Frame" = c("Fatal Frame"),   "Fatal Fury" = c("Fatal Fury"),   "Fieldrunners" = c("Fieldrunners"),   "Fighting Vipers" = c("Fighting Vipers"),   "Final Lap" = c("Final Lap"),   "Fire Emblem" = c("Fire Emblem"),   "Five Nights at Freddy's" = c("Five Nights at Freddy's"),   "FlatOut" = c("FlatOut"),   "Flowers" = c("Flowers"),   "Forza" = c("Forza"),   "Fossil Fighters" = c("Fossil Fighters"),   "Frequency" = c("Frequency"),   "Front Mission" = c("Front Mission"),   "Frostpunk" = c("Frostpunk"),   "F-Zero" = c("F-Zero"),   "Gabriel Knight" = c("Gabriel Knight"),   "Gal Gun" = c("Gal Gun"),   "Galaxian" = c("Galaxian"),   "Galaxy Force" = c("Galaxy Force"),   "Game Party" = c("Game Party"),   "Game Tengoku" = c("Game Tengoku"),   "Ganbare Goemon" = c("Ganbare Goemon"),   "Gauntlet" = c("Gauntlet"),   "Gekisha Boy" = c("Gekisha Boy"),   "Geneforge" = c("Geneforge"),   "Genpei Tōma Den" = c("Genpei Tōma Den"),   "Geometry Wars" = c("Geometry Wars"),   "Gex" = c("Gex"),   "Ghosts 'n Goblins" = c("Ghosts 'n Goblins"),   "Giana Sisters" = c("Giana Sisters"),   "Giga Wing" = c("Giga Wing"),   "Glory of Heracles" = c("Glory of Heracles"),   "God Eater" = c("God Eater"),   "Golden Axe" = c("Golden Axe"),   "Golden Sun" = c("Golden Sun"),   "Golly! Ghost!" = c("Golly! Ghost!"),   "Gothic" = c("Gothic"),   "G-Police" = c("G-Police"),   "Gradius" = c("Gradius"),   "Grandia" = c("Grandia"),   "Groove Coaster" = c("Groove Coaster"),   "Ground Control" = c("Ground Control"),   "Growlanser" = c("Growlanser"),   "Guacamelee!" = c("Guacamelee!"),   "Guardian Heroes" = c("Guardian Heroes"),   "Guild" = c("Guild"),   "Guild Wars" = c("Guild Wars"),   "Guilty Gear" = c("Guilty Gear"),   "Gunbird" = c("Gunbird"),   "Gunpey" = c("Gunpey"),   "Gunslinger Stratos" = c("Gunslinger Stratos"),   "Gunstar Heroes" = c("Gunstar Heroes"),   "Gunvolt" = c("Gunvolt"),   "Hammerin' Harry" = c("Hammerin' Harry"),   "Hang-On" = c("Hang-On"),   "Hard Drivin'" = c("Hard Drivin'"),   "Harry Potter" = c("Harry Potter"),   "Harvest Moon" = c("Harvest Moon"),   "Hat Trick Hero" = c("Hat Trick Hero"),   "Hatsune Miku: Project DIVA" = c("Hatsune Miku: Project DIVA"),   "Hearts of Iron" = c("Hearts of Iron"),   "Hebereke" = c("Hebereke"),   "Heiankyo Alien" = c("Heiankyo Alien"),   "Herzog" = c("Herzog"),   "Hexic" = c("Hexic"),   "Hidden & Dangerous" = c("Hidden & Dangerous"),   "Hitman" = c("Hitman"),   "Homeworld" = c("Homeworld"),   "Hotline Miami" = c("Hotline Miami"),   "Hydlide" = c("Hydlide"),   "Hydro Thunder" = c("Hydro Thunder"),   "Hyperdimension Neptunia" = c("Hyperdimension Neptunia"),   "Hyrule Warriors" = c("Hyrule Warriors"),   "Icewind Dale" = c("Icewind Dale"),   "Ikari Warriors" = c("Ikari Warriors"),   "Illusion" = c("Illusion"),   "Image Fight" = c("Image Fight"),   "Inazuma Eleven" = c("Inazuma Eleven"), 
                   "Infamous" = c("Infamous"),   "Infinity" = c("Infinity"),   "Infinity Blade" = c("Infinity Blade"),   "Injustice" = c("Injustice"),   "International Superstar Soccer" = c("International Superstar Soccer"),   "Invizimals" = c("Invizimals"),   "Iron Soldier" = c("Iron Soldier"),   "Itadaki Street" = c("Itadaki Street"),   "Jagged Alliance" = c("Jagged Alliance"),   "Jak and Daxter" = c("Jak and Daxter"),   "Jake Hunter" = c("Jake Hunter"),   "Jazz Jackrabbit" = c("Jazz Jackrabbit"),   "Jet Moto" = c("Jet Moto"),   "Jet Set Radio" = c("Jet Set Radio"),   "Jetpac" = c("Jetpac"),   "Joe & Mac" = c("Joe & Mac"),   "Joe Danger" = c("Joe Danger"),   "Juiced" = c("Juiced"),   "Jumping Flash!" = c("Jumping Flash!"),   "Just Cause" = c("Just Cause"),   "Kane & Lynch" = c("Kane & Lynch"),   "Kao the Kangaroo" = c("Kao the Kangaroo"),   "Katamari" = c("Katamari"),   "Keyboardmania" = c("Keyboardmania"),   "Kid Icarus" = c("Kid Icarus"),   "Kid Niki" = c("Kid Niki"),   "KiKi KaiKai" = c("KiKi KaiKai"),   "Killer Instinct" = c("Killer Instinct"),   "Killing Floor" = c("Killing Floor"),   "Killzone" = c("Killzone"),   "Kinect Sports" = c("Kinect Sports"),   "King's Field" = c("King's Field"),   "King's Quest" = c("King's Quest"),   "Klonoa" = c("Klonoa"),   "Knights of the Old Temple" = c("Knights of the Old Temple"),   "Kotoba no Puzzle" = c("Kotoba no Puzzle"),   "Kunio-kun" = c("Kunio-kun"),   "Kururin" = c("Kururin"),   "Kyle Hyde" = c("Kyle Hyde"),   "Lands of Lore" = c("Lands of Lore"),   "Langrisser" = c("Langrisser"),   "Last Ninja" = c("Last Ninja"),   "League of Legends" = c("League of Legends"),   "Left 4 Dead" = c("Left 4 Dead"),   "Legacy of Kain" = c("Legacy of Kain"),   "Legend of Legaia" = c("Legend of Legaia"),   "Lego Ninjago" = c("Lego Ninjago"),   "Leisure Suit Larry" = c("Leisure Suit Larry"),   "Lethal Enforcers" = c("Lethal Enforcers"),   "Life Is Strange" = c("Life Is Strange"),   "Lineage" = c("Lineage"),   "Lips" = c("Lips"),   "Little Nightmares" = c("Little Nightmares"),   "LittleBigPlanet" = c("LittleBigPlanet"),   "Lode Runner" = c("Lode Runner"),   "Lost Kingdoms" = c("Lost Kingdoms"),   "Lost Planet" = c("Lost Planet"),   "Lotus" = c("Lotus"),   "LovePlus" = c("LovePlus"),   "Lucky's Tale" = c("Lucky's Tale"),   "Lufia" = c("Lufia"),   "Luigi" = c("Luigi"),   "Lumines" = c("Lumines"),   "Lunar" = c("Lunar"),   "Mafia" = c("Mafia"),   "Magical Drop" = c("Magical Drop"),   "Mana" = c("Mana"),   "Manhunt" = c("Manhunt"),   "Maniac Mansion" = c("Maniac Mansion"),   "Mappy" = c("Mappy"),   "Marvel" = c("Marvel"),   "Marvel vs. Capcom" = c("Marvel vs. Capcom"),   "Master of Orion" = c("Master of Orion"),   "Math Blaster" = c("Math Blaster"),   "Max Payne" = c("Max Payne"),   "MechAssault" = c("MechAssault"),   "MechWarrior" = c("MechWarrior"),   "MediEvil" = c("MediEvil"),   "Mercenaries" = c("Mercenaries"),   "Metal Max" = c("Metal Max"),   "Metal Slug" = c("Metal Slug"),   "Metro" = c("Metro"),   "Microsoft Combat Flight Simulator" = c("Microsoft Combat Flight Simulator"),   "Midnight Club" = c("Midnight Club"),   "Midtown Madness" = c("Midtown Madness"),   "Might and Magic" = c("Might and Magic"),   "Milon's Secret Castle" = c("Milon's Secret Castle"),   "Mirror's Edge" = c("Mirror's Edge"),   "MLB 2K" = c("MLB 2K"),   "MLB: The Show" = c("MLB: The Show"),   "Momoko 120%" = c("Momoko 120%"),   "Momotaro Densetsu" = c("Momotaro Densetsu"),   "Momotaro Dentetsu" = c("Momotaro Dentetsu"),   "Monaco GP" = c("Monaco GP"),   "Monkey Island" = c("Monkey Island"),   "Monster Rancher" = c("Monster Rancher"),   "Monster Truck Madness" = c("Monster Truck Madness"),   "Monument Valley" = c("Monument Valley"),   "Mother" = c("Mother"),   "Moto Racer" = c("Moto Racer"),   "Motocross Madness" = c("Motocross Madness"),   "MotoGP" = c("MotoGP"),   "MotorStorm" = c("MotorStorm"),   "Mount & Blade" = c("Mount & Blade"),   "Mr. Do!" = c("Mr. Do!"),   "Mr. Driller" = c("Mr. Driller"),   "MX vs. ATV" = c("MX vs. ATV"), 
                   "Myst" = c("Myst"),   "Myth" = c("Myth"),   "Namco Anthology" = c("Namco Anthology"),   "Namco Museum" = c("Namco Museum"),   "Nancy Drew" = c("Nancy Drew"),   "Naruto: Ultimate Ninja" = c("Naruto: Ultimate Ninja"),   "NASCAR" = c("NASCAR"),   "Navy Field" = c("Navy Field"),   "NBA Jam" = c("NBA Jam"),   "NCAA Football" = c("NCAA Football"),   "Nectaris / Military Madness" = c("Nectaris / Military Madness"),   "Nekopara" = c("Nekopara"),   "NES Remix" = c("NES Remix"),   "Neutopia" = c("Neutopia"),   "Neverwinter Nights" = c("Neverwinter Nights"),   "NFL 2K" = c("NFL 2K"),   "NHL (EA Sports) " = c("NHL (EA Sports) "),   "Ni no Kuni" = c("Ni no Kuni"),   "Nidhogg" = c("Nidhogg"),   "NieR" = c("NieR"),   "Nights" = c("Nights"),   "Ninja Gaiden" = c("Ninja Gaiden"),   "Ninja JaJaMaru-kun" = c("Ninja JaJaMaru-kun"),   "Nioh" = c("Nioh"),   "No More Heroes" = c("No More Heroes"),   "Nobunaga's Ambition" = c("Nobunaga's Ambition"),   "Numan Athletics" = c("Numan Athletics"),   "Oddworld" = c("Oddworld"),   "Ogre" = c("Ogre"),   "Ōkami" = c("Ōkami"),   "One Must Fall" = c("One Must Fall"),   "One Piece" = c("One Piece"),   "Onechanbara" = c("Onechanbara"),   "Onimusha" = c("Onimusha"),   "Operation Wolf" = c("Operation Wolf"),   "Orcs Must Die!" = c("Orcs Must Die!"),   "Osu! Tatakae! Ouendan" = c("Osu! Tatakae! Ouendan"),   "Otogi" = c("Otogi"),   "Otomedius" = c("Otomedius"),   "Out Run" = c("Out Run"),   "Outlast" = c("Outlast"),   "Overcooked" = c("Overcooked"),   "Pac-Man World" = c("Pac-Man World"),   "Panzer Dragoon" = c("Panzer Dragoon"),   "PaRappa the Rapper" = c("PaRappa the Rapper"),   "Parasite Eve" = c("Parasite Eve"),   "Parkan" = c("Parkan"),   "Parodius" = c("Parodius"),   "Patapon" = c("Patapon"),   "Pathologic" = c("Pathologic"),   "Payday" = c("Payday"),   "Peggle" = c("Peggle"),   "Pengo" = c("Pengo"),   "Penumbra" = c("Penumbra"),   "Perfect Dark" = c("Perfect Dark"),   "Persona" = c("Persona"),   "PGA Tour" = c("PGA Tour"),   "Phantasy Star" = c("Phantasy Star"),   "Pikmin" = c("Pikmin"),   "Pillars of Eternity" = c("Pillars of Eternity"),   "Pilotwings" = c("Pilotwings"),   "Pinball FX" = c("Pinball FX"),   "Pirate Ship Higemaru" = c("Pirate Ship Higemaru"),   "Pitfall!" = c("Pitfall!"),   "PixelJunk" = c("PixelJunk"),   "PlanetSide" = c("PlanetSide"),   "Plants vs. Zombies" = c("Plants vs. Zombies"),   "Point Blank" = c("Point Blank"),   "Pole Position" = c("Pole Position"),   "Pong" = c("Pong"),   "Populous" = c("Populous"),   "Portal" = c("Portal"),   "Postal" = c("Postal"),   "Power Stone" = c("Power Stone"),   "Princess Maker" = c("Princess Maker"),   "Pro Evolution Soccer" = c("Pro Evolution Soccer"),   "Professor Layton" = c("Professor Layton"),   "Project Gotham Racing" = c("Project Gotham Racing"),   "Project X Zone" = c("Project X Zone"),   "Prototype" = c("Prototype"),   "Psychonauts" = c("Psychonauts"),   "Punch-Out!!" = c("Punch-Out!!"),   "Pushmo" = c("Pushmo"),   "Putt-Putt" = c("Putt-Putt"),   "Puyo Puyo" = c("Puyo Puyo"),   "Puzzle & Dragons" = c("Puzzle & Dragons"),   "Puzzle Bobble" = c("Puzzle Bobble"),   "Puzzle League" = c("Puzzle League"),   "Puzzle Quest" = c("Puzzle Quest"),   "Q*bert" = c("Q*bert"),   "Qix" = c("Qix"),   "Quake" = c("Quake"),   "Quest for Glory" = c("Quest for Glory"),   "R.C. Pro-Am" = c("R.C. Pro-Am"),   "Raiden" = c("Raiden"),   "Railroad Tycoon" = c("Railroad Tycoon"),   "Rakugaki Ōkoku" = c("Rakugaki Ōkoku"),   "Rally-X" = c("Rally-X"),   "Rampage" = c("Rampage"),   "Rastan" = c("Rastan"),   "Raving Rabbids" = c("Raving Rabbids"),   "RayForce" = c("RayForce"),   "Red Faction" = c("Red Faction"),   "Red Steel" = c("Red Steel"),   "Remnant" = c("Remnant"),   "Resistance" = c("Resistance"),   "Retro Game Challenge" = c("Retro Game Challenge"),   "Return Fire" = c("Return Fire"),   "Rhythm Heaven" = c("Rhythm Heaven"),   "Rick Dangerous" = c("Rick Dangerous"),   "Ridge Racer" = c("Ridge Racer"),   "Risen" = c("Risen"), 
                   "Risk of Rain" = c("Risk of Rain"),   "Road Fighter" = c("Road Fighter"),   "Road Rash" = c("Road Rash"),   "Robopon" = c("Robopon"),   "Robot" = c("Robot"),   "Robotron: 2084" = c("Robotron: 2084"),   "Rock Band" = c("Rock Band"),   "Rod Land" = c("Rod Land"),   "Rogue Legacy" = c("Rogue Legacy"),   "RollerCoaster Tycoon" = c("RollerCoaster Tycoon"),   "Rolling Thunder" = c("Rolling Thunder"),   "Romance of the Three Kingdoms" = c("Romance of the Three Kingdoms"),   "RPG Maker" = c("RPG Maker"),   "R-Type" = c("R-Type"),   "Rugby League" = c("Rugby League"),   "Rumble Roses" = c("Rumble Roses"),   "Rune Factory" = c("Rune Factory"),   "Runescape" = c("Runescape"),   "Rush" = c("Rush"),   "Rush'n Attack" = c("Rush'n Attack"),   "Rygar" = c("Rygar"),   "S.T.A.L.K.E.R." = c("S.T.A.L.K.E.R."),   "Sabreman" = c("Sabreman"),   "SaGa" = c("SaGa"),   "Sakura Wars" = c("Sakura Wars"),   "Salamander" = c("Salamander"),   "Sam & Max" = c("Sam & Max"),   "Samurai Shodown" = c("Samurai Shodown"),   "Samurai Warriors" = c("Samurai Warriors"),   "Sanctum" = c("Sanctum"),   "Scene It?" = c("Scene It?"),   "Schoolgirl Strikers" = c("Schoolgirl Strikers"),   "Science Adventure" = c("Science Adventure"),   "Scramble" = c("Scramble"),   "Scribblenauts" = c("Scribblenauts"),   "Sea Battle" = c("Sea Battle"),   "Seaman" = c("Seaman"),   "Sega Ages" = c("Sega Ages"),   "Sega Bass Fishing" = c("Sega Bass Fishing"),   "Sega Rally" = c("Sega Rally"),   "Sengoku Basara" = c("Sengoku Basara"),   "Senran Kagura" = c("Senran Kagura"),   "Serious Sam" = c("Serious Sam"),   "Shadow Man" = c("Shadow Man"),   "Shadow of the Beast" = c("Shadow of the Beast"),   "Shadow Warrior" = c("Shadow Warrior"),   "Shadowgate" = c("Shadowgate"),   "Shank" = c("Shank"),   "Shantae" = c("Shantae"),   "Shenmue" = c("Shenmue"),   "Sherlock Holmes" = c("Sherlock Holmes"),   "Shining" = c("Shining"),   "Shinobi" = c("Shinobi"),   "Shōnen Jump" = c("Shōnen Jump"),   "Shoot Away" = c("Shoot Away"),   "Silent Hill" = c("Silent Hill"),   "Silent Scope" = c("Silent Scope"),   "Silpheed" = c("Silpheed"),   "Sin and Punishment" = c("Sin and Punishment"),   "Skate" = c("Skate"),   "Skylanders" = c("Skylanders"),   "Sly Cooper" = c("Sly Cooper"),   "Snake Rattle 'n' Roll" = c("Snake Rattle 'n' Roll"),   "Sniper Elite" = c("Sniper Elite"),   "Sniper: Ghost Warrior" = c("Sniper: Ghost Warrior"),   "Snowboard Kids" = c("Snowboard Kids"),   "SOCOM" = c("SOCOM"),   "Soldier of Fortune" = c("Soldier of Fortune"),   "Sonic Blast Man" = c("Sonic Blast Man"),   "Soulcalibur" = c("Soulcalibur"),   "South Park" = c("South Park"),   "Space Channel 5" = c("Space Channel 5"),   "Space Empires" = c("Space Empires"),   "Space Harrier" = c("Space Harrier"),   "Space Invaders" = c("Space Invaders"),   "Space Quest" = c("Space Quest"),   "Speedball" = c("Speedball"),   "SpellForce" = c("SpellForce"),   "Splashdown" = c("Splashdown"),   "Splatterhouse" = c("Splatterhouse"),   "'Splosion Man" = c("'Splosion Man"),   "Spy Hunter" = c("Spy Hunter"),   "SSX" = c("SSX"),   "Star Control" = c("Star Control"),   "Star Force" = c("Star Force"),   "Star Fox" = c("Star Fox"),   "Star Luster" = c("Star Luster"),   "Star Ocean" = c("Star Ocean"),   "Star Raiders" = c("Star Raiders"),   "Star Soldier" = c("Star Soldier"),   "StarCraft" = c("StarCraft"),   "StarTropics" = c("StarTropics"),   "State of Decay" = c("State of Decay"),   "SteamWorld" = c("SteamWorld"),   "Steel Battalion" = c("Steel Battalion"),   "Steel Division" = c("Steel Division"),   "Steel Gunner" = c("Steel Gunner"),   "Story of Seasons" = c("Story of Seasons"),   "Streets of Rage" = c("Streets of Rage"),   "Strider" = c("Strider"),   "Strikers 1945" = c("Strikers 1945"),   "Stronghold" = c("Stronghold"),   "Subnautica" = c("Subnautica"),   "Suikoden" = c("Suikoden"),   "Summon Night" = c("Summon Night"),   "Super Mega Baseball" = c("Super Mega Baseball"),   "Super Monkey Ball" = c("Super Monkey Ball"),   "Supreme Commander" = c("Supreme Commander"), 
                   "Surgeon Simulator" = c("Surgeon Simulator"),   "Sutte Hakkun" = c("Sutte Hakkun"),   "Swordquest" = c("Swordquest"),   "Syberia" = c("Syberia"),   "Syndicate" = c("Syndicate"),   "Syphon Filter" = c("Syphon Filter"),   "System Shock" = c("System Shock"),   "Taiko no Tatsujin" = c("Taiko no Tatsujin"),   "Taito Memories" = c("Taito Memories"),   "Tak" = c("Tak"),   "Tank Battalion" = c("Tank Battalion"),   "Team Fortress" = c("Team Fortress"),   "Tecmo Bowl" = c("Tecmo Bowl"),   "Teenage Mutant Ninja Turtles" = c("Teenage Mutant Ninja Turtles"),   "Tempest" = c("Tempest"),   "Tenchu" = c("Tenchu"),   "Terra Cresta" = c("Terra Cresta"),   "Test Drive" = c("Test Drive"),   "The Black Mirror" = c("The Black Mirror"),   "The Conduit" = c("The Conduit"),   "The Crew" = c("The Crew"),   "The Culling" = c("The Culling"),   "The Dark Pictures Anthology" = c("The Dark Pictures Anthology"),   "The Evil Within" = c("The Evil Within"),   "The Fancy Pants Adventures" = c("The Fancy Pants Adventures"),   "The Getaway" = c("The Getaway"),   "The Golf Club" = c("The Golf Club"),   "The House of the Dead" = c("The House of the Dead"),   "The Idolmaster" = c("The Idolmaster"),   "The Journeyman Project" = c("The Journeyman Project"),   "The King of Fighters" = c("The King of Fighters"),   "The Last Blade" = c("The Last Blade"),   "The Legend of Heroes" = c("The Legend of Heroes"),   "The Legend of Kage" = c("The Legend of Kage"),   "The Legendary Starfy" = c("The Legendary Starfy"),   "The Lord of the Rings" = c("The Lord of the Rings"),   "The Lost Vikings" = c("The Lost Vikings"),   "The Political Machine" = c("The Political Machine"),   "The Settlers" = c("The Settlers"),   "Thief" = c("Thief"),   "This Is Football" = c("This Is Football"),   "Thunder Ceptor" = c("Thunder Ceptor"),   "Thunder Cross" = c("Thunder Cross"),   "Thunder Force" = c("Thunder Force"),   "Tiger Heli" = c("Tiger Heli"),   "Time Crisis" = c("Time Crisis"),   "Time Pilot" = c("Time Pilot"),   "TimeSplitters" = c("TimeSplitters"),   "Titanfall" = c("Titanfall"),   "Tobal" = c("Tobal"),   "TOCA" = c("TOCA"),   "ToeJam & Earl" = c("ToeJam & Earl"),   "Tokimeki Memorial" = c("Tokimeki Memorial"),   "Tomodachi Collection" = c("Tomodachi Collection"),   "Torchlight" = c("Torchlight"),   "Total Annihilation" = c("Total Annihilation"),   "Touch! Generations" = c("Touch! Generations"),   "Touhou Project" = c("Touhou Project"),   "Toukiden" = c("Toukiden"),   "Track & Field" = c("Track & Field"),   "Train Simulator" = c("Train Simulator"),   "Transformers" = c("Transformers"),   "Trauma Center" = c("Trauma Center"),   "Trials" = c("Trials"),   "Tribes" = c("Tribes"),   "Trine" = c("Trine"),   "Tropico" = c("Tropico"),   "Truck Simulator" = c("Truck Simulator"),   "True Crime" = c("True Crime"),   "Truxton" = c("Truxton"),   "Turok" = c("Turok"),   "Turrican" = c("Turrican"),   "Twilight Syndrome" = c("Twilight Syndrome"),   "TwinBee" = c("TwinBee"),   "Twisted Metal" = c("Twisted Metal"),   "Two Worlds" = c("Two Worlds"),   "Ty the Tasmanian Tiger" = c("Ty the Tasmanian Tiger"),   "Ultima" = c("Ultima"),   "Unravel" = c("Unravel"),   "Unreal" = c("Unreal"),   "Uridium" = c("Uridium"),   "Valis" = c("Valis"),   "Valkyria Chronicles" = c("Valkyria Chronicles"),   "Valkyrie" = c("Valkyrie"),   "Valkyrie Profile" = c("Valkyrie Profile"),   "Vampire: The Masquerade" = c("Vampire: The Masquerade"),   "Vandal Hearts" = c("Vandal Hearts"),   "Vanguard" = c("Vanguard"),   "Vectorman" = c("Vectorman"),   "Vib-Ribbon" = c("Vib-Ribbon"),   "Viewtiful Joe" = c("Viewtiful Joe"),   "Vigilante 8" = c("Vigilante 8"),   "Violence Fight" = c("Violence Fight"),   "Virtua Cop" = c("Virtua Cop"),   "Virtua Fighter" = c("Virtua Fighter"),   "Virtua Striker" = c("Virtua Striker"),   "Virtua Tennis" = c("Virtua Tennis"),   "Virtual On" = c("Virtual On"),   "Viva Piñata" = c("Viva Piñata"),   "V-Rally" = c("V-Rally"),   "Wagan Land" = c("Wagan Land"), 
                   "Wangan Midnight" = c("Wangan Midnight"),   "Warcraft" = c("Warcraft"),   "Warhammer 40,000" = c("Warhammer 40,000"),   "Warhammer Fantasy" = c("Warhammer Fantasy"),   "Wario" = c("Wario"),   "Warlords" = c("Warlords"),   "Wars" = c("Wars"),   "Wasteland" = c("Wasteland"),   "Wave Race" = c("Wave Race"),   "White Knight Chronicles" = c("White Knight Chronicles"),   "Wild Arms" = c("Wild Arms"),   "Wing Commander" = c("Wing Commander"),   "Winning Run" = c("Winning Run"),   "Wipeout" = c("Wipeout"),   "Wizardry" = c("Wizardry"),   "Wizards & Warriors" = c("Wizards & Warriors"),   "Wolf Fang" = c("Wolf Fang"),   "Wolfenstein" = c("Wolfenstein"),   "Wonder Boy" = c("Wonder Boy"),   "Wonder Momo" = c("Wonder Momo"),   "Wonder Project" = c("Wonder Project"),   "World Heroes" = c("World Heroes"),   "World Stadium" = c("World Stadium"),   "Wrecking Crew" = c("Wrecking Crew"),   "X (Egosoft)" = c("X (Egosoft)"),   "X (Nintendo)" = c("X (Nintendo)"),   "Xanadu" = c("Xanadu"),   "X-COM" = c("X-COM"),   "Xeno" = c("Xeno"),   "Xevious" = c("Xevious"),   "Yakuza" = c("Yakuza"),   "Yie Ar Kung-Fu" = c("Yie Ar Kung-Fu"),   "Yo-kai Watch" = c("Yo-kai Watch"),   "Yooka-Laylee" = c("Yooka-Laylee"),   "Yoshi" = c("Yoshi"),   "You Don't Know Jack" = c("You Don't Know Jack"),   "Ys" = c("Ys"),   "Zanac" = c("Zanac"),   "Zaxxon" = c("Zaxxon"),   "Zero Escape" = c("Zero Escape"),   "Zill O'll" = c("Zill O'll"),   "Zombie Tycoon" = c("Zombie Tycoon"),   "Zone of the Enders" = c("Zone of the Enders"),   "Zoo Tycoon" = c("Zoo Tycoon"),   "Zool" = c("Zool"),   "Zoombinis" = c("Zoombinis"),   "Zork" = c("Zork"),   "Zuma" = c("Zuma"),   "Zumba Fitness" = c("Zumba Fitness") 
)

# Apply the function to create the Franchise column
df1$Franchise <- sapply(df1$Name, assign_franchise)
df2$Franchise <- sapply(df2$Name, assign_franchise)

df1 <- df1 %>% select(Index, Name, Franchise, everything())
df2 <- df2 %>% select(Index, Name, Franchise, everything())
df1 <- df1 %>% mutate(Franchise = ifelse(is.na(Franchise),"Unknown", Franchise))
df2 <- df2 %>% mutate(Franchise = ifelse(is.na(Franchise),"Unknown", Franchise))
missing_values(df1)
missing_values(df2)
summary(df1)
summary(df2)
str(df1)
str(df2)

write.csv(df1, file = "Final_Data_1.csv", row.names = FALSE)
write.csv(df2, file = "Final_Data_2.csv", row.names = FALSE)

# DATA ENCODING----
df1 <- read.csv("Final_Data_1.csv", header = TRUE)
df2 <- read.csv("Final_Data_2.csv", header = TRUE)
colnames(df1)

df_numerical1 <- df1 %>% select(Year_of_Release, Critic_Score, Critic_Count, User_Score, User_Count)
df_categorical1 <- df1 %>% select(Franchise, Platform, Genre, Publisher, Developer, Rating)
df_index1 <- df1 %>% select(Index)
df_text1 <- df1 %>% select(Name)
df_target_NA1 <- df1 %>% select(NA_Sales)
df_target_EU1 <- df1 %>% select(EU_Sales)
df_target_JP1 <- df1 %>% select(JP_Sales)
df_target_Others1 <- df1 %>% select(Other_Sales)
df_target_Global1 <- df1 %>% select(Global_Sales)

# Dummy coding categorical variables
str(df_categorical1)
df_categorical1 <- df_categorical1 %>% mutate_all(as.factor)
df_categorical_encoded1 <- as.data.frame(model.matrix(~.-1, df_categorical1))
dim(df_categorical_encoded1)
str(df_categorical_encoded1)

df1_cleaned_NA <- cbind(df_index1, df_target_NA1, df_text1, df_numerical1, df_categorical_encoded1)


colnames(df2)
df_numerical2 <- df2 %>% select(Year_of_Release, Critic_Score, Critic_Count, User_Score, User_Count, Critic_Missing, User_Missing)
df_categorical2 <- df2 %>% select(Franchise, Platform, Genre, Publisher, Developer, Rating)
df_index2 <- df2 %>% select(Index)
df_text2 <- df2 %>% select(Name)
df_target_NA2 <- df2 %>% select(NA_Sales)
df_target_EU2 <- df2 %>% select(EU_Sales)
df_target_JP2 <- df2 %>% select(JP_Sales)
df_target_Others2 <- df2 %>% select(Other_Sales)
df_target_Global2 <- df2 %>% select(Global_Sales)

df_categorical2 <- df_categorical2 %>% mutate_all(as.factor)
df_categorical_encoded2 <- as.data.frame(model.matrix(~.-1, df_categorical2))
df2_cleaned_NA <- cbind(df_index2, df_target_NA2, df_text2, df_numerical2, df_categorical_encoded2)

# save file
write.csv(df1_cleaned_NA, file = "Cleaned_Data_NA_1.csv", row.names = FALSE)
write.csv(df2_cleaned_NA, file = "Cleaned_Data_NA_2.csv", row.names = FALSE)

# TESTING LR WITHOUT LOG TRANSFORMATION----
df1 <- read.csv("Cleaned_Data_NA_1.csv", header = TRUE)
df2 <- read.csv("Cleaned_Data_NA_2.csv", header = TRUE)

data <- df1 %>% select(-Index, -Name)
summary(data$NA_Sales)
data <- data %>% filter(NA_Sales > 0)

data_scaled <- min_max_scale(data)

model1_NA <- lm(NA_Sales ~ ., data = data_scaled)
summary(model1_NA)
coef_pvalues_NA_1 <- as.data.frame(coef(summary(model1_NA)))
sig_predictors_NA_1 <- coef_pvalues_NA_1[coef_pvalues_NA_1$`Pr(>|t|)` < 0.05, ]


data2 <- df2 %>% select(-Index, -Name)
summary(data2$NA_Sales)
data2 <- data2 %>% filter(NA_Sales > 0)
data_scaled2 <- min_max_scale(data2)

model2_NA <- lm(NA_Sales ~ ., data = data_scaled2)
summary(model2_NA)
coef_pvalues_NA_2 <- as.data.frame(coef(summary(model2_NA)))
sig_predictors_NA_2 <- coef_pvalues_NA_2[coef_pvalues_NA_2$`Pr(>|t|)` < 0.05, ]

# TESTING LR WITH LOG TRANSFORMATION----
df1 <- read.csv("Cleaned_Data_NA_1.csv", header = TRUE)
df2 <- read.csv("Cleaned_Data_NA_2.csv", header = TRUE)

data1 <- df1 %>% select(-Index, -Name)
summary(data1$NA_Sales)
data1 <- data1 %>% filter(NA_Sales > 0)
columns_to_log <- c("NA_Sales", "Year_of_Release", "Critic_Score", "Critic_Count", "User_Score", "User_Count")
df1_log <- data1 %>% mutate(across(all_of(columns_to_log), ~ log(.)))
model1_log_NA <- lm(NA_Sales ~ ., data = df1_log)
summary(model1_log_NA)
coef_pvalues_log_NA_1 <- as.data.frame(coef(summary(model1_log_NA)))
sig_predictors_log_NA_1 <- coef_pvalues_log_NA_1[coef_pvalues_log_NA_1$`Pr(>|t|)` < 0.05, ]

data2 <- df2 %>% select(-Index, -Name)
summary(data2$NA_Sales)
data2 <- data2 %>% filter(NA_Sales > 0)
columns_to_log <- c("NA_Sales", "Year_of_Release", "Critic_Score", "Critic_Count", "User_Score", "User_Count")
df2_log <- data2 %>% mutate(across(all_of(columns_to_log), ~ log(.)))
model2_log_NA <- lm(NA_Sales ~ ., data = df2_log)
summary(model2_log_NA)
coef_pvalues_log_NA_2 <- as.data.frame(coef(summary(model2_log_NA)))
sig_predictors_log_NA_2 <- coef_pvalues_log_NA_2[coef_pvalues_log_NA_2$`Pr(>|t|)` < 0.05, ]


# DATA EXPLORATION----
df <- read_excel("ALL_Data_Refined.xlsx", sheet = "ALL")
missing_values(df)
colnames(df)

Franchise_Count <- count_occurrences(df, "Franchise")
Platform_Count <- count_occurrences(df, "Platform")
Genre_Count <- count_occurrences(df, "Genre")
Publisher_Count <- count_occurrences(df, "Publisher")
Developer_Count <- count_occurrences(df, "Developer")
Rating_Count <- count_occurrences(df, "Rating")
Release_Count <- count_occurrences(df, "Year_of_Release")

## Top 15 franchises by sales----
top_franchises <- df %>%
  filter(Franchise != "Unknown") %>%  # Exclude "Unknown" franchises
  group_by(Franchise) %>%
  summarize(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) %>%
  top_n(15, Total_Sales)

ggplot(top_franchises, aes(x = reorder(Franchise, Total_Sales), y = Total_Sales, fill = Franchise)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Total_Sales,0)), vjust = 0.5, hjust = 1.5, size = 3) +
  coord_flip() +
  labs(title = "Top 15 Franchises by Global Sales", x = "Franchise", y = "Total Sales (million USD)") +
  theme_minimal() +
  theme(legend.position = "none")

## Sales distribution by region----
sales_data <- df %>%
  gather(key = "Region", value = "Sales", NA_Sales, EU_Sales, JP_Sales, Other_Sales) %>%
  group_by(Region) %>%
  summarize(Total_Sales = sum(Sales, na.rm = TRUE))

sales_data <- sales_data %>%
  mutate(Percentage = (Total_Sales / sum(Total_Sales)) * 100)

ggplot(sales_data, aes(x = Region, y = Total_Sales, fill = Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 4) +
  labs(title = "Sales Distribution by Region", 
       x = "Region", 
       y = "Sales (million USD)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +
  scale_x_discrete(labels = c("NA_Sales" = "North America", 
                              "EU_Sales" = "Europe", 
                              "JP_Sales" = "Japan", 
                              "Other_Sales" = "Other Regions")) +
  theme(legend.position = "none")


## Sales by Genre by Region----
df <- df %>%
  mutate(Genre = factor(Genre, levels = unique(Genre)))

# Function to calculate percentage distribution and create plot
create_sales_plot <- function(df, sales_col, title) {
  # Define the custom order for Genre
  genre_order <- c("Action", "Sports", "Shooter", "Platform", "Misc", "Racing", "Role-Playing",
                   "Fighting", "Simulation", "Puzzle", "Adventure", "Strategy")
  
  sales_data <- df %>%
    group_by(Genre) %>%
    summarise(Total_Sales = sum(!!sym(sales_col), na.rm = TRUE)) %>%
    mutate(Sales_Percent = Total_Sales / sum(Total_Sales) * 100)
  
  ggplot(sales_data, aes(x = factor(Genre, levels = genre_order), y = Sales_Percent)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = round(Sales_Percent, 1),
                  vjust = ifelse(Sales_Percent < 25, -0.5, 1.5),
                  color = ifelse(Sales_Percent < 25, "outside", "inside")),
              size = 3.5) +
    scale_color_manual(values = c("inside" = "black", "outside" = "black"), guide = "none") +
    labs(title = title,
         x = "Genre",
         y = "Percentage of Sales") +
    scale_x_discrete(labels = genre_order) +  # Set custom order for x-axis
    coord_cartesian(ylim = c(0, 30)) +  # Set y-axis limits
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create individual plots
plot_na_sales <- create_sales_plot(df, "NA_Sales", "North America")
plot_eu_sales <- create_sales_plot(df, "EU_Sales", "Europe")
plot_jp_sales <- create_sales_plot(df, "JP_Sales", "Japan")
plot_other_sales <- create_sales_plot(df, "Other_Sales", "Rest of the World")

# Arrange the plots in a 2x2 grid with a custom title
grid.arrange(plot_na_sales, plot_eu_sales, plot_jp_sales, plot_other_sales, 
             nrow = 2, ncol = 2,
             top = textGrob("Sales Distribution by Genre", gp = gpar(fontsize = 14, fontface = "bold")))

## Sales by Franchise by Region----
df_filtered <- df %>% filter(Franchise_15 != "Others")

df_filtered <- df_filtered %>%
  mutate(Franchise_15 = factor(Franchise_15, levels = unique(Franchise_15)))

create_sales_plot <- function(df, sales_col, title) {
  # Define the custom order for Franchise
  order <- c("Mario", "COD", "MaddenNFL", "Wii", "Pokemon", "GTA", "Lego",
                   "StarWars", "NFS", "Halo", "TheLegendOfZelda", "Assassin'sCreed",
                   "FinalFantasy", "FIFA", "GranTurismo")
  sales_data <- df %>%
    group_by(Franchise_15) %>%
    summarise(Total_Sales = sum(!!sym(sales_col), na.rm = TRUE)) %>%
    mutate(Sales_Percent = Total_Sales / sum(Total_Sales) * 100)
  
  ggplot(sales_data, aes(x = factor(Franchise_15, levels = order), y = Sales_Percent)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = round(Sales_Percent, 1),
                  vjust = ifelse(Sales_Percent < 70, -0.5, 1.5),
                  color = ifelse(Sales_Percent < 70, "outside", "inside")),
              size = 3.5) +
    scale_color_manual(values = c("inside" = "black", "outside" = "black"), guide = "none") +
    labs(title = title,
         x = "Franchise",
         y = "Percentage of Sales") +
    scale_x_discrete(labels = order) +
    coord_cartesian(ylim = c(0, 40)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create individual plots
plot_na_sales <- create_sales_plot(df_filtered, "NA_Sales", "North America")
plot_eu_sales <- create_sales_plot(df_filtered, "EU_Sales", "Europe")
plot_jp_sales <- create_sales_plot(df_filtered, "JP_Sales", "Japan")
plot_other_sales <- create_sales_plot(df_filtered, "Other_Sales", "Rest of the World")

# Arrange the plots in a 2x2 grid with a custom title
grid.arrange(plot_na_sales, plot_eu_sales, plot_jp_sales, plot_other_sales, 
             nrow = 2, ncol = 2,
             top = textGrob("Sales Distribution by Top Franchises", gp = gpar(fontsize = 14, fontface = "bold")))


## Sales by Platform by Region----
df <- df %>%
  mutate(Game_Platform = factor(Game_Platform, levels = unique(Game_Platform)))


create_sales_plot <- function(df, sales_col, title) {
  # Define the custom order for Franchise
  order <- c("Nintendo", "PlayStation", "XBOX", "PC", "Atari", "Sega", 
             "WonderSwan", "NeoGeo", "NEC & Hudson Soft", "3DO")
  
  sales_data <- df %>%
    group_by(Game_Platform) %>%
    summarise(Total_Sales = sum(!!sym(sales_col), na.rm = TRUE)) %>%
    mutate(Sales_Percent = Total_Sales / sum(Total_Sales) * 100)
  
  ggplot(sales_data, aes(x = factor(Game_Platform, levels = order), y = Sales_Percent)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = round(Sales_Percent, 1),
                  vjust = ifelse(Sales_Percent < 50, -0.5, 1.5),
                  color = ifelse(Sales_Percent < 50, "outside", "inside")),
              size = 3.5) +
    scale_color_manual(values = c("inside" = "black", "outside" = "black"), guide = "none") +
    labs(title = title,
         x = "Platform",
         y = "Percentage of Sales") +
    scale_x_discrete(labels = order) +  # Set custom order for x-axis
    coord_cartesian(ylim = c(0, 60)) +  # Set y-axis limits
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create individual plots
plot_na_sales <- create_sales_plot(df, "NA_Sales", "North America")
plot_eu_sales <- create_sales_plot(df, "EU_Sales", "Europe")
plot_jp_sales <- create_sales_plot(df, "JP_Sales", "Japan")
plot_other_sales <- create_sales_plot(df, "Other_Sales", "Rest of the World")

# Arrange the plots in a 2x2 grid with a custom title
grid.arrange(plot_na_sales, plot_eu_sales, plot_jp_sales, plot_other_sales, 
             nrow = 2, ncol = 2,
             top = textGrob("Sales Distribution by Platform", gp = gpar(fontsize = 14, fontface = "bold")))


## Sales by Rating by Region----
df <- df %>%
  mutate(Rating = factor(Rating, levels = unique(Rating)))

# Function to calculate percentage distribution and create plot
create_sales_plot <- function(df, sales_col, title) {
  # Define the desired order of Rating levels
  rating_levels <- c("E", "E10+", "T", "M")
  
  # Convert Rating column to factor with specified levels and order
  df <- df %>%
    mutate(Rating = factor(Rating, levels = rating_levels, ordered = TRUE))
  
  # Calculate total sales and percentage for each rating level
  sales_data <- df %>%
    group_by(Rating) %>%
    summarise(Total_Sales = sum(!!sym(sales_col), na.rm = TRUE)) %>%
    mutate(Sales_Percent = Total_Sales / sum(Total_Sales) * 100)
  
  # Create the plot
  ggplot(sales_data, aes(x = Rating, y = Sales_Percent)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = round(Sales_Percent, 0),
                  vjust = ifelse(Sales_Percent < 50, -0.5, 1.5),
                  color = ifelse(Sales_Percent < 50, "outside", "inside")),
              size = 3.5) +
    scale_color_manual(values = c("inside" = "black", "outside" = "black"), guide = "none") +
    labs(title = title,
         x = "ESRB Rating",
         y = "Percentage of Sales") +
    scale_x_discrete(labels = rating_levels) +  # Set custom order for x-axis
    coord_cartesian(ylim = c(0, 50)) +  # Set y-axis limits
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create individual plots
plot_na_sales <- create_sales_plot(df, "NA_Sales", "North America")
plot_eu_sales <- create_sales_plot(df, "EU_Sales", "Europe")
plot_jp_sales <- create_sales_plot(df, "JP_Sales", "Japan")
plot_other_sales <- create_sales_plot(df, "Other_Sales", "Rest of the World")

# Arrange the plots in a 2x2 grid with a custom title
grid.arrange(plot_na_sales, plot_eu_sales, plot_jp_sales, plot_other_sales, 
             nrow = 2, ncol = 2,
             top = textGrob("Sales Distribution by ESRB Rating", gp = gpar(fontsize = 14, fontface = "bold")))


## Sales Trends Over the Years----

# Define platform release years
platform_release_years <- tribble(
  ~Year, ~Platform,
  1977, "Atari 2600",
  1983, "NES",
  1987, "TurboGrafx-16",
  1988, "Genesis",
  1989, "GB",
  1990, "SNES, GG, NG",
  1991, "Sega CD",
  1993, "3DO",
  1994, "Saturn, PC-FX",
  1995, "PS",
  1996, "Nintendo 64",
  1998, "Sega DC",
  1999, "WonderSwan",
  2000, "PS2",
  2001, "Xbox, GBA, GC",
  2004, "Nintendo DS",
  2005, "PSP, Xbox 360",
  2006, "PS3, Wii",
  2011, "PSV, 3DS",
  2012, "WiiU",
  2013, "PS4, Xbox One"
)

# Convert Platform to character to ensure it's not a factor
platform_release_years$Platform <- as.character(platform_release_years$Platform)

# Function to aggregate platforms by year into a single label
aggregate_platforms <- function(df) {
  df %>%
    group_by(Year) %>%
    summarise(Platform = paste(Platform, collapse = ", "))
}

# Aggregate platforms
platform_labels <- aggregate_platforms(platform_release_years)

# Plot sales trends with platform release years
ggplot(sales_trends, aes(x = Year_of_Release, y = Sales, color = Region)) +
  geom_line() +
  scale_color_manual(values = setNames(region_colors, unique(sales_trends$Region))) +
  labs(title = "Impact of Platform Launches on Sales Trends (1980-2016)",
       x = "Year",
       y = "Total Sales (million USD)",
       color = "Region") +
  theme_minimal() +
  geom_vline(data = platform_release_years, aes(xintercept = Year),
             linetype = "dashed", color = "grey", alpha = 0.8) +
  geom_text(data = platform_labels, aes(x = Year, y = 250, label = paste(Year, "-", Platform)),
            vjust = -0.2, hjust = 0, size = 3, angle = 90, color = "#3F3F3F") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

# DATA PREPARATION----

# Read data
df <- read_excel("ALL_Data_Refined - LOG.xlsx", sheet = "ALL")
df$Year_of_Release <- as.character(df$Year_of_Release)
df$Year_Bin_ <- as.character(df$Year_Bin_)

colnames(df)
str(df)

df_index <- df %>% select(Index)

df_num <- df %>% select(Critic_Score, Critic_Score_2, Critic_Count, 
                              User_Score, User_Score_2, User_Count)

df_num_log <- df %>% select(Log_Critic_Score, Log_Critic_Score_2, Log_Critic_Count, 
                            Log_User_Score, Log_User_Score_2, Log_User_Count)

df_cat <- df %>% select(Franchise_, Platform_, Genre_, Rating_, Year_Bin_)


df_others <- df %>% select(Name, Franchise_Old, Platform_Old, Publisher, Developer, Year_of_Release, Critic_Missing, User_Missing, Log_Critic_Missing, Log_User_Missing)

df_target_NA <- df %>% select(NA_Sales)
df_target_EU <- df %>% select(EU_Sales)
df_target_JP <- df %>% select(JP_Sales)
df_target_Other <- df %>% select(Other_Sales)
df_target_Global <- df %>% select(Global_Sales)

df_target_NA_log <- df %>% select(Log_NA_Sales)
df_target_EU_log <- df %>% select(Log_EU_Sales)
df_target_JP_log <- df %>% select(Log_JP_Sales)
df_target_Other_log <- df %>% select(Log_Other_Sales)
df_target_Global_log <- df %>% select(Log_Global_Sales)

df_cat <- df_cat %>% mutate_all(as.factor)
df_cat_encoded <- as.data.frame(model.matrix(~.-1, df_cat))

df_all_log <- cbind(df_target_NA_log, df_target_EU_log, df_target_JP_log, df_target_Other_log, df_target_Global_log, df_num_log, df_cat_encoded)

# save file
write.csv(df_all_log, file = "df_all_log.csv", row.names = FALSE)

# LR MODELLING (with LOG transformation)----

# Read data
df_all_log <- read.csv("df_all_log.csv", header = TRUE)

# Split data
set.seed(467)
smp_size <- floor(0.8 * nrow(df_all_log))
training_indices <- sample(nrow(df_all_log), smp_size)
train <- df_all_log[training_indices,]
test <- df_all_log[-training_indices,]
all_columns <- colnames(df_all_log)
exclude_columns <- c("Franchise_Wii", "Log_NA_Sales", "Log_EU_Sales", "Log_JP_Sales", "Log_Other_Sales", "Log_Global_Sales")
predictor_columns <- setdiff(all_columns, exclude_columns)

## NA----
# Train linear regression model
formula_string <- paste("Log_NA_Sales ~", paste(predictor_columns, collapse = " + "))
model_formula <- as.formula(formula_string)

model_lr_na <- lm(model_formula, data = train)
coef <- as.data.frame(coef(summary(model_lr_na)))
predictors <- coef[coef$`Pr(>|t|)` < 0.05, ]
sum_model <- summary(model_lr_na)

# Make predictions on the training data
train_predictions_lr_na <- predict(model_lr_na, newdata = train)
train_predictions_lr_na <- as.numeric(train_predictions_lr_na)
train_actuals_lr_na <- train$Log_NA_Sales
train_actuals_lr_na <- as.numeric(train_actuals_lr_na)

# Make predictions on the test data
test_predictions_lr_na<- predict(model_lr_na, newdata = test)
test_predictions_lr_na <- as.numeric(test_predictions_lr_na)
test_actuals_lr_na <- test$Log_NA_Sales
test_actuals_lr_na <- as.numeric(test_actuals_lr_na)

# Calculate evaluation metrics for training data
train_mae <- mae(train_actuals_lr_na, train_predictions_lr_na)
train_mse <- mse(train_actuals_lr_na, train_predictions_lr_na)
train_rmse <- rmse(train_actuals_lr_na, train_predictions_lr_na)
train_r_squared <- summary(model_lr_na)$r.squared

# Calculate evaluation metrics for test data
test_mae <- mae(test_actuals_lr_na, test_predictions_lr_na)
test_mse <- mse(test_actuals_lr_na, test_predictions_lr_na)
test_rmse <- rmse(test_actuals_lr_na, test_predictions_lr_na)
ss_res <- sum((test_actuals_lr_na - test_predictions_lr_na)^2)
ss_tot <- sum((test_actuals_lr_na - mean(test_actuals_lr_na))^2)
test_r_squared <- 1 - (ss_res / ss_tot)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared))

# Return the results
results_lr_na <- list(
  Summary = sum_model,
  Metrics = metrics)

# Print results
print(results_lr_na)

# Feature importance based on absolute values of coefficients
importance <- coef(summary(model_lr_na))
importance_df <- data.frame(
  Feature = rownames(importance),
  Coefficient = importance[, "Estimate"],
  p_value = importance[, "Pr(>|t|)"]
)
importance_df_lr_na <- importance_df[order(abs(importance_df$Coefficient), decreasing = TRUE), ]

# Calculate Residuals
train_residuals_lr_na <- train_actuals_lr_na - train_predictions_lr_na
test_residuals_lr_na <- test_actuals_lr_na - test_predictions_lr_na

## NA REFIT----

# Extract significant predictors from the initial model
significant_predictors <- rownames(sum_model$coefficients)[sum_model$coefficients[, "Pr(>|t|)"] < 0.05]
significant_predictors <- significant_predictors[significant_predictors != "(Intercept)"]
significant_predictors

# Create a formula with only the significant predictors
significant_formula <- as.formula(paste("Log_NA_Sales ~", paste(significant_predictors, collapse = " + ")))
significant_formula

# Refit the model using the significant predictors
significant_model <- lm(significant_formula, data = train)
significant_summary <- summary(significant_model)
significant_summary

# Make predictions on the training data
train_predictions <- predict(significant_model, newdata = train)
train_predictions <- as.numeric(train_predictions)
train_actuals <- train$Log_NA_Sales
train_actuals <- as.numeric(train_actuals)

# Make predictions on the test data
test_predictions <- predict(significant_model, newdata = test)
test_predictions <- as.numeric(test_predictions)
test_actuals <- test$Log_NA_Sales
test_actuals <- as.numeric(test_actuals)

# Calculate evaluation metrics for training data
train_mae <- mae(train_actuals, train_predictions)
train_mse <- mse(train_actuals, train_predictions)
train_rmse <- rmse(train_actuals, train_predictions)
train_r_squared <- summary(significant_model)$r.squared

# Calculate evaluation metrics for test data
test_mae <- mae(test_actuals, test_predictions)
test_mse <- mse(test_actuals, test_predictions)
test_rmse <- rmse(test_actuals, test_predictions)
ss_res <- sum((test_actuals - test_predictions)^2)
ss_tot <- sum((test_actuals - mean(test_actuals))^2)
test_r_squared <- 1 - (ss_res / ss_tot)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared))

# Return the results
results <- list(
  Summary = significant_summary,
  Metrics = metrics)

# Print results
print(results)

## EU----
# Train linear regression model
formula_string <- paste("Log_EU_Sales ~", paste(predictor_columns, collapse = " + "))
model_formula <- as.formula(formula_string)

model_lr_eu <- lm(model_formula, data = train)
coef <- as.data.frame(coef(summary(model_lr_eu)))
predictors <- coef[coef$`Pr(>|t|)` < 0.05, ]
sum_model <- summary(model_lr_eu)

# Make predictions on the training data
train_predictions_lr_eu <- predict(model_lr_eu, newdata = train)
train_predictions_lr_eu <- as.numeric(train_predictions_lr_eu)
train_actuals_lr_eu <- train$Log_EU_Sales
train_actuals_lr_eu <- as.numeric(train_actuals_lr_eu)

# Make predictions on the test data
test_predictions_lr_eu<- predict(model_lr_eu, newdata = test)
test_predictions_lr_eu <- as.numeric(test_predictions_lr_eu)
test_actuals_lr_eu <- test$Log_EU_Sales
test_actuals_lr_eu <- as.numeric(test_actuals_lr_eu)

# Calculate evaluation metrics for training data
train_mae <- mae(train_actuals_lr_eu, train_predictions_lr_eu)
train_mse <- mse(train_actuals_lr_eu, train_predictions_lr_eu)
train_rmse <- rmse(train_actuals_lr_eu, train_predictions_lr_eu)
train_r_squared <- summary(model_lr_eu)$r.squared

# Calculate evaluation metrics for test data
test_mae <- mae(test_actuals_lr_eu, test_predictions_lr_eu)
test_mse <- mse(test_actuals_lr_eu, test_predictions_lr_eu)
test_rmse <- rmse(test_actuals_lr_eu, test_predictions_lr_eu)
ss_res <- sum((test_actuals_lr_eu - test_predictions_lr_eu)^2)
ss_tot <- sum((test_actuals_lr_eu - mean(test_actuals_lr_eu))^2)
test_r_squared <- 1 - (ss_res / ss_tot)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared))

# Return the results
results_lr_eu <- list(
  Summary = sum_model,
  Metrics = metrics)

# Print results
print(results_lr_eu)

# Feature importance based on absolute values of coefficients
importance <- coef(summary(model_lr_eu))
importance_df <- data.frame(
  Feature = rownames(importance),
  Coefficient = importance[, "Estimate"],
  p_value = importance[, "Pr(>|t|)"]
)
importance_df_lr_eu <- importance_df[order(abs(importance_df$Coefficient), decreasing = TRUE), ]

# Calculate Residuals
train_residuals_lr_eu <- train_actuals_lr_eu - train_predictions_lr_eu
test_residuals_lr_eu <- test_actuals_lr_eu - test_predictions_lr_eu

## EU REFIT----

# Extract significant predictors from the initial model
significant_predictors <- rownames(sum_model$coefficients)[sum_model$coefficients[, "Pr(>|t|)"] < 0.05]
significant_predictors <- significant_predictors[significant_predictors != "(Intercept)"]
significant_predictors

# Create a formula with only the significant predictors
significant_formula <- as.formula(paste("Log_EU_Sales ~", paste(significant_predictors, collapse = " + ")))
significant_formula

# Refit the model using the significant predictors
significant_model <- lm(significant_formula, data = train)
significant_summary <- summary(significant_model)

# Make predictions on the training data
train_predictions <- predict(significant_model, newdata = train)
train_predictions <- as.numeric(train_predictions)
train_actuals <- train$Log_EU_Sales
train_actuals <- as.numeric(train_actuals)

# Make predictions on the test data
test_predictions <- predict(significant_model, newdata = test)
test_predictions <- as.numeric(test_predictions)
test_actuals <- test$Log_EU_Sales
test_actuals <- as.numeric(test_actuals)

# Calculate evaluation metrics for training data
train_mae <- mae(train_actuals, train_predictions)
train_mse <- mse(train_actuals, train_predictions)
train_rmse <- rmse(train_actuals, train_predictions)
train_r_squared <- summary(significant_model)$r.squared

# Calculate evaluation metrics for test data
test_mae <- mae(test_actuals, test_predictions)
test_mse <- mse(test_actuals, test_predictions)
test_rmse <- rmse(test_actuals, test_predictions)
ss_res <- sum((test_actuals - test_predictions)^2)
ss_tot <- sum((test_actuals - mean(test_actuals))^2)
test_r_squared <- 1 - (ss_res / ss_tot)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared))

# Return the results
results <- list(
  Summary = significant_summary,
  Metrics = metrics)

# Print results
print(results)

## JP----
# Train linear regression model
formula_string <- paste("Log_JP_Sales ~", paste(predictor_columns, collapse = " + "))
model_formula <- as.formula(formula_string)

model_lr_jp <- lm(model_formula, data = train)
coef <- as.data.frame(coef(summary(model_lr_jp)))
predictors <- coef[coef$`Pr(>|t|)` < 0.05, ]
sum_model <- summary(model_lr_jp)

# Make predictions on the training data
train_predictions_lr_jp <- predict(model_lr_jp, newdata = train)
train_predictions_lr_jp <- as.numeric(train_predictions_lr_jp)
train_actuals_lr_jp <- train$Log_JP_Sales
train_actuals_lr_jp <- as.numeric(train_actuals_lr_jp)

# Make predictions on the test data
test_predictions_lr_jp<- predict(model_lr_jp, newdata = test)
test_predictions_lr_jp <- as.numeric(test_predictions_lr_jp)
test_actuals_lr_jp <- test$Log_JP_Sales
test_actuals_lr_jp <- as.numeric(test_actuals_lr_jp)

# Calculate evaluation metrics for training data
train_mae <- mae(train_actuals_lr_jp, train_predictions_lr_jp)
train_mse <- mse(train_actuals_lr_jp, train_predictions_lr_jp)
train_rmse <- rmse(train_actuals_lr_jp, train_predictions_lr_jp)
train_r_squared <- summary(model_lr_jp)$r.squared

# Calculate evaluation metrics for test data
test_mae <- mae(test_actuals_lr_jp, test_predictions_lr_jp)
test_mse <- mse(test_actuals_lr_jp, test_predictions_lr_jp)
test_rmse <- rmse(test_actuals_lr_jp, test_predictions_lr_jp)
ss_res <- sum((test_actuals_lr_jp - test_predictions_lr_jp)^2)
ss_tot <- sum((test_actuals_lr_jp - mean(test_actuals_lr_jp))^2)
test_r_squared <- 1 - (ss_res / ss_tot)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared))

# Return the results
results_lr_jp <- list(
  Summary = sum_model,
  Metrics = metrics)

# Print results
print(results_lr_jp)

# Feature importance based on absolute values of coefficients
importance <- coef(summary(model_lr_jp))
importance_df <- data.frame(
  Feature = rownames(importance),
  Coefficient = importance[, "Estimate"],
  p_value = importance[, "Pr(>|t|)"]
)
importance_df_lr_jp <- importance_df[order(abs(importance_df$Coefficient), decreasing = TRUE), ]

# Calculate Residuals
train_residuals_lr_jp <- train_actuals_lr_jp - train_predictions_lr_jp
test_residuals_lr_jp <- test_actuals_lr_jp - test_predictions_lr_jp

## JP REFIT----

# Extract significant predictors from the initial model
significant_predictors <- rownames(sum_model$coefficients)[sum_model$coefficients[, "Pr(>|t|)"] < 0.05]
significant_predictors <- significant_predictors[significant_predictors != "(Intercept)"]
significant_predictors

# Create a formula with only the significant predictors
significant_formula <- as.formula(paste("Log_JP_Sales ~", paste(significant_predictors, collapse = " + ")))
significant_formula

# Refit the model using the significant predictors
significant_model <- lm(significant_formula, data = train)
significant_summary <- summary(significant_model)

# Make predictions on the training data
train_predictions <- predict(significant_model, newdata = train)
train_predictions <- as.numeric(train_predictions)
train_actuals <- train$Log_JP_Sales
train_actuals <- as.numeric(train_actuals)

# Make predictions on the test data
test_predictions <- predict(significant_model, newdata = test)
test_predictions <- as.numeric(test_predictions)
test_actuals <- test$Log_JP_Sales
test_actuals <- as.numeric(test_actuals)

# Calculate evaluation metrics for training data
train_mae <- mae(train_actuals, train_predictions)
train_mse <- mse(train_actuals, train_predictions)
train_rmse <- rmse(train_actuals, train_predictions)
train_r_squared <- summary(significant_model)$r.squared

# Calculate evaluation metrics for test data
test_mae <- mae(test_actuals, test_predictions)
test_mse <- mse(test_actuals, test_predictions)
test_rmse <- rmse(test_actuals, test_predictions)
ss_res <- sum((test_actuals - test_predictions)^2)
ss_tot <- sum((test_actuals - mean(test_actuals))^2)
test_r_squared <- 1 - (ss_res / ss_tot)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared))

# Return the results
results <- list(
  Summary = significant_summary,
  Metrics = metrics)

# Print results
print(results)


## Other----
# Train linear regression model
formula_string <- paste("Log_Other_Sales ~", paste(predictor_columns, collapse = " + "))
model_formula <- as.formula(formula_string)

model_lr_other <- lm(model_formula, data = train)
coef <- as.data.frame(coef(summary(model_lr_other)))
predictors <- coef[coef$`Pr(>|t|)` < 0.05, ]
sum_model <- summary(model_lr_other)

# Make predictions on the training data
train_predictions_lr_other <- predict(model_lr_other, newdata = train)
train_predictions_lr_other <- as.numeric(train_predictions_lr_other)
train_actuals_lr_other <- train$Log_Other_Sales
train_actuals_lr_other <- as.numeric(train_actuals_lr_other)

# Make predictions on the test data
test_predictions_lr_other<- predict(model_lr_other, newdata = test)
test_predictions_lr_other <- as.numeric(test_predictions_lr_other)
test_actuals_lr_other <- test$Log_Other_Sales
test_actuals_lr_other <- as.numeric(test_actuals_lr_other)

# Calculate evaluation metrics for training data
train_mae <- mae(train_actuals_lr_other, train_predictions_lr_other)
train_mse <- mse(train_actuals_lr_other, train_predictions_lr_other)
train_rmse <- rmse(train_actuals_lr_other, train_predictions_lr_other)
train_r_squared <- summary(model_lr_other)$r.squared

# Calculate evaluation metrics for test data
test_mae <- mae(test_actuals_lr_other, test_predictions_lr_other)
test_mse <- mse(test_actuals_lr_other, test_predictions_lr_other)
test_rmse <- rmse(test_actuals_lr_other, test_predictions_lr_other)
ss_res <- sum((test_actuals_lr_other - test_predictions_lr_other)^2)
ss_tot <- sum((test_actuals_lr_other - mean(test_actuals_lr_other))^2)
test_r_squared <- 1 - (ss_res / ss_tot)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared))

# Return the results
results_lr_other <- list(
  Summary = sum_model,
  Metrics = metrics)

# Print results
print(results_lr_other)

# Feature importance based on absolute values of coefficients
importance <- coef(summary(model_lr_other))
importance_df <- data.frame(
  Feature = rownames(importance),
  Coefficient = importance[, "Estimate"],
  p_value = importance[, "Pr(>|t|)"]
)
importance_df_lr_other <- importance_df[order(abs(importance_df$Coefficient), decreasing = TRUE), ]

# Calculate Residuals
train_residuals_lr_other <- train_actuals_lr_other - train_predictions_lr_other
test_residuals_lr_other <- test_actuals_lr_other - test_predictions_lr_other

## Other REFIT----

# Extract significant predictors from the initial model
significant_predictors <- rownames(sum_model$coefficients)[sum_model$coefficients[, "Pr(>|t|)"] < 0.05]
significant_predictors <- significant_predictors[significant_predictors != "(Intercept)"]
significant_predictors

# Create a formula with only the significant predictors
significant_formula <- as.formula(paste("Log_Other_Sales ~", paste(significant_predictors, collapse = " + ")))
significant_formula

# Refit the model using the significant predictors
significant_model <- lm(significant_formula, data = train)
significant_summary <- summary(significant_model)

# Make predictions on the training data
train_predictions <- predict(significant_model, newdata = train)
train_predictions <- as.numeric(train_predictions)
train_actuals <- train$Log_Other_Sales
train_actuals <- as.numeric(train_actuals)

# Make predictions on the test data
test_predictions <- predict(significant_model, newdata = test)
test_predictions <- as.numeric(test_predictions)
test_actuals <- test$Log_Other_Sales
test_actuals <- as.numeric(test_actuals)

# Calculate evaluation metrics for training data
train_mae <- mae(train_actuals, train_predictions)
train_mse <- mse(train_actuals, train_predictions)
train_rmse <- rmse(train_actuals, train_predictions)
train_r_squared <- summary(significant_model)$r.squared

# Calculate evaluation metrics for test data
test_mae <- mae(test_actuals, test_predictions)
test_mse <- mse(test_actuals, test_predictions)
test_rmse <- rmse(test_actuals, test_predictions)
ss_res <- sum((test_actuals - test_predictions)^2)
ss_tot <- sum((test_actuals - mean(test_actuals))^2)
test_r_squared <- 1 - (ss_res / ss_tot)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared))

# Return the results
results <- list(
  Summary = significant_summary,
  Metrics = metrics)

# Print results
print(results)


## Global----
# Train linear regression model
formula_string <- paste("Log_Global_Sales ~", paste(predictor_columns, collapse = " + "))
model_formula <- as.formula(formula_string)

model_lr_global <- lm(model_formula, data = train)
coef <- as.data.frame(coef(summary(model_lr_global)))
predictors <- coef[coef$`Pr(>|t|)` < 0.05, ]
sum_model <- summary(model_lr_global)

# Make predictions on the training data
train_predictions_lr_global <- predict(model_lr_global, newdata = train)
train_predictions_lr_global <- as.numeric(train_predictions_lr_global)
train_actuals_lr_global <- train$Log_Global_Sales
train_actuals_lr_global <- as.numeric(train_actuals_lr_global)

# Make predictions on the test data
test_predictions_lr_global<- predict(model_lr_global, newdata = test)
test_predictions_lr_global <- as.numeric(test_predictions_lr_global)
test_actuals_lr_global <- test$Log_Global_Sales
test_actuals_lr_global <- as.numeric(test_actuals_lr_global)

# Calculate evaluation metrics for training data
train_mae <- mae(train_actuals_lr_global, train_predictions_lr_global)
train_mse <- mse(train_actuals_lr_global, train_predictions_lr_global)
train_rmse <- rmse(train_actuals_lr_global, train_predictions_lr_global)
train_r_squared <- summary(model_lr_global)$r.squared

# Calculate evaluation metrics for test data
test_mae <- mae(test_actuals_lr_global, test_predictions_lr_global)
test_mse <- mse(test_actuals_lr_global, test_predictions_lr_global)
test_rmse <- rmse(test_actuals_lr_global, test_predictions_lr_global)
ss_res <- sum((test_actuals_lr_global - test_predictions_lr_global)^2)
ss_tot <- sum((test_actuals_lr_global - mean(test_actuals_lr_global))^2)
test_r_squared <- 1 - (ss_res / ss_tot)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared))

# Return the results
results_lr_global <- list(
  Summary = sum_model,
  Metrics = metrics)

# Print results
print(results_lr_global)

# Feature importance based on absolute values of coefficients
importance <- coef(summary(model_lr_global))
importance_df <- data.frame(
  Feature = rownames(importance),
  Coefficient = importance[, "Estimate"],
  p_value = importance[, "Pr(>|t|)"]
)
importance_df_lr_global <- importance_df[order(abs(importance_df$Coefficient), decreasing = TRUE), ]

# Calculate Residuals
train_residuals_lr_global <- train_actuals_lr_global - train_predictions_lr_global
test_residuals_lr_global <- test_actuals_lr_global - test_predictions_lr_global

## Global REFIT----

# Extract significant predictors from the initial model
significant_predictors <- rownames(sum_model$coefficients)[sum_model$coefficients[, "Pr(>|t|)"] < 0.05]
significant_predictors <- significant_predictors[significant_predictors != "(Intercept)"]
significant_predictors

# Create a formula with only the significant predictors
significant_formula <- as.formula(paste("Log_Global_Sales ~", paste(significant_predictors, collapse = " + ")))
significant_formula

# Refit the model using the significant predictors
significant_model <- lm(significant_formula, data = train)
significant_summary <- summary(significant_model)

# Make predictions on the training data
train_predictions <- predict(significant_model, newdata = train)
train_predictions <- as.numeric(train_predictions)
train_actuals <- train$Log_Global_Sales
train_actuals <- as.numeric(train_actuals)

# Make predictions on the test data
test_predictions <- predict(significant_model, newdata = test)
test_predictions <- as.numeric(test_predictions)
test_actuals <- test$Log_Global_Sales
test_actuals <- as.numeric(test_actuals)

# Calculate evaluation metrics for training data
train_mae <- mae(train_actuals, train_predictions)
train_mse <- mse(train_actuals, train_predictions)
train_rmse <- rmse(train_actuals, train_predictions)
train_r_squared <- summary(significant_model)$r.squared

# Calculate evaluation metrics for test data
test_mae <- mae(test_actuals, test_predictions)
test_mse <- mse(test_actuals, test_predictions)
test_rmse <- rmse(test_actuals, test_predictions)
ss_res <- sum((test_actuals - test_predictions)^2)
ss_tot <- sum((test_actuals - mean(test_actuals))^2)
test_r_squared <- 1 - (ss_res / ss_tot)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared))

# Return the results
results <- list(
  Summary = significant_summary,
  Metrics = metrics)

# Print results
print(results)

## Importance scores----

combined_importance_df <- read.csv("LR_importance.csv", header = TRUE)
combined_importance_df <- combined_importance_df %>%
  arrange(desc(abs(NA_Coefficient)))
combined_importance_df$Feature <- factor(combined_importance_df$Feature, levels = combined_importance_df$Feature)
#combined_importance_long <- combined_importance_df %>%
#  gather(key = "Region", value = "Importance", NA_Coefficient:Other_Coefficient)

combined_importance_long <- combined_importance_df %>%
  gather(key = "Region", value = "Importance", NA_Coefficient:Other_Coefficient) %>%
  mutate(Sig = case_when(
    Region == "NA_Coefficient" ~ combined_importance_df$NA_sig[match(Feature, combined_importance_df$Feature)],
    Region == "EU_Coefficient" ~ combined_importance_df$EU_sig[match(Feature, combined_importance_df$Feature)],
    Region == "JP_Coefficient" ~ combined_importance_df$JP_sig[match(Feature, combined_importance_df$Feature)],
    Region == "Other_Coefficient" ~ combined_importance_df$Other_sig[match(Feature, combined_importance_df$Feature)],
    TRUE ~ NA_real_
  ))


plot_na <- ggplot(combined_importance_long[combined_importance_long$Region == "NA_Coefficient", ], 
                  aes(x = Feature, y = abs(Importance), fill = factor(Sig))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("1" = "skyblue", "0" = "skyblue"), guide = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("NA Region Importance") +
  ylab("Importance") +
  xlab("Features")

plot_eu <- ggplot(combined_importance_long[combined_importance_long$Region == "EU_Coefficient", ], 
                  aes(x = Feature, y = abs(Importance), fill = factor(Sig))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("1" = "skyblue", "0" = "skyblue"), guide = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("EU Region Importance") +
  ylab("Importance") +
  xlab("Features")

plot_jp <- ggplot(combined_importance_long[combined_importance_long$Region == "JP_Coefficient", ], 
                  aes(x = Feature, y = abs(Importance), fill = factor(Sig))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("1" = "skyblue", "0" = "skyblue"), guide = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("JP Region Importance") +
  ylab("Importance") +
  xlab("Features")

plot_other <- ggplot(combined_importance_long[combined_importance_long$Region == "Other_Coefficient", ], 
                  aes(x = Feature, y = abs(Importance), fill = factor(Sig))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("1" = "skyblue", "0" = "skyblue"), guide = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("Other Region Importance") +
  ylab("Importance") +
  xlab("Features")


# Arrange the plots in a 2x2 grid layout
grid.arrange(plot_na, plot_eu, plot_jp, plot_other, ncol = 2, nrow = 2)

# Global plot

importance_lr_global <- importance_df_lr_global %>%
  arrange(desc(Coefficient))
importance_lr_global <- importance_lr_global %>%
  filter(Feature != "(Intercept)")
importance_lr_global$Feature <- factor(importance_lr_global$Feature, levels = importance_lr_global$Feature)
#combined_importance_long <- combined_importance_df %>%
#  gather(key = "Region", value = "Importance", NA_Coefficient:Other_Coefficient)

feature_order <- levels(importance_df_lr_global$Feature)
importance_df_lr_global <- importance_df_lr_global %>%
  mutate(Feature = factor(Feature, levels = feature_order))


ggplot(importance_lr_global, aes(x = Feature, y = Coefficient, fill = Coefficient < 0)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("FALSE" = "skyblue", "TRUE" = "#FA8072"), guide = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("Impact on Global Sales") +
  ylab("Coefficient") +
  xlab("Features")

# RANDOM FOREST----

df_all_log <- read.csv("df_all_log.csv", header = TRUE)
set.seed(467)
smp_size <- floor(0.8 * nrow(df_all_log))
training_indices <- sample(nrow(df_all_log), smp_size)
train <- df_all_log[training_indices,]
test <- df_all_log[-training_indices,]
all_columns <- colnames(df_all_log)
exclude_columns <- c("Franchise_Wii", "Log_NA_Sales", "Log_EU_Sales", "Log_JP_Sales", "Log_Other_Sales", "Log_Global_Sales")
predictor_columns <- setdiff(all_columns, exclude_columns)

# Define the control for cross-validation
control <- trainControl(method = "cv", number = 5, search = "grid")

# Define the grid of hyperparameters to tune
tuneGrid <- expand.grid(
  .mtry = c(2, 4, 6)
)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

## NA----
# Train the random forest model
model_rf_na <- train(
  Log_NA_Sales ~ .,
  data = train[, c(predictor_columns, "Log_NA_Sales")],
  method = "rf",
  trControl = control,
  tuneGrid = tuneGrid,
  ntree = 200,
  nodesize = 10
)

# Print the best parameters
best_param <- model_rf_na$bestTune
sum_model <- summary(model_rf_na)

# Make predictions on the training data
train_predictions_rf_na<- predict(model_rf_na, train[, predictor_columns])

# Make predictions on the testing data
test_predictions_rf_na <- predict(model_rf_na, test[, predictor_columns])

# Calculate RMSE, R-squared, MAE, and MSE for training data
train_rmse <- RMSE(train_predictions_rf_na, train$Log_NA_Sales)
train_r2 <- R2(train_predictions_rf_na, train$Log_NA_Sales)
train_mae <- MAE(train_predictions_rf_na, train$Log_NA_Sales)
train_mse <- mean((train_predictions_rf_na - train$Log_NA_Sales)^2)

# Calculate RMSE, R-squared, MAE, and MSE for testing data
test_rmse <- RMSE(test_predictions_rf_na, test$Log_NA_Sales)
test_r2 <- R2(test_predictions_rf_na, test$Log_NA_Sales)
test_mae <- MAE(test_predictions_rf_na, test$Log_NA_Sales)
test_mse <- mean((test_predictions_rf_na - test$Log_NA_Sales)^2)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r2, test_r2))

# Return the results
results_rf_na <- list(
  Summary = sum_model,
  BestTune = best_param,
  Metrics = metrics)

# Print results
print(results_rf_na)

# Model Analysis
plot(model_rf_na)

# Feature Importance
importance <- varImp(model_rf_na, scale = FALSE)
plot(importance, main = "NA_Sales: Feature Importance")
importance_df_rf_na <- as.data.frame(importance$importance)

# Calculate residuals for training and testing data
train_residuals_rf_na <- train$Log_NA_Sales - train_predictions_rf_na
test_residuals_rf_na <- test$Log_NA_Sales - test_predictions_rf_na

## EU----
# Train the random forest model
model_rf_eu <- train(
  Log_EU_Sales ~ .,
  data = train[, c(predictor_columns, "Log_EU_Sales")],
  method = "rf",
  trControl = control,
  tuneGrid = tuneGrid,
  ntree = 200,
  nodesize = 10
)

# Print the best parameters
best_param <- model_rf_eu$bestTune
sum_model <- summary(model_rf_eu)

# Make predictions on the training data
train_predictions_rf_eu<- predict(model_rf_eu, train[, predictor_columns])

# Make predictions on the testing data
test_predictions_rf_eu <- predict(model_rf_eu, test[, predictor_columns])

# Calculate RMSE, R-squared, MAE, and MSE for training data
train_rmse <- RMSE(train_predictions_rf_eu, train$Log_EU_Sales)
train_r2 <- R2(train_predictions_rf_eu, train$Log_EU_Sales)
train_mae <- MAE(train_predictions_rf_eu, train$Log_EU_Sales)
train_mse <- mean((train_predictions_rf_eu - train$Log_EU_Sales)^2)

# Calculate RMSE, R-squared, MAE, and MSE for testing data
test_rmse <- RMSE(test_predictions_rf_eu, test$Log_EU_Sales)
test_r2 <- R2(test_predictions_rf_eu, test$Log_EU_Sales)
test_mae <- MAE(test_predictions_rf_eu, test$Log_EU_Sales)
test_mse <- mean((test_predictions_rf_eu - test$Log_EU_Sales)^2)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r2, test_r2))

# Return the results
results_rf_eu <- list(
  Summary = sum_model,
  BestTune = best_param,
  Metrics = metrics)

# Print results
print(results_rf_eu)

# Model Analysis
plot(model_rf_eu)

# Feature Importance
importance <- varImp(model_rf_eu, scale = FALSE)
plot(importance, main = "EU_Sales: Feature Importance")
importance_df_rf_eu <- as.data.frame(importance$importance)

# Calculate residuals for training and testing data
train_residuals_rf_eu <- train$Log_EU_Sales - train_predictions_rf_eu
test_residuals_rf_eu <- test$Log_EU_Sales - test_predictions_rf_eu

## JP----
# Train the random forest model
model_rf_jp <- train(
  Log_JP_Sales ~ .,
  data = train[, c(predictor_columns, "Log_JP_Sales")],
  method = "rf",
  trControl = control,
  tuneGrid = tuneGrid,
  ntree = 200,
  nodesize = 10
)

# Print the best parameters
best_param <- model_rf_jp$bestTune
sum_model <- summary(model_rf_jp)

# Make predictions on the training data
train_predictions_rf_jp<- predict(model_rf_jp, train[, predictor_columns])

# Make predictions on the testing data
test_predictions_rf_jp <- predict(model_rf_jp, test[, predictor_columns])

# Calculate RMSE, R-squared, MAE, and MSE for training data
train_rmse <- RMSE(train_predictions_rf_jp, train$Log_JP_Sales)
train_r2 <- R2(train_predictions_rf_jp, train$Log_JP_Sales)
train_mae <- MAE(train_predictions_rf_jp, train$Log_JP_Sales)
train_mse <- mean((train_predictions_rf_jp - train$Log_JP_Sales)^2)

# Calculate RMSE, R-squared, MAE, and MSE for testing data
test_rmse <- RMSE(test_predictions_rf_jp, test$Log_JP_Sales)
test_r2 <- R2(test_predictions_rf_jp, test$Log_JP_Sales)
test_mae <- MAE(test_predictions_rf_jp, test$Log_JP_Sales)
test_mse <- mean((test_predictions_rf_jp - test$Log_JP_Sales)^2)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r2, test_r2))

# Return the results
results_rf_jp <- list(
  Summary = sum_model,
  BestTune = best_param,
  Metrics = metrics)

# Print results
print(results_rf_jp)

# Model Analysis
plot(model_rf_jp)

# Feature Importance
importance <- varImp(model_rf_jp, scale = FALSE)
plot(importance, main = "JP_Sales: Feature Importance")
importance_df_rf_jp <- as.data.frame(importance$importance)

# Calculate residuals for training and testing data
train_residuals_rf_jp <- train$Log_JP_Sales - train_predictions_rf_jp
test_residuals_rf_jp <- test$Log_JP_Sales - test_predictions_rf_jp

## Other----
# Train the random forest model
model_rf_other <- train(
  Log_Other_Sales ~ .,
  data = train[, c(predictor_columns, "Log_Other_Sales")],
  method = "rf",
  trControl = control,
  tuneGrid = tuneGrid,
  ntree = 200,
  nodesize = 10
)

# Print the best parameters
best_param <- model_rf_other$bestTune
sum_model <- summary(model_rf_other)

# Make predictions on the training data
train_predictions_rf_other<- predict(model_rf_other, train[, predictor_columns])

# Make predictions on the testing data
test_predictions_rf_other <- predict(model_rf_other, test[, predictor_columns])

# Calculate RMSE, R-squared, MAE, and MSE for training data
train_rmse <- RMSE(train_predictions_rf_other, train$Log_Other_Sales)
train_r2 <- R2(train_predictions_rf_other, train$Log_Other_Sales)
train_mae <- MAE(train_predictions_rf_other, train$Log_Other_Sales)
train_mse <- mean((train_predictions_rf_other - train$Log_Other_Sales)^2)

# Calculate RMSE, R-squared, MAE, and MSE for testing data
test_rmse <- RMSE(test_predictions_rf_other, test$Log_Other_Sales)
test_r2 <- R2(test_predictions_rf_other, test$Log_Other_Sales)
test_mae <- MAE(test_predictions_rf_other, test$Log_Other_Sales)
test_mse <- mean((test_predictions_rf_other - test$Log_Other_Sales)^2)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r2, test_r2))

# Return the results
results_rf_other <- list(
  Summary = sum_model,
  BestTune = best_param,
  Metrics = metrics)

# Print results
print(results_rf_other)

# Model Analysis
plot(model_rf_other)

# Feature Importance
importance <- varImp(model_rf_other, scale = FALSE)
plot(importance, main = "Other_Sales: Feature Importance")
importance_df_rf_other <- as.data.frame(importance$importance)

# Calculate residuals for training and testing data
train_residuals_rf_other <- train$Log_Other_Sales - train_predictions_rf_other
test_residuals_rf_other <- test$Log_Other_Sales - test_predictions_rf_other

## Global----
# Train the random forest model
model_rf_global <- train(
  Log_Global_Sales ~ .,
  data = train[, c(predictor_columns, "Log_Global_Sales")],
  method = "rf",
  trControl = control,
  tuneGrid = tuneGrid,
  ntree = 200,
  nodesize = 10
)

# Print the best parameters
best_param <- model_rf_global$bestTune
sum_model <- summary(model_rf_global)

# Make predictions on the training data
train_predictions_rf_global<- predict(model_rf_global, train[, predictor_columns])

# Make predictions on the testing data
test_predictions_rf_global <- predict(model_rf_global, test[, predictor_columns])

# Calculate RMSE, R-squared, MAE, and MSE for training data
train_rmse <- RMSE(train_predictions_rf_global, train$Log_Global_Sales)
train_r2 <- R2(train_predictions_rf_global, train$Log_Global_Sales)
train_mae <- MAE(train_predictions_rf_global, train$Log_Global_Sales)
train_mse <- mean((train_predictions_rf_global - train$Log_Global_Sales)^2)

# Calculate RMSE, R-squared, MAE, and MSE for testing data
test_rmse <- RMSE(test_predictions_rf_global, test$Log_Global_Sales)
test_r2 <- R2(test_predictions_rf_global, test$Log_Global_Sales)
test_mae <- MAE(test_predictions_rf_global, test$Log_Global_Sales)
test_mse <- mean((test_predictions_rf_global - test$Log_Global_Sales)^2)

# Create a data frame for the metrics
metrics <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r2, test_r2))

# Return the results
results_rf_global <- list(
  Summary = sum_model,
  BestTune = best_param,
  Metrics = metrics)

# Print results
print(results_rf_global)

# Model Analysis
plot(model_rf_global)

# Feature Importance
importance <- varImp(model_rf_global, scale = FALSE)
plot(importance, main = "Global_Sales: Feature Importance")
importance_df_rf_global <- as.data.frame(importance$importance)

# Calculate residuals for training and testing data
train_residuals_rf_global <- train$Log_Global_Sales - train_predictions_rf_global
test_residuals_rf_global <- test$Log_Global_Sales - test_predictions_rf_global

## Importance scores----
stopCluster(cl)
registerDoSEQ()

importance_df_rf_na$Feature <- rownames(importance_df_rf_na)
importance_df_rf_na <- importance_df_rf_na %>% rename(NA_Imp = Overall)
importance_df_rf_eu$Feature <- rownames(importance_df_rf_eu)
importance_df_rf_eu <- importance_df_rf_eu %>% rename(EU_Imp = Overall)
importance_df_rf_jp$Feature <- rownames(importance_df_rf_jp)
importance_df_rf_jp <- importance_df_rf_jp %>% rename(JP_Imp = Overall)
importance_df_rf_other$Feature <- rownames(importance_df_rf_other)
importance_df_rf_other <- importance_df_rf_other %>% rename(Other_Imp = Overall)

importance_df_rf <- merge(importance_df_rf_na, importance_df_rf_eu, by = "Feature")
importance_df_rf <- merge(importance_df_rf, importance_df_rf_jp, by = "Feature")
importance_df_rf <- merge(importance_df_rf, importance_df_rf_other, by = "Feature")
importance_df_rf <- importance_df_rf[, c("Feature", "NA_Imp", "EU_Imp", "JP_Imp", "Other_Imp")]
write.csv(importance_df_rf, "Data2/RF_importance.csv", row.names = FALSE)
combined_importance_df <- importance_df_rf

combined_importance_df <- combined_importance_df %>%
  arrange(desc(NA_Imp))
combined_importance_df$Feature <- factor(combined_importance_df$Feature, levels = combined_importance_df$Feature)
combined_importance_long <- combined_importance_df %>%
  gather(key = "Region", value = "Importance", NA_Imp:Other_Imp)

plot_na <- ggplot(combined_importance_long[combined_importance_long$Region == "NA_Imp", ], 
                  aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("NA Region Importance") +
  ylab("Importance") +
  xlab("Features") +
  ylim(0, 80)

plot_eu <- ggplot(combined_importance_long[combined_importance_long$Region == "EU_Imp", ], 
                  aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgrey", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("EU Region Importance") +
  ylab("Importance") +
  xlab("Features") +
  ylim(0, 80)

plot_jp <- ggplot(combined_importance_long[combined_importance_long$Region == "JP_Imp", ], 
                  aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgrey", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("JP Region Importance") +
  ylab("Importance") +
  xlab("Features") +
  ylim(0, 80)

plot_other <- ggplot(combined_importance_long[combined_importance_long$Region == "Other_Imp", ], 
                     aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgrey", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("Other Region Importance") +
  ylab("Importance") +
  xlab("Features") +
  ylim(0, 80)

# Arrange the plots in a 2x2 grid layout
grid.arrange(plot_na, plot_eu, plot_jp, plot_other, ncol = 2, nrow = 2)

# XGBOOST----
df_all_log <- read.csv("df_all_log.csv", header = TRUE)
set.seed(467)

# Split the data into 80% train and 20% test
smp_size <- floor(0.8 * nrow(df_all_log))
training_indices <- sample(nrow(df_all_log), smp_size)
train <- df_all_log[training_indices, ]
test <- df_all_log[-training_indices, ]

# Define predictor variables
exclude_columns <- c("Franchise_Wii", "Log_NA_Sales", "Log_EU_Sales", "Log_JP_Sales", "Log_Other_Sales", "Log_Global_Sales")
all_columns <- colnames(df_all_log)
predictor_columns <- setdiff(all_columns, exclude_columns)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

## NA----
target <- "Log_NA_Sales"

# Prepare training and test data matrices for XGBoost
train_matrix <- model.matrix(as.formula(paste(target, "~ . - 1")), data = train[, c(target, predictor_columns)])
test_matrix <- model.matrix(as.formula(paste(target, "~ . - 1")), data = test[, c(target, predictor_columns)])

# Convert target variable to numeric vectors
train_labels_xgb_na <- train[[target]]
test_labels_xgb_na <- test[[target]]

# Set up training control with cross-validation and random search
train_control <- trainControl(
  method = "cv",
  number = 5,
  search = "random",
  verboseIter = TRUE,
  allowParallel = TRUE
)

# Limit the number of random combinations to try
random_search_iterations <- 20

# Train the XGBoost model
xgb_train_na <- train(
  x = train_matrix,
  y = train_labels_xgb_na,
  trControl = train_control,
  tuneLength = random_search_iterations,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE"
)

# Use the best parameters to train on the full dataset
best_params <- xgb_train_na$bestTune

model_xgb_na <- xgb.train(
  params = list(
    objective = "reg:squarederror",
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    gamma = best_params$gamma,
    colsample_bytree = best_params$colsample_bytree,
    min_child_weight = best_params$min_child_weight,
    subsample = best_params$subsample
  ),
  data = xgb.DMatrix(data = train_matrix, label = train_labels_xgb_na),
  nrounds = best_params$nrounds
)

# Predict and evaluate
train_predictions_xgb_na <- predict(model_xgb_na, xgb.DMatrix(data = train_matrix), iteration_range = c(1, best_params$nrounds))
test_predictions_xgb_na <- predict(model_xgb_na, xgb.DMatrix(data = test_matrix), iteration_range = c(1, best_params$nrounds))

# Calculate evaluation metrics for training data
train_mae <- mae(train_labels_xgb_na, train_predictions_xgb_na)
train_mse <- mse(train_labels_xgb_na, train_predictions_xgb_na)
train_rmse <- rmse(train_labels_xgb_na, train_predictions_xgb_na)
train_r_squared <- 1 - (sum((train_labels_xgb_na - train_predictions_xgb_na)^2) / sum((train_labels_xgb_na - mean(train_labels_xgb_na))^2))

# Calculate evaluation metrics for test data
test_mae <- mae(test_labels_xgb_na, test_predictions_xgb_na)
test_mse <- mse(test_labels_xgb_na, test_predictions_xgb_na)
test_rmse <- rmse(test_labels_xgb_na, test_predictions_xgb_na)
test_r_squared <- 1 - (sum((test_labels_xgb_na - test_predictions_xgb_na)^2) / sum((test_labels_xgb_na - mean(test_labels_xgb_na))^2))

# Create a data frame for the metrics
metrics_xgb_na <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared)
)

# Print metrics
print(metrics_xgb_na)

# Interpret the model using feature importance
importance_matrix <- xgb.importance(model = model_xgb_na)
xgb.plot.importance(importance_matrix, main = "NA: Feature Importance")
importance_df_xgb_na <- importance_matrix %>%
  select(Feature, Importance) %>%
  rename(NA_Imp = Importance) %>%
  mutate(NA_Imp = NA_Imp * 100)

# Calculate residuals for training and testing data
train_residuals_xgb_na <- train_labels_xgb_na - train_predictions_xgb_na
test_residuals_xgb_na <- test_labels_xgb_na  - test_predictions_xgb_na

## EU----
target <- "Log_EU_Sales"

# Prepare training and test data matrices for XGBoost
train_matrix <- model.matrix(as.formula(paste(target, "~ . - 1")), data = train[, c(target, predictor_columns)])
test_matrix <- model.matrix(as.formula(paste(target, "~ . - 1")), data = test[, c(target, predictor_columns)])

# Convert target variable to numeric vectors
train_labels_xgb_eu <- train[[target]]
test_labels_xgb_eu <- test[[target]]

# Set up training control with cross-validation and random search
train_control <- trainControl(
  method = "cv",
  number = 5,
  search = "random",
  verboseIter = TRUE,
  allowParallel = TRUE
)

# Limit the number of random combinations to try
random_search_iterations <- 20

# Train the XGBoost model
train_xgb_eu <- train(
  x = train_matrix,
  y = train_labels_xgb_eu,
  trControl = train_control,
  tuneLength = random_search_iterations,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE"
)

# Use the best parameters to train on the full dataset
best_params <- train_xgb_eu$bestTune
best_params

model_xgb_eu <- xgb.train(
  params = list(
    objective = "reg:squarederror",
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    gamma = best_params$gamma,
    colsample_bytree = best_params$colsample_bytree,
    min_child_weight = best_params$min_child_weight,
    subsample = best_params$subsample
  ),
  data = xgb.DMatrix(data = train_matrix, label = train_labels_xgb_eu),
  nrounds = best_params$nrounds
)

# Predict and evaluate
train_predictions_xgb_eu <- predict(model_xgb_eu, xgb.DMatrix(data = train_matrix), iteration_range = c(1, best_params$nrounds))
test_predictions_xgb_eu <- predict(model_xgb_eu, xgb.DMatrix(data = test_matrix), iteration_range = c(1, best_params$nrounds))

# Calculate evaluation metrics for training data
train_mae <- mae(train_labels_xgb_eu, train_predictions_xgb_eu)
train_mse <- mse(train_labels_xgb_eu, train_predictions_xgb_eu)
train_rmse <- rmse(train_labels_xgb_eu, train_predictions_xgb_eu)
train_r_squared <- 1 - (sum((train_labels_xgb_eu - train_predictions_xgb_eu)^2) / sum((train_labels_xgb_eu - mean(train_labels_xgb_eu))^2))

# Calculate evaluation metrics for test data
test_mae <- mae(test_labels_xgb_eu, test_predictions_xgb_eu)
test_mse <- mse(test_labels_xgb_eu, test_predictions_xgb_eu)
test_rmse <- rmse(test_labels_xgb_eu, test_predictions_xgb_eu)
test_r_squared <- 1 - (sum((test_labels_xgb_eu - test_predictions_xgb_eu)^2) / sum((test_labels_xgb_eu - mean(test_labels_xgb_eu))^2))

# Create a data frame for the metrics
metrics_xgb_eu <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared)
)

# Print metrics
print(metrics_xgb_eu)

# Interpret the model using feature importance
importance_matrix <- xgb.importance(model = model_xgb_eu)
xgb.plot.importance(importance_matrix, main = "EU: Feature Importance")
importance_df_xgb_eu <- importance_matrix %>%
  select(Feature, Importance) %>%
  rename(EU_Imp = Importance) %>%
  mutate(EU_Imp = EU_Imp * 100)

# Calculate residuals for training and testing data
train_residuals_xgb_eu <- train_labels_xgb_eu - train_predictions_xgb_eu
test_residuals_xgb_eu <- test_labels_xgb_eu  - test_predictions_xgb_eu

## JP----
target <- "Log_JP_Sales"

# Prepare training and test data matrices for XGBoost
train_matrix <- model.matrix(as.formula(paste(target, "~ . - 1")), data = train[, c(target, predictor_columns)])
test_matrix <- model.matrix(as.formula(paste(target, "~ . - 1")), data = test[, c(target, predictor_columns)])

# Convert target variable to numeric vectors
train_labels_xgb_jp <- train[[target]]
test_labels_xgb_jp <- test[[target]]

# Set up training control with cross-validation and random search
train_control <- trainControl(
  method = "cv",
  number = 5,
  search = "random",
  verboseIter = TRUE,
  allowParallel = TRUE
)

# Limit the number of random combinations to try
random_search_iterations <- 20

# Train the XGBoost model
train_xgb_jp <- train(
  x = train_matrix,
  y = train_labels_xgb_jp,
  trControl = train_control,
  tuneLength = random_search_iterations,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE"
)

# Use the best parameters to train on the full dataset
best_params <- train_xgb_jp$bestTune
best_params

model_xgb_jp <- xgb.train(
  params = list(
    objective = "reg:squarederror",
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    gamma = best_params$gamma,
    colsample_bytree = best_params$colsample_bytree,
    min_child_weight = best_params$min_child_weight,
    subsample = best_params$subsample
  ),
  data = xgb.DMatrix(data = train_matrix, label = train_labels_xgb_jp),
  nrounds = best_params$nrounds
)

# Predict and evaluate
train_predictions_xgb_jp <- predict(model_xgb_jp, xgb.DMatrix(data = train_matrix), iteration_range = c(1, best_params$nrounds))
test_predictions_xgb_jp <- predict(model_xgb_jp, xgb.DMatrix(data = test_matrix), iteration_range = c(1, best_params$nrounds))

# Calculate evaluation metrics for training data
train_mae <- mae(train_labels_xgb_jp, train_predictions_xgb_jp)
train_mse <- mse(train_labels_xgb_jp, train_predictions_xgb_jp)
train_rmse <- rmse(train_labels_xgb_jp, train_predictions_xgb_jp)
train_r_squared <- 1 - (sum((train_labels_xgb_jp - train_predictions_xgb_jp)^2) / sum((train_labels_xgb_jp - mean(train_labels_xgb_jp))^2))

# Calculate evaluation metrics for test data
test_mae <- mae(test_labels_xgb_jp, test_predictions_xgb_jp)
test_mse <- mse(test_labels_xgb_jp, test_predictions_xgb_jp)
test_rmse <- rmse(test_labels_xgb_jp, test_predictions_xgb_jp)
test_r_squared <- 1 - (sum((test_labels_xgb_jp - test_predictions_xgb_jp)^2) / sum((test_labels_xgb_jp - mean(test_labels_xgb_jp))^2))

# Create a data frame for the metrics
metrics_xgb_jp <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared)
)

# Print metrics
print(metrics_xgb_jp)

# Interpret the model using feature importance
importance_matrix <- xgb.importance(model = model_xgb_jp)
xgb.plot.importance(importance_matrix, main = "JP: Feature Importance")
importance_df_xgb_jp <- importance_matrix %>%
  select(Feature, Importance) %>%
  rename(JP_Imp = Importance) %>%
  mutate(JP_Imp = JP_Imp * 100)

# Calculate residuals for training and testing data
train_residuals_xgb_jp <- train_labels_xgb_jp - train_predictions_xgb_jp
test_residuals_xgb_jp <- test_labels_xgb_jp  - test_predictions_xgb_jp

## Other----
target <- "Log_Other_Sales"

# Prepare training and test data matrices for XGBoost
train_matrix <- model.matrix(as.formula(paste(target, "~ . - 1")), data = train[, c(target, predictor_columns)])
test_matrix <- model.matrix(as.formula(paste(target, "~ . - 1")), data = test[, c(target, predictor_columns)])

# Convert target variable to numeric vectors
train_labels_xgb_other <- train[[target]]
test_labels_xgb_other <- test[[target]]

# Set up training control with cross-validation and random search
train_control <- trainControl(
  method = "cv",
  number = 5,
  search = "random",
  verboseIter = TRUE,
  allowParallel = TRUE
)

# Limit the number of random combinations to try
random_search_iterations <- 20

# Train the XGBoost model
train_xgb_other <- train(
  x = train_matrix,
  y = train_labels_xgb_other,
  trControl = train_control,
  tuneLength = random_search_iterations,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE"
)

# Use the best parameters to train on the full dataset
best_params <- train_xgb_other$bestTune
best_params

model_xgb_other <- xgb.train(
  params = list(
    objective = "reg:squarederror",
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    gamma = best_params$gamma,
    colsample_bytree = best_params$colsample_bytree,
    min_child_weight = best_params$min_child_weight,
    subsample = best_params$subsample
  ),
  data = xgb.DMatrix(data = train_matrix, label = train_labels_xgb_other),
  nrounds = best_params$nrounds
)

# Predict and evaluate
train_predictions_xgb_other <- predict(model_xgb_other, xgb.DMatrix(data = train_matrix), iteration_range = c(1, best_params$nrounds))
test_predictions_xgb_other <- predict(model_xgb_other, xgb.DMatrix(data = test_matrix), iteration_range = c(1, best_params$nrounds))

# Calculate evaluation metrics for training data
train_mae <- mae(train_labels_xgb_other, train_predictions_xgb_other)
train_mse <- mse(train_labels_xgb_other, train_predictions_xgb_other)
train_rmse <- rmse(train_labels_xgb_other, train_predictions_xgb_other)
train_r_squared <- 1 - (sum((train_labels_xgb_other - train_predictions_xgb_other)^2) / sum((train_labels_xgb_other - mean(train_labels_xgb_other))^2))

# Calculate evaluation metrics for test data
test_mae <- mae(test_labels_xgb_other, test_predictions_xgb_other)
test_mse <- mse(test_labels_xgb_other, test_predictions_xgb_other)
test_rmse <- rmse(test_labels_xgb_other, test_predictions_xgb_other)
test_r_squared <- 1 - (sum((test_labels_xgb_other - test_predictions_xgb_other)^2) / sum((test_labels_xgb_other - mean(test_labels_xgb_other))^2))

# Create a data frame for the metrics
metrics_xgb_other <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared)
)

# Print metrics
print(metrics_xgb_other)

# Interpret the model using feature importance
importance_matrix <- xgb.importance(model = model_xgb_other)
xgb.plot.importance(importance_matrix, main = "Other: Feature Importance")
importance_df_xgb_other <- importance_matrix %>%
  select(Feature, Importance) %>%
  rename(Other_Imp = Importance) %>%
  mutate(Other_Imp = Other_Imp * 100)

# Calculate residuals for training and testing data
train_residuals_xgb_other <- train_labels_xgb_other - train_predictions_xgb_other
test_residuals_xgb_other <- test_labels_xgb_other  - test_predictions_xgb_other

## Global----
target <- "Log_Global_Sales"

# Prepare training and test data matrices for XGBoost
train_matrix <- model.matrix(as.formula(paste(target, "~ . - 1")), data = train[, c(target, predictor_columns)])
test_matrix <- model.matrix(as.formula(paste(target, "~ . - 1")), data = test[, c(target, predictor_columns)])

# Convert target variable to numeric vectors
train_labels_xgb_global <- train[[target]]
test_labels_xgb_global <- test[[target]]

# Set up training control with cross-validation and random search
train_control <- trainControl(
  method = "cv",
  number = 5,
  search = "random",
  verboseIter = TRUE,
  allowParallel = TRUE
)

# Limit the number of random combinations to try
random_search_iterations <- 20

# Train the XGBoost model
train_xgb_global <- train(
  x = train_matrix,
  y = train_labels_xgb_global,
  trControl = train_control,
  tuneLength = random_search_iterations,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE"
)

# Use the best parameters to train on the full dataset
best_params <- train_xgb_global$bestTune
best_params

model_xgb_global <- xgb.train(
  params = list(
    objective = "reg:squarederror",
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    gamma = best_params$gamma,
    colsample_bytree = best_params$colsample_bytree,
    min_child_weight = best_params$min_child_weight,
    subsample = best_params$subsample
  ),
  data = xgb.DMatrix(data = train_matrix, label = train_labels_xgb_global),
  nrounds = best_params$nrounds
)

# Predict and evaluate
train_predictions_xgb_global <- predict(model_xgb_global, xgb.DMatrix(data = train_matrix), iteration_range = c(1, best_params$nrounds))
test_predictions_xgb_global <- predict(model_xgb_global, xgb.DMatrix(data = test_matrix), iteration_range = c(1, best_params$nrounds))

# Calculate evaluation metrics for training data
train_mae <- mae(train_labels_xgb_global, train_predictions_xgb_global)
train_mse <- mse(train_labels_xgb_global, train_predictions_xgb_global)
train_rmse <- rmse(train_labels_xgb_global, train_predictions_xgb_global)
train_r_squared <- 1 - (sum((train_labels_xgb_global - train_predictions_xgb_global)^2) / sum((train_labels_xgb_global - mean(train_labels_xgb_global))^2))

# Calculate evaluation metrics for test data
test_mae <- mae(test_labels_xgb_global, test_predictions_xgb_global)
test_mse <- mse(test_labels_xgb_global, test_predictions_xgb_global)
test_rmse <- rmse(test_labels_xgb_global, test_predictions_xgb_global)
test_r_squared <- 1 - (sum((test_labels_xgb_global - test_predictions_xgb_global)^2) / sum((test_labels_xgb_global - mean(test_labels_xgb_global))^2))

# Create a data frame for the metrics
metrics_xgb_global <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared)
)

# Return the results
results_xgb_global <- list(
  Summary = summary(model_xgb_global),
  Metrics = metrics_xgb_global)

# Print results
print(results_xgb_global)

# Interpret the model using feature importance
importance_matrix <- xgb.importance(model = model_xgb_global)
xgb.plot.importance(importance_matrix, main = "Global: Feature Importance")
importance_df_xgb_global <- importance_matrix %>%
  select(Feature, Importance) %>%
  rename(Global_Imp = Importance) %>%
  mutate(Global_Imp = Global_Imp * 100)

# Calculate residuals for training and testing data
train_residuals_xgb_global <- train_labels_xgb_global - train_predictions_xgb_global
test_residuals_xgb_global <- test_labels_xgb_global  - test_predictions_xgb_global

## Importance scores----
stopCluster(cl)
registerDoSEQ()

importance_df_xgb <- merge(importance_df_xgb_na, importance_df_xgb_eu, by = "Feature")
importance_df_xgb <- merge(importance_df_xgb, importance_df_xgb_jp, by = "Feature")
importance_df_xgb <- merge(importance_df_xgb, importance_df_xgb_other, by = "Feature")
importance_df_xgb <- importance_df_xgb[, c("Feature", "NA_Imp", "EU_Imp", "JP_Imp", "Other_Imp")]
write.csv(importance_df_xgb, "Data2/XGB_importance.csv", row.names = FALSE)
combined_importance_df <- importance_df_xgb

combined_importance_df <- combined_importance_df %>%
  arrange(desc(NA_Imp))
combined_importance_df$Feature <- factor(combined_importance_df$Feature, levels = combined_importance_df$Feature)
combined_importance_long <- combined_importance_df %>%
  gather(key = "Region", value = "Importance", NA_Imp:Other_Imp)


plot_na <- ggplot(combined_importance_long[combined_importance_long$Region == "NA_Imp", ], 
                  aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("NA Region Importance") +
  ylab("Importance") +
  xlab("Features") +
  ylim(0, 30)

plot_eu <- ggplot(combined_importance_long[combined_importance_long$Region == "EU_Imp", ], 
                  aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgrey", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("EU Region Importance") +
  ylab("Importance") +
  xlab("Features") +
  ylim(0, 30)

plot_jp <- ggplot(combined_importance_long[combined_importance_long$Region == "JP_Imp", ], 
                  aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgrey", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("JP Region Importance") +
  ylab("Importance") +
  xlab("Features") +
  ylim(0, 30)

plot_other <- ggplot(combined_importance_long[combined_importance_long$Region == "Other_Imp", ], 
                     aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgrey", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("Other Region Importance") +
  ylab("Importance") +
  xlab("Features") +
  ylim(0, 30)

# Arrange the plots in a 2x2 grid layout
grid.arrange(plot_na, plot_eu, plot_jp, plot_other, ncol = 2, nrow = 2)

# ANN----
df_all_log <- read.csv("df_all_log.csv", header = TRUE)
set.seed(467)

# Split the data into 80% train and 20% test
smp_size <- floor(0.8 * nrow(df_all_log))
training_indices <- sample(nrow(df_all_log), smp_size)
train <- df_all_log[training_indices, ]
test <- df_all_log[-training_indices, ]

# Define predictor variables
exclude_columns <- c("Franchise_Wii", "Log_NA_Sales", "Log_EU_Sales", "Log_JP_Sales", "Log_Other_Sales", "Log_Global_Sales")
all_columns <- colnames(df_all_log)
predictor_columns <- setdiff(all_columns, exclude_columns)

## NA----
target <- "Log_NA_Sales"

# Create train and test data
train_x <- train[, predictor_columns]
train_y <- train[, target]
test_x <- test[, predictor_columns]
test_y <- test[, target]
train <- cbind(train_y, train_x)
colnames(train)[1] <- target

# Define the control for cross-validation
train_control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# Define the grid of hyperparameters to tune
tune_grid <- expand.grid(size = c(10, 15, 20), 
                         decay = c(0.1, 0.01, 0.001))

# Train the ANN model
set.seed(467)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
model_ann_na <- train(train_x, train_y,
                      method = "nnet",
                      trControl = train_control,
                      tuneGrid = tune_grid,
                      linout = TRUE,
                      trace = TRUE,
                      MaxNWts = 10000,
                      maxit = 100)
stopCluster(cl)
registerDoSEQ()

# View the results
print(model_ann_na)

# Get the best model parameters
best_params <- model_ann_na$bestTune
print(best_params)

# Extract the best model
best_ann <- model_ann_na$finalModel

# Plot the network
plotnet(best_ann, struct = TRUE)

# Predict and evaluate
train_predictions_ann_na <- predict(model_ann_na, newdata = train_x)
test_predictions_ann_na <- predict(model_ann_na, newdata = test_x)

# Calculate evaluation metrics for training data
train_mae <- mae(train_y, train_predictions_ann_na)
train_mse <- mse(train_y, train_predictions_ann_na)
train_rmse <- rmse(train_y, train_predictions_ann_na)
train_r_squared <- 1 - (sum((train_y - train_predictions_ann_na)^2) / sum((train_y - mean(train_y))^2))

# Calculate evaluation metrics for test data
test_mae <- mae(test_y, test_predictions_ann_na)
test_mse <- mse(test_y, test_predictions_ann_na)
test_rmse <- rmse(test_y, test_predictions_ann_na)
test_r_squared <- 1 - (sum((test_y - test_predictions_ann_na)^2) / sum((test_y - mean(test_y))^2))

# Create a data frame for the metrics
metrics_ann_na <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared)
)

# Print metrics
print(metrics_ann_na)

# Extract the best parameters from the ann_model
best_size <- best_params$size
best_decay <- best_params$decay

# Variable importance
importance <- varImp(model_ann_na)
plot(importance, main = "NA: Variable Importance")
importance_df_ann_na <- data.frame(Feature = rownames(importance$importance), NA_Imp = importance$importance$Overall)
rownames(importance_df_ann_na) <- NULL

# Learning curve
plot(model_ann_na)

# Calculate Residuals
train_residuals_ann_na <- train_y - train_predictions_ann_na
test_residuals_ann_na <- test_y - test_predictions_ann_na

## EU----
target <- "Log_EU_Sales"

# Create train and test data
train_x <- train[, predictor_columns]
train_y <- train[, target]
test_x <- test[, predictor_columns]
test_y <- test[, target]

# Define the control for cross-validation
train_control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# Define the grid of hyperparameters to tune
tune_grid <- expand.grid(size = c(10, 15, 20), 
                         decay = c(0.1, 0.01, 0.001))

# Train the ANN model
set.seed(467)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
model_ann_eu <- train(train_x, train_y,
                      method = "nnet",
                      trControl = train_control,
                      tuneGrid = tune_grid,
                      linout = TRUE,
                      trace = TRUE,
                      MaxNWts = 10000,
                      maxit = 100)
stopCluster(cl)
registerDoSEQ()

# View the results
print(model_ann_eu)

# Get the best model parameters
best_params <- model_ann_eu$bestTune
print(best_params)

# Extract the best model
best_ann <- model_ann_eu$finalModel

# Plot the network
plotnet(best_ann, struct = TRUE)

# Predict and evaluate
train_predictions_ann_eu <- predict(model_ann_eu, newdata = train_x)
test_predictions_ann_eu <- predict(model_ann_eu, newdata = test_x)

# Calculate evaluation metrics for training data
train_mae <- mae(train_y, train_predictions_ann_eu)
train_mse <- mse(train_y, train_predictions_ann_eu)
train_rmse <- rmse(train_y, train_predictions_ann_eu)
train_r_squared <- 1 - (sum((train_y - train_predictions_ann_eu)^2) / sum((train_y - mean(train_y))^2))

# Calculate evaluation metrics for test data
test_mae <- mae(test_y, test_predictions_ann_eu)
test_mse <- mse(test_y, test_predictions_ann_eu)
test_rmse <- rmse(test_y, test_predictions_ann_eu)
test_r_squared <- 1 - (sum((test_y - test_predictions_ann_eu)^2) / sum((test_y - mean(test_y))^2))

# Create a data frame for the metrics
metrics_ann_eu <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared)
)

# Print metrics
print(metrics_ann_eu)

# Extract the best parameters from the ann_model
best_size <- best_params$size
best_decay <- best_params$decay

# Variable importance
importance <- varImp(model_ann_eu)
plot(importance, main = "EU: Variable Importance")
importance_df_ann_eu <- data.frame(Feature = rownames(importance$importance), EU_Imp = importance$importance$Overall)
rownames(importance_df_ann_eu) <- NULL

# Learning curve
plot(model_ann_eu)

# Calculate Residuals
train_residuals_ann_eu <- train_y - train_predictions_ann_eu
test_residuals_ann_eu <- test_y - test_predictions_ann_eu

## JP----
target <- "Log_JP_Sales"

# Create train and test data
train_x <- train[, predictor_columns]
train_y <- train[, target]
test_x <- test[, predictor_columns]
test_y <- test[, target]

# Define the control for cross-validation
train_control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# Define the grid of hyperparameters to tune
tune_grid <- expand.grid(size = c(10, 15, 20), 
                         decay = c(0.1, 0.01, 0.001))

# Train the ANN model
set.seed(467)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
model_ann_jp <- train(train_x, train_y,
                      method = "nnet",
                      trControl = train_control,
                      tuneGrid = tune_grid,
                      linout = TRUE,
                      trace = TRUE,
                      MaxNWts = 10000,
                      maxit = 100)
stopCluster(cl)
registerDoSEQ()

# View the results
print(model_ann_jp)

# Get the best model parameters
best_params <- model_ann_jp$bestTune
print(best_params)

# Extract the best model
best_ann <- model_ann_jp$finalModel

# Plot the network
plotnet(best_ann, struct = TRUE)

# Predict and evaluate
train_predictions_ann_jp <- predict(model_ann_jp, newdata = train_x)
test_predictions_ann_jp <- predict(model_ann_jp, newdata = test_x)

# Calculate evaluation metrics for training data
train_mae <- mae(train_y, train_predictions_ann_jp)
train_mse <- mse(train_y, train_predictions_ann_jp)
train_rmse <- rmse(train_y, train_predictions_ann_jp)
train_r_squared <- 1 - (sum((train_y - train_predictions_ann_jp)^2) / sum((train_y - mean(train_y))^2))

# Calculate evaluation metrics for test data
test_mae <- mae(test_y, test_predictions_ann_jp)
test_mse <- mse(test_y, test_predictions_ann_jp)
test_rmse <- rmse(test_y, test_predictions_ann_jp)
test_r_squared <- 1 - (sum((test_y - test_predictions_ann_jp)^2) / sum((test_y - mean(test_y))^2))

# Create a data frame for the metrics
metrics_ann_jp <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared)
)

# Print metrics
print(metrics_ann_jp)

# Extract the best parameters from the ann_model
best_size <- best_params$size
best_decay <- best_params$decay

# Variable importance
importance <- varImp(model_ann_jp)
plot(importance, main = "JP: Variable Importance")
importance_df_ann_jp <- data.frame(Feature = rownames(importance$importance), JP_Imp = importance$importance$Overall)
rownames(importance_df_ann_jp) <- NULL

# Learning curve
plot(model_ann_jp)

# Calculate Residuals
train_residuals_ann_jp <- train_y - train_predictions_ann_jp
test_residuals_ann_jp <- test_y - test_predictions_ann_jp


## Other----
target <- "Log_Other_Sales"

# Create train and test data
train_x <- train[, predictor_columns]
train_y <- train[, target]
test_x <- test[, predictor_columns]
test_y <- test[, target]

# Define the control for cross-validation
train_control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# Define the grid of hyperparameters to tune
tune_grid <- expand.grid(size = c(10, 15, 20), 
                         decay = c(0.1, 0.01, 0.001))

# Train the ANN model
set.seed(467)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
model_ann_other <- train(train_x, train_y,
                         method = "nnet",
                         trControl = train_control,
                         tuneGrid = tune_grid,
                         linout = TRUE,
                         trace = TRUE,
                         MaxNWts = 10000,
                         maxit = 100)
stopCluster(cl)
registerDoSEQ()

# View the results
print(model_ann_other)

# Get the best model parameters
best_params <- model_ann_other$bestTune
print(best_params)

# Extract the best model
best_ann <- model_ann_other$finalModel

# Plot the network
plotnet(best_ann, struct = TRUE)

# Predict and evaluate
train_predictions_ann_other <- predict(model_ann_other, newdata = train_x)
test_predictions_ann_other <- predict(model_ann_other, newdata = test_x)

# Calculate evaluation metrics for training data
train_mae <- mae(train_y, train_predictions_ann_other)
train_mse <- mse(train_y, train_predictions_ann_other)
train_rmse <- rmse(train_y, train_predictions_ann_other)
train_r_squared <- 1 - (sum((train_y - train_predictions_ann_other)^2) / sum((train_y - mean(train_y))^2))

# Calculate evaluation metrics for test data
test_mae <- mae(test_y, test_predictions_ann_other)
test_mse <- mse(test_y, test_predictions_ann_other)
test_rmse <- rmse(test_y, test_predictions_ann_other)
test_r_squared <- 1 - (sum((test_y - test_predictions_ann_other)^2) / sum((test_y - mean(test_y))^2))

# Create a data frame for the metrics
metrics_ann_other <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared)
)

# Print metrics
print(metrics_ann_other)

# Extract the best parameters from the ann_model
best_size <- best_params$size
best_decay <- best_params$decay

# Variable importance
importance <- varImp(model_ann_other)
plot(importance, main = "Other: Variable Importance")
importance_df_ann_other <- data.frame(Feature = rownames(importance$importance), Other_Imp = importance$importance$Overall)
rownames(importance_df_ann_other) <- NULL

# Learning curve
plot(model_ann_other)

# Calculate Residuals
train_residuals_ann_other <- train_y - train_predictions_ann_other
test_residuals_ann_other <- test_y - test_predictions_ann_other


## Global----
target <- "Log_Global_Sales"

# Create train and test data
train_x <- train[, predictor_columns]
train_y <- train[, target]
test_x <- test[, predictor_columns]
test_y <- test[, target]

# Define the control for cross-validation
train_control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# Define the grid of hyperparameters to tune
tune_grid <- expand.grid(size = c(10, 15, 20), 
                         decay = c(0.1, 0.01, 0.001))

# Train the ANN model
set.seed(467)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
model_ann_global <- train(train_x, train_y,
                          method = "nnet",
                          trControl = train_control,
                          tuneGrid = tune_grid,
                          linout = TRUE,
                          trace = TRUE,
                          MaxNWts = 10000,
                          maxit = 100)
stopCluster(cl)
registerDoSEQ()

# View the results
print(model_ann_global)

# Get the best model parameters
best_params <- model_ann_global$bestTune
print(best_params)

# Extract the best model
best_ann <- model_ann_global$finalModel

# Plot the network
plotnet(best_ann, struct = TRUE)

# Predict and evaluate
train_predictions_ann_global <- predict(model_ann_global, newdata = train_x)
test_predictions_ann_global <- predict(model_ann_global, newdata = test_x)

# Calculate evaluation metrics for training data
train_mae <- mae(train_y, train_predictions_ann_global)
train_mse <- mse(train_y, train_predictions_ann_global)
train_rmse <- rmse(train_y, train_predictions_ann_global)
train_r_squared <- 1 - (sum((train_y - train_predictions_ann_global)^2) / sum((train_y - mean(train_y))^2))

# Calculate evaluation metrics for test data
test_mae <- mae(test_y, test_predictions_ann_global)
test_mse <- mse(test_y, test_predictions_ann_global)
test_rmse <- rmse(test_y, test_predictions_ann_global)
test_r_squared <- 1 - (sum((test_y - test_predictions_ann_global)^2) / sum((test_y - mean(test_y))^2))

# Create a data frame for the metrics
metrics_ann_global <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_mae, test_mae),
  MSE = c(train_mse, test_mse),
  RMSE = c(train_rmse, test_rmse),
  R_squared = c(train_r_squared, test_r_squared)
)

# Return the results
results_ann_global <- list(
  Summary = summary(model_ann_global),
  Metrics = metrics_ann_global)

# Print results
print(results_ann_global)

# Extract the best parameters from the ann_model
best_size <- best_params$size
best_decay <- best_params$decay

# Variable importance
importance <- varImp(model_ann_global)
plot(importance, main = "Global: Variable Importance")
importance_df_ann_global <- data.frame(Feature = rownames(importance$importance), Global_Imp = importance$importance$Overall)
rownames(importance_df_ann_global) <- NULL

# Learning curve
plot(model_ann_global)

# Calculate Residuals
train_residuals_ann_global <- train_y - train_predictions_ann_global
test_residuals_ann_global <- test_y - test_predictions_ann_global


## Importance scores----

importance_df_ann <- merge(importance_df_ann_na, importance_df_ann_eu, by = "Feature")
importance_df_ann <- merge(importance_df_ann, importance_df_ann_jp, by = "Feature")
importance_df_ann <- merge(importance_df_ann, importance_df_ann_other, by = "Feature")

combined_importance_df <- importance_df_ann
combined_importance_df <- combined_importance_df %>%
  arrange(desc(NA_Imp))
combined_importance_df$Feature <- factor(combined_importance_df$Feature, levels = combined_importance_df$Feature)
combined_importance_long <- combined_importance_df %>%
  gather(key = "Region", value = "Importance", NA_Imp:Other_Imp)


plot_na <- ggplot(combined_importance_long[combined_importance_long$Region == "NA_Imp", ], 
                  aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("NA Region Importance") +
  ylab("Importance") +
  xlab("Features")

plot_eu <- ggplot(combined_importance_long[combined_importance_long$Region == "EU_Imp", ], 
                  aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgrey", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("EU Region Importance") +
  ylab("Importance") +
  xlab("Features")

plot_jp <- ggplot(combined_importance_long[combined_importance_long$Region == "JP_Imp", ], 
                  aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgrey", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("JP Region Importance") +
  ylab("Importance") +
  xlab("Features")

plot_other <- ggplot(combined_importance_long[combined_importance_long$Region == "Other_Imp", ], 
                     aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "lightgrey", linewidth = 0.2),
        panel.grid.minor = element_line(color = "whitesmoke", linewidth = 0.2),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0)) +
  ggtitle("Other Region Importance") +
  ylab("Importance") +
  xlab("Features")

# Arrange the plots in a 2x2 grid layout
grid.arrange(plot_na, plot_eu, plot_jp, plot_other, ncol = 2, nrow = 2)

# PLOT ANALYSIS----

# Calculate common x and y limits across all training and test data
xlim_common <- range(c(train_predictions_lr_global, train_predictions_rf_global, 
                       train_predictions_xgb_global, train_predictions_ann_global,
                       test_predictions_lr_global, test_predictions_rf_global, 
                       test_predictions_xgb_global, test_predictions_ann_global))

ylim_common <- range(c(train_residuals_lr_global, train_residuals_rf_global, 
                       train_residuals_xgb_global, train_residuals_ann_global,
                       test_residuals_lr_global, test_residuals_rf_global, 
                       test_residuals_xgb_global, test_residuals_ann_global))

## GLOBAL TRAINING RESIDUALS----

par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
# Residual plot for LR training data
plot(train_predictions_lr_global, train_residuals_lr_global, 
     main = "Linear Regression Model",
     xlab = "Fitted values", ylab = "Residuals",
     col = "blue", pch = 20, xlim = xlim_common, ylim = ylim_common)
abline(h = 0, col = "red", lwd = 1)

# Residual plot for RF training data
plot(train_predictions_rf_global, train_residuals_rf_global, 
     main = "Random Forest Model",
     xlab = "Fitted values", ylab = "Residuals",
     col = "blue", pch = 20, xlim = xlim_common, ylim = ylim_common)
abline(h = 0, col = "red", lwd = 1)

# Residual plot for XGB training data
plot(train_predictions_xgb_global, train_residuals_xgb_global, 
     main = "XGBoost Model",
     xlab = "Fitted values", ylab = "Residuals",
     col = "blue", pch = 20, xlim = xlim_common, ylim = ylim_common)
abline(h = 0, col = "red", lwd = 1)

# Residual plot for ANN training data
plot(train_predictions_ann_global, train_residuals_ann_global, 
     main = "Neural Network Model",
     xlab = "Fitted values", ylab = "Residuals",
     col = "blue", pch = 20, xlim = xlim_common, ylim = ylim_common)
abline(h = 0, col = "red", lwd = 1)

# Add a main title to the entire window
mtext("Residual Plots on Training Data", outer = TRUE, side = 3, line = 1, cex = 1.5)

par(mfrow = c(1, 1))

## GLOBAL TESTING RESIDUALS----

par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
# Residual plot for LR test data
plot(test_predictions_lr_global, test_residuals_lr_global, 
     main = "Linear Regression Model",
     xlab = "Fitted values", ylab = "Residuals",
     col = "green", pch = 20, xlim = xlim_common, ylim = ylim_common)
abline(h = 0, col = "red", lwd = 1)

# Residual plot for RF test data
plot(test_predictions_rf_global, test_residuals_rf_global, 
     main = "Random Forest Model",
     xlab = "Fitted values", ylab = "Residuals",
     col = "green", pch = 20, xlim = xlim_common, ylim = ylim_common)
abline(h = 0, col = "red", lwd = 1)

# Residual plot for XGB test data
plot(test_predictions_xgb_global, test_residuals_xgb_global, 
     main = "XGBoost Model",
     xlab = "Fitted values", ylab = "Residuals",
     col = "green", pch = 20, xlim = xlim_common, ylim = ylim_common)
abline(h = 0, col = "red", lwd = 1)

# Residual plot for ANN test data
plot(test_predictions_ann_global, test_residuals_ann_global, 
     main = "Neural Network Model",
     xlab = "Fitted values", ylab = "Residuals",
     col = "green", pch = 20, xlim = xlim_common, ylim = ylim_common)
abline(h = 0, col = "red", lwd = 1)

# Add a main title to the entire window
mtext("Residual Plots on Test Data", outer = TRUE, side = 3, line = 1, cex = 1.5)

par(mfrow = c(1, 1))
