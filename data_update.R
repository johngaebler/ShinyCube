library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(scryr)
library(tidyr)
library(reshape2)
library(tibble)
library(shinythemes)
library(shinycssloaders)
library(rsconnect)
library(tibble)
library(vegan)
library(umap)
library(jsonlite)
library(shinyjs)
library(DT)
library(purrr)

# read in raw csv data
decks <- read.csv("Cube_Stats - Deck Info (15).csv")
decklists <- read.csv("Cube_Stats - All Decklists (4).csv", na.strings = c("", "NA"), check.names = FALSE)
game_log <- read.csv("Cube_Stats - game_log (8).csv", stringsAsFactors = F)
players <- read.csv("Cube_Stats - Players (1).csv", stringsAsFactors =F)
dir <- getwd()
decks$Date <- as.Date(decks$Date, format = "%m/%d/%y")

long_decklists <- decklists %>%
  pivot_longer(
    cols = everything(),
    names_to = "deck_id",
    values_to = "card"
  ) %>%
  filter(!is.na(card)) %>%
  distinct(deck_id, card)

binary_matrix <- long_decklists %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = card, values_from = present, values_fill = 0) %>%
  column_to_rownames("deck_id")
#data cleaning
decks$Deck.ID <- sub("^", "X", decks$Deck.ID)

#getting all of the scryfall data, run this if you havent yet to initialize scryfall data

# 1. Get metadata for Scryfall bulk data files
#bulk_meta <- fromJSON("https://api.scryfall.com/bulk-data")

# 2. Get the download URL for the "default_cards" bulk list
#bulk_url <- bulk_meta$data %>%
#  dplyr::filter(type == "default_cards") %>%
#  dplyr::pull(download_uri)

# 3. Download the full card list (~40MB)
#all_cards <- fromJSON(bulk_url)

# Optional: save to local RDS to avoid downloading every time
#saveRDS(all_cards, "scryfall_cards.rds")
# scryfall_data <- readRDS("scryfall_cards.rds")
# scryfall_data$image_url <- scryfall_data$image_uris$normal
# card_chunks <- split(scryfall_data, ceiling(seq_len(nrow(scryfall_data)) / 35000))
# saveRDS(card_chunks[[1]], "cards_part1.rds")
# saveRDS(card_chunks[[2]], "cards_part2.rds")
# saveRDS(card_chunks[[3]], "cards_part3.rds")
# saveRDS(card_chunks[[4]], "cards_part4.rds")

#once it is all saved, you can load it like this 
scryfall_data <- readRDS("C:\\scryfall_cards.rds")


used_card_names <- long_decklists %>%
  distinct(card) %>%
  pull(card)

# Step 2: Filter the full scryfall data
scryfall_trimmed <- scryfall_data %>%
  filter(name %in% used_card_names)
scryfall_trimmed$image_url <- scryfall_trimmed$image_uris$normal
scryfall_trimmed <- scryfall_trimmed %>% 
  select(name, cmc, type_line, mana_cost, image_url, oracle_text)
# Step 3: Save this trimmed version
saveRDS(scryfall_trimmed, "scryfall_cards_trimmed.rds")
# scryfall_data <- scryfall_data %>%
#   mutate(image_url = normal)
# Filter Scryfall cards to only those used in your cube
scryfall_data <- readRDS("scryfall_cards_trimmed.rds")
card_meta <- scryfall_data %>%
  dplyr::filter(name %in% colnames(binary_matrix)) %>%
  dplyr::select(name, type_line)

# Extract nonland card names
nonland_cards <- card_meta %>%
  filter(!grepl("\\bLand\\b", type_line, ignore.case = TRUE)) %>%
  pull(name)

#calculate winrate Data
cardWinrate <- function(card){
  #figure out the decks its in
  present <- c()
  for(i in 1:ncol(decklists)){
    present <- append(present, card %in% decklists[,i])
  }
  decknames <- colnames(decklists)[present]
  winrate <- sum(decks$Wins[which(decks$Deck.ID %in% decknames)]) /
    sum(decks$Games.Played[which(decks$Deck.ID %in% decknames)])
  #calculate winrate from weighted average deck winrate
  return(winrate)
}

playerWinrate <- function(player){
  winrate <- sum(decks$Wins[which(decks$PlayerName == player)]) /
    sum(decks$Games.Played[which(decks$PlayerName == player)])
  return(winrate)
}

playerGames <- function(player){
  games <- sum(decks$Games.Played[which(decks$PlayerName == player)])
  return(games)
}

archWinrate <- function(arch){
  winrate <- sum(decks$Wins[which(decks$Classification == arch)]) /
    sum(decks$Games.Played[which(decks$Classification == arch)])
  return(winrate)
}

archGames <- function(arch){
  games <- sum(decks$Games.Played[which(decks$Classification == arch)])
  return(games)
}

colorWinrate <- function(color){
  winrate <- sum(decks$Wins[which(grepl(color, decks$Color.Identity))]) /
    sum(decks$Games.Played[which(grepl(color, decks$Color.Identity))])
  return(winrate)
}

colorComboWinrate <- function(colorCombo){
  winrate <- sum(decks$Wins[which(decks$Color.Identity == colorCombo)]) /
    sum(decks$Games.Played[which(decks$Color.Identity == colorCombo)])
  return(winrate)
}

colorGames <- function(color){
  games <- sum(decks$Games.Played[which(grepl(color, decks$Color.Identity))])
  return(games)
}

colorComboGames <- function(colorCombo){
  games <- sum(decks$Games.Played[which(decks$Color.Identity == colorCombo)])
  return(games)
}


#playerstats
playerWinrates <- as.data.frame(sapply(unique(decks$PlayerName), playerWinrate))
playerWinrates <- tibble::rownames_to_column(playerWinrates, "VALUE")
playerWinrates$GamesPlayed <- sapply(unique(decks$PlayerName), playerGames)
colnames(playerWinrates) <- c('PlayerName', 'Winrate', 'GamesPlayed')
saveRDS(playerWinrates, "player_winrates.rds")
playerWinrates <- readRDS("player_winrates.rds")

#archetype stats
archWinrates <- as.data.frame(sapply(unique(decks$Classification), archWinrate))
archWinrates <- tibble::rownames_to_column(archWinrates, "VALUE")
archWinrates$GamesPlayed <- sapply(unique(decks$Classification), archGames)
colnames(archWinrates) <- c('Archetype', 'Winrate', 'GamesPlayed')
saveRDS(archWinrates, "arch_winrates.rds")
archWinrates <- readRDS("arch_winrates.rds")

#colorComboStats
colorComboWinrates <- as.data.frame(sapply(unique(decks$Color.Identity), colorComboWinrate))
colorComboWinrates <- tibble::rownames_to_column(colorComboWinrates, "VALUE")
colorComboWinrates$GamesPlayed <- sapply(unique(decks$Color.Identity), colorComboGames)
colnames(colorComboWinrates) <- c('Color', 'Winrate', 'GamesPlayed')
saveRDS(colorComboWinrates, "combo_winrates.rds")
colorComboWinrates <- readRDS("combo_winrates.rds")

#colorStats
colorWinrates <- as.data.frame(sapply(c('W','U','B','R','G'), colorWinrate))
colorWinrates <- tibble::rownames_to_column(colorWinrates, "VALUE")
colorWinrates$GamesPlayed <- sapply(c('W','U','B','R','G'), colorGames)
colorWinrates$Colors <- c('White', 'Blue','Black','Red','Green')
colnames(colorWinrates) <- c('Color', 'Winrate', 'GamesPlayed')
saveRDS(colorWinrates, "color_winrates.rds")
colorWinrates <- readRDS("color_winrates.rds")

### Going a step further, create unified result structure
excluded_names <- c( "Sky", "Gretchen", "Tini", "Shane", "Zeth", "Alex", "Tay","Mack ", "Matt")
matches_long <- game_log %>%
  mutate(
    winner_id = case_when(
      result == 1 ~ player1,
      result == 2 ~ player2
    )
  ) %>%
  select(player1, player2, result, winner_id) 

matches_long2 <- 
  # Generate rows from both players' perspectives
  bind_rows(
    matches_long %>%
      mutate(
        player_id = player2,
        opponent_id = player1,
        outcome = case_when(
          result == 1 ~ "loss",
          result == 2 ~ "win"
        )
      ) %>%
      select(player_id, opponent_id, outcome),
    matches_long %>%
      mutate(
        player_id = player1,
        opponent_id = player2,
        outcome = case_when(
          result == 1 ~ "win",
          result == 2 ~ "loss"
        )
      ) %>%
      select(player_id, opponent_id, outcome)
  )
#summarize winrates

winrate_summary <- matches_long2 %>%
  group_by(player_id, opponent_id) %>%
  dplyr::summarize(
    wins = sum(outcome == "win"),
    games = n(),
    winrate = wins / games,
    .groups = "drop"
  )

winrate_matrix <- tidyr::pivot_wider(
  winrate_summary,
  names_from = opponent_id,
  values_from = winrate,
  values_fill = 0
)

# Optional: convert to matrix and set row names
winrate_mat <- as.matrix(winrate_matrix[,-1])
rownames(winrate_mat) <- winrate_matrix$player_id

winrate_named <- winrate_summary %>%
  left_join(players, by = c("player_id" = "PlayerId")) %>%
  dplyr::rename(player_name = Name) %>%
  left_join(players, by = c("opponent_id" = "PlayerId")) %>%
  dplyr::rename(opponent_name = Name)

filtered_winrate_named <- winrate_named %>%
  filter(!(player_name %in% excluded_names | opponent_name %in% excluded_names))

winrate_wide <- filtered_winrate_named %>%
  select(player_name, opponent_name, winrate) %>%
  pivot_wider(
    names_from = opponent_name,
    values_from = winrate,
    values_fill = 0
  )
all_players <- sort(unique(c(filtered_winrate_named$player_name, filtered_winrate_named$opponent_name)))

full_grid <- expand.grid(
  player_name = all_players,
  opponent_name = all_players,
  stringsAsFactors = FALSE
)

winrate_full <- full_grid %>%
  left_join(filtered_winrate_named %>% select(player_name, opponent_name, winrate), 
            by = c("player_name", "opponent_name"))

winrate_full <- winrate_full %>%
  mutate(winrate = ifelse(player_name == opponent_name, NA, winrate))

winrate_mat <- winrate_full %>%
  pivot_wider(names_from = opponent_name, values_from = winrate) %>%
  arrange(match(player_name, all_players)) 



winrate_mat <- winrate_mat[!is.na(rownames(winrate_mat)) & rownames(winrate_mat) != "NA",
                           !is.na(colnames(winrate_mat)) & colnames(winrate_mat) != "NA"]
# Prepare data for ggplot
heatmap_data <- melt(winrate_mat, varnames = c("Player", "Opponent"), value.name = "Winrate")
saveRDS(heatmap_data, "heatmap_data.rds")
heatmap_data <- readRDS("heatmap_data.rds")

colnames(heatmap_data) <- c("Player", "Opponent", "Winrate")

## Archetype stats
decks$deckID <- as.integer(sub('.', '', decks$Deck.ID))
game_with_archetypes <- game_log %>%
  left_join(decks %>% select(deckID, archetype1 = Classification), by = c("deck1" = "deckID")) %>%
  left_join(decks %>% select(deckID, archetype2 = Classification), by = c("deck2" = "deckID"))

matchups <- game_with_archetypes %>%
  filter(!is.na(archetype1), !is.na(archetype2)) %>%
  mutate(
    player_archetype   = ifelse(result == 1, archetype1, archetype2),
    opponent_archetype = ifelse(result == 1, archetype2, archetype1),
    win = 1
  ) %>%
  select(player_archetype, opponent_archetype, win)

reverse_matchups <- matchups %>%
  dplyr::rename(
    player_archetype = opponent_archetype,
    opponent_archetype = player_archetype
  ) %>%
  mutate(win = 0)

all_games <- bind_rows(matchups, reverse_matchups)

all_matchups <- all_games %>%
  group_by(player_archetype, opponent_archetype) %>%
  dplyr::summarise(
    player_wins = sum(win),
    total_games = n(),
    winrate = player_wins / total_games,
    .groups = "drop"
  )
filtered_matchups <- all_matchups %>%
  filter(
    player_archetype != "???",
    opponent_archetype != "???"
  )
saveRDS(filtered_matchups, "filtered_matchups.rds")
filtered_matchups <- readRDS("filtered_matchups.rds")
