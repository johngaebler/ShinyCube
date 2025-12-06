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
library(stringr)
library(Matrix)
library(glmnet)

# read in raw csv data
decks <- read.csv("Cube_Stats - Deck Info.csv")
decklists <- read.csv("Cube_Stats - All Decklists.csv", na.strings = c("", "NA"), check.names = FALSE)
game_log <- read.csv("Cube_Stats - game_log.csv", stringsAsFactors = F)
players <- read.csv("Cube_Stats - Players.csv", stringsAsFactors =F)
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
# bulk_meta <- fromJSON("https://api.scryfall.com/bulk-data")
# 
# # 2. Get the download URL for the "default_cards" bulk list
# bulk_url <- bulk_meta$data %>%
#   dplyr::filter(type == "default_cards") %>%
#   dplyr::pull(download_uri)

# 3. Download the full card list (~40MB)
#all_cards <- fromJSON(bulk_url)

# Optional: save to local RDS to avoid downloading every time
#saveRDS(scryfall_data, "C:\\scryfall_cards.rds")
scryfall_data <- readRDS("C:\\scryfall_cards.rds")
#fetch data for cards not showing an image

#filter down to cards we care about
used_card_names <- long_decklists %>%
  distinct(card) %>%
  pull(card)
# 
# # Step 2: Filter the full scryfall data
pattern <- paste0("\\Q", used_card_names, "\\E")
pattern <- paste(pattern, collapse = "|")

##                !!!!!!!!!!!!!!!!!!!!!!!!!!!!
## ADD A STEP HERE TO EXCLUDE ART CARDS , card type Card // Card
##                !!!!!!!!!!!!!!!!!!!!!!!!!!!!

scryfall_trimmed <- scryfall_data %>%
  filter(str_detect(name, pattern))
scryfall_trimmed$image_url <- scryfall_trimmed$image_uris$normal
scryfall_trimmed <- scryfall_trimmed %>%
  select(id, name, cmc, type_line, mana_cost, image_url, oracle_text)
no_image <- scryfall_trimmed[is.na(scryfall_trimmed$image_url),]

no_mana <- scryfall_trimmed[is.na(scryfall_trimmed$mana_cost) & 
                                    scryfall_trimmed$cmc > 0 ,]

#adding in the mana cost for cards missing it
lapply(na.omit(no_mana$id), function(x) {
  data <- fromJSON(paste0("https://api.scryfall.com/cards/", x))
  #print(data$card_faces$image_uris$normal[1])
  value <- data$card_faces$mana_cost[1]

  scryfall_trimmed$mana_cost[scryfall_trimmed$id == data$id] <<- value


})

#adding in the image_url for cards missing it
test <- lapply(na.omit(no_image$id), function(x) {
  data <- fromJSON(paste0("https://api.scryfall.com/cards/", x))
  
  value <- data$card_faces$image_uris$normal[1]
  #print(value)
  if(!is.null(value )){
    scryfall_trimmed$image_url[scryfall_trimmed$id == data$id] <<- value
  }
})


#scryfall_data$image_url <- scryfall_data$image_uris$normal
#card_chunks <- split(scryfall_data, ceiling(seq_len(nrow(scryfall_data)) / 35000))
#saveRDS(card_chunks[[1]], "cards_part1.rds")
#saveRDS(card_chunks[[2]], "cards_part2.rds")
#saveRDS(card_chunks[[3]], "cards_part3.rds")
#saveRDS(card_chunks[[4]], "cards_part4.rds")

#once it is all saved, you can load it like this 
# scryfall_data <- readRDS("C:\\scryfall_cards.rds")




# Step 3: Save this trimmed version
saveRDS(scryfall_trimmed, "scryfall_cards_trimmed.rds")
# scryfall_data <- scryfall_data %>%
#   mutate(image_url = normal)
# Filter Scryfall cards to only those used in your cube
scryfall_data <- readRDS("scryfall_cards_trimmed.rds")
card_meta <- scryfall_data %>%
  dplyr::filter(name %in% colnames(binary_matrix)) %>%
  dplyr::select(name, type_line)

# scryfall_data <- readRDS("scryfall_cards_trimmed.rds")

scryfall_lookup <- scryfall_data %>%
  mutate(name = str_trim(str_extract(name, "^[^/]+"))) %>%
  distinct(name, .keep_all = TRUE) %>%
  select(name, mana_cost, cmc, type_line, image_url)
saveRDS(scryfall_lookup, "scryfall_lookup.rds")
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
excluded_names <- c( "Sky", "Gretchen", "Tini", "Shane", "Zeth", "Alex", "Tay","Mack ", "Asher")
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

game_log2 <- game_log %>%
  transmute(
    id,
    deck1 = as.character(deck1),
    deck2 = as.character(deck2),
    deck1_win = (result == 1)
  )
saveRDS(game_log2, "game_log2.rds")
# Long deck->card map from your binary_matrix
deck_cards <- binary_matrix %>%
  as.data.frame() %>%
  tibble::rownames_to_column("deckId") %>%
  pivot_longer(-deckId, names_to = "card_name", values_to = "present") %>%
  filter(present == 1) %>%
  distinct(deckId = as.character(deckId), card_name)

# Defensive: dedupe any accidental duplicate card rows per deck
deck_cards <- deck_cards %>% distinct(deckId, card_name)

# Build a minimal scryfall lookup with "front face" names and image URLs
# (works for double-faced cards like "Front // Back")

saveRDS(deck_cards, "deck_cards.rds")

# ## Card level analysis >:)
# # 1) Deck -> Card presence (unique pairs)
# deck_cards <- binary_matrix %>%
#   as.data.frame() %>%
#   rownames_to_column("deckId") %>%
#   pivot_longer(-deckId, names_to = "card_name", values_to = "present") %>%
#   filter(present == 1) %>%
#   distinct(deckId = as.character(deckId), card_name)
# 
# # 2) Expand games to "card appearances by side"
# cards_deck1 <- game_log2 %>%
#   inner_join(deck_cards, by = c("deck1" = "deckId")) %>%
#   transmute(card_name, is_win = deck1_win)
# 
# cards_deck2 <- game_log2 %>%
#   inner_join(deck_cards, by = c("deck2" = "deckId")) %>%
#   transmute(card_name, is_win = !deck1_win)
# 
# # 3) Per-card winrate (across both sides)
# card_winrate <- bind_rows(cards_deck1, cards_deck2) %>%
#   group_by(card_name) %>%
#   summarise(
#     games_with_card = n(),
#     wins_with_card  = sum(is_win),
#     winrate_with_card = wins_with_card / games_with_card,
#     .groups = "drop"
#   )
# # 0) Clean game log
# game_log2 <- game_log %>%
#   transmute(
#     id,
#     deck1 = as.character(deck1),
#     deck2 = as.character(deck2),
#     deck1_win = (result == 1)
#   )
# 
# # 1) Map each deckId -> vector of cards it contains
# stopifnot(identical(rownames(binary_matrix), as.character(rownames(binary_matrix))))
# all_cards <- colnames(binary_matrix)
# 
# deck_to_cards <- apply(binary_matrix, 1, function(row) names(which(row == 1)))
# # deck_to_cards is a named list; names are deckIds
# 
# # 2) Build sparse design: for each game and each card:
# #    +1 if card is in deck1 only, -1 if in deck2 only, 0 if in both or neither
# card_index <- setNames(seq_along(all_cards), all_cards)
# 
# make_row <- function(cards1, cards2) {
#   if (is.null(cards1)) cards1 <- character(0)
#   if (is.null(cards2)) cards2 <- character(0)
#   both   <- intersect(cards1, cards2)
#   only1  <- setdiff(cards1, both)
#   only2  <- setdiff(cards2, both)
#   idx    <- c(card_index[only1], card_index[only2])
#   vals   <- c(rep.int( 1L, length(only1)),
#               rep.int(-1L, length(only2)))
#   list(idx = as.integer(idx), vals = as.numeric(vals))
# }
# 
# n <- nrow(game_log2)
# p <- length(all_cards)
# 
# # Preallocate i, j, x for a sparseMatrix in triplet form
# rows_list <- vector("list", n)
# nnz <- 0L
# for (k in seq_len(n)) {
#   d1 <- game_log2$deck1[k]; d2 <- game_log2$deck2[k]
#   r  <- make_row(deck_to_cards[[d1]], deck_to_cards[[d2]])
#   rows_list[[k]] <- r
#   nnz <- nnz + length(r$idx)
# }
# 
# i <- integer(nnz)
# j <- integer(nnz)
# x <- numeric(nnz)
# pos <- 1L
# for (k in seq_len(n)) {
#   r <- rows_list[[k]]
#   L <- length(r$idx)
#   if (L > 0) {
#     rng <- pos:(pos + L - 1L)
#     i[rng] <- k
#     j[rng] <- r$idx
#     x[rng] <- r$vals
#     pos <- pos + L
#   }
# }
# X_cards <- sparseMatrix(i = i, j = j, x = x, dims = c(n, p),
#                         dimnames = list(NULL, all_cards))
# 
# y <- as.numeric(game_log2$deck1_win)  # 1 if deck1 won, 0 otherwise
# 
# # 3) Optional controls (recommended if certain players/archetypes dominate)
# # --- Player fixed effects (difference-coded: +1 for player as P1, -1 as P2)
# # Build a column per player id present in any game (drop one to avoid collinearity)
# all_pids <- sort(unique(c(game_log$player1, game_log$player2)))
# pid_index <- setNames(seq_along(all_pids), all_pids)
# 
# i_pid <- integer(0); j_pid <- integer(0); x_pid <- numeric(0)
# for (k in seq_len(n)) {
#   p1 <- as.character(game_log$player1[k])
#   p2 <- as.character(game_log$player2[k])
#   if (!is.na(p1) && p1 %in% names(pid_index)) {
#     i_pid <- c(i_pid, k); j_pid <- c(j_pid, pid_index[[p1]]); x_pid <- c(x_pid,  1)
#   }
#   if (!is.na(p2) && p2 %in% names(pid_index)) {
#     i_pid <- c(i_pid, k); j_pid <- c(j_pid, pid_index[[p2]]); x_pid <- c(x_pid, -1)
#   }
# }
# if (length(all_pids) > 0) {
#   X_players_full <- sparseMatrix(i = i_pid, j = j_pid, x = x_pid,
#                                  dims = c(n, length(all_pids)),
#                                  dimnames = list(NULL, paste0("P_", all_pids)))
#   # Drop one column to set a reference (avoid perfect collinearity with intercept)
#   if (ncol(X_players_full) > 0) {
#     X_players <- X_players_full[, -1, drop = FALSE]
#   } else {
#     X_players <- NULL
#   }
# } else {
#   X_players <- NULL
# }
# 
# # (Optional) Archetype difference controls — similar pattern if you have deckInfo$archetype per deckId
# # Left as extension: cBind more columns just like players.
# 
# # 4) Final design matrix: [cards | players (optional)]
# X <- if (!is.null(X_players)) cbind(X_cards, X_players) else X_cards
# 
# # 5) Fit ridge logistic regression
# set.seed(123)
# cvfit <- cv.glmnet(X, y, family = "binomial", alpha = 0)  # alpha=0 => ridge
# # Use a stable lambda (1se) or more aggressive (min)
# beta  <- as.vector(coef(cvfit, s = "lambda.1se"))
# beta_names <- rownames(coef(cvfit, s = "lambda.1se"))
# 
# # 6) Extract per-card coefficients and convert to Elo-like scale
# # Logistic Δ -> Elo Δ: multiply by 400*log10(e) ≈ 173.7178
# ELO_SCALE <- 173.7178
# 
# coef_tbl <- tibble(term = beta_names, coef = beta)
# 
# card_impact <- coef_tbl %>%
#   filter(term %in% all_cards) %>%
#   transmute(
#     card_name = term,
#     coef_logit = coef,
#     elo_like   = coef_logit * ELO_SCALE
#   ) %>%
#   arrange(desc(elo_like))
# 
# # 7) (Recommended) Quality filters for stability
# # A card only influences a game if it is present in exactly one of the two decks.
# # Count those “differential appearances” to filter noisy cards.
# diff_counts <- map_dfr(seq_len(n), function(k) {
#   d1 <- game_log2$deck1[k]; d2 <- game_log2$deck2[k]
#   c1 <- deck_to_cards[[d1]] %||% character(0)
#   c2 <- deck_to_cards[[d2]] %||% character(0)
#   both <- intersect(c1, c2)
#   tibble(card_name = c(setdiff(c1, both), setdiff(c2, both)))
# }) %>%
#   count(card_name, name = "diff_games")
# 
# card_impact <- card_impact %>%
#   left_join(diff_counts, by = "card_name") %>%
#   mutate(diff_games = replace_na(diff_games, 0L)) %>%
#   filter(diff_games >= 3) 


