# data_update.R
# Offline ETL for ShinyCube. Reads raw CSVs + Scryfall data, produces all
# .rds artifacts that app.R consumes at runtime.
#
# Run this whenever the source CSVs change. The Shiny app should NEVER
# perform any of these computations — it should only readRDS().
#
# The Scryfall refresh block (commented at the bottom) only needs to run
# when new cards are added to the cube.

library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(jsonlite)

source("config.R")

# ---------------------------------------------------------------------------
# 1. Load raw data
# ---------------------------------------------------------------------------

decks     <- read.csv("Cube_Stats - Deck Info.csv", stringsAsFactors = FALSE)
decklists <- read.csv("Cube_Stats - All Decklists.csv",
                      na.strings = c("", "NA"), check.names = FALSE,
                      stringsAsFactors = FALSE)
game_log  <- read.csv("Cube_Stats - game_log.csv", stringsAsFactors = FALSE)
players   <- read.csv("Cube_Stats - Players.csv", stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# 2. Clean & normalize
# ---------------------------------------------------------------------------

# Decks: parse date, build a clean integer deckID. Note: we do NOT add an
# "X" prefix anywhere — decklist column names are bare integer strings,
# and using them consistently as strings is the simplest scheme.
decks <- decks %>%
  mutate(
    Date   = as.Date(Date, format = "%m/%d/%y"),
    deckID = as.integer(Deck.ID)
  )

# Game log: parse date once.
game_log <- game_log %>%
  mutate(date = as.Date(date, format = "%m-%d-%y"))

bad_dates <- sum(is.na(game_log$date))
if (bad_dates > 0) {
  warning(sprintf("game_log has %d unparseable dates — these will be dropped from Elo and time-series views",
                  bad_dates))
}

# Long & wide deck-card representations.
# Long & wide deck-card representations.
#
# Card-name handling note: decklists store double-faced cards as
# "Front // Back" but Scryfall (and our scryfall_lookup) keys on the front
# face only. We keep the full slashed name in `long_decklists` and
# `deck_id_to_cards` for display purposes, but use the front-face name as
# the join key in `binary_matrix` and `deck_cards`. That way every
# downstream join against scryfall_lookup works without special-casing.
front_face <- function(x) str_trim(str_extract(x, "^[^/]+"))

long_decklists <- decklists %>%
  pivot_longer(cols = everything(), names_to = "deck_id", values_to = "card") %>%
  filter(!is.na(card), nzchar(str_trim(card))) %>%
  mutate(card = str_trim(card)) %>%
  distinct(deck_id, card) %>%
  mutate(card_front = front_face(card))

binary_matrix <- long_decklists %>%
  distinct(deck_id, card_front) %>%
  mutate(present = 1L) %>%
  pivot_wider(names_from = card_front, values_from = present, values_fill = 0L) %>%
  column_to_rownames("deck_id")

# Named list deckID (string) -> character vector of cards (full display names).
# Used by Deck Explorer to render the cardlist as the user typed it.
deck_id_to_cards <- split(long_decklists$card, long_decklists$deck_id)

# ---------------------------------------------------------------------------
# 3. Scryfall lookup
# ---------------------------------------------------------------------------
# Loads the trimmed scryfall RDS produced by the refresh block at the
# bottom of this file. If you've never run that block, do so once.

scryfall_data <- readRDS("scryfall_cards_trimmed.rds")

scryfall_lookup <- scryfall_data %>%
  mutate(name = str_trim(str_extract(name, "^[^/]+"))) %>%   # front face only
  distinct(name, .keep_all = TRUE) %>%
  select(name, mana_cost, cmc, type_line, image_url)

card_meta <- scryfall_lookup %>%
  filter(name %in% colnames(binary_matrix)) %>%
  select(name, type_line)

# Two nonland-name vectors. Both are front-face names (since binary_matrix
# is now front-face-keyed and scryfall_lookup is too). The distinction:
#   nonland_cards       = nonland cards actually present in some deck
#   nonland_front_faces = all nonland cards in the trimmed Scryfall lookup
# In practice these are nearly identical, but the second is the safer
# filter for card-stat aggregation in case the cube is mid-update.
nonland_cards <- card_meta %>%
  filter(!grepl("\\bLand\\b", type_line, ignore.case = TRUE)) %>%
  pull(name)

nonland_front_faces <- scryfall_lookup %>%
  filter(!grepl("\\bLand\\b", type_line, ignore.case = TRUE)) %>%
  pull(name)

# ---------------------------------------------------------------------------
# 4. Aggregate winrates (player / archetype / color combo / single color)
# ---------------------------------------------------------------------------
# All four collapse to a single group_by/summarise from the `decks` table.

playerWinrates <- decks %>%
  group_by(PlayerName) %>%
  summarise(
    Winrate     = sum(Wins) / sum(Games.Played),
    GamesPlayed = sum(Games.Played),
    .groups = "drop"
  )

archWinrates <- decks %>%
  group_by(Archetype = Classification) %>%
  summarise(
    Winrate     = sum(Wins) / sum(Games.Played),
    GamesPlayed = sum(Games.Played),
    .groups = "drop"
  ) %>%
  mutate(games = round(GamesPlayed * Winrate))

colorComboWinrates <- decks %>%
  group_by(Color = Color.Identity) %>%
  summarise(
    Winrate     = sum(Wins) / sum(Games.Played),
    GamesPlayed = sum(Games.Played),
    .groups = "drop"
  )

# Single colors: a deck contributes to each color in its Color.Identity.
colorWinrates <- decks %>%
  mutate(color_chars = strsplit(Color.Identity, "")) %>%
  unnest(color_chars) %>%
  filter(color_chars %in% c("W", "U", "B", "R", "G")) %>%
  group_by(Color = color_chars) %>%
  summarise(
    Winrate     = sum(Wins) / sum(Games.Played),
    GamesPlayed = sum(Games.Played),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# 5. Head-to-head heatmap
# ---------------------------------------------------------------------------

matches_per_perspective <- bind_rows(
  game_log %>% transmute(player_id = player1, opponent_id = player2,
                         outcome = if_else(result == 1, "win", "loss")),
  game_log %>% transmute(player_id = player2, opponent_id = player1,
                         outcome = if_else(result == 2, "win", "loss"))
)

winrate_named <- matches_per_perspective %>%
  group_by(player_id, opponent_id) %>%
  summarise(
    wins    = sum(outcome == "win"),
    games   = n(),
    winrate = wins / games,
    .groups = "drop"
  ) %>%
  left_join(players, by = c("player_id"   = "PlayerId")) %>%
  rename(player_name = Name) %>%
  left_join(players, by = c("opponent_id" = "PlayerId")) %>%
  rename(opponent_name = Name) %>%
  filter(!(player_name %in% EXCLUDED_NAMES |
           opponent_name %in% EXCLUDED_NAMES))

# Build a square grid so missing matchups show up as NA cells, not gaps.
all_players <- sort(unique(c(winrate_named$player_name,
                             winrate_named$opponent_name)))

heatmap_data <- expand.grid(
    Player   = all_players,
    Opponent = all_players,
    stringsAsFactors = FALSE
  ) %>%
  left_join(
    winrate_named %>% select(Player = player_name,
                             Opponent = opponent_name,
                             Winrate = winrate),
    by = c("Player", "Opponent")
  ) %>%
  mutate(Winrate = if_else(Player == Opponent, NA_real_, Winrate))

# ---------------------------------------------------------------------------
# 6. Archetype-vs-archetype matchups
# ---------------------------------------------------------------------------

game_with_archetypes <- game_log %>%
  left_join(decks %>% select(deckID, archetype1 = Classification),
            by = c("deck1" = "deckID")) %>%
  left_join(decks %>% select(deckID, archetype2 = Classification),
            by = c("deck2" = "deckID"))

matchups <- game_with_archetypes %>%
  filter(!is.na(archetype1), !is.na(archetype2)) %>%
  transmute(
    player_archetype   = if_else(result == 1, archetype1, archetype2),
    opponent_archetype = if_else(result == 1, archetype2, archetype1),
    win = 1L
  )

reverse_matchups <- matchups %>%
  transmute(
    player_archetype   = opponent_archetype,
    opponent_archetype = player_archetype,
    win = 0L
  )

filtered_matchups <- bind_rows(matchups, reverse_matchups) %>%
  group_by(player_archetype, opponent_archetype) %>%
  summarise(
    player_wins = sum(win),
    total_games = n(),
    winrate     = player_wins / total_games,
    .groups = "drop"
  ) %>%
  filter(player_archetype != "???", opponent_archetype != "???")

# ---------------------------------------------------------------------------
# 7. Per-card structures used by the Card Stats tab
# ---------------------------------------------------------------------------

# game_log2: thin shape used by card-winrate joins. is_win is from deck1's POV.
game_log2 <- game_log %>%
  transmute(
    id,
    deck1     = as.character(deck1),
    deck2     = as.character(deck2),
    deck1_win = (result == 1)
  )

# Long deck->card map. deckId is a string to match game_log2$deck1/deck2.
# card_name is front-face (joins to scryfall_lookup); display_name is the
# original full slashed form (what the deck list table renders).
deck_cards <- long_decklists %>%
  transmute(
    deckId       = deck_id,
    card_name    = card_front,
    display_name = card
  ) %>%
  distinct(deckId, card_name, display_name)

# Deck -> archetype lookup, used by the (now-fixed) card_stats reactive.
deck_archetypes <- decks %>%
  transmute(deck = as.character(deckID), archetype = Classification)

# Archetype-level winrate (raw) for use as a baseline in card_stats.
archetype_stats <- archWinrates %>%
  transmute(archetype = Archetype, winrate = Winrate)

# ---------------------------------------------------------------------------
# 8. Elo
# ---------------------------------------------------------------------------

elo_update <- function(rating1, rating2, p1_won, k = ELO_K_FACTOR) {
  exp1   <- 1 / (1 + 10^((rating2 - rating1) / 400))
  r1_new <- rating1 + k * (p1_won - exp1)
  r2_new <- rating2 + k * ((1 - p1_won) - (1 - exp1))
  list(r1 = r1_new, r2 = r2_new)
}

calculate_player_elo <- function(game_log, k = ELO_K_FACTOR, init_rating = ELO_INIT) {
  gl <- game_log %>% filter(!is.na(date)) %>% arrange(date, id)

  player_ids <- unique(c(gl$player1, gl$player2))
  ratings    <- setNames(rep(init_rating, length(player_ids)),
                         as.character(player_ids))

  n <- nrow(gl)
  out_gameId <- integer(2 * n)
  out_date   <- as.Date(rep(NA, 2 * n))
  out_player <- integer(2 * n)
  out_rating <- numeric(2 * n)

  for (i in seq_len(n)) {
    p1 <- as.character(gl$player1[i])
    p2 <- as.character(gl$player2[i])
    p1_won <- as.integer(gl$result[i] == 1)

    upd <- elo_update(ratings[[p1]], ratings[[p2]], p1_won, k)
    ratings[[p1]] <- upd$r1
    ratings[[p2]] <- upd$r2

    j <- 2 * i - 1
    out_gameId[j:(j+1)] <- gl$id[i]
    out_date[j:(j+1)]   <- gl$date[i]
    out_player[j:(j+1)] <- c(as.integer(p1), as.integer(p2))
    out_rating[j:(j+1)] <- c(ratings[[p1]], ratings[[p2]])
  }

  tibble(gameId = out_gameId, date = out_date,
         player = out_player, rating = out_rating)
}

elo_history <- calculate_player_elo(game_log)

# Per-game Elo movements collapsed to one row per (player, day) — the
# end-of-day rating. Used by the Player Summary Elo plot to avoid the
# within-session jitter you get when a player swings through 5-6 games
# in an evening.
daily_elo_history <- elo_history %>%
  arrange(date, gameId) %>%
  group_by(player, date) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(player, date, rating)

current_elo_lookup <- elo_history %>%
  arrange(date, gameId) %>%
  group_by(player) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(player, rating)

# ---------------------------------------------------------------------------
# 9. Save artifacts
# ---------------------------------------------------------------------------

saveRDS(decks,                "decks.rds")
saveRDS(players,              "players.rds")
saveRDS(game_log,             "game_log.rds")
saveRDS(game_log2,            "game_log2.rds")

saveRDS(binary_matrix,        "binary_matrix.rds")
saveRDS(deck_id_to_cards,     "deck_id_to_cards.rds")
saveRDS(deck_cards,           "deck_cards.rds")

saveRDS(scryfall_lookup,      "scryfall_lookup.rds")
saveRDS(nonland_cards,        "nonland_cards.rds")
saveRDS(nonland_front_faces,  "nonland_front_faces.rds")

saveRDS(playerWinrates,       "player_winrates.rds")
saveRDS(archWinrates,         "arch_winrates.rds")
saveRDS(colorWinrates,        "color_winrates.rds")
saveRDS(colorComboWinrates,   "combo_winrates.rds")

saveRDS(heatmap_data,         "heatmap_data.rds")
saveRDS(filtered_matchups,    "filtered_matchups.rds")

saveRDS(deck_archetypes,      "deck_archetypes.rds")
saveRDS(archetype_stats,      "archetype_stats.rds")

saveRDS(elo_history,          "elo_history.rds")
saveRDS(daily_elo_history,    "daily_elo_history.rds")
saveRDS(current_elo_lookup,   "current_elo_lookup.rds")

message("data_update.R: artifacts written successfully.")

# ---------------------------------------------------------------------------
# Scryfall refresh — ONLY needed when adding new cards to the cube.
# Edit SCRYFALL_BULK_PATH in config.R if your local path differs.
# ---------------------------------------------------------------------------
#
# refresh_scryfall <- function() {
#   bulk_meta <- fromJSON("https://api.scryfall.com/bulk-data")
#   bulk_url  <- bulk_meta$data %>%
#     filter(type == "default_cards") %>%
#     pull(download_uri)
#   all_cards <- fromJSON(bulk_url)
#   saveRDS(all_cards, SCRYFALL_BULK_PATH)
# 
#   scryfall_data <- readRDS(SCRYFALL_BULK_PATH)
#   used_card_names <- long_decklists %>% distinct(card) %>% pull(card)
#   pattern <- paste(paste0("\\Q", used_card_names, "\\E"), collapse = "|")
# 
#   scryfall_trimmed <- scryfall_data %>%
#     filter(str_detect(name, pattern))
#   scryfall_trimmed$image_url <- scryfall_trimmed$image_uris$normal
#   scryfall_trimmed <- scryfall_trimmed %>%
#     select(id, name, cmc, type_line, mana_cost, image_url, oracle_text)
# 
#   # Backfill mana_cost / image_url for double-faced cards by hitting the API
#   no_mana <- scryfall_trimmed %>% filter(is.na(mana_cost), cmc > 0)
#   for (cid in na.omit(no_mana$id)) {
#     d <- tryCatch(fromJSON(paste0("https://api.scryfall.com/cards/", cid)),
#                   error = function(e) NULL)
#     if (!is.null(d)) {
#       scryfall_trimmed$mana_cost[scryfall_trimmed$id == d$id] <- d$card_faces$mana_cost[1]
#     }
#   }
# 
#   no_image <- scryfall_trimmed %>% filter(is.na(image_url))
#   for (cid in na.omit(no_image$id)) {
#     d <- tryCatch(fromJSON(paste0("https://api.scryfall.com/cards/", cid)),
#                   error = function(e) NULL)
#     if (!is.null(d) && !is.null(d$card_faces$image_uris$normal[1])) {
#       scryfall_trimmed$image_url[scryfall_trimmed$id == d$id] <- d$card_faces$image_uris$normal[1]
#     }
#   }
# 
#   saveRDS(scryfall_trimmed, "scryfall_cards_trimmed.rds")
# }
