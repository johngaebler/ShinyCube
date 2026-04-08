# config.R
# Shared constants for ShinyCube. Sourced by both data_update.R and app.R.

# Players excluded from head-to-head heatmaps (too few games to be meaningful)
EXCLUDED_NAMES <- c("Sky", "Gretchen", "Tini", "Shane", "Zeth",
                    "Alex", "Tay", "Mack ", "Asher")

# Elo
ELO_K_FACTOR   <- 32
ELO_INIT       <- 1000

# Card-stat thresholds
MIN_GAMES_DEFAULT <- 5
PRIOR_WEIGHT_DEFAULT <- 10

# Filesystem
# Path to the full Scryfall bulk dump (only needed when refreshing scryfall data
# locally; not used at app runtime). Override per-machine if needed.
SCRYFALL_BULK_PATH <- Sys.getenv("SCRYFALL_BULK_PATH",
                                 unset = "C:/scryfall_cards.rds")
