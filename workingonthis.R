# Expand games into (card, is_win) rows for both sides
cards_deck1 <- game_log2 %>%
  inner_join(deck_cards, by = c("deck1" = "deckId"), relationship = "many-to-many") %>%
  transmute(card_name, is_win = deck1_win)

cards_deck2 <- game_log2 %>%
  inner_join(deck_cards, by = c("deck2" = "deckId"), relationship = "many-to-many") %>%
  transmute(card_name, is_win = !deck1_win)

card_games <- bind_rows(cards_deck1, cards_deck2) %>%
  filter(card_name %in% nonland_front_faces)  # <-- front-face aware

if (nrow(card_games) == 0) {
  return(tibble(card_name = character(), 
                games = integer(), 
                wins = integer(),
                raw_wr = numeric(), 
                shrink_wr = numeric(),
                archetype_wr = numeric(), 
                wr_diff = numeric()))
}

overall_wr <- mean(card_games$is_win) # baseline

# Summarise & shrink
card_wr <- card_games %>%
  group_by(card_name) %>%
  summarise(games = n(), 
            wins = sum(is_win), 
            .groups = "drop") %>%
  mutate(
    raw_wr    = ifelse(games > 0, wins / games, NA_real_),
    shrink_wr = (wins + overall_wr * 5) / (games + 5)
  )

# ============================================================
# === NEW: Archetype-adjusted winrate benchmarking ===========
# ============================================================

# Step 1: attach deck → archetype
card_games_arche <- card_games %>%
  left_join(deck_archetypes, by = "deck") %>%   # deck_archetypes: deck, archetype
  filter(!is.na(archetype))

# Step 2: count card occurrences in each archetype
card_arche_mix <- card_games_arche %>%
  count(card_name, archetype, name = "appearances")

# Step 3: attach archetype winrates
card_arche_mix <- card_arche_mix %>%
  left_join(archetype_stats %>% select(archetype, arche_wr = winrate),
            by = "archetype")

# Step 4: compute weighted baseline archetype WR per card
arche_baseline <- card_arche_mix %>%
  group_by(card_name) %>%
  summarise(
    archetype_wr = weighted.mean(arche_wr, appearances),
    .groups = "drop"
  )

# Step 5: join into card_wr & compute Δ
card_wr_adj <- card_wr %>%
  left_join(arche_baseline, by = "card_name") %>%
  mutate(
    wr_diff = shrink_wr - archetype_wr   # card’s performance vs expected
  )