# MTG Cube Match Analysis Shiny App

This Shiny app provides interactive visualizations and insights for my MTG Cube list found [HERE](https://cubecobra.com/cube/list/johngaebler-360). It tracks 1v1 match results, deck performance, player stats, and card usage. I dont think anyone I draft with uses this app despite me spamming them with it every time there is an update lol 

## 🔍 Features

- **Player Profiles**: See cumulative winrates, preferred colors/archetypes, and head-to-head records.
- **Deck Explorer**: Browse decks by player, date, or winrate. View decklists, mana curves, and individual card previews.
- **Matchup Analysis**:
  - Player-vs-Player winrate heatmap
  - Archetype-vs-Archetype matchup matrix
- **Archetype and Color Stats**: Winrates broken down by archetype, color pairings, and mono-color decks.
- **Card Stats**:
  - Favorite cards per player (based on deck inclusion frequency)
  - Card previews via Scryfall integration

## 📊 Data Inputs

The app uses the following CSV data sources (example schemas):

- `Cube_Stats - game_log.csv`: `gameId`, `player1`, `player2`, `deck1`, `deck2`, `result` 1:Player1 Win, 2:Player2 Win
- `Cube_Stats - Deck Info.csv`: `PlayerName`, `Deck Id`, `PlayerId`, `Color Identity`, `Theme`, `Classification`, `Wins`, `Winrate`, `Games Played`, `Date`
- `Cube_Stats - All Decklists.csv`: Column names = deck IDs, Rows = card names
- `Cube_Stats - Players.csv`: `PlayerId`, `Name`
- `scryfall_cards_trimmed.rds`: Filtered Scryfall metadata for cards in the cube 

## ⚙️ How It Works

- Decklists are converted to a binary matrix for clustering and similarity analysis
- Scryfall card data is used to display card types, mana costs, and card images
- Winrates are computed from `game_log` and aggregated by player, color, and archetype
- Summary statistics and visualizations are cached for fast app performance

## 🚀 Deployment

- First work through the data_update.R file to ingest the raw data and save the results to make load faster. Also collect the trimmed scryfall data during this step
- Load the saved .rds data in the app.R file, run locally to check that the app works
- Deploy the shiny app using the cloud solution of your choice, I am using [Posit Connect](https://posit.co/products/enterprise/connect/)
