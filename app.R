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

#setup for the app, data reading, table prep
#loading data
decks <- read.csv("Cube_Stats - Deck Info (13).csv")
decklists <- read.csv("Cube_Stats - All Decklists (3).csv", na.strings = c("", "NA"), check.names = FALSE)
game_log <- read.csv("Cube_Stats - game_log (3).csv", stringsAsFactors = F)
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

#getting all of the scryfall data
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
scryfall_data <- bind_rows(
  readRDS("cards_part1.rds"),
  readRDS("cards_part2.rds"),
  readRDS("cards_part3.rds"),
  readRDS("cards_part4.rds")
)
# scryfall_data <- scryfall_data %>%
#   mutate(image_url = normal)
# Filter Scryfall cards to only those used in your cube
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

#archetype stats
archWinrates <- as.data.frame(sapply(unique(decks$Classification), archWinrate))
archWinrates <- tibble::rownames_to_column(archWinrates, "VALUE")
archWinrates$GamesPlayed <- sapply(unique(decks$Classification), archGames)
colnames(archWinrates) <- c('Archetype', 'Winrate', 'GamesPlayed')

#colorComboStats
colorComboWinrates <- as.data.frame(sapply(unique(decks$Color.Identity), colorComboWinrate))
colorComboWinrates <- tibble::rownames_to_column(colorComboWinrates, "VALUE")
colorComboWinrates$GamesPlayed <- sapply(unique(decks$Color.Identity), colorComboGames)
colnames(colorComboWinrates) <- c('Color', 'Winrate', 'GamesPlayed')

#colorStats
colorWinrates <- as.data.frame(sapply(c('W','U','B','R','G'), colorWinrate))
colorWinrates <- tibble::rownames_to_column(colorWinrates, "VALUE")
colorWinrates$GamesPlayed <- sapply(c('W','U','B','R','G'), colorGames)
colorWinrates$Colors <- c('White', 'Blue','Black','Red','Green')
colnames(colorWinrates) <- c('Color', 'Winrate', 'GamesPlayed')


#data viz

playerWinrateChart <- ggplot(data = playerWinrates, 
                             aes(x = reorder(PlayerName, -Winrate),y=Winrate, size = GamesPlayed))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_hline(yintercept=0.5, linetype = 2)+
  labs(title = "Player Winrate", x = "Player", y= "Winrate", size = "Games Played")

archWinrateChart <- ggplot(data = archWinrates, 
                           aes(x = reorder(Archetype, -Winrate),y=Winrate, size = GamesPlayed))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_hline(yintercept=0.5, linetype = 2)+
  labs(title = "Archetype Winrate", x = "Archetype", y= "Winrate", size = "Games Played")

colorWinrateChart <- ggplot(data = colorWinrates, 
                            aes(x = reorder(Color, -Winrate),y=Winrate, color = Color))+
  geom_point(size = 6)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Color Winrate", x = "Color", y= "Winrate")+
  geom_hline(yintercept=0.5, linetype = 2)+
  scale_color_manual(values = c("black", "green", "red", 
                                "blue", "white"))

colorComboWinrateChart <- ggplot(data = colorComboWinrates, 
                                 aes(x = reorder(Color, -Winrate),y=Winrate, size = GamesPlayed))+
  geom_point()+
  geom_hline(yintercept=0.5, linetype = 2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "ColorCombo Winrate", x = "ColorCombo", y= "Winrate", size = "Games Played")

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

colnames(heatmap_data) <- c("Player", "Opponent", "Winrate")

winrate_heatmap <- ggplot(heatmap_data, aes(x = Opponent, y = Player, fill = Winrate)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0.5, limits = c(0, 1)) +
  geom_text(aes(label = sprintf("%.2f", Winrate)), size = 3) +
  theme_minimal() +
  labs(title = "Head-to-Head Winrates", x = "Opponent", y = "Player")
playersummary <- function(playerName){
  return("nothing here yet")
}

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

matchup_winrates <- ggplot(filtered_matchups, aes(x = opponent_archetype, y = player_archetype, fill = winrate)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "green",
    midpoint = 0.5, limits = c(0, 1), na.value = "grey90"
  ) +
  geom_text(aes(label = sprintf("%.1f%%", winrate * 100)), size = 3) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Matchup Winrates by Archetype",
    x = "Opponent Archetype",
    y = "Player Archetype",
    fill = "Winrate"
  )


get_player_media <- function(player_name) {
  safe_name <- gsub(" ", "", tolower(player_name))
  extensions <- c("gif", "jpg", "jpeg", "png")
  
  for (ext in extensions) {
    candidate <- paste0("www/", safe_name, ".", ext)
    if (file.exists(candidate)) {
      return(paste0(safe_name, ".", ext))  # relative to www/ for use in <img src>
    }
  }
  
  return(NULL)  # fallback if nothing found
}

## ==== Begin UI and Server Functions ==== ##

ui <- fluidPage(theme = shinytheme("slate"),
                useShinyjs(),
                tags$head(
                  tags$head(
                    tags$style(HTML("
                      .dataTables_wrapper {
                        color: #eee;
                      }
                  
                      .dataTables_length select,
                      .dataTables_filter input {
                        background-color: #222;
                        color: #eee;
                        border: 1px solid #555;
                      }
                  
                      .dataTables_info {
                        color: #ccc;
                      }
                  
                      .dataTables_wrapper .dataTables_paginate .paginate_button {
                        background-color: #222;
                        color: #eee !important;
                        border: 1px solid #444;
                      }
                  
                      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
                        background-color: #555 !important;
                        color: #fff !important;
                      }
                    "))
                  )
                ),
                titlePanel("Cube Stats!!!  >:)"),
                
                tabsetPanel(
                  tabPanel(title ="Overall Stats",
                           fluidRow(
                                    wellPanel(
                                      layout_columns(
                                      plotOutput(outputId = "PlayerWinrates"),
                                      plotOutput(outputId = "ArchWinrates"),
                                      plotOutput(outputId = "ColorWinrates"),
                                      plotOutput(outputId = "ComboWinrates"),
                                      plotOutput(outputId = "Heatmap"),
                                      plotOutput(outputId = "MatchupHeat")
                                      ))
                             )
                  ),
                  tabPanel(title = 'Player Summary Dashboard',
                      #uiOutput("player_header"),    
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("selected_player", "Select Player:", choices = NULL),
                           uiOutput("player_image")
                            ),
                         
                         mainPanel(
                           h3("Player Summary"),
                           verbatimTextOutput("winrate"),
                           verbatimTextOutput("preferred_colors"),
                           verbatimTextOutput("preferred_archetype"),
                           verbatimTextOutput("worst_matchup"),
                           br(),
                           h4("Favorite Cards"),
                           uiOutput("favorite_card_ui"),
                           br(),
                           h4("Cumulative Winrate Over Time"),
                           plotOutput("cumulative_plot", height = "300px")
                            )    
                        )
                  ),
                  tabPanel("Deck Explorer",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("selected_deck", "Select Deck:", choices = NULL)
                             ),
                             mainPanel(
                               h4("Deck Stats"),
                               uiOutput("deck_stats"),
                               h4("Mana Curve"),
                               plotOutput("mana_curve"),
                               h4("Cards"),
                               DTOutput("deck_cards")
                             )
                           )
                  )
                )
)



server <- function(input, output, session) {
  scryfall_cache <- reactiveValues(data = list())
  output$PlayerWinrates <- renderPlot(playerWinrateChart)
  output$ArchWinrates <- renderPlot(archWinrateChart)
  output$ColorWinrates <- renderPlot(colorWinrateChart)
  output$ComboWinrates <- renderPlot(colorComboWinrateChart)
  output$Heatmap <- renderPlot(winrate_heatmap)
  output$MatchupHeat <- renderPlot(matchup_winrates)
  
  #player logic
  # Assume players and game_log are dataframes already loaded
  updateSelectInput(session, "selected_player", choices = unique(players$Name))
  
  # Reactive: Get selected player's ID
  selected_player_id <- reactive({
    players %>% filter(Name == input$selected_player) %>% pull(PlayerId)
  })
  
  # Overall winrate
  output$winrate <- renderText({
    pid <- selected_player_id()
    games <- game_log %>%
      filter(player1 == pid | player2 == pid)
    
    wins <- sum((games$player1 == pid & games$result == 1) |
                  (games$player2 == pid & games$result == 2))
    
    total <- nrow(games)
    paste0("Winrate: ", round(100 * wins / total, 1), "% (", wins, "/", total, ")")
  })
  
  # Preferred color combo
  output$preferred_colors <- renderText({
    pid <- selected_player_id()
    deck_colors <- decks %>%
      filter(PlayerId == pid) %>%
      count(Color.Identity, sort = TRUE)
    
    top <- deck_colors$Color.Identity[1]
    paste("Preferred Color Identity:", top)
  })
  
  # Preferred archetype
  output$preferred_archetype <- renderText({
    pid <- selected_player_id()
    archetypes <- decks %>%
      filter(PlayerId == pid) %>%
      count(Classification, sort = TRUE)
    
    paste("Most Played Archetype:", archetypes$Classification[1])
  })
  
  # Worst winrate opponent
  output$worst_matchup <- renderText({
    pid <- selected_player_id()
    
    matchups <- game_log %>%
      filter(player1 == pid | player2 == pid) %>%
      mutate(opponent = ifelse(player1 == pid, player2, player1),
             win = (player1 == pid & result == 1) | (player2 == pid & result == 2)) %>%
      group_by(opponent) %>%
      dplyr::summarise(wins = sum(win), total = n(), winrate = wins / total) %>%
      filter(total >= 2) %>%  # Optional: Only include opponents played multiple times
      arrange(winrate)
    
    if (nrow(matchups) == 0) {
      "No opponents with enough games played."
    } else {
      opp_name <- players$Name[matchups$opponent[1] == players$PlayerId]
      paste("Nemesis Winrate:", opp_name, "-", round(100 * matchups$winrate[1], 1), "%")
    }
  })
  
  output$cumulative_plot <- renderPlot({
    pid <- selected_player_id()
    
    # Filter games where player participated
    player_games <- game_log %>%
      filter(player1 == pid | player2 == pid) %>%
      mutate(
        date = as.Date(date, format = "%m-%d-%y"),
        is_win = (player1 == pid & result == 1) | (player2 == pid & result == 2)
      ) %>%
      arrange(date) %>%
      mutate(
        game_num = row_number(),
        cum_wins = cumsum(is_win),
        cum_winrate = cum_wins / game_num
      )
    
    ggplot(player_games, aes(x = date, y = cum_winrate)) +
      geom_line(color = "blue", size = 1) +
      geom_point(size = 1.5, alpha = 0.7) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      labs(
        title = "Cumulative Winrate Over Time",
        x = "Date",
        y = "Winrate"
      ) +
      theme_minimal()
  })
  
  output$favorite_card_ui <- renderUI({
    req(selected_player_id())
    pid <- selected_player_id()
    
    # Get deck IDs from winning games
    #winning_decks <- game_log %>%
    #  filter((player1 == pid & result == 1) | (player2 == pid & result == 2)) %>%
    #  mutate(winning_deck = ifelse(player1 == pid, deck1, deck2)) %>%
    #  pull(winning_deck) %>%
    #  as.character()
    player_decks <- game_log %>%
      filter(player1 == pid | player2 == pid) %>%
      mutate(player_deck = ifelse(player1 == pid, deck1, deck2)) %>%
      pull(player_deck) %>%
      as.character()
    
    deck_subset <- binary_matrix[rownames(binary_matrix) %in% player_decks, , drop = FALSE]
    
    if (nrow(deck_subset) == 0) {
      return("No winning deck data available.")
    }
    
    # Count card frequencies
    card_counts <- colSums(deck_subset)
    
    # Filter to nonland cards only
    card_counts <- card_counts[names(card_counts) %in% nonland_cards]
    
    if (length(card_counts) == 0 || all(card_counts == 0)) {
      return("No nonland cards found in winning decks.")
    }
    
    # Get top 3 cards
    top_cards <- names(sort(card_counts, decreasing = TRUE))[1:5]
    
    # Generate image tags
    card_ui <- lapply(top_cards, function(card_name) {
      # Check cache
      if (!card_name %in% names(scryfall_cache$data)) {
        query <- URLencode(card_name, reserved = TRUE)
        api_url <- paste0("https://api.scryfall.com/cards/named?exact=", query)
        
        card_data <- tryCatch({
          jsonlite::fromJSON(api_url)
        }, error = function(e) NULL)
        
        # Cache the result
        scryfall_cache$data[[card_name]] <- card_data
      } else {
        card_data <- scryfall_cache$data[[card_name]]
      }
      
      # Render card if valid
      if (!is.null(card_data) && !is.null(card_data$image_uris)) {
        tags$div(style = "display: inline-block; margin-right: 15px;",
                 strong(card_name),
                 br(),
                 tags$img(src = card_data$image_uris$normal,
                          style = "width: 223px; height: 310px; border: 1px solid #ccc;")
        )
      } else {
        tags$div(strong(card_name), br(), "(Image not available)")
      }
    })
    
    do.call(tagList, card_ui)
  })
  

  
  output$player_image <- renderUI({
    req(input$selected_player)
    
    safe_name <- gsub(" ", "", tolower(input$selected_player))
    image_file <- get_player_media(input$selected_player)
    
    if (is.null(image_file)) {
      return(NULL)
    }
    
    tags$img(
      src = image_file,
      style = "width:100%; object-fit:cover; max-height:600px; border-radius:8px;"
    )
  })
  
  deck_winrates <- reactive({
    game_log %>%
      pivot_longer(cols = c(deck1, deck2), names_to = "role", values_to = "deck") %>%
      mutate(
        is_win = (role == "deck1" & result == 1) | (role == "deck2" & result == 2)
      ) %>%
      group_by(deck) %>%
      summarise(
        games = n(),
        wins = sum(is_win),
        winrate = round(100 * wins / games, 1),
        .groups = "drop"
      ) %>%
      left_join(decks, by = c("deck" = "deckID")) %>%
      mutate(label = paste0(PlayerName, ": ", Date, " (", winrate, "% winrate)"))
  })
  
  observe({
    choices <- deck_winrates() %>%
      arrange(desc(winrate))  # highest winrate first
    
    updateSelectInput(
      inputId = "selected_deck",
      choices = setNames(choices$deck, choices$label)
    )
  })
  
  output$deck_stats <- renderUI({
    req(input$selected_deck)
    
    stats <- deck_winrates() %>%
      filter(deck == input$selected_deck)
    
    # Get player name
    deck_player <- decks %>%
      filter(deckID == input$selected_deck) %>%
      pull(PlayerName) %>%
      unique()
    
    deck_date <- decks %>%
      filter(deckID == input$selected_deck) %>%
      pull(Date) %>%
      unique()
    
    HTML(paste0(
      "<h4>", deck_player, "</h4>",
      "<b>Deck Played:</b> ", as.Date(deck_date, '%m/%d%y'), "<br/>",
      "<b>Games Played:</b> ", stats$games, "<br/>",
      "<b>Wins:</b> ", stats$wins, "<br/>",
      "<b>Winrate:</b> ", stats$winrate, "%"
    ))
  })

  output$mana_curve <- renderPlot({
    req(input$selected_deck)
    
    card_names <- decklists[[input$selected_deck]]
    card_names <- na.omit(card_names)
    
    deck_info <- scryfall_data %>%
      filter(name %in% card_names, !grepl("Land", type_line, ignore.case = TRUE))
    
    hist(deck_info$cmc, breaks = 0:10, col = "steelblue",
         main = "Mana Curve", xlab = "Converted Mana Cost (CMC)", ylab = "Count")
  })
  
  output$deck_cards <- renderDT({
    req(input$selected_deck)
    
    card_names <- decklists[[input$selected_deck]]
    card_names <- na.omit(card_names)
    
    # Get card counts
    card_table <- as.data.frame(table(card_names))
    colnames(card_table) <- c("name", "count")
    
    # Join with Scryfall image and details
    scryfall_deduped <- scryfall_data %>%
      distinct(name, .keep_all = TRUE)
    card_data <- card_table %>%
      left_join(scryfall_deduped, by = "name") %>%
      select(name, mana_cost, type_line, count, image_url)
    
    # Add custom HTML hover tooltips
    card_data$tooltip <- paste0(
      "<span title=\"\"><img src='", card_data$image_url,
      "' style='width:200px;'/></span>"
    )
    
    # Render as datatable with image on hover
    datatable(
      card_data[, c("name", "mana_cost", "type_line", "count", "image_url")],
      escape = FALSE,
      rownames = FALSE,
      options = list(
        columnDefs = list(
          list(
            targets = 0,  # card name column
            render = JS(
              "function(data, type, row, meta) {",
              "  if (type === 'display') {",
              "    return '<div style=\"position:relative; display:inline-block;\">' +",
              "           data +",
              "           '<img src=\"' + row[4] + '\" style=\"display:none; position:absolute; top:1.5em; left:0; z-index:1000; width:200px;\" class=\"hover-img\"/>' +",
              "           '</div>';",
              "  } else { return data; }",
              "}"
            )
          ),
          list(
            targets = 4,  # hide image_url column
            visible = FALSE
          )
        )
      ),
      callback = JS("
    table.on('mouseenter', 'td', function() {
      $(this).find('img.hover-img').show();
    }).on('mouseleave', 'td', function() {
      $(this).find('img.hover-img').hide();
    });
    ")
    )
    
    
  })
  
}

shinyApp(ui, server)