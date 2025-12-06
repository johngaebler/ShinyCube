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

#setup for the app, data reading, table prep
#loading data
decks <- read.csv("Cube_Stats - Deck Info.csv")
decklists <- read.csv("Cube_Stats - All Decklists.csv", na.strings = c("", "NA"), check.names = FALSE)
game_log <- read.csv("Cube_Stats - game_log.csv", stringsAsFactors = F)
game_log2 <- readRDS("game_log2.rds")
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

scryfall_lookup <- readRDS("scryfall_lookup.rds")

nonland_front_faces <- scryfall_lookup %>%
  filter(!grepl("\\bLand\\b", type_line, ignore.case = TRUE)) %>%
  mutate(name = str_trim(str_extract(name, "^[^/]+"))) %>%
  distinct(name) %>%
  pull(name)


card_meta <- scryfall_lookup %>%
  dplyr::filter(name %in% colnames(binary_matrix)) %>%
  dplyr::select(name, type_line)

# Extract nonland card names
nonland_cards <- card_meta %>%
  filter(!grepl("\\bLand\\b", type_line, ignore.case = TRUE)) %>%
  pull(name)

playerWinrates <- readRDS("player_winrates.rds")

archWinrates <- readRDS("arch_winrates.rds")

colorComboWinrates <- readRDS("combo_winrates.rds")

colorWinrates <- readRDS("color_winrates.rds")

deck_cards <- readRDS("deck_cards.rds")

### Going a step further, create unified result structure
excluded_names <- c( "Sky", "Gretchen", "Tini", "Shane", "Zeth", "Alex", "Tay","Mack ", 'Asher')

heatmap_data <- readRDS("heatmap_data.rds")
colnames(heatmap_data) <- c("Player", "Opponent", "Winrate")

## Archetype stats
decks$deckID <- as.integer(sub('.', '', decks$Deck.ID))
filtered_matchups <- readRDS("filtered_matchups.rds")


get_player_media <- function(player_name) {
  safe_name <- gsub(" ", "", tolower(player_name))
  extensions <- c("gif", "jpg", "jpeg", "png")
  
  for (ext in extensions) {
    candidate <- paste0("www/", safe_name, ".", ext)
    if (file.exists(candidate)) {
      return(paste0(safe_name, ".", ext))  # relative to www/ for use in <img src>
    }
  }
  
  return(NULL)  #null if nothing found
}


## ELO Calculation 

elo_update <- function(rating1, rating2, result, k = 32) {
  # result = 1 if player1 wins, 0 if player2 wins
  exp1 <- 1 / (1 + 10^((rating2 - rating1) / 400))
  exp2 <- 1 - exp1
  r1_new <- rating1 + k * (result - exp1)
  r2_new <- rating2 + k * ((1 - result) - exp2)
  list(r1 = r1_new, r2 = r2_new)
}

calculate_player_elo <- function(game_log, k = 32, init_rating = 1000) {
  players <- unique(c(game_log$player1, game_log$player2))
  ratings <- setNames(rep(init_rating, length(players)), players)
  
  history <- list()
  
  game_log <- game_log %>% arrange(as.Date(date, format = "%m-%d-%y"))
  
  for (i in seq_len(nrow(game_log))) {
    g <- game_log[i, ]
    p1 <- as.character(g$player1)
    p2 <- as.character(g$player2)
    res <- ifelse(g$result == 1, 1, 0)  # 1 if p1 win, else 0
    
    upd <- elo_update(ratings[p1], ratings[p2], res, k)
    ratings[p1] <- upd$r1
    ratings[p2] <- upd$r2
    
    history[[i]] <- data.frame(
      gameId = g$id,
      date = g$date,
      player = c(p1, p2),
      rating = c(ratings[p1], ratings[p2])
    )
  }
  
  bind_rows(history)
}
elo_history <- calculate_player_elo(game_log)
elo_history$player <- as.integer(elo_history$player)

current_elo_lookup <- elo_history %>%
  dplyr::group_by(player) %>%
  dplyr::slice_tail(n = 1) %>%      # last row per player (history is in order)
  dplyr::ungroup() %>%
  dplyr::select(player, rating)


## ==== Begin UI and Server Functions ==== ##

ui <- fluidPage(theme = shinytheme("slate"),
                useShinyjs(),
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
                      #background-image {
                        position: fixed;
                        top: 0; left: 0;
                        width: 100vw;
                        height: 100vh;
                        background-image: url('backgrounds/mindovermatter.jpg');
                        background-size: cover;
                        background-position: center;
                        opacity: 0.3; /* Adjust transparency here */
                        z-index: -1;
                      }
                    "))
                ),
                div(id = "background-image"),
                titlePanel("Cube Stats!!!  >:)"),
                
                tabsetPanel(
                  tabPanel(title ="Overall Stats",
                           fluidRow(
                                    wellPanel(
                                      layout_columns(
                                      plotOutput("player_winrate_plot"),
                                      
                                      plotOutput("archetype_winrate_plot"),
                                      
                                      plotOutput("color_winrate_plot"),
                                      
                                      plotOutput("colorcombo_winrate_plot"),
                                      
                                      plotOutput("player_confusion_plot"),
                                      
                                      plotOutput("archetype_matchup_plot")
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
                           verbatimTextOutput("current_elo_chip"),
                           br(),
                           h4("Favorite Cards"),
                           uiOutput("favorite_card_ui"),
                           br(),
                           h4("Player Elo Over Time"),
                           plotOutput("elo_plot"),
                           h4("Cumulative Winrate Over Time"),
                           plotOutput("cumulative_plot", height = "300px")
                            )    
                        )
                  ),
                  tabPanel("Deck Explorer",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput(
                                 inputId = "deck_sort_order",
                                 label = "Sort decks by:",
                                 choices = c("Winrate" = "winrate", "Player" = "player", "Date Played" = "date"),
                                 selected = "winrate"
                               ),
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
                  ),
                  tabPanel(
                    "Card Stats",
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("min_games_card", "Minimum games with card:", min = 0, max = 30, value = 5, step = 1),
                        sliderInput("prior_weight", "Bayesian prior weight (games):", min = 5, max = 40, value = 10, step = 1),
                        tags$a(href="https://kiwidamien.github.io/shrinkage-and-empirical-bayes-to-improve-inference.html", "Bayesian Shrinkage...?!", style = "color:lightBlue;"), 
                        selectInput("sort_cards_by", "Sort by:", 
                                    choices = c("Shrinkage Winrate" = "shrink_wr",
                                                "Raw Winrate"       = "raw_wr",
                                                "Games"             = "games"),
                                    selected = "shrink_wr"),
                        selectizeInput("card_pick", "Find a card:", choices = NULL, options = list(placeholder = "Type to search..."))
                      ),
                      mainPanel(
                        h4("Selected Card"),
                        uiOutput("card_detail"),
                        hr(),
                        h4("All Cards (filterable)"),
                        DTOutput("card_table")
                      )
                    )
                  )
                  
                )
                
)



server <- function(input, output, session) {
  scryfall_cache <- reactiveValues(data = list())
  #player logic
  updateSelectInput(session, "selected_player", choices = unique(players$Name))
  
  # Reactive: Get selected player's ID for use pretty much everywhere else on this page
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
  
  output$current_elo_chip <- renderText({
    # use current_elo_lookup() if reactive; otherwise current_elo_lookup
    df <- if (is.reactive(current_elo_lookup)) current_elo_lookup() else current_elo_lookup
    req(selected_player_id())
    pid <- selected_player_id()
    
    val <- df %>%
      dplyr::filter(player == pid) %>%
      dplyr::pull(rating) %>%
      tail(1)
    
    req(length(val) == 1)
    paste0("Current Elo: ", round(val))
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
      filter(total >= 2) %>%  # Only include opponents played multiple times, otherwise whats the point
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
      geom_line(color = "blue", linewidth = 1) +
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
    
    # Filter to nonland cards only, is this superfluous since i do this above??
    card_counts <- card_counts[names(card_counts) %in% nonland_cards]
    
    if (length(card_counts) == 0 || all(card_counts == 0)) {
      return("No nonland cards found in winning decks.")
    }
    
    top_cards <- names(sort(card_counts, decreasing = TRUE))[1:5]
    
    # Generate image tags
    card_ui <- lapply(top_cards, function(card_name) {
      # Check cache for speeeeeeeed
      if (!card_name %in% names(scryfall_cache$data)) {
        #full_card_name <- scryfall_data$name[grep(card_name, scryfall_data$name)][1]
        #query <- URLencode(full_card_name, reserved = TRUE)
        query <- URLencode(card_name, reserved = TRUE)
        api_url <- paste0("https://api.scryfall.com/cards/named?exact=", query)
        
        card_data <- tryCatch({
          jsonlite::fromJSON(api_url)
        }, error = function(e) NULL)
        
        # Cache result
        scryfall_cache$data[[card_name]] <- card_data
      } else {
        card_data <- scryfall_cache$data[[card_name]]
      }
      
      # Render card if thers a valid image available
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
  
  output$elo_plot <- renderPlot({
    req(selected_player_id())
    pid <- selected_player_id()
    df<- left_join(elo_history, players,join_by(player == PlayerId))
    df <- df %>% filter(player == pid)
    ggplot(df, aes(x = as.Date(date, format = "%m-%d-%y"), y = rating)) +
      geom_line(color = "steelblue") +
      geom_point() +
      labs(title = paste("Elo rating over time -", df$Name),
           x = "Date", y = "Elo Rating") +
      theme_minimal()
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
    req(deck_winrates)  #n breaks without this line in here idk why 
    
    # Sort based on user-selected criteria
    sorted_decks <- switch(
      input$deck_sort_order,
      "winrate" = deck_winrates() %>% arrange(desc(winrate)),
      "player" = deck_winrates() %>% arrange(PlayerName),
      "date" = deck_winrates() %>% arrange(desc(Date)),
      deck_winrates()
    )
    
    deck_labels <- 
      sorted_decks$label
    
    updateSelectInput(
      session,
      inputId = "selected_deck",
      choices = setNames(sorted_decks$deck, deck_labels)
    )
  })
  
  output$deck_stats <- renderUI({
    req(input$selected_deck)
    
    stats <- deck_winrates() %>%
      filter(deck == input$selected_deck)
    
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
    
    deck_info <- scryfall_lookup %>%
      filter(name %in% card_names, !grepl("Land", type_line, ignore.case = TRUE)) %>%
      distinct(name, .keep_all = TRUE)  # Deduping here
    
    deck_info <- deck_info %>%
      mutate(cmc = as.numeric(cmc)) %>%
      filter(!is.na(cmc), cmc >= 0, cmc <= 15)
    
    hist(deck_info$cmc, breaks = 0:max(deck_info$cmc), col = "steelblue",
         main = "Mana Curve", xlab = "Converted Mana Cost (CMC)", ylab = "Count")
  })
  
  output$deck_cards <- renderDT({
    req(input$selected_deck)
    
    card_names <- decklists[[input$selected_deck]]
    card_names <- na.omit(card_names)
    
    card_table <- as.data.frame(table(card_names))
    colnames(card_table) <- c("name", "count")
    
    # Join with Scryfall image and details, avoid bad joins here !!!!!
    card_data <- card_table %>%
      left_join(scryfall_lookup, by = "name") %>%
      select(name, mana_cost, cmc, type_line, image_url)
    
    # Add custom HTML hover tooltips
    card_data$tooltip <- paste0(
      "<span title=\"\"><img src='", card_data$image_url,
      "' style='width:200px;'/></span>"
    )
    
    # Render as datatable with image on hover
    datatable(
      card_data[, c("name", "mana_cost", "cmc", "type_line", "image_url")],
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
  
  output$player_winrate_plot <- renderPlot({
    ggplot(data = playerWinrates, 
           aes(x = reorder(PlayerName, -Winrate),y=Winrate, size = GamesPlayed))+
      geom_point()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      geom_hline(yintercept=0.5, linetype = 2)+
      labs(title = "Player Winrate", x = "Player", y= "Winrate", size = "Games Played")
  })
  
  output$archetype_winrate_plot <- renderPlot({
    ggplot(data = archWinrates, 
           aes(x = reorder(Archetype, -Winrate),y=Winrate, size = GamesPlayed))+
      geom_point()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      geom_hline(yintercept=0.5, linetype = 2)+
      labs(title = "Archetype Winrate", x = "Archetype", y= "Winrate", size = "Games Played")
  })
  
  output$color_winrate_plot <- renderPlot({
    ggplot(data = colorWinrates, 
           aes(x = reorder(Color, -Winrate),y=Winrate, color = Color))+
      geom_point(size = 6)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(title = "Color Winrate", x = "Color", y= "Winrate")+
      geom_hline(yintercept=0.5, linetype = 2)+
      scale_color_manual(values = c("black", "green", "red", 
                                    "blue", "white"))
  })
  
  output$colorcombo_winrate_plot <- renderPlot({
    ggplot(data = colorComboWinrates, 
           aes(x = reorder(Color, -Winrate),y=Winrate, size = GamesPlayed))+
      geom_point()+
      geom_hline(yintercept=0.5, linetype = 2)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(title = "ColorCombo Winrate", x = "ColorCombo", y= "Winrate", size = "Games Played")
  })
  
  output$player_confusion_plot <- renderPlot({
    ggplot(heatmap_data, aes(x = Opponent, y = Player, fill = Winrate)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0.5, limits = c(0, 1)) +
      geom_text(aes(label = sprintf("%.2f", Winrate)), size = 3) +
      theme_minimal() +
      labs(title = "Head-to-Head Winrates", x = "Opponent", y = "Player")
  })
  
  output$archetype_matchup_plot <- renderPlot({
    ggplot(filtered_matchups, aes(x = opponent_archetype, y = player_archetype, fill = winrate)) +
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
  })
  
  # Core reactive: per-card winrates with Bayesian shrinkage
  card_stats <- reactive({
    req(nonland_cards)  # your earlier vector of non-land cards
    
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
      return(tibble(card_name = character(), games = integer(), wins = integer(),
                    raw_wr = numeric(), shrink_wr = numeric()))
    }
    
    overall_wr <- mean(card_games$is_win) # baseline
    
    # Summarise & shrink
    card_wr <- card_games %>%
      group_by(card_name) %>%
      summarise(games = n(), wins = sum(is_win), .groups = "drop") %>%
      mutate(
        raw_wr    = ifelse(games > 0, wins / games, NA_real_),
        shrink_wr = (wins + overall_wr * input$prior_weight) / (games + input$prior_weight)
      )
    
    # Join images/meta by front-face partial match (deck names are front face)
    out <- card_wr %>%
      left_join(scryfall_lookup, by = c("card_name" = "name"))
    
    out
  })
  
  # Populate card selector with filtered list
  observe({
    cs <- card_stats()
    cs <- cs %>% filter(games >= input$min_games_card)
    updateSelectizeInput(session, "card_pick", choices = sort(cs$card_name), server = TRUE)
  })
  
  # Card detail header (name, games, winrates, image)
  output$card_detail <- renderUI({
    cs <- card_stats()
    req(nrow(cs) > 0)
    
    # Pick selected or default to top by chosen sort
    if (isTruthy(input$card_pick) && input$card_pick %in% cs$card_name) {
      row <- cs %>% filter(card_name == input$card_pick)
    } else {
      metric <- match.arg(input$sort_cards_by, c("shrink_wr", "raw_wr", "games"))
      row <- cs %>% arrange(dplyr::desc(.data[[metric]])) %>% slice(1)
    }
    req(nrow(row) == 1)
    
    pretty_wr <- function(x) ifelse(is.na(x), "NA", paste0(round(100*x, 1), "%"))
    
    tags$div(
      style = "display:flex; gap:16px; align-items:flex-start; flex-wrap:wrap;",
      tags$div(
        style = "min-width:220px;",
        tags$h3(
          as.character(row$card_name[[1]]),
          style = "margin:0 0 6px 0; font-weight:300; color:red;"
        ),
        tags$div(paste("Games:", row$games)),
        tags$div(paste("Wins:", row$wins)),
        tags$div(paste("Raw Winrate:",  pretty_wr(row$raw_wr))),
        tags$div(paste("Shrink Winrate:", pretty_wr(row$shrink_wr))),
        if (!is.na(row$type_line)) tags$div(paste("Type:", row$type_line))
      ),
      if (!is.na(row$image_url)) tags$img(
        src = row$image_url,
        style = "width:223px; height:310px; border:1px solid #444; border-radius:6px;"
      )
    )
  })
  
  # Card table with hover images (hide image_url column)
  output$card_table <- renderDT({
    cs <- card_stats() %>% filter(games >= input$min_games_card)
    
    # sort the data frame server-side too (nice for the detail panel)
    metric <- match.arg(input$sort_cards_by, c("shrink_wr", "raw_wr", "games"))
    cs <- cs %>% arrange(dplyr::desc(.data[[metric]]))
    
    tbl <- cs %>%
      transmute(
        name = card_name,
        games, wins,
        raw_winrate    = ifelse(is.na(raw_wr), NA, round(100*raw_wr, 1)),
        shrink_winrate = ifelse(is.na(shrink_wr), NA, round(100*shrink_wr, 1)),
        image_url
      )
    
    # map metric -> DataTables column index (0-based)
    order_idx <- switch(metric,
                        "shrink_wr" = 4L,  # shrink_winrate
                        "raw_wr"    = 3L,  # raw_winrate
                        "games"     = 1L   # games
    )
    
    datatable(
      tbl[, c("name", "games", "wins", "raw_winrate", "shrink_winrate", "image_url")],
      escape = FALSE, rownames = FALSE,
      options = list(
        pageLength = 15,
        order = list(list(order_idx, "desc")),  # <<< key line: initial sort
        columnDefs = list(
          list(
            targets = 0, # name column with hover image
            render = JS(
              "function(data, type, row, meta) {",
              "  if (type === 'display') {",
              "    return '<div style=\"position:relative; display:inline-block;\">' +",
              "           data +",
              "           '<img src=\"' + row[5] + '\" style=\"display:none; position:absolute; top:1.5em; left:0; z-index:1000; width:200px;\" class=\"hover-img\"/>' +",
              "           '</div>';",
              "  } else { return data; }",
              "}"
            )
          ),
          list(targets = 5, visible = FALSE) # hide image_url
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
