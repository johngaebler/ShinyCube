# app.R
# ShinyCube — load-only Shiny app. All data prep lives in data_update.R.
# Run data_update.R first to (re)generate the .rds artifacts this app reads.

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(shinythemes)
library(shinyjs)
library(DT)
library(stringr)
library(purrr)

source("config.R")

# ---------------------------------------------------------------------------
# Load all artifacts produced by data_update.R
# ---------------------------------------------------------------------------

decks               <- readRDS("decks.rds")
players             <- readRDS("players.rds")
game_log            <- readRDS("game_log.rds")
game_log2           <- readRDS("game_log2.rds")

binary_matrix       <- readRDS("binary_matrix.rds")
deck_id_to_cards    <- readRDS("deck_id_to_cards.rds")
deck_cards          <- readRDS("deck_cards.rds")

scryfall_lookup     <- readRDS("scryfall_lookup.rds")
nonland_cards       <- readRDS("nonland_cards.rds")
nonland_front_faces <- readRDS("nonland_front_faces.rds")

playerWinrates      <- readRDS("player_winrates.rds")
archWinrates        <- readRDS("arch_winrates.rds")
colorWinrates       <- readRDS("color_winrates.rds")
colorComboWinrates  <- readRDS("combo_winrates.rds")

heatmap_data        <- readRDS("heatmap_data.rds")
filtered_matchups   <- readRDS("filtered_matchups.rds")

elo_history         <- readRDS("elo_history.rds")
daily_elo_history   <- readRDS("daily_elo_history.rds")
current_elo_lookup  <- readRDS("current_elo_lookup.rds")

# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------

get_player_media <- function(player_name) {
  safe_name <- gsub(" ", "", tolower(player_name))
  for (ext in c("gif", "jpg", "jpeg", "png")) {
    candidate <- paste0("www/", safe_name, ".", ext)
    if (file.exists(candidate)) return(paste0(safe_name, ".", ext))
  }
  NULL
}

pretty_wr <- function(x) ifelse(is.na(x), "NA", paste0(round(100 * x, 1), "%"))

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("slate"),
  useShinyjs(),
  tags$head(tags$style(HTML("
    .dataTables_wrapper { color: #eee; }
    .dataTables_length select,
    .dataTables_filter input {
      background-color: #222; color: #eee; border: 1px solid #555;
    }
    .dataTables_info { color: #ccc; }
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      background-color: #222; color: #eee !important; border: 1px solid #444;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current {
      background-color: #555 !important; color: #fff !important;
    }
    #background-image {
      position: fixed; top: 0; left: 0;
      width: 100vw; height: 100vh;
      background-image: url('backgrounds/mindovermatter.jpg');
      background-size: cover; background-position: center;
      opacity: 0.3; z-index: -1;
    }
  "))),
  div(id = "background-image"),
  titlePanel("Cube Stats!!!  >:)"),

  tabsetPanel(
    tabPanel("Overall Stats",
      fluidRow(wellPanel(
        layout_columns(
          plotOutput("player_winrate_plot"),
          plotOutput("archetype_winrate_plot"),
          plotOutput("color_winrate_plot"),
          plotOutput("colorcombo_winrate_plot"),
          plotOutput("player_confusion_plot"),
          plotOutput("archetype_matchup_plot")
        )
      ))
    ),

    tabPanel("Player Summary Dashboard",
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
          selectInput("deck_sort_order", "Sort decks by:",
                      choices = c("Winrate" = "winrate",
                                  "Player"  = "player",
                                  "Date Played" = "date"),
                      selected = "winrate"),
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

    tabPanel("Card Stats",
      sidebarLayout(
        sidebarPanel(
          sliderInput("min_games_card", "Minimum games with card:",
                      min = 0, max = 30, value = MIN_GAMES_DEFAULT, step = 1),
          sliderInput("prior_weight", "Bayesian prior weight (games):",
                      min = 5, max = 40, value = PRIOR_WEIGHT_DEFAULT, step = 1),
          tags$a(href = "https://kiwidamien.github.io/shrinkage-and-empirical-bayes-to-improve-inference.html",
                 "Bayesian Shrinkage...?!", style = "color:lightBlue;"),
          selectInput("sort_cards_by", "Sort by:",
                      choices = c("Shrinkage Winrate" = "shrink_wr",
                                  "Raw Winrate"       = "raw_wr",
                                  "Games"             = "games"),
                      selected = "shrink_wr"),
          selectizeInput("card_pick", "Find a card:", choices = NULL,
                         options = list(placeholder = "Type to search..."))
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

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  updateSelectInput(session, "selected_player", choices = unique(players$Name))

  selected_player_id <- reactive({
    players %>% filter(Name == input$selected_player) %>% pull(PlayerId)
  })

  # ---- Player Summary ----

  output$winrate <- renderText({
    pid <- selected_player_id()
    games <- game_log %>% filter(player1 == pid | player2 == pid)
    wins  <- sum((games$player1 == pid & games$result == 1) |
                 (games$player2 == pid & games$result == 2))
    total <- nrow(games)
    paste0("Winrate: ", round(100 * wins / total, 1), "% (", wins, "/", total, ")")
  })

  output$preferred_colors <- renderText({
    pid <- selected_player_id()
    top <- decks %>%
      filter(PlayerId == pid) %>%
      count(Color.Identity, sort = TRUE) %>%
      slice(1) %>%
      pull(Color.Identity)
    paste("Preferred Color Identity:", top)
  })

  output$preferred_archetype <- renderText({
    pid <- selected_player_id()
    top <- decks %>%
      filter(PlayerId == pid) %>%
      count(Classification, sort = TRUE) %>%
      slice(1) %>%
      pull(Classification)
    paste("Most Played Archetype:", top)
  })

  output$current_elo_chip <- renderText({
    req(selected_player_id())
    pid <- selected_player_id()
    val <- current_elo_lookup %>% filter(player == pid) %>% pull(rating)
    if (length(val) != 1) "Current Elo: -" else paste0("Current Elo: ", round(val))
  })

  output$worst_matchup <- renderText({
    pid <- selected_player_id()
    matchups <- game_log %>%
      filter(player1 == pid | player2 == pid) %>%
      mutate(opponent = if_else(player1 == pid, player2, player1),
             win = (player1 == pid & result == 1) | (player2 == pid & result == 2)) %>%
      group_by(opponent) %>%
      summarise(wins = sum(win), total = n(), winrate = wins / total,
                .groups = "drop") %>%
      filter(total >= 2) %>%
      arrange(winrate)

    if (nrow(matchups) == 0) {
      "No opponents with enough games played."
    } else {
      opp_name <- players$Name[players$PlayerId == matchups$opponent[1]]
      paste("Nemesis Winrate:", opp_name, "-", round(100 * matchups$winrate[1], 1), "%")
    }
  })

  output$cumulative_plot <- renderPlot({
    pid <- selected_player_id()
    player_games <- game_log %>%
      filter(player1 == pid | player2 == pid) %>%
      mutate(is_win = (player1 == pid & result == 1) |
                      (player2 == pid & result == 2)) %>%
      arrange(date, id) %>%
      mutate(game_num    = row_number(),
             cum_wins    = cumsum(is_win),
             cum_winrate = cum_wins / game_num) %>%
      # End-of-day collapse: one point per session, using the cumulative
      # winrate after the last game of the day. Avoids the within-session
      # zigzag that mirrors the Elo plot's old behavior.
      group_by(date) %>%
      slice_tail(n = 1) %>%
      ungroup()

    ggplot(player_games, aes(x = date, y = cum_winrate)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_point(size = 1.5, alpha = 0.7) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      labs(title = "Cumulative Winrate Over Time", x = "Date", y = "Winrate") +
      theme_minimal()
  })

  output$favorite_card_ui <- renderUI({
    req(selected_player_id())
    pid <- selected_player_id()

    player_decks <- game_log %>%
      filter(player1 == pid | player2 == pid) %>%
      mutate(player_deck = if_else(player1 == pid, deck1, deck2)) %>%
      pull(player_deck) %>%
      as.character()

    deck_subset <- binary_matrix[rownames(binary_matrix) %in% player_decks, , drop = FALSE]
    if (nrow(deck_subset) == 0) return("No deck data available.")

    # binary_matrix columns are front-face names, matching scryfall_lookup.
    card_counts <- colSums(deck_subset)
    card_counts <- card_counts[names(card_counts) %in% nonland_cards]
    if (length(card_counts) == 0 || all(card_counts == 0)) {
      return("No nonland cards found in player's decks.")
    }

    top_cards <- names(sort(card_counts, decreasing = TRUE))[1:5]

    # Single local join — no API roundtrip, no special handling for
    # double-faced cards (front-face name matches scryfall_lookup directly).
    top_card_info <- tibble(name = top_cards) %>%
      left_join(scryfall_lookup, by = "name")

    card_ui <- pmap(top_card_info, function(name, image_url, ...) {
      if (!is.na(image_url)) {
        tags$div(style = "display:inline-block; margin-right:15px;",
                 strong(name), br(),
                 tags$img(src = image_url,
                          style = "width:223px; height:310px; border:1px solid #ccc;"))
      } else {
        tags$div(strong(name), br(), "(Image not available)")
      }
    })
    do.call(tagList, card_ui)
  })

  output$player_image <- renderUI({
    req(input$selected_player)
    image_file <- get_player_media(input$selected_player)
    if (is.null(image_file)) return(NULL)
    tags$img(src = image_file,
             style = "width:100%; object-fit:cover; max-height:600px; border-radius:8px;")
  })

  output$elo_plot <- renderPlot({
    req(selected_player_id())
    pid <- selected_player_id()

    df <- daily_elo_history %>%
      filter(player == pid) %>%
      left_join(players, by = c("player" = "PlayerId"))
    req(nrow(df) > 0)

    ggplot(df, aes(x = date, y = rating)) +
      geom_line(color = "steelblue") +
      geom_point() +
      labs(title = paste("Elo rating over time -", df$Name[1]),
           x = "Date", y = "Elo Rating") +
      theme_minimal()
  })

  # ---- Deck Explorer ----

  deck_winrates <- reactive({
    game_log %>%
      pivot_longer(cols = c(deck1, deck2), names_to = "role", values_to = "deck") %>%
      mutate(is_win = (role == "deck1" & result == 1) |
                      (role == "deck2" & result == 2)) %>%
      group_by(deck) %>%
      summarise(games = n(), wins = sum(is_win),
                winrate = round(100 * wins / games, 1),
                .groups = "drop") %>%
      left_join(decks, by = c("deck" = "deckID")) %>%
      mutate(label = paste0(PlayerName, ": ", Date, " (", winrate, "% winrate)"))
  })

  observe({
    dw <- deck_winrates()
    sorted_decks <- switch(input$deck_sort_order,
      "winrate" = dw %>% arrange(desc(winrate)),
      "player"  = dw %>% arrange(PlayerName),
      "date"    = dw %>% arrange(desc(Date)),
      dw
    )
    # Use deck-id-as-string as the value so downstream lookups into
    # deck_id_to_cards work directly.
    updateSelectInput(
      session, "selected_deck",
      choices = setNames(as.character(sorted_decks$deck), sorted_decks$label)
    )
  })

  output$deck_stats <- renderUI({
    req(input$selected_deck)
    sel <- as.integer(input$selected_deck)
    stats <- deck_winrates() %>% filter(deck == sel)
    info  <- decks %>% filter(deckID == sel)
    req(nrow(info) > 0, nrow(stats) > 0)

    HTML(paste0(
      "<h4>", info$PlayerName[1], "</h4>",
      "<b>Deck Played:</b> ", format(info$Date[1], "%Y-%m-%d"), "<br/>",
      "<b>Games Played:</b> ", stats$games, "<br/>",
      "<b>Wins:</b> ",         stats$wins, "<br/>",
      "<b>Winrate:</b> ",      stats$winrate, "%"
    ))
  })

  output$mana_curve <- renderPlot({
    req(input$selected_deck)
    card_names <- deck_id_to_cards[[input$selected_deck]]
    req(length(card_names) > 0)

    # Front-face the names so doubles match scryfall_lookup.
    front_face_names <- str_trim(str_extract(card_names, "^[^/]+"))

    deck_info <- scryfall_lookup %>%
      filter(name %in% front_face_names,
             !grepl("Land", type_line, ignore.case = TRUE)) %>%
      distinct(name, .keep_all = TRUE) %>%
      mutate(cmc = as.numeric(cmc)) %>%
      filter(!is.na(cmc), cmc >= 0, cmc <= 15)

    if (nrow(deck_info) == 0) return(NULL)
    hist(deck_info$cmc, breaks = 0:max(deck_info$cmc),
         col = "steelblue", main = "Mana Curve",
         xlab = "Converted Mana Cost (CMC)", ylab = "Count")
  })

  output$deck_cards <- renderDT({
    req(input$selected_deck)
    card_names <- deck_id_to_cards[[input$selected_deck]]
    req(length(card_names) > 0)

    # display_name preserves the "Front // Back" form for the user;
    # join_name is front-face only and matches scryfall_lookup.
    card_data <- tibble(display_name = card_names) %>%
      count(display_name, name = "count") %>%
      mutate(join_name = str_trim(str_extract(display_name, "^[^/]+"))) %>%
      left_join(scryfall_lookup, by = c("join_name" = "name")) %>%
      transmute(name = display_name, mana_cost, cmc, type_line, image_url)

    datatable(
      card_data,
      escape = FALSE, rownames = FALSE,
      options = list(
        columnDefs = list(
          list(targets = 0, render = JS(
            "function(data, type, row, meta) {",
            "  if (type === 'display') {",
            "    return '<div style=\"position:relative; display:inline-block;\">' +",
            "           data +",
            "           '<img src=\"' + row[4] + '\" style=\"display:none; position:absolute; top:1.5em; left:0; z-index:1000; width:200px;\" class=\"hover-img\"/>' +",
            "           '</div>';",
            "  } else { return data; }",
            "}"
          )),
          list(targets = 4, visible = FALSE)
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

  # ---- Overall Stats plots ----

  output$player_winrate_plot <- renderPlot({
    ggplot(playerWinrates,
           aes(x = reorder(PlayerName, -Winrate), y = Winrate, size = GamesPlayed)) +
      geom_point() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      geom_hline(yintercept = 0.5, linetype = 2) +
      labs(title = "Player Winrate", x = "Player", y = "Winrate", size = "Games Played")
  })

  output$archetype_winrate_plot <- renderPlot({
    ggplot(archWinrates,
           aes(x = reorder(Archetype, -Winrate), y = Winrate, size = GamesPlayed)) +
      geom_point() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      geom_hline(yintercept = 0.5, linetype = 2) +
      labs(title = "Archetype Winrate", x = "Archetype", y = "Winrate", size = "Games Played")
  })

  output$color_winrate_plot <- renderPlot({
    ggplot(colorWinrates,
           aes(x = reorder(Color, -Winrate), y = Winrate, color = Color)) +
      geom_point(size = 6) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(title = "Color Winrate", x = "Color", y = "Winrate") +
      geom_hline(yintercept = 0.5, linetype = 2) +
      scale_color_manual(values = c("black", "green", "red", "blue", "white"))
  })

  output$colorcombo_winrate_plot <- renderPlot({
    ggplot(colorComboWinrates,
           aes(x = reorder(Color, -Winrate), y = Winrate, size = GamesPlayed)) +
      geom_point() +
      geom_hline(yintercept = 0.5, linetype = 2) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(title = "ColorCombo Winrate", x = "ColorCombo", y = "Winrate", size = "Games Played")
  })

  output$player_confusion_plot <- renderPlot({
    ggplot(heatmap_data, aes(x = Opponent, y = Player, fill = Winrate)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "red", mid = "white", high = "green",
                           midpoint = 0.5, limits = c(0, 1)) +
      geom_text(aes(label = sprintf("%.2f", Winrate)), size = 3) +
      theme_minimal() +
      labs(title = "Head-to-Head Winrates", x = "Opponent", y = "Player")
  })

  output$archetype_matchup_plot <- renderPlot({
    ggplot(filtered_matchups,
           aes(x = opponent_archetype, y = player_archetype, fill = winrate)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "red", mid = "white", high = "green",
                           midpoint = 0.5, limits = c(0, 1), na.value = "grey90") +
      geom_text(aes(label = sprintf("%.1f%%", winrate * 100)), size = 3) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid = element_blank()) +
      labs(title = "Matchup Winrates by Archetype",
           x = "Opponent Archetype", y = "Player Archetype", fill = "Winrate")
  })

  # ---- Card Stats ----

  card_stats <- reactive({
    req(nonland_front_faces)

    cards_deck1 <- game_log2 %>%
      inner_join(deck_cards, by = c("deck1" = "deckId"),
                 relationship = "many-to-many") %>%
      transmute(card_name, is_win = deck1_win)

    cards_deck2 <- game_log2 %>%
      inner_join(deck_cards, by = c("deck2" = "deckId"),
                 relationship = "many-to-many") %>%
      transmute(card_name, is_win = !deck1_win)

    card_games <- bind_rows(cards_deck1, cards_deck2) %>%
      filter(card_name %in% nonland_front_faces)

    if (nrow(card_games) == 0) {
      return(tibble(card_name = character(), games = integer(),
                    wins = integer(), raw_wr = numeric(),
                    shrink_wr = numeric()))
    }

    overall_wr <- mean(card_games$is_win)

    card_wr <- card_games %>%
      group_by(card_name) %>%
      summarise(games = n(), wins = sum(is_win), .groups = "drop") %>%
      mutate(
        raw_wr    = if_else(games > 0, wins / games, NA_real_),
        shrink_wr = (wins + overall_wr * input$prior_weight) /
                    (games + input$prior_weight)
      )

    card_wr %>% left_join(scryfall_lookup, by = c("card_name" = "name"))
  })

  observe({
    cs <- card_stats() %>% filter(games >= input$min_games_card)
    updateSelectizeInput(session, "card_pick",
                         choices = sort(cs$card_name), server = TRUE)
  })

  output$card_detail <- renderUI({
    cs <- card_stats()
    req(nrow(cs) > 0)

    if (isTruthy(input$card_pick) && input$card_pick %in% cs$card_name) {
      row <- cs %>% filter(card_name == input$card_pick)
    } else {
      metric <- match.arg(input$sort_cards_by, c("shrink_wr", "raw_wr", "games"))
      row <- cs %>% arrange(desc(.data[[metric]])) %>% slice(1)
    }
    req(nrow(row) == 1)

    tags$div(
      style = "display:flex; gap:16px; align-items:flex-start; flex-wrap:wrap;",
      tags$div(
        style = "min-width:220px;",
        tags$h3(as.character(row$card_name[[1]]),
                style = "margin:0 0 6px 0; font-weight:300; color:red;"),
        tags$div(paste("Games:", row$games)),
        tags$div(paste("Wins:", row$wins)),
        tags$div(paste("Raw Winrate:",    pretty_wr(row$raw_wr))),
        tags$div(paste("Shrink Winrate:", pretty_wr(row$shrink_wr))),
        if (!is.na(row$type_line)) tags$div(paste("Type:", row$type_line))
      ),
      if (!is.na(row$image_url)) tags$img(
        src = row$image_url,
        style = "width:223px; height:310px; border:1px solid #444; border-radius:6px;"
      )
    )
  })

  output$card_table <- renderDT({
    cs <- card_stats() %>% filter(games >= input$min_games_card)
    metric <- match.arg(input$sort_cards_by, c("shrink_wr", "raw_wr", "games"))
    cs <- cs %>% arrange(desc(.data[[metric]]))

    tbl <- cs %>%
      transmute(
        name = card_name, games, wins,
        raw_winrate    = if_else(is.na(raw_wr),    NA_real_, round(100 * raw_wr, 1)),
        shrink_winrate = if_else(is.na(shrink_wr), NA_real_, round(100 * shrink_wr, 1)),
        image_url
      )

    order_idx <- switch(metric, "shrink_wr" = 4L, "raw_wr" = 3L, "games" = 1L)

    datatable(
      tbl, escape = FALSE, rownames = FALSE,
      options = list(
        pageLength = 15,
        order = list(list(order_idx, "desc")),
        columnDefs = list(
          list(targets = 0, render = JS(
            "function(data, type, row, meta) {",
            "  if (type === 'display') {",
            "    return '<div style=\"position:relative; display:inline-block;\">' +",
            "           data +",
            "           '<img src=\"' + row[5] + '\" style=\"display:none; position:absolute; top:1.5em; left:0; z-index:1000; width:200px;\" class=\"hover-img\"/>' +",
            "           '</div>';",
            "  } else { return data; }",
            "}"
          )),
          list(targets = 5, visible = FALSE)
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
