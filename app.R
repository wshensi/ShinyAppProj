library(shiny)
library(shinyjs)
library(tidyverse)
library(tidygraph)
library(igraph)
library(jsonlite)
library(visNetwork)
library(ggraph)
library(tidyr)
library(plotly)
library(DT)

# wanganqi Load data
kg <- fromJSON("data/MC1_graph.json")
nodes_tbl <- as_tibble(kg$nodes)
edges_tbl <- as_tibble(kg$links)

nodes_tbl <- nodes_tbl %>% mutate(id = as.character(id))
edges_tbl <- edges_tbl %>% mutate(source = as.character(source), target = as.character(target))

# UI
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  titlePanel("🎵 Interactive Music Network - Relationship & Analysis"),
  tabsetPanel(
    tabPanel("EDA",
             tabsetPanel(
               tabPanel("Edge Type Distribution",
                        fluidRow(
                          column(3,
                                 sliderInput("eda_edge_top_n", "Top N Edge Types:",
                                             min = 5, max = 20, value = 10, step = 1),
                                 checkboxInput("eda_edge_sort", "Sort by Count", value = TRUE)
                          ),
                          column(9, plotlyOutput("eda_edge_type"))
                        )),
               tabPanel("Node Type Distribution",
                        fluidRow(
                          column(3,
                                 div(style = "display:none;",
                                     sliderInput("eda_node_top_n", "Top N Node Types:",
                                                 min = 5, max = 15, value = 10, step = 1)
                                 )
                          ),
                          column(9, plotlyOutput("eda_node_type"))
                        ))
             )),
    
    tabPanel("Task1",
             tabsetPanel(
               tabPanel("Sailor Shift's Influence & Collaboration Network", 
                        fluidRow(
                          column(3,
                                 selectInput("network_layout", "Layout:",
                                             choices = c("Fruchterman-Reingold" = "fr",
                                                         "Circle" = "circle",
                                                         "Kamada-Kawai" = "kk",
                                                         "Tree" = "tree"), selected = "fr")
                          ),
                          column(9, plotOutput("plot_sailor_network", height = "700px"))
                        )),
               tabPanel("Interactive Network", visNetworkOutput("network_plot", height = "700px")),
               tabPanel("Top Influencers", 
                        fluidRow(
                          column(3,
                                 sliderInput("influencers_top_n", "Top N Influencers:",
                                             min = 5, max = 20, value = 10, step = 1)
                          ),
                          column(9, 
                                 plotlyOutput("plot_top_influencers"),
                                 DTOutput("influencers_table"))
                        )),
               tabPanel("Top Collaborators", 
                        fluidRow(
                          column(3,
                                 sliderInput("collaborators_top_n", "Top N Collaborators:",
                                             min = 5, max = 20, value = 10, step = 1)
                          ),
                          column(9, 
                                 plotlyOutput("plot_top_collaborators"),
                                 DTOutput("collaborators_table"))
                        ))
             )),
    
    tabPanel("Task2",
             tabsetPanel(
               tabPanel("Oceanus Folk Influence",
                        visNetworkOutput("oceanus_network", height = "700px")),
               tabPanel("Influence Over Time",
                        fluidRow(
                          column(3,
                                 sliderInput("oceanus_time_range", "Year Range:",
                                             min = 1900, max = 2025, value = c(1950, 2025), step = 1)
                          ),
                          column(9, plotlyOutput("plot_oceanus_timeline"))
                        )),
               tabPanel("Genres & Artists Influenced",
                        plotlyOutput("plot_top_genres"),
                        plotlyOutput("plot_top_artists")),
               tabPanel("Evolution of Oceanus Folk",
                        fluidRow(
                          column(3,
                                 sliderInput("evolution_year_range", "Year Range:",
                                             min = 1900, max = 2025, value = c(1950, 2025), step = 1)
                          ),
                          column(9, plotlyOutput("plot_genre_evolution"))
                        ))
             )),
    
    tabPanel("Task3",
             tabsetPanel(
               tabPanel("New Artist Discovery",
                        h3("This section is reserved for future analysis of emerging artists. (UI only, no logic implemented yet)"))
             ))
  )
)


# Server
server <- function(input, output, session) {
  sailor_id <- nodes_tbl %>% filter(name == "Sailor Shift") %>% pull(id)
  
  # EDA
  output$eda_edge_type <- renderPlotly({
    data <- edges_tbl %>%
      count(`Edge Type`, sort = input$eda_edge_sort) %>%
      head(input$eda_edge_top_n)
    
    ggplotly(
      ggplot(data, aes(x = n, y = reorder(`Edge Type`, n))) +
        geom_col(fill = "#3498db") +
        labs(title = "Edge Type Distribution", x = "Count", y = "Edge Type") +
        theme_minimal()
    )
  })
  
  output$eda_node_type <- renderPlotly({
    data <- nodes_tbl %>%
      count(`Node Type`, sort = TRUE) %>%
      head(input$eda_node_top_n)
    
    ggplotly(
      ggplot(data, aes(x = n, y = reorder(`Node Type`, n))) +
        geom_col(fill = "#2ecc71") +
        labs(title = "Node Type Distribution", x = "Count", y = "Node Type") +
        theme_minimal()
    )
  })
  
  # Static Sailor Shift Network
  output$plot_sailor_network <- renderPlot({
    influence_types <- c("InterpolatesFrom", "CoverOf", "InStyleOf",
                         "PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf", "MemberOf")
    
    edges_sailor <- edges_tbl %>%
      filter((source == sailor_id | target == sailor_id) & `Edge Type` %in% influence_types)
    
    group_ids <- edges_tbl %>% filter(`Edge Type` == "MemberOf", source == sailor_id) %>% pull(target)
    song_ids <- edges_tbl %>% filter(`Edge Type` == "PerformerOf", source == sailor_id) %>% pull(target)
    
    extra_edges <- edges_tbl %>%
      filter(target %in% c(group_ids, song_ids)) %>%
      filter(`Edge Type` %in% influence_types)
    
    all_edges <- bind_rows(edges_sailor, extra_edges) %>% distinct()
    node_ids <- unique(c(all_edges$source, all_edges$target))
    nodes_sub <- nodes_tbl %>% filter(id %in% node_ids)
    
    if (nrow(nodes_sub) == 0 || nrow(all_edges) == 0) {
      return(ggplot() + theme_void() + labs(title = "No data to display."))
    }
    
    graph <- tbl_graph(
      nodes = nodes_sub %>% select(id, name, `Node Type`),
      edges = all_edges %>% select(source, target, `Edge Type`),
      node_key = "id"
    )
    
    ggraph(graph, layout = input$network_layout) +
      geom_edge_link(aes(color = `Edge Type`), alpha = 0.5) +
      geom_node_point(aes(color = `Node Type`), size = 3) +
      geom_node_text(aes(label = name), repel = TRUE, size = 3) +
      theme_void() +
      labs(title = "Sailor Shift's Influence & Collaboration Network")
  })
  
  # visNetwork
  output$network_plot <- renderVisNetwork({
    edges_sub <- edges_tbl %>% filter(source == sailor_id | target == sailor_id)
    node_ids <- unique(c(edges_sub$source, edges_sub$target))
    nodes_sub <- nodes_tbl %>% filter(id %in% node_ids)
    
    nodes <- nodes_sub %>% mutate(label = name, group = `Node Type`)
    edges <- edges_sub %>% mutate(from = source, to = target, arrows = "to", title = `Edge Type`)
    
    visNetwork(nodes, edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visInteraction(navigationButtons = TRUE)
  })
  
  # Top Influencers
  influencers_data <- reactive({
    works <- edges_tbl %>%
      filter(source == sailor_id, `Edge Type` %in% c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf")) %>%
      pull(target)
    
    infl_edges <- edges_tbl %>%
      filter(source %in% works, `Edge Type` %in% c("InStyleOf", "InterpolatesFrom", "CoverOf", "DirectlySamples"))
    
    infl_sources <- edges_tbl %>%
      filter(target %in% infl_edges$target,
             `Edge Type` %in% c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf")) %>%
      count(source, sort = TRUE) %>%
      left_join(nodes_tbl, by = c("source" = "id")) %>%
      filter(!is.na(name)) %>%
      select(name, genre, times = n) %>%
      head(input$influencers_top_n)
    
    infl_sources
  })
  
  output$plot_top_influencers <- renderPlotly({
    data <- influencers_data()
    ggplotly(
      ggplot(data, aes(x = reorder(name, times), y = times)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(title = "Top Influencers", x = "Artist", y = "Influences") +
        theme_minimal()
    )
  })
  
  output$influencers_table <- renderDT({
    influencers_data()
  })
  
  # Top Collaborators
  collaborators_data <- reactive({
    works <- edges_tbl %>% filter(source == sailor_id) %>% pull(target)
    
    co_edges <- edges_tbl %>%
      filter(target %in% works, source != sailor_id) %>%
      count(source, sort = TRUE) %>%
      left_join(nodes_tbl, by = c("source" = "id")) %>%
      filter(!is.na(name)) %>%
      select(name, genre, times = n) %>%
      head(input$collaborators_top_n)
    
    co_edges
  })
  
  output$plot_top_collaborators <- renderPlotly({
    data <- collaborators_data()
    ggplotly(
      ggplot(data, aes(x = reorder(name, times), y = times)) +
        geom_col(fill = "darkorange") +
        coord_flip() +
        labs(title = "Top Collaborators", x = "Artist", y = "Collaborations") +
        theme_minimal()
    )
  })
  
  output$collaborators_table <- renderDT({
    collaborators_data()
  })
  
  # Oceanus Influence Network
  output$oceanus_network <- renderVisNetwork({
    oceanus_ids <- nodes_tbl %>% filter(str_detect(tolower(genre), "oceanus folk")) %>% pull(id)
    influence_edges <- edges_tbl %>%
      filter(source %in% oceanus_ids) %>%
      filter(`Edge Type` %in% c("InStyleOf", "LyricalReferenceTo", "CoverOf", "InterpolatesFrom", "DirectlySamples"))
    
    sub_nodes <- nodes_tbl %>%
      filter(id %in% unique(c(influence_edges$source, influence_edges$target))) %>%
      mutate(index = row_number())
    
    id_map <- sub_nodes %>% select(id, index)
    edges_sub <- influence_edges %>%
      left_join(id_map, by = c("source" = "id")) %>%
      rename(from = index) %>%
      left_join(id_map, by = c("target" = "id")) %>%
      rename(to = index) %>%
      select(from, to, `Edge Type`)
    
    visNetwork(
      sub_nodes %>% mutate(id = index, label = name, group = genre),
      edges_sub %>% rename(title = `Edge Type`)
    ) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE) %>%
      visLegend() %>%
      visLayout(randomSeed = 42)
  })
  
  output$plot_oceanus_timeline <- renderPlotly({
    oceanus_ids <- nodes_tbl %>% 
      filter(str_detect(tolower(genre), "oceanus folk")) %>% 
      pull(id)
    
    influenced_targets <- edges_tbl %>% 
      filter(source %in% oceanus_ids,
             `Edge Type` %in% c("InStyleOf", "LyricalReferenceTo", "CoverOf", "InterpolatesFrom", "DirectlySamples")) %>%
      pull(target) %>% 
      unique()
    
    timeline_data <- nodes_tbl %>%
      filter(id %in% influenced_targets) %>%
      filter(!is.na(release_date) | !is.na(written_date)) %>%
      mutate(
        date_str = coalesce(as.character(release_date), as.character(written_date)),
        year = as.integer(substr(date_str, 1, 4))
      ) %>%
      filter(!is.na(year),
             year >= input$oceanus_time_range[1],
             year <= input$oceanus_time_range[2]) %>%
      count(year)
    
    ggplotly(
      ggplot(timeline_data, aes(x = year, y = n)) +
        geom_line(color = "steelblue") +
        geom_point(color = "tomato") +
        labs(title = "Oceanus Folk Influence Over Time", x = "Year", y = "Influenced Works") +
        theme_minimal()
    )
  })
  
  
  output$plot_top_genres <- renderPlotly({
    oceanus_ids <- nodes_tbl %>% filter(str_detect(tolower(genre), "oceanus folk")) %>% pull(id)
    influenced <- edges_tbl %>% filter(source %in% oceanus_ids) %>% pull(target)
    
    genres <- nodes_tbl %>%
      filter(id %in% influenced, !is.na(genre)) %>%
      count(genre, sort = TRUE) %>%
      head(10)
    
    ggplotly(
      ggplot(genres, aes(x = reorder(genre, n), y = n)) +
        geom_col(fill = "darkgreen") +
        coord_flip() +
        labs(title = "Top Genres Influenced by Oceanus Folk", x = "Genre", y = "Count") +
        theme_minimal()
    )
  })
  
  output$plot_top_artists <- renderPlotly({
    oceanus_ids <- nodes_tbl %>% filter(str_detect(tolower(genre), "oceanus folk")) %>% pull(id)
    influenced <- edges_tbl %>% filter(source %in% oceanus_ids) %>% pull(target)
    
    artists <- nodes_tbl %>%
      filter(id %in% influenced, `Node Type` == "Person") %>%
      count(name, sort = TRUE) %>%
      head(10)
    
    ggplotly(
      ggplot(artists, aes(x = reorder(name, n), y = n)) +
        geom_col(fill = "purple") +
        coord_flip() +
        labs(title = "Top Artists Influenced by Oceanus Folk", x = "Artist", y = "Count") +
        theme_minimal()
    )
  })
  
  output$plot_genre_evolution <- renderPlotly({
    data <- nodes_tbl %>%
      filter(str_detect(tolower(genre), "oceanus folk"),
             !is.na(release_date) | !is.na(written_date)) %>%
      mutate(year = as.integer(substr(coalesce(release_date, written_date), 1, 4))) %>%
      filter(!is.na(year),
             year >= input$evolution_year_range[1],
             year <= input$evolution_year_range[2]) %>%
      separate_rows(genre, sep = ",\\s*") %>%
      count(year, genre)
    
    ggplotly(
      ggplot(data, aes(x = year, y = n, fill = genre)) +
        geom_area(alpha = 0.7) +
        labs(title = "Evolution of Oceanus Folk and Related Genres", x = "Year", y = "Count") +
        theme_minimal()
    )
  })
}

# Run the app
shinyApp(ui, server)
