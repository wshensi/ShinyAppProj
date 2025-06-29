library(shiny)
library(shinyjs)
library(bslib)
library(tidyverse)
library(tidygraph)
library(igraph)
library(jsonlite)
library(visNetwork)
library(ggraph)
library(tidyr)
library(plotly)
library(DT)

# Load data
kg <- fromJSON("data/MC1_graph.json")
nodes_tbl <- as_tibble(kg$nodes)
edges_tbl <- as_tibble(kg$links)

nodes_tbl <- nodes_tbl %>% mutate(id = as.character(id))
edges_tbl <- edges_tbl %>% mutate(source = as.character(source), target = as.character(target))

# Define custom theme with sophisticated styling
ui <- fluidPage(
  useShinyjs(),
  
  # Custom CSS styling to match the target design
  tags$head(
    tags$style(HTML("
      /* Global styles */
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #f8f9fa;
        margin: 0;
        padding: 0;
      }
      
      /* Header styling */
      .header {
        background: linear-gradient(135deg, #6E3B47 0%, #A85D6B 100%);
        color: white;
        padding: 15px 25px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.15);
        position: relative;
        z-index: 1000;
      }
      
      .header h1 {
        margin: 0;
        font-size: 22px;
        font-weight: 600;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      .header .fa-bars {
        font-size: 18px;
        margin-right: 15px;
        cursor: pointer;
      }
      
      /* Main container */
      .main-container {
        display: flex;
        min-height: calc(100vh - 70px);
      }
      
      /* Sidebar styling */
      .sidebar {
        width: 260px;
        background-color: #6E3B47;
        color: white;
        padding: 0;
        box-shadow: 2px 0 10px rgba(0,0,0,0.1);
        overflow-y: auto;
      }
      
      .sidebar-item {
        display: block;
        padding: 15px 20px;
        color: #ecf0f1;
        text-decoration: none;
        border-bottom: 1px solid #34495e;
        transition: all 0.3s ease;
        cursor: pointer;
        display: flex;
        align-items: center;
        gap: 12px;
      }
      
      .sidebar-item:hover {
        background-color: #34495e;
        color: white;
        text-decoration: none;
      }
      
      .sidebar-item.active {
        background-color: #8B4B5C;
        border-left: 4px solid #ffffff;
      }
      
      .sidebar-item i {
        width: 20px;
        text-align: center;
        font-size: 16px;
      }
      
      /* Content area */
      .content-area {
        flex: 1;
        padding: 0;
        background-color: #f8f9fa;
      }
      
      /* Tab navigation */
      .tab-navigation {
        background-color: white;
        padding: 0;
        border-bottom: 1px solid #dee2e6;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      
      .tab-nav-item {
        display: inline-block;
        padding: 15px 25px;
        color: #6c757d;
        text-decoration: none;
        border-bottom: 3px solid transparent;
        transition: all 0.3s ease;
        cursor: pointer;
      }
      
      .tab-nav-item:hover {
        color: #8B4B5C;
        text-decoration: none;
      }
      
      .tab-nav-item.active {
        color: #8B4B5C;
        border-bottom-color: #8B4B5C;
        font-weight: 600;
      }
      
      /* Main content panel */
      .main-content {
        padding: 25px;
      }
      
      /* Panel styling */
      .analysis-panel {
        background: white;
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        margin-bottom: 25px;
        overflow: hidden;
      }
      
      .panel-header {
        background: linear-gradient(135deg, #8B4B5C 0%, #A85D6B 100%);
        color: white;
        padding: 15px 20px;
        font-size: 18px;
        font-weight: 600;
      }
      
      .panel-content {
        padding: 20px;
      }
      
      /* Control panels */
      .control-panel {
        background: #f8f9fa;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
        border: 1px solid #e9ecef;
      }
      
      .control-section {
        margin-bottom: 20px;
      }
      
      .control-section:last-child {
        margin-bottom: 0;
      }
      
      .control-label {
        font-weight: 600;
        color: #495057;
        margin-bottom: 8px;
        display: block;
      }
      
      /* Input styling */
      .form-control, .form-select {
        border: 1px solid #ced4da;
        border-radius: 6px;
        padding: 8px 12px;
        font-size: 14px;
        transition: border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out;
      }
      
      .form-control:focus, .form-select:focus {
        border-color: #8B4B5C;
        box-shadow: 0 0 0 0.2rem rgba(139, 75, 92, 0.25);
      }
      
      /* Button styling */
      .btn-primary {
        background-color: #8B4B5C;
        border-color: #8B4B5C;
        padding: 10px 20px;
        font-weight: 500;
        border-radius: 6px;
        transition: all 0.3s ease;
      }
      
      .btn-primary:hover {
        background-color: #7a4451;
        border-color: #7a4451;
        transform: translateY(-1px);
      }
      
      /* Slider styling */
      .irs--shiny .irs-bar {
        background: #8B4B5C;
      }
      
      .irs--shiny .irs-handle {
        background: #8B4B5C;
      }
      
      /* Checkbox and radio styling */
      .form-check-input:checked {
        background-color: #8B4B5C;
        border-color: #8B4B5C;
      }
      
      /* Layout grids */
      .two-column-layout {
        display: grid;
        grid-template-columns: 1fr 2fr;
        gap: 25px;
        align-items: start;
      }
      
      .three-column-layout {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        gap: 20px;
      }
      
      /* Plot containers */
      .plot-container {
        background: white;
        border-radius: 8px;
        padding: 15px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
      }
      
      /* Table styling */
      .dataTables_wrapper {
        font-size: 14px;
      }
      
      .dataTables_wrapper .dataTables_info {
        color: #6c757d;
      }
      
      /* Responsive design */
      @media (max-width: 768px) {
        .sidebar {
          width: 100%;
          height: auto;
        }
        
        .main-container {
          flex-direction: column;
        }
        
        .two-column-layout {
          grid-template-columns: 1fr;
        }
        
        .three-column-layout {
          grid-template-columns: 1fr;
        }
      }
      
      /* Custom scrollbar */
      ::-webkit-scrollbar {
        width: 8px;
      }
      
      ::-webkit-scrollbar-track {
        background: #f1f1f1;
      }
      
      ::-webkit-scrollbar-thumb {
        background: #8B4B5C;
        border-radius: 4px;
      }
      
      ::-webkit-scrollbar-thumb:hover {
        background: #7a4451;
      }
    "))
  ),
  
  # Header
  div(class = "header",
      h1(
        tags$i(class = "fas fa-bars"),
        tags$i(class = "fas fa-music"),
        "Music Network Analysis"
      )
  ),
  
  # Main container
  div(class = "main-container",
      # Sidebar
      div(class = "sidebar",
          div(class = "sidebar-item active", `data-tab` = "overview",
              tags$i(class = "fas fa-chart-pie"),
              "Overview Analysis"
          ),
          div(class = "sidebar-item", `data-tab` = "eda",
              tags$i(class = "fas fa-chart-bar"),
              "Exploratory Data Analysis"
          ),
          div(class = "sidebar-item", `data-tab` = "network",
              tags$i(class = "fas fa-project-diagram"),
              "Network Analysis"
          ),
          div(class = "sidebar-item", `data-tab` = "influence",
              tags$i(class = "fas fa-users"),
              "Influence & Collaboration"
          ),
          div(class = "sidebar-item", `data-tab` = "oceanus",
              tags$i(class = "fas fa-water"),
              "Oceanus Folk Analysis"
          ),
          div(class = "sidebar-item", `data-tab` = "emerging",
              tags$i(class = "fas fa-star"),
              "Emerging Artists"
          ),
          div(class = "sidebar-item", `data-tab` = "data",
              tags$i(class = "fas fa-table"),
              "Data Source"
          ),
          div(class = "sidebar-item", `data-tab` = "guide",
              tags$i(class = "fas fa-question-circle"),
              "User Guide"
          )
      ),
      
      # Content area
      div(class = "content-area",
          # Tab content for Overview
          div(id = "overview-content", class = "tab-content",
              div(class = "tab-navigation",
                  span(class = "tab-nav-item active", "Overview"),
                  span(class = "tab-nav-item", "Summary Statistics")
              ),
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Music Network Overview"),
                      div(class = "panel-content",
                          div(class = "three-column-layout",
                              div(class = "plot-container",
                                  h4("Network Statistics"),
                                  p("Total Nodes: ", textOutput("total_nodes", inline = TRUE)),
                                  p("Total Edges: ", textOutput("total_edges", inline = TRUE)),
                                  p("Network Density: ", textOutput("network_density", inline = TRUE))
                              ),
                              div(class = "plot-container",
                                  h4("Node Types"),
                                  plotlyOutput("overview_node_types", height = "200px")
                              ),
                              div(class = "plot-container",
                                  h4("Edge Types"),
                                  plotlyOutput("overview_edge_types", height = "200px")
                              )
                          )
                      )
                  )
              )
          ),
          
          # Tab content for EDA
          div(id = "eda-content", class = "tab-content", style = "display: none;",
              div(class = "tab-navigation",
                  span(class = "tab-nav-item active", "Distribution Analysis"),
                  span(class = "tab-nav-item", "Network Properties")
              ),
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Edge Type Distribution"),
                      div(class = "panel-content",
                          div(class = "two-column-layout",
                              div(class = "control-panel",
                                  div(class = "control-section",
                                      tags$label(class = "control-label", "Top N Edge Types:"),
                                      sliderInput("eda_edge_top_n", "", min = 5, max = 20, value = 10, step = 1)
                                  ),
                                  div(class = "control-section",
                                      checkboxInput("eda_edge_sort", "Sort by Count", value = TRUE)
                                  )
                              ),
                              div(class = "plot-container",
                                  plotlyOutput("eda_edge_type", height = "400px")
                              )
                          )
                      )
                  ),
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Node Type Distribution"),
                      div(class = "panel-content",
                          div(class = "plot-container",
                              plotlyOutput("eda_node_type", height = "400px")
                          )
                      )
                  )
              )
          ),
          
          # Tab content for Network Analysis
          div(id = "network-content", class = "tab-content", style = "display: none;",
              div(class = "tab-navigation",
                  span(class = "tab-nav-item active", "Network Overview"),
                  span(class = "tab-nav-item", "Interactive Network")
              ),
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Network Visualization"),
                      div(class = "panel-content",
                          div(class = "two-column-layout",
                              div(class = "control-panel",
                                  div(class = "control-section",
                                      tags$label(class = "control-label", "Network Layout:"),
                                      selectInput("network_layout", "",
                                                  choices = c("Fruchterman-Reingold" = "fr",
                                                              "Circle" = "circle",
                                                              "Kamada-Kawai" = "kk",
                                                              "Tree" = "tree"), 
                                                  selected = "fr")
                                  )
                              ),
                              div(class = "plot-container",
                                  plotOutput("plot_sailor_network", height = "500px")
                              )
                          )
                      )
                  ),
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Interactive Network Explorer"),
                      div(class = "panel-content",
                          div(class = "plot-container",
                              visNetworkOutput("network_plot", height = "600px")
                          )
                      )
                  )
              )
          ),
          
          # Tab content for Influence & Collaboration
          div(id = "influence-content", class = "tab-content", style = "display: none;",
              div(class = "tab-navigation",
                  span(class = "tab-nav-item active", "Top Influencers"),
                  span(class = "tab-nav-item", "Top Collaborators")
              ),
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Top Influencers Analysis"),
                      div(class = "panel-content",
                          div(class = "two-column-layout",
                              div(class = "control-panel",
                                  div(class = "control-section",
                                      tags$label(class = "control-label", "Top N Influencers:"),
                                      sliderInput("influencers_top_n", "", min = 5, max = 20, value = 10)
                                  )
                              ),
                              div(
                                div(class = "plot-container",
                                    plotlyOutput("plot_top_influencers", height = "300px")
                                ),
                                div(class = "plot-container", style = "margin-top: 20px;",
                                    DTOutput("influencers_table")
                                )
                              )
                          )
                      )
                  ),
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Top Collaborators Analysis"),
                      div(class = "panel-content",
                          div(class = "two-column-layout",
                              div(class = "control-panel",
                                  div(class = "control-section",
                                      tags$label(class = "control-label", "Top N Collaborators:"),
                                      sliderInput("collaborators_top_n", "", min = 5, max = 20, value = 10)
                                  )
                              ),
                              div(
                                div(class = "plot-container",
                                    plotlyOutput("plot_top_collaborators", height = "300px")
                                ),
                                div(class = "plot-container", style = "margin-top: 20px;",
                                    DTOutput("collaborators_table")
                                )
                              )
                          )
                      )
                  )
              )
          ),
          
          # Tab content for Oceanus Folk Analysis
          div(id = "oceanus-content", class = "tab-content", style = "display: none;",
              div(class = "tab-navigation",
                  span(class = "tab-nav-item active", "Influence Network"),
                  span(class = "tab-nav-item", "Timeline Analysis"),
                  span(class = "tab-nav-item", "Genre Evolution")
              ),
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Oceanus Folk Influence Network"),
                      div(class = "panel-content",
                          div(class = "plot-container",
                              visNetworkOutput("oceanus_network", height = "600px")
                          )
                      )
                  ),
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Timeline Trends"),
                      div(class = "panel-content",
                          div(class = "two-column-layout",
                              div(class = "control-panel",
                                  div(class = "control-section",
                                      tags$label(class = "control-label", "Year Range:"),
                                      sliderInput("oceanus_time_range", "", min = 1900, max = 2025, value = c(1950, 2025))
                                  )
                              ),
                              div(class = "plot-container",
                                  plotlyOutput("plot_oceanus_timeline", height = "400px")
                              )
                          )
                      )
                  )
              )
          ),
          
          # Tab content for Emerging Artists
          div(id = "emerging-content", class = "tab-content", style = "display: none;",
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Emerging Artists Discovery"),
                      div(class = "panel-content",
                          div(style = "text-align: center; padding: 40px;",
                              tags$i(class = "fas fa-star", style = "font-size: 48px; color: #8B4B5C; margin-bottom: 20px;"),
                              h3("Coming Soon: Emerging Artists Analysis"),
                              p("This section will explore potential breakout artists based on influence propagation and collaborative patterns.", 
                                style = "color: #6c757d; font-size: 16px; max-width: 600px; margin: 0 auto;")
                          )
                      )
                  )
              )
          ),
          
          # Additional tab contents for Data Source and User Guide
          div(id = "data-content", class = "tab-content", style = "display: none;",
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Data Source Information"),
                      div(class = "panel-content",
                          p("Music knowledge graph data loaded from MC1_graph.json"),
                          p("Data contains nodes and edges representing musical entities and their relationships.")
                      )
                  )
              )
          ),
          
          div(id = "guide-content", class = "tab-content", style = "display: none;",
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "User Guide"),
                      div(class = "panel-content",
                          p("Navigate through different sections using the sidebar menu."),
                          p("Each section provides different analytical perspectives on the music network data.")
                      )
                  )
              )
          )
      )
  ),
  
  # JavaScript for tab switching
  tags$script(HTML("
    $(document).ready(function() {
      $('.sidebar-item').click(function() {
        // Remove active class from all sidebar items
        $('.sidebar-item').removeClass('active');
        // Add active class to clicked item
        $(this).addClass('active');
        
        // Hide all tab contents
        $('.tab-content').hide();
        
        // Show selected tab content
        var tabId = $(this).data('tab') + '-content';
        $('#' + tabId).show();
      });
    });
  "))
)



## Server
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
