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
library(dplyr)
library(ggplot2)

# Load data
kg <- fromJSON("data/MC1_graph.json")
nodes_tbl <- as_tibble(kg$nodes)
edges_tbl <- as_tibble(kg$links)

nodes_tbl <- nodes_tbl %>% mutate(id = as.character(id))
edges_tbl <- edges_tbl %>% mutate(source = as.character(source), target = as.character(target))

# 简化的CSS - 移除复杂的z-index和不必要的样式
ui <- fluidPage(
  useShinyjs(),
  
  # 大幅简化的CSS
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', sans-serif;
        background-color: #f8f9fa;
        margin: 0;
      }
      
      .header {
        background: linear-gradient(135deg, #6E3B47 0%, #A85D6B 100%);
        color: white;
        padding: 15px 25px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .header h1 {
        margin: 0;
        font-size: 22px;
        font-weight: 600;
      }
      
      .main-container {
        display: flex;
        min-height: calc(100vh - 70px);
      }
      
      .sidebar {
        width: 250px;
        background-color: #6E3B47;
        color: white;
        overflow-y: auto;
      }
      
      .sidebar-item {
        padding: 15px 20px;
        color: #ecf0f1;
        border-bottom: 1px solid #34495e;
        cursor: pointer;
        transition: background-color 0.2s;
      }
      
      .sidebar-item:hover {
        background-color: #34495e;
      }
      
      .sidebar-item.active {
        background-color: #8B4B5C;
        border-left: 4px solid #ffffff;
      }
      
      .content-area {
        flex: 1;
        background-color: #f8f9fa;
      }
      
      .tab-content {
        display: none;
      }
      
      .tab-content.active {
        display: block;
      }
      
      .main-content {
        padding: 25px;
      }
      
      .analysis-panel {
        background: white;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 25px;
      }
      
      .panel-header {
        background: linear-gradient(135deg, #8B4B5C 0%, #A85D6B 100%);
        color: white;
        padding: 15px 20px;
        font-size: 18px;
        font-weight: 600;
        border-radius: 8px 8px 0 0;
      }
      
      .panel-content {
        padding: 20px;
      }
      
      .control-panel {
        background: #f8f9fa;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
        border: 1px solid #e9ecef;
      }
      
      .control-section {
        margin-bottom: 15px;
      }
      
      .control-label {
        font-weight: 600;
        color: #495057;
        margin-bottom: 8px;
        display: block;
      }
      
      .btn-primary {
        background-color: #8B4B5C;
        border-color: #8B4B5C;
        padding: 10px 20px;
        border-radius: 6px;
        transition: background-color 0.2s;
        width: 100%;
      }
      
      .btn-primary:hover {
        background-color: #7a4451;
        border-color: #7a4451;
      }
      
      .two-column-layout {
        display: grid;
        grid-template-columns: 280px 1fr;
        gap: 25px;
      }
      
      .three-column-layout {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
        gap: 20px;
      }
      
      .plot-container {
        background: white;
        border-radius: 8px;
        padding: 15px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      
      /* 移除所有复杂的z-index规则 */
      /* 简化响应式设计 */
      @media (max-width: 768px) {
        .main-container {
          flex-direction: column;
        }
        .sidebar {
          width: 100%;
        }
        .two-column-layout {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  
  # Header
  div(class = "header",
      h1("Music Network Analysis")
  ),
  
  # Main container
  div(class = "main-container",
      # Sidebar
      div(class = "sidebar",
          div(class = "sidebar-item active", `data-tab` = "overview", "Overview"),
          div(class = "sidebar-item", `data-tab` = "eda", "Data Analysis"),
          div(class = "sidebar-item", `data-tab` = "network", " Network"),
          div(class = "sidebar-item", `data-tab` = "influence", "Influence"),
          div(class = "sidebar-item", `data-tab` = "oceanus", "Oceanus Folk"),
          div(class = "sidebar-item", `data-tab` = "emerging", "merging"),
          div(class = "sidebar-item", `data-tab` = "data", "Data"),
          div(class = "sidebar-item", `data-tab` = "guide", "Guide")
      ),
      
      # Content area
      div(class = "content-area",
          # Overview Tab
          div(id = "overview-content", class = "tab-content active",
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Network Overview"),
                      div(class = "panel-content",
                          div(class = "three-column-layout",
                              div(class = "plot-container",
                                  h4("Statistics"),
                                  p("Nodes: ", textOutput("total_nodes", inline = TRUE)),
                                  p("Edges: ", textOutput("total_edges", inline = TRUE)),
                                  p("Density: ", textOutput("network_density", inline = TRUE))
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
          
          # EDA Tab
          div(id = "eda-content", class = "tab-content",
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Edge Distribution"),
                      div(class = "panel-content",
                          div(class = "two-column-layout",
                              div(class = "control-panel",
                                  div(class = "control-section",
                                      tags$label(class = "control-label", "Top N:"),
                                      sliderInput("eda_edge_top_n", "", min = 5, max = 20, value = 10)
                                  ),
                                  div(class = "control-section",
                                      checkboxInput("eda_edge_sort", "Sort by Count", value = TRUE)
                                  ),
                                  actionButton("update_eda_edge", "Update", class = "btn-primary")
                              ),
                              div(class = "plot-container",
                                  plotlyOutput("eda_edge_type", height = "400px")
                              )
                          )
                      )
                  )
              )
          ),
          
          # Network Tab
          div(id = "network-content", class = "tab-content",
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Network Visualization"),
                      div(class = "panel-content",
                          div(class = "two-column-layout",
                              div(class = "control-panel",
                                  div(class = "control-section",
                                      tags$label(class = "control-label", "Layout:"),
                                      selectInput("network_layout", "",
                                                  choices = c("Force" = "fr", "Circle" = "circle", 
                                                              "Tree" = "tree"), selected = "fr")
                                  ),
                                  actionButton("update_network", "Update", class = "btn-primary")
                              ),
                              div(class = "plot-container",
                                  plotOutput("plot_sailor_network", height = "500px")
                              )
                          )
                      )
                  ),
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Interactive Network"),
                      div(class = "panel-content",
                          div(class = "plot-container",
                              visNetworkOutput("network_plot", height = "600px")
                          )
                      )
                  )
              )
          ),
          
          # Influence Tab
          div(id = "influence-content", class = "tab-content",
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Top Influencers"),
                      div(class = "panel-content",
                          div(class = "two-column-layout",
                              div(class = "control-panel",
                                  div(class = "control-section",
                                      tags$label(class = "control-label", "Top N:"),
                                      sliderInput("influencers_top_n", "", min = 5, max = 20, value = 10)
                                  ),
                                  actionButton("update_influencers", "Update", class = "btn-primary")
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
                  )
              )
          ),
          
          # Oceanus Tab
          div(id = "oceanus-content", class = "tab-content",
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Oceanus Folk Network"),
                      div(class = "panel-content",
                          div(class = "plot-container",
                              visNetworkOutput("oceanus_network", height = "600px")
                          )
                      )
                  )
              )
          ),
          
          # Emerging Tab
          div(id = "emerging-content", class = "tab-content",
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Emerging Artists"),
                      div(class = "panel-content",
                          div(style = "text-align: center; padding: 40px;",
                              h3("Coming Soon"),
                              p("Emerging artists analysis will be available soon.")
                          )
                      )
                  )
              )
          ),
          
          # Data Tab
          div(id = "data-content", class = "tab-content",
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Data Source"),
                      div(class = "panel-content",
                          p("Music knowledge graph data from MC1_graph.json"),
                          p("Contains nodes and edges representing musical relationships.")
                      )
                  )
              )
          ),
          
          # Guide Tab
          div(id = "guide-content", class = "tab-content",
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "User Guide"),
                      div(class = "panel-content",
                          p("Navigate using the sidebar menu."),
                          p("Each section provides different analytical perspectives.")
                      )
                  )
              )
          )
      )
  ),
  
  # 大幅简化的JavaScript
  tags$script(HTML("
    $(document).ready(function() {
      $('.sidebar-item').click(function() {
        $('.sidebar-item').removeClass('active');
        $('.tab-content').removeClass('active');
        $(this).addClass('active');
        var tabId = $(this).data('tab') + '-content';
        $('#' + tabId).addClass('active');
        
        // 简单的窗口resize触发
        setTimeout(function() {
          $(window).trigger('resize');
        }, 100);
      });
    });
  "))
)

# 优化的Server逻辑
server <- function(input, output, session) {
  sailor_id <- nodes_tbl %>% filter(name == "Sailor Shift") %>% pull(id)
  
  # 缓存基础数据
  overview_data <- reactive({
    list(
      total_nodes = nrow(nodes_tbl),
      total_edges = nrow(edges_tbl),
      density = round(nrow(edges_tbl) / (nrow(nodes_tbl) * (nrow(nodes_tbl) - 1)), 4)
    )
  })
  
  # Overview outputs - 移除不必要的反应性
  output$total_nodes <- renderText({ overview_data()$total_nodes })
  output$total_edges <- renderText({ overview_data()$total_edges })
  output$network_density <- renderText({ overview_data()$density })
  
  output$overview_node_types <- renderPlotly({
    data <- nodes_tbl %>%
      count(`Node Type`, sort = TRUE) %>%
      head(5)
    
    p <- ggplot(data, aes(x = reorder(`Node Type`, n), y = n)) +
      geom_col(fill = "#8B4B5C") +
      coord_flip() +
      labs(x = "", y = "Count") +
      theme_minimal(base_size = 10)
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 80, r = 20, t = 20, b = 30))
  })
  
  output$overview_edge_types <- renderPlotly({
    data <- edges_tbl %>%
      count(`Edge Type`, sort = TRUE) %>%
      head(5)
    
    p <- ggplot(data, aes(x = reorder(`Edge Type`, n), y = n)) +
      geom_col(fill = "#A85D6B") +
      coord_flip() +
      labs(x = "", y = "Count") +
      theme_minimal(base_size = 10)
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 80, r = 20, t = 20, b = 30))
  })
  
  # EDA - 只在按钮点击时更新
  eda_edge_data <- eventReactive(input$update_eda_edge, {
    edges_tbl %>%
      count(`Edge Type`, sort = input$eda_edge_sort) %>%
      head(input$eda_edge_top_n)
  }, ignoreNULL = FALSE)
  
  output$eda_edge_type <- renderPlotly({
    data <- eda_edge_data()
    
    p <- ggplot(data, aes(x = n, y = reorder(`Edge Type`, n))) +
      geom_col(fill = "#3498db") +
      labs(title = "Edge Type Distribution", x = "Count", y = "") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 120, r = 20, t = 40, b = 40))
  })
  
  # Network - 简化网络图生成
  network_data <- eventReactive(input$update_network, {
    influence_types <- c("InterpolatesFrom", "CoverOf", "InStyleOf",
                         "PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf", "MemberOf")
    
    edges_sailor <- edges_tbl %>%
      filter((source == sailor_id | target == sailor_id) & `Edge Type` %in% influence_types)
    
    node_ids <- unique(c(edges_sailor$source, edges_sailor$target))
    nodes_sub <- nodes_tbl %>% filter(id %in% node_ids)
    
    list(nodes = nodes_sub, edges = edges_sailor)
  }, ignoreNULL = FALSE)
  
  output$plot_sailor_network <- renderPlot({
    data <- network_data()
    
    if (nrow(data$nodes) == 0 || nrow(data$edges) == 0) {
      return(ggplot() + theme_void() + labs(title = "No data available"))
    }
    
    graph <- tbl_graph(
      nodes = data$nodes %>% select(id, name, `Node Type`),
      edges = data$edges %>% select(source, target, `Edge Type`),
      node_key = "id"
    )
    
    ggraph(graph, layout = input$network_layout) +
      geom_edge_link(aes(color = `Edge Type`), alpha = 0.6) +
      geom_node_point(aes(color = `Node Type`), size = 3) +
      geom_node_text(aes(label = name), repel = TRUE, size = 2.5) +
      theme_void() +
      labs(title = "Sailor Shift Network")
  })
  
  # 简化的visNetwork
  output$network_plot <- renderVisNetwork({
    edges_sub <- edges_tbl %>% filter(source == sailor_id | target == sailor_id) %>% head(50) # 限制数量
    node_ids <- unique(c(edges_sub$source, edges_sub$target))
    nodes_sub <- nodes_tbl %>% filter(id %in% node_ids)
    
    nodes <- nodes_sub %>% mutate(label = name, group = `Node Type`)
    edges <- edges_sub %>% mutate(from = source, to = target, arrows = "to")
    
    visNetwork(nodes, edges) %>%
      visOptions(highlightNearest = TRUE) %>%
      visLayout(randomSeed = 42)
  })
  
  # Influencers - 简化计算
  influencers_data <- eventReactive(input$update_influencers, {
    works <- edges_tbl %>%
      filter(source == sailor_id, `Edge Type` %in% c("PerformerOf", "ComposerOf")) %>%
      pull(target) %>% head(20) # 限制数量
    
    infl_edges <- edges_tbl %>%
      filter(source %in% works, `Edge Type` %in% c("InStyleOf", "CoverOf"))
    
    edges_tbl %>%
      filter(target %in% infl_edges$target, `Edge Type` %in% c("PerformerOf", "ComposerOf")) %>%
      count(source, sort = TRUE) %>%
      left_join(nodes_tbl, by = c("source" = "id")) %>%
      filter(!is.na(name)) %>%
      select(name, genre, times = n) %>%
      head(input$influencers_top_n)
  }, ignoreNULL = FALSE)
  
  output$plot_top_influencers <- renderPlotly({
    data <- influencers_data()
    
    if(nrow(data) == 0) {
      p <- ggplot() + theme_void() + labs(title = "Click Update to view")
    } else {
      p <- ggplot(data, aes(x = reorder(name, times), y = times)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(title = "Top Influencers", x = "", y = "Influences") +
        theme_minimal()
    }
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 100, r = 20, t = 40, b = 40))
  })
  
  output$influencers_table <- renderDT({
    influencers_data()
  }, options = list(pageLength = 5, dom = 't'))
  
  # 简化的Oceanus网络
  output$oceanus_network <- renderVisNetwork({
    oceanus_ids <- nodes_tbl %>% 
      filter(str_detect(tolower(genre), "oceanus")) %>% 
      pull(id) %>% head(10) # 限制数量
    
    if(length(oceanus_ids) == 0) {
      nodes <- data.frame(id = 1, label = "No Oceanus Folk Data", group = "Unknown")
      edges <- data.frame(from = integer(0), to = integer(0))
    } else {
      influence_edges <- edges_tbl %>%
        filter(source %in% oceanus_ids, `Edge Type` %in% c("InStyleOf", "CoverOf")) %>%
        head(30) # 限制边数量
      
      node_ids <- unique(c(influence_edges$source, influence_edges$target))
      nodes_sub <- nodes_tbl %>% filter(id %in% node_ids)
      
      nodes <- nodes_sub %>% mutate(label = name, group = genre)
      edges <- influence_edges %>% mutate(from = source, to = target, arrows = "to")
    }
    
    visNetwork(nodes, edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE) %>%
      visLayout(randomSeed = 42)
  })
}

# Run the app
shinyApp(ui = ui, server = server)