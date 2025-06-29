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
        z-index: 5; /* 降低z-index */
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
        min-width: 260px; /* 确保最小宽度 */
        max-width: 260px; /* 确保最大宽度 */
        flex-shrink: 0; /* 防止缩小 */
        background-color: #6E3B47;
        color: white;
        padding: 0;
        box-shadow: 2px 0 10px rgba(0,0,0,0.1);
        overflow-y: auto;
        position: relative;
        z-index: 5; /* 添加z-index */
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
        min-width: 0; /* 允许收缩 */
        padding: 0;
        background-color: #f8f9fa;
        position: relative;
        z-index: 1; /* 添加z-index */
        overflow-x: auto; /* 允许水平滚动 */
      }
      
      /* Tab content styling - 关键修复 */
      .tab-content {
        min-height: 100vh;
        visibility: visible !important;
        opacity: 1 !important;
      }
      
      .tab-content[style*='display: none'] {
        display: block !important;
        position: absolute !important;
        left: -9999px !important;
        top: -9999px !important;
        visibility: hidden !important;
        opacity: 0 !important;
        z-index: -1 !important;
      }
      
      .tab-content.active {
        position: static !important;
        left: auto !important;
        top: auto !important;
        visibility: visible !important;
        opacity: 1 !important;
        z-index: 1 !important;
      }
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
        position: relative;
        z-index: 2; /* 添加z-index */
      }
      
      /* Panel styling */
      .analysis-panel {
        background: white;
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        margin-bottom: 25px;
        overflow: visible; /* 改为visible */
        position: relative;
        z-index: 3; /* 添加z-index */
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
        position: relative;
        z-index: 4; /* 添加z-index */
        overflow: visible; /* 改为visible */
      }
      
      /* Control panels */
      .control-panel {
        background: #f8f9fa;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
        border: 1px solid #e9ecef;
        position: relative;
        z-index: 5; /* 添加z-index */
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
        position: relative;
        z-index: 10; /* 添加z-index */
      }
      
      .form-control:focus, .form-select:focus {
        border-color: #8B4B5C;
        box-shadow: 0 0 0 0.2rem rgba(139, 75, 92, 0.25);
        z-index: 15; /* 聚焦时提高z-index */
      }
      
      /* Button styling */
      .btn-primary {
        background-color: #8B4B5C;
        border-color: #8B4B5C;
        padding: 10px 20px;
        font-weight: 500;
        border-radius: 6px;
        transition: all 0.3s ease;
        position: relative;
        z-index: 10; /* 添加z-index */
        margin-top: 10px;
        width: 100%;
      }
      
      .btn-primary:hover {
        background-color: #7a4451;
        border-color: #7a4451;
        transform: translateY(-1px);
        z-index: 15; /* 悬停时提高z-index */
      }
      
      .btn-update {
        background-color: #28a745;
        border-color: #28a745;
        color: white;
        font-weight: 600;
      }
      
      .btn-update:hover {
        background-color: #218838;
        border-color: #1e7e34;
      }
      
      /* Slider styling - 修复圆球被遮挡的问题 */
      .irs {
        position: relative;
        z-index: 100 !important; /* 大幅提高z-index */
      }
      
      .irs--shiny {
        z-index: 100 !important;
      }
      
      .irs--shiny .irs-bar {
        background: #8B4B5C;
        z-index: 101 !important;
      }
      
      .irs--shiny .irs-handle {
        background: #8B4B5C;
        z-index: 102 !important;
        position: relative;
      }
      
      .irs--shiny .irs-handle > i:first-child {
        background: #8B4B5C;
        z-index: 103 !important;
      }
      
      .irs--shiny .irs-handle:hover {
        z-index: 104 !important;
      }
      
      .irs-slider {
        z-index: 105 !important;
      }
      
      /* Checkbox and radio styling */
      .form-check-input:checked {
        background-color: #8B4B5C;
        border-color: #8B4B5C;
      }
      
      /* Layout grids */
      .two-column-layout {
        display: grid;
        grid-template-columns: minmax(250px, 300px) 1fr; /* 左侧控制面板最小250px，最大300px */
        gap: 25px;
        align-items: start;
      }
      
      .three-column-layout {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); /* 响应式列 */
        gap: 20px;
      }
      
      /* Plot containers - 关键修复 */
      .plot-container {
        background: white;
        border-radius: 8px;
        padding: 15px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
        position: relative;
        z-index: 10; /* 提高z-index */
        overflow: visible; /* 改为visible */
        min-width: 0; /* 允许收缩 */
        width: 100%; /* 确保占满容器 */
      }
      
      /* 图表容器内的图表元素 */
      .plot-container .plotly,
      .plot-container .vis-network,
      .plot-container .shiny-plot-output {
        min-width: 300px; /* 设置最小宽度 */
        width: 100%;
        height: auto;
      }
      
      /* Shiny输出元素的样式 - 关键修复 */
      .shiny-plot-output,
      .shiny-html-output {
        position: relative !important;
        z-index: 15 !important;
        overflow: visible !important;
      }
      
      /* Plotly特定样式 - 关键修复 */
      .plotly,
      .plotly-plot {
        position: relative !important;
        z-index: 20 !important;
        overflow: visible !important;
      }
      
      .plotly .modebar {
        z-index: 25 !important;
      }
      
      .plotly .plotly-notifier {
        z-index: 25 !important;
      }
      
      /* visNetwork特定样式 - 关键修复 */
      .vis-network {
        position: relative !important;
        z-index: 20 !important;
        overflow: visible !important;
      }
      
      #network_plot,
      #oceanus_network {
        position: relative !important;
        z-index: 20 !important;
        height: 600px !important;
      }
      
      /* DataTable样式 */
      .dataTables_wrapper {
        font-size: 14px;
        position: relative;
        z-index: 10;
      }
      
      .dataTables_wrapper .dataTables_info {
        color: #6c757d;
        z-index: 11;
      }
      
      .dataTables_scrollBody {
        overflow: visible !important;
        z-index: 12;
      }
      
      /* 下拉菜单和选择器样式 */
      .selectize-dropdown,
      .selectize-control,
      .bootstrap-select .dropdown-menu {
        z-index: 30 !important;
      }
      
      .selectize-control .selectize-input {
        z-index: 15 !important;
      }
      
      /* 表格容器样式 */
      .dt-container {
        position: relative;
        z-index: 10;
        overflow: visible;
      }
      
      /* Responsive design */
      @media (max-width: 768px) {
        .sidebar {
          width: 100%;
          min-width: 100%;
          max-width: 100%;
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
        
        .plot-container .plotly,
        .plot-container .vis-network,
        .plot-container .shiny-plot-output {
          min-width: 250px;
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
          div(id = "overview-content", class = "tab-content active",
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
          div(id = "eda-content", class = "tab-content",
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
                                  ),
                                  div(class = "control-section",
                                      actionButton("update_eda_edge", "Update Chart", class = "btn-primary btn-update")
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
                          div(class = "two-column-layout",
                              div(class = "control-panel",
                                  div(class = "control-section",
                                      actionButton("update_eda_node", "Update Chart", class = "btn-primary btn-update")
                                  )
                              ),
                              div(class = "plot-container",
                                  plotlyOutput("eda_node_type", height = "400px")
                              )
                          )
                      )
                  )
              )
          ),
          
          # Tab content for Network Analysis
          div(id = "network-content", class = "tab-content",
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
                                  ),
                                  div(class = "control-section",
                                      actionButton("update_network", "Update Network", class = "btn-primary btn-update")
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
                          div(class = "two-column-layout",
                              div(class = "control-panel",
                                  div(class = "control-section",
                                      actionButton("update_interactive_network", "Update Interactive Network", class = "btn-primary btn-update")
                                  )
                              ),
                              div(class = "plot-container",
                                  visNetworkOutput("network_plot", height = "600px")
                              )
                          )
                      )
                  )
              )
          ),
          
          # Tab content for Influence & Collaboration
          div(id = "influence-content", class = "tab-content",
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
                                  ),
                                  div(class = "control-section",
                                      actionButton("update_influencers", "Update Analysis", class = "btn-primary btn-update")
                                  )
                              ),
                              div(
                                div(class = "plot-container",
                                    plotlyOutput("plot_top_influencers", height = "300px")
                                ),
                                div(class = "plot-container dt-container", style = "margin-top: 20px;",
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
                                  ),
                                  div(class = "control-section",
                                      actionButton("update_collaborators", "Update Analysis", class = "btn-primary btn-update")
                                  )
                              ),
                              div(
                                div(class = "plot-container",
                                    plotlyOutput("plot_top_collaborators", height = "300px")
                                ),
                                div(class = "plot-container dt-container", style = "margin-top: 20px;",
                                    DTOutput("collaborators_table")
                                )
                              )
                          )
                      )
                  )
              )
          ),
          
          # Tab content for Oceanus Folk Analysis
          div(id = "oceanus-content", class = "tab-content",
              div(class = "tab-navigation",
                  span(class = "tab-nav-item active", "Influence Network"),
                  span(class = "tab-nav-item", "Timeline Analysis"),
                  span(class = "tab-nav-item", "Genre Evolution")
              ),
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Oceanus Folk Influence Network"),
                      div(class = "panel-content",
                          div(class = "two-column-layout",
                              div(class = "control-panel",
                                  div(class = "control-section",
                                      actionButton("update_oceanus_network", "Update Network", class = "btn-primary btn-update")
                                  )
                              ),
                              div(class = "plot-container",
                                  visNetworkOutput("oceanus_network", height = "600px")
                              )
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
                                  ),
                                  div(class = "control-section",
                                      actionButton("update_oceanus_timeline", "Update Timeline", class = "btn-primary btn-update")
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
          div(id = "emerging-content", class = "tab-content",
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
          div(id = "data-content", class = "tab-content",
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
          
          div(id = "guide-content", class = "tab-content",
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
      // Initialize: show only overview, hide others
      $('.tab-content').removeClass('active');
      $('#overview-content').addClass('active');
      
      $('.sidebar-item').click(function() {
        // Remove active class from all sidebar items and tab contents
        $('.sidebar-item').removeClass('active');
        $('.tab-content').removeClass('active');
        
        // Add active class to clicked item
        $(this).addClass('active');
        
        // Show selected tab content
        var tabId = $(this).data('tab') + '-content';
        $('#' + tabId).addClass('active');
        
        // Trigger window resize to force plot re-rendering
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
          
          // Force Plotly plots to resize
          $('.plotly').each(function() {
            if (this.layout) {
              Plotly.Plots.resize(this);
            }
          });
          
          // Force visNetwork to redraw
          if (window.HTMLWidgets && window.HTMLWidgets.widgets) {
            window.HTMLWidgets.widgets.forEach(function(widget) {
              if (widget.name === 'visNetwork' && widget.instance) {
                try {
                  widget.instance.redraw();
                  widget.instance.fit();
                } catch(e) {
                  console.log('Could not redraw visNetwork widget');
                }
              }
            });
          }
        }, 200);
      });
    });
  "))
)



## Server
server <- function(input, output, session) {
  sailor_id <- nodes_tbl %>% filter(name == "Sailor Shift") %>% pull(id)
  
  # Overview outputs
  output$total_nodes <- renderText({
    nrow(nodes_tbl)
  })
  
  output$total_edges <- renderText({
    nrow(edges_tbl)
  })
  
  output$network_density <- renderText({
    round(nrow(edges_tbl) / (nrow(nodes_tbl) * (nrow(nodes_tbl) - 1)), 4)
  })
  
  output$overview_node_types <- renderPlotly({
    data <- nodes_tbl %>%
      count(`Node Type`, sort = TRUE) %>%
      head(5)
    
    p <- ggplot(data, aes(x = reorder(`Node Type`, n), y = n)) +
      geom_col(fill = "#8B4B5C") +
      coord_flip() +
      labs(title = "", x = "", y = "Count") +
      theme_minimal() +
      theme(
        text = element_text(size = 10),
        axis.text = element_text(size = 8)
      )
    
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
      labs(title = "", x = "", y = "Count") +
      theme_minimal() +
      theme(
        text = element_text(size = 10),
        axis.text = element_text(size = 8)
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 80, r = 20, t = 20, b = 30))
  })
  
  # EDA outputs
  output$eda_edge_type <- renderPlotly({
    # 只有在点击更新按钮后才渲染
    input$update_eda_edge
    
    isolate({
      data <- edges_tbl %>%
        count(`Edge Type`, sort = input$eda_edge_sort) %>%
        head(input$eda_edge_top_n)
      
      p <- ggplot(data, aes(x = n, y = reorder(`Edge Type`, n))) +
        geom_col(fill = "#3498db") +
        labs(title = "Edge Type Distribution", x = "Count", y = "Edge Type") +
        theme_minimal()
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        config(displayModeBar = TRUE) %>%
        layout(margin = list(l = 150, r = 50, t = 50, b = 50))
    })
  })
  
  output$eda_node_type <- renderPlotly({
    # 只有在点击更新按钮后才渲染
    input$update_eda_node
    
    isolate({
      data <- nodes_tbl %>%
        count(`Node Type`, sort = TRUE) %>%
        head(10)
      
      p <- ggplot(data, aes(x = n, y = reorder(`Node Type`, n))) +
        geom_col(fill = "#2ecc71") +
        labs(title = "Node Type Distribution", x = "Count", y = "Node Type") +
        theme_minimal()
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        config(displayModeBar = TRUE) %>%
        layout(margin = list(l = 100, r = 50, t = 50, b = 50))
    })
  })
  
  # Static Sailor Shift Network
  output$plot_sailor_network <- renderPlot({
    # 只有在点击更新按钮后才渲染
    input$update_network
    
    isolate({
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
  })
  
  # visNetwork
  output$network_plot <- renderVisNetwork({
    # 只有在点击更新按钮后才渲染
    input$update_interactive_network
    
    isolate({
      edges_sub <- edges_tbl %>% filter(source == sailor_id | target == sailor_id)
      node_ids <- unique(c(edges_sub$source, edges_sub$target))
      nodes_sub <- nodes_tbl %>% filter(id %in% node_ids)
      
      nodes <- nodes_sub %>% mutate(label = name, group = `Node Type`)
      edges <- edges_sub %>% mutate(from = source, to = target, arrows = "to", title = `Edge Type`)
      
      visNetwork(nodes, edges) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visInteraction(navigationButtons = TRUE) %>%
        visLayout(randomSeed = 42) %>%
        visEvents(stabilizationIterationsDone = "function() {
          this.setData(this.body.data);
          this.redraw();
        }")
    })
c(edges_sub$source, edges_sub$target)
nodes_sub <- nodes_tbl %>% filter(id %in% node_ids)

nodes <- nodes_sub %>% mutate(label = name, group = `Node Type`)
edges <- edges_sub %>% mutate(from = source, to = target, arrows = "to", title = `Edge Type`)

visNetwork(nodes, edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visEvents(stabilizationIterationsDone = "function() {
        this.setData(this.body.data);
        this.redraw();
      }")
})

# Top Influencers
influencers_data <- reactive({
  # 只有在点击更新按钮后才更新数据
  input$update_influencers
  
  isolate({
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
})

output$plot_top_influencers <- renderPlotly({
  data <- influencers_data()
  if(nrow(data) == 0) {
    p <- ggplot() + theme_void() + labs(title = "Click 'Update Analysis' to view chart")
  } else {
    p <- ggplot(data, aes(x = reorder(name, times), y = times)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top Influencers", x = "Artist", y = "Influences") +
      theme_minimal()
  }
  
  ggplotly(p, tooltip = c("x", "y")) %>%
    config(displayModeBar = TRUE) %>%
    layout(margin = list(l = 120, r = 50, t = 50, b = 50))
})

output$influencers_table <- renderDT({
  influencers_data()
}, options = list(pageLength = 5, scrollX = TRUE))

# Top Collaborators
collaborators_data <- reactive({
  # 只有在点击更新按钮后才更新数据
  input$update_collaborators
  
  isolate({
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
})

output$plot_top_collaborators <- renderPlotly({
  data <- collaborators_data()
  if(nrow(data) == 0) {
    p <- ggplot() + theme_void() + labs(title = "Click 'Update Analysis' to view chart")
  } else {
    p <- ggplot(data, aes(x = reorder(name, times), y = times)) +
      geom_col(fill = "darkorange") +
      coord_flip() +
      labs(title = "Top Collaborators", x = "Artist", y = "Collaborations") +
      theme_minimal()
  }
  
  ggplotly(p, tooltip = c("x", "y")) %>%
    config(displayModeBar = TRUE) %>%
    layout(margin = list(l = 120, r = 50, t = 50, b = 50))
})

output$collaborators_table <- renderDT({
  collaborators_data()
}, options = list(pageLength = 5, scrollX = TRUE))

# Oceanus Influence Network
output$oceanus_network <- renderVisNetwork({
  # 只有在点击更新按钮后才渲染
  input$update_oceanus_network
  
  isolate({
    oceanus_ids <- nodes_tbl %>% filter(str_detect(tolower(genre), "oceanus folk")) %>% pull(id)
    
    if(length(oceanus_ids) == 0) {
      # Create a simple network if no oceanus folk found
      nodes <- data.frame(id = 1, label = "Click 'Update Network' to view", group = "Unknown")
      edges <- data.frame(from = integer(0), to = integer(0))
    } else {
      influence_edges <- edges_tbl %>%
        filter(source %in% oceanus_ids) %>%
        filter(`Edge Type` %in% c("InStyleOf", "LyricalReferenceTo", "CoverOf", "InterpolatesFrom", "DirectlySamples"))
      
      sub_nodes <- nodes_tbl %>%
        filter(id %in% unique(c(influence_edges$source, influence_edges$target))) %>%
        mutate(index = row_number())
      
      if(nrow(sub_nodes) == 0) {
        nodes <- data.frame(id = 1, label = "No Network Data", group = "Unknown")
        edges <- data.frame(from = integer(0), to = integer(0))
      } else {
        id_map <- sub_nodes %>% select(id, index)
        edges_sub <- influence_edges %>%
          left_join(id_map, by = c("source" = "id")) %>%
          rename(from = index) %>%
          left_join(id_map, by = c("target" = "id")) %>%
          rename(to = index) %>%
          select(from, to, `Edge Type`)
        
        nodes <- sub_nodes %>% mutate(id = index, label = name, group = genre)
        edges <- edges_sub %>% rename(title = `Edge Type`)
      }
    }
    
    visNetwork(nodes, edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE) %>%
      visLegend() %>%
      visLayout(randomSeed = 42) %>%
      visEvents(stabilizationIterationsDone = "function() {
          this.setData(this.body.data);
          this.redraw();
        }")
  })
})

  output$plot_oceanus_timeline <- renderPlotly({
    # 只有在点击更新按钮后才渲染
    input$update_oceanus_timeline
    
    isolate({
      oceanus_ids <- nodes_tbl %>% 
        filter(str_detect(tolower(genre), "oceanus folk")) %>% 
        pull(id)
      
      if(length(oceanus_ids) == 0) {
        p <- ggplot() + theme_void() + labs(title = "Click 'Update Timeline' to view chart")
      } else {
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
        
        if(nrow(timeline_data) == 0) {
          p <- ggplot() + theme_void() + labs(title = "No timeline data in selected range")
        } else {
          p <- ggplot(timeline_data, aes(x = year, y = n)) +
            geom_line(color = "steelblue") +
            geom_point(color = "tomato") +
            labs(title = "Oceanus Folk Influence Over Time", x = "Year", y = "Influenced Works") +
            theme_minimal()
        }
      }
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        config(displayModeBar = TRUE) %>%
        layout(margin = list(l = 50, r = 50, t = 50, b = 50))
    })
  })
}


# Run the app
shinyApp(ui = ui, server = server)