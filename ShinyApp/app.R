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

# 优化后的UI
ui <- fluidPage(
  useShinyjs(),
  
  # 简化的CSS - 移除问题代码
  tags$head(
    tags$style(HTML("
      /* 基础样式 */
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #f8f9fa;
        margin: 0;
        padding: 0;
      }
      
      /* Header */
      .header {
        background: linear-gradient(135deg, #6E3B47 0%, #A85D6B 100%);
        color: white;
        padding: 15px 25px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.15);
        position: relative;
        z-index: 10;
      }
      
      .header h1 {
        margin: 0;
        font-size: 22px;
        font-weight: 600;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      /* 主容器 */
      .main-container {
        display: flex;
        min-height: calc(100vh - 70px);
      }
      
      /* 侧边栏 */
      .sidebar {
        width: 260px;
        background-color: #6E3B47;
        color: white;
        padding: 0;
        box-shadow: 2px 0 10px rgba(0,0,0,0.1);
        overflow-y: auto;
        flex-shrink: 0;
      }
      
      .sidebar-item {
        display: flex;
        align-items: center;
        gap: 12px;
        padding: 15px 20px;
        color: #ecf0f1;
        border-bottom: 1px solid #34495e;
        transition: all 0.3s ease;
        cursor: pointer;
      }
      
      .sidebar-item:hover {
        background-color: #34495e;
        color: white;
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
      
      /* 内容区域 - 关键修复：简化tab切换逻辑 */
      .content-area {
        flex: 1;
        background-color: #f8f9fa;
        overflow-x: auto;
      }
      
      /* Tab内容 - 移除复杂的显示/隐藏逻辑 */
      .tab-content {
        display: none;
        min-height: 100vh;
      }
      
      .tab-content.active {
        display: block;
        min-height: 700px;
      }
      
      /* 导航标签 */
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
        border-bottom: 3px solid transparent;
        transition: all 0.3s ease;
        cursor: pointer;
      }
      
      .tab-nav-item:hover {
        color: #8B4B5C;
      }
      
      .tab-nav-item.active {
        color: #8B4B5C;
        border-bottom-color: #8B4B5C;
        font-weight: 600;
      }
      
      /* 主内容 */
      .main-content {
        padding: 25px;
      }
      
      /* 面板样式 */
      .analysis-panel {
        background: white;
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        margin-bottom: 25px;
        overflow: visible;
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
      
      /* 控制面板 */
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
      
      /* 表单控件 */
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
      
      /* 按钮 */
      .btn-primary {
        background-color: #8B4B5C;
        border-color: #8B4B5C;
        padding: 10px 20px;
        font-weight: 500;
        border-radius: 6px;
        transition: all 0.3s ease;
        margin-top: 10px;
        width: 100%;
      }
      
      .btn-primary:hover {
        background-color: #7a4451;
        border-color: #7a4451;
        transform: translateY(-1px);
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
      
      /* 滑块样式 - 简化z-index */
      .irs {
        position: relative;
        z-index: 50;
      }
      
      .irs--shiny .irs-bar {
        background: #8B4B5C;
      }
      
      .irs--shiny .irs-handle {
        background: #8B4B5C;
        z-index: 1005 !important;
      }
      
      .irs--shiny .irs-handle > i:first-child {
        background: #8B4B5C;
      }
      
      /* 复选框 */
      .form-check-input:checked {
        background-color: #8B4B5C;
        border-color: #8B4B5C;
      }
      
      /* 布局网格 */
      .two-column-layout {
        display: grid;
        grid-template-columns: 280px 1fr;
        gap: 25px;
        align-items: start;
      }
      
      .three-column-layout {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
        gap: 20px;
      }
      
      /* 图表容器 - 简化样式 */
      .plot-container {
        background: white;
        border-radius: 8px;
        padding: 15px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
        position: relative;
        overflow: visible;
        min-width: 0;
        width: 100%;
        height: auto;
      }
      
      /* 图表元素 */
      .plot-container .plotly,
      .plot-container .vis-network,
      .plot-container .shiny-plot-output {
        width: 100%;
        height: auto;
      }
      
      /* Shiny输出元素 - 简化z-index */
      .shiny-plot-output,
      .shiny-html-output {
        position: relative;
        overflow: visible;
      }
      
      /* Plotly图表 */
      .plotly {
        position: relative;
        overflow: visible;
      }
      
      /* visNetwork */
      .vis-network {
        position: relative;
        overflow: visible;
      }
      
      #network_plot,
      #oceanus_network,
      #sailor_network_vis {
        height: 600px !important;
      }
      
      /* DataTable */
      .dataTables_wrapper {
        font-size: 14px;
        position: relative;
      }
      
      .dt-container {
        position: relative;
        overflow: visible;
      }
      
      /* 下拉菜单 */
      .selectize-dropdown,
      .selectize-control {
        z-index: 999;
      }
      
      /* 响应式设计 */
      @media (max-width: 768px) {
        .sidebar {
          width: 100%;
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
      
      /* 自定义滚动条 */
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
          
          # Tab content for Network Analysis - UPDATED SECTION
          div(id = "network-content", class = "tab-content",
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Sailor Shift Network Visualization"),
                      div(class = "panel-content",
                          div(class = "two-column-layout",
                              div(class = "control-panel",
                                  div(class = "control-section",
                                      tags$label(class = "control-label", "Physics Settings:"),
                                      sliderInput("physics_strength", "Gravitational Constant", 
                                                  min = -15000, max = -2000, value = -8000, step = 500)
                                  ),
                                  div(class = "control-section",
                                      sliderInput("spring_length", "Spring Length", 
                                                  min = 100, max = 300, value = 200, step = 25)
                                  ),
                                  div(class = "control-section",
                                      checkboxInput("show_labels", "Show All Labels", value = TRUE)
                                  ),
                                  div(class = "control-section",
                                      actionButton("update_sailor_network", "Update Network", class = "btn-primary btn-update")
                                  ),
                                  div(class = "control-section",
                                      tags$hr(),
                                      tags$h5("Network Statistics:", style = "font-weight: 600; margin-bottom: 10px;"),
                                      textOutput("sailor_network_stats")
                                  )
                              ),
                              div(class = "plot-container",
                                  visNetworkOutput("sailor_network_vis", height = "700px")
                              )
                          )
                      )
                  ),
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Sailor Shift Influence Analysis Network"),
                      div(class = "panel-content",
                          p("This network shows songs influenced by Sailor Shift's notable works and their creators. 
                            Light blue nodes represent influenced songs, while colored nodes represent their creators 
                            (color intensity indicates number of influenced works created)."),
                          div(class = "plot-container",
                              visNetworkOutput("network_plot", height = "600px")
                          )
                      )
                  )
              )
          ),
          
          # Tab content for Influence & Collaboration
          div(id = "influence-content", class = "tab-content",
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
                                      checkboxInput("influencers_include_samples", "Include Direct Samples", value = TRUE)
                                  ),
                                  div(class = "control-section",
                                      checkboxInput("influencers_include_style", "Include Style Influences", value = TRUE)
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
                                      checkboxInput("collaborators_include_performers", "Include Performers", value = TRUE)
                                  ),
                                  div(class = "control-section",
                                      checkboxInput("collaborators_include_producers", "Include Producers", value = TRUE)
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
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Oceanus Folk Influence Network"),
                      div(class = "panel-content",
                          div(class = "two-column-layout",
                              div(class = "control-panel",
                                  div(class = "control-section",
                                      tags$label(class = "control-label", "Network Settings:"),
                                      checkboxInput("oceanus_include_covers", "Include Covers", value = TRUE)
                                  ),
                                  div(class = "control-section",
                                      checkboxInput("oceanus_include_samples", "Include Samples", value = TRUE)
                                  ),
                                  div(class = "control-section",
                                      checkboxInput("oceanus_include_style", "Include Style Influence", value = TRUE)
                                  ),
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
          
          # Tab content for Data Source
          div(id = "data-content", class = "tab-content",
              div(class = "main-content",
                  div(class = "analysis-panel",
                      div(class = "panel-header", "Data Source Information"),
                      div(class = "panel-content",
                          p("Music knowledge graph data loaded from MC1_graph.json"),
                          p("Data contains nodes and edges representing musical entities and their relationships."),
                          
                          div(class = "two-column-layout",
                              div(class = "control-panel",
                                  div(class = "control-section",
                                      tags$label(class = "control-label", "View Data:"),
                                      selectInput("data_view_type", "",
                                                  choices = c("Nodes" = "nodes", "Edges" = "edges"), 
                                                  selected = "nodes")
                                  ),
                                  div(class = "control-section",
                                      tags$label(class = "control-label", "Rows to Display:"),
                                      sliderInput("data_rows_display", "", min = 10, max = 100, value = 20, step = 10)
                                  ),
                                  div(class = "control-section",
                                      actionButton("update_data_view", "Update View", class = "btn-primary btn-update")
                                  ),
                                  div(class = "control-section",
                                      tags$hr(),
                                      tags$h5("Data Summary:", style = "font-weight: 600; margin-bottom: 10px;"),
                                      textOutput("data_summary")
                                  )
                              ),
                              div(class = "plot-container dt-container",
                                  DTOutput("data_table")
                              )
                          )
                      )
                  )
              )
          ),
          
          # Tab content for User Guide
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
  
  # 优化后的JavaScript - 移除复杂的重绘逻辑
  tags$script(HTML("
    $(document).ready(function() {
      // 初始化：只显示overview，隐藏其他
      $('.tab-content').removeClass('active');
      $('#overview-content').addClass('active');
      
      $('.sidebar-item').click(function() {
        // 移除所有活动状态
        $('.sidebar-item').removeClass('active');
        $('.tab-content').removeClass('active');
        
        // 设置当前选中状态
        $(this).addClass('active');
        
        // 显示对应的tab内容
        var tabId = $(this).data('tab') + '-content';
        $('#' + tabId).addClass('active');
        
        // 简化的重绘逻辑 - 延迟执行避免冲突
        setTimeout(function() {
          // 触发窗口resize事件
          $(window).trigger('resize');
          
          // 对于Plotly图表，使用更简单的resize方法
          if (window.Plotly) {
            $('.plotly:visible').each(function() {
              try {
                window.Plotly.Plots.resize(this);
              } catch(e) {
                console.log('Plotly resize failed:', e);
              }
            });
          }
          
          // 对于visNetwork，使用更安全的重绘方法
          if (window.HTMLWidgets) {
            setTimeout(function() {
              try {
                window.HTMLWidgets.staticRender();
              } catch(e) {
                console.log('HTMLWidgets render failed:', e);
              }
            }, 100);
          }
        }, 300);
      });
    });
  "))
)

# Server部分
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
  
  # EDA部分修复 - 方法1：响应式数据 + 事件响应
  
  # 1. 创建响应式数据函数，提供默认值
  eda_edge_data <- reactive({
    # 提供默认值以防输入为NULL
    top_n <- if(is.null(input$eda_edge_top_n)) 10 else input$eda_edge_top_n
    sort_flag <- if(is.null(input$eda_edge_sort)) TRUE else input$eda_edge_sort
    
    # 返回处理后的数据
    edges_tbl %>%
      count(`Edge Type`, sort = sort_flag) %>%
      head(top_n)
  })
  
  eda_node_data <- reactive({
    # Node数据相对简单，不需要太多参数
    nodes_tbl %>%
      count(`Node Type`, sort = TRUE) %>%
      head(10)
  })
  
  # 2. 初始化渲染 - 页面加载时显示默认图表
  output$eda_edge_type <- renderPlotly({
    # 使用固定的默认值进行初始化
    data <- edges_tbl %>%
      count(`Edge Type`, sort = TRUE) %>%
      head(10)
    
    if(nrow(data) == 0) {
      p <- ggplot() + 
        theme_void() + 
        labs(title = "No data available")
    } else {
      p <- ggplot(data, aes(x = n, y = reorder(`Edge Type`, n))) +
        geom_col(fill = "#3498db") +
        labs(title = "Edge Type Distribution", x = "Count", y = "Edge Type") +
        theme_minimal() +
        theme(
          text = element_text(size = 12),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 14, hjust = 0.5)
        )
    }
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = TRUE) %>%
      layout(margin = list(l = 150, r = 50, t = 50, b = 50))
  })
  
  output$eda_node_type <- renderPlotly({
    # 使用响应式数据进行初始化
    data <- eda_node_data()
    
    if(nrow(data) == 0) {
      p <- ggplot() + 
        theme_void() + 
        labs(title = "No data available")
    } else {
      p <- ggplot(data, aes(x = n, y = reorder(`Node Type`, n))) +
        geom_col(fill = "#2ecc71") +
        labs(title = "Node Type Distribution", x = "Count", y = "Node Type") +
        theme_minimal() +
        theme(
          text = element_text(size = 12),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 14, hjust = 0.5)
        )
    }
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = TRUE) %>%
      layout(margin = list(l = 100, r = 50, t = 50, b = 50))
  })
  
  # 3. 事件响应 - 监听按钮点击，使用响应式数据更新图表
  observeEvent(input$update_eda_edge, {
    output$eda_edge_type <- renderPlotly({
      # 使用响应式数据
      data <- eda_edge_data()
      
      if(nrow(data) == 0) {
        p <- ggplot() + 
          theme_void() + 
          labs(title = "No data available") +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        p <- ggplot(data, aes(x = n, y = reorder(`Edge Type`, n))) +
          geom_col(fill = "#3498db") +
          labs(title = "Edge Type Distribution (Updated)", x = "Count", y = "Edge Type") +
          theme_minimal() +
          theme(
            text = element_text(size = 12),
            axis.text = element_text(size = 10),
            plot.title = element_text(size = 14, hjust = 0.5)
          )
      }
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        config(displayModeBar = TRUE) %>%
        layout(margin = list(l = 150, r = 50, t = 50, b = 50))
    })
    
    # 可选：显示更新提示
    showNotification("Edge type chart updated!", type = "message", duration = 2)
  })
  
  observeEvent(input$update_eda_node, {
    output$eda_node_type <- renderPlotly({
      # 使用响应式数据
      data <- eda_node_data()
      
      if(nrow(data) == 0) {
        p <- ggplot() + 
          theme_void() + 
          labels(title = "No data available") +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        p <- ggplot(data, aes(x = n, y = reorder(`Node Type`, n))) +
          geom_col(fill = "#2ecc71") +
          labs(title = "Node Type Distribution (Updated)", x = "Count", y = "Node Type") +
          theme_minimal() +
          theme(
            text = element_text(size = 12),
            axis.text = element_text(size = 10),
            plot.title = element_text(size = 14, hjust = 0.5)
          )
      }
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        config(displayModeBar = TRUE) %>%
        layout(margin = list(l = 100, r = 50, t = 50, b = 50))
    })
    
    # 可选：显示更新提示
    showNotification("Node type chart updated!", type = "message", duration = 2)
  })
  
  # =============================================================================
  # NEW SAILOR SHIFT NETWORK VISUALIZATION - COMPLETE REPLACEMENT
  # =============================================================================
  
  # Create reactive data for the new Sailor Shift network
  sailor_network_data <- reactive({
    tryCatch({
      # Get physics settings from UI inputs
      physics_strength <- if(is.null(input$physics_strength)) -8000 else input$physics_strength
      spring_length <- if(is.null(input$spring_length)) 200 else input$spring_length
      show_labels <- if(is.null(input$show_labels)) TRUE else input$show_labels
      
      # Debug: Check if sailor_id exists
      if(is.null(sailor_id) || length(sailor_id) == 0) {
        cat("Warning: sailor_id not found\n")
        return(list(
          nodes = data.frame(
            id = "no_sailor",
            label = "Sailor Shift not found in data",
            group = "Error",
            title = "Sailor Shift not found",
            size = 30,
            color = "#ff9999"
          ),
          edges = data.frame(from = character(0), to = character(0), type = character(0)),
          physics_settings = list(gravitationalConstant = -8000, springLength = 200),
          stats = list(total_nodes = 1, total_edges = 0, sailor_works = 0, notable_works = 0)
        ))
      }
      
      # Get Sailor Shift's PerformerOf edges corresponding works
      sailor_works <- edges_tbl %>%
        filter(source == sailor_id, `Edge Type` == "PerformerOf") %>%
        pull(target)
      
      # Debug: Check sailor works
      cat("Found", length(sailor_works), "sailor works\n")
      
      if(length(sailor_works) == 0) {
        return(list(
          nodes = data.frame(
            id = sailor_id,
            label = "Sailor Shift (No works found)",
            group = "Person",
            title = "No PerformerOf relationships found",
            size = 40,
            color = "#1f78b4"
          ),
          edges = data.frame(from = character(0), to = character(0), type = character(0)),
          physics_settings = list(gravitationalConstant = physics_strength, springLength = spring_length),
          stats = list(total_nodes = 1, total_edges = 0, sailor_works = 0, notable_works = 0)
        ))
      }
      
      # Extract notable works among them
      sailor_notable_works <- nodes_tbl %>%
        filter(id %in% sailor_works, notable == TRUE)
      
      # Debug: Check notable works
      cat("Found", nrow(sailor_notable_works), "notable works\n")
      
      # If no notable works, use all works
      if(nrow(sailor_notable_works) == 0) {
        sailor_notable_works <- nodes_tbl %>%
          filter(id %in% sailor_works) %>%
          head(10)  # Limit to first 10 works to avoid overcrowding
        cat("Using", nrow(sailor_notable_works), "regular works instead\n")
      }
      
      if(nrow(sailor_notable_works) == 0) {
        return(list(
          nodes = data.frame(
            id = sailor_id,
            label = "Sailor Shift (No accessible works)",
            group = "Person", 
            title = "Works exist but not accessible in nodes table",
            size = 40,
            color = "#1f78b4"
          ),
          edges = data.frame(from = character(0), to = character(0), type = character(0)),
          physics_settings = list(gravitationalConstant = physics_strength, springLength = spring_length),
          stats = list(total_nodes = 1, total_edges = 0, sailor_works = length(sailor_works), notable_works = 0)
        ))
      }
      
      # Define edge types of interest (music creation, production, style, etc.)
      edge_types <- c("ComposerOf", "LyricistOf", "PerformerOf", "ProducerOf",
                      "RecordedBy", "DistributedBy", "DirectlySamples", "CoverOf",
                      "InterpolatesFrom", "LyricalReferenceTo", "InStyleOf")
      
      # Build edges related to works (bidirectional)
      linked_edges <- edges_tbl %>%
        filter(`Edge Type` %in% edge_types,
               source %in% sailor_notable_works$id | target %in% sailor_notable_works$id) %>%
        mutate(type = `Edge Type`) %>%
        select(from = source, to = target, type)
      
      # Debug: Check linked edges
      cat("Found", nrow(linked_edges), "linked edges\n")
      
      if(nrow(linked_edges) == 0) {
        # Create a simple network with just Sailor and his works
        combined_nodes <- rbind(
          data.frame(id = sailor_id, stringsAsFactors = FALSE),
          data.frame(id = sailor_notable_works$id, stringsAsFactors = FALSE)
        )
        graph_nodes <- nodes_tbl %>%
          filter(id %in% combined_nodes$id) %>%
          mutate(
            label = if(show_labels) name else "",
            group = `Node Type`,
            title = paste0("<b>", name, "</b><br>Type: ", `Node Type`,
                           "<br>Genre: ", coalesce(genre, "Unknown"), "<br>",
                           "Notable: ", coalesce(notable, FALSE)),
            size = ifelse(id == sailor_id, 40, 25),
            color = ifelse(id == sailor_id, "#1f78b4", "#a6cee3")
          )
        
        # Create basic edges from Sailor to his works
        basic_edges <- edges_tbl %>%
          filter(source == sailor_id, target %in% sailor_notable_works$id) %>%
          select(from = source, to = target, type = `Edge Type`) %>%
          head(20)  # Limit edges
        
        return(list(
          nodes = graph_nodes,
          edges = basic_edges,
          physics_settings = list(gravitationalConstant = physics_strength, springLength = spring_length),
          stats = list(
            total_nodes = nrow(graph_nodes),
            total_edges = nrow(basic_edges),
            sailor_works = length(sailor_works),
            notable_works = nrow(sailor_notable_works)
          )
        ))
      }
      
      # Extract all nodes participating in these edges
      related_node_ids <- unique(c(linked_edges$from, linked_edges$to))
      
      # Build node table for graph, setting labels and grouping info
      graph_nodes <- nodes_tbl %>%
        filter(id %in% related_node_ids) %>%
        mutate(label = if(show_labels) name else "",
               group = `Node Type`,
               title = paste0("<b>", name, "</b><br>Type: ", `Node Type`,
                              "<br>Genre: ", coalesce(genre, "Unknown"),
                              "<br>Notable: ", coalesce(notable, FALSE)))
      
      # Calculate degree to set initial size
      node_degree <- linked_edges %>%
        pivot_longer(cols = c(from, to), values_to = "id") %>%
        count(id, name = "degree")
      
      graph_nodes <- graph_nodes %>%
        left_join(node_degree, by = "id") %>%
        mutate(degree = coalesce(degree, 0),
               size = 10 + degree * 1.5)
      
      # Categorize Sailor himself, works, and other nodes
      sailor_work_ids_by_date <- nodes_tbl %>%
        filter(
          `Node Type` %in% c("Song", "Album"),
          !is.na(release_date),
          as.integer(release_date) < 2028,
          id %in% sailor_works
        ) %>%
        pull(id)
      
      graph_nodes <- graph_nodes %>%
        mutate(category = case_when(
          id == sailor_id ~ "Sailor",
          id %in% sailor_work_ids_by_date ~ "SailorWork",
          TRUE ~ "Other"
        ))
      
      # Count connections of other nodes to works (more connections = more important)
      related_edges_to_works <- linked_edges %>%
        filter(from %in% sailor_notable_works$id | to %in% sailor_notable_works$id)
      
      other_node_connection <- related_edges_to_works %>%
        pivot_longer(cols = c(from, to), values_to = "id") %>%
        filter(!id %in% c(sailor_id, sailor_notable_works$id)) %>%
        count(id, name = "connection_to_works")
      
      # Combine connection count to set node color and size
      graph_nodes <- graph_nodes %>%
        left_join(other_node_connection, by = "id") %>%
        mutate(
          connection_to_works = coalesce(connection_to_works, 0),
          size = case_when(
            category == "Sailor" ~ 40,
            category == "SailorWork" ~ 30,
            TRUE ~ 10 + 2 * connection_to_works
          )
        )
      
      # Set colors separately to avoid case_when issues
      graph_nodes$color <- "#eeeeee"  # Default color
      
      # Set colors based on category
      graph_nodes$color[graph_nodes$category == "Sailor"] <- "#1f78b4"  # Deep blue
      graph_nodes$color[graph_nodes$category == "SailorWork"] <- "#a6cee3"  # Light blue
      
      # Set colors for other nodes based on connections
      if(nrow(other_node_connection) > 0 && max(other_node_connection$connection_to_works, na.rm = TRUE) > 0) {
        max_conn <- max(other_node_connection$connection_to_works, na.rm = TRUE)
        color_palette <- colorRampPalette(c("#eeeeee", "#ff3300"))(max_conn)
        
        for(i in 1:nrow(graph_nodes)) {
          if(graph_nodes$category[i] == "Other" && graph_nodes$connection_to_works[i] > 0) {
            color_index <- min(graph_nodes$connection_to_works[i], length(color_palette))
            graph_nodes$color[i] <- color_palette[color_index]
          }
        }
      }
      
      # Return processed data
      list(
        nodes = graph_nodes,
        edges = linked_edges,
        physics_settings = list(
          gravitationalConstant = physics_strength,
          springLength = spring_length
        ),
        stats = list(
          total_nodes = nrow(graph_nodes),
          total_edges = nrow(linked_edges),
          sailor_works = length(sailor_works),
          notable_works = nrow(sailor_notable_works)
        )
      )
      
    }, error = function(e) {
      # Error handling: return simple error network
      cat("Error in sailor_network_data:", e$message, "\n")
      list(
        nodes = data.frame(
          id = "error",
          label = paste("Error:", e$message),
          group = "Error",
          title = paste("Error:", e$message),
          size = 20,
          color = "#ff0000"
        ),
        edges = data.frame(from = character(0), to = character(0), type = character(0)),
        physics_settings = list(gravitationalConstant = -8000, springLength = 200),
        stats = list(total_nodes = 0, total_edges = 0, sailor_works = 0, notable_works = 0)
      )
    })
  })
  
  # Initialize rendering - show default network on page load
  output$sailor_network_vis <- renderVisNetwork({
    network_data <- sailor_network_data()
    
    # Create visNetwork with the new data
    visNetwork(network_data$nodes, network_data$edges, height = "700px", width = "100%") %>%
      visEdges(arrows = "to", color = list(color = "#848484", highlight = "#ff0000")) %>%
      visOptions(
        highlightNearest = TRUE, 
        nodesIdSelection = TRUE,
        selectedBy = "group"
      ) %>%
      visInteraction(
        navigationButtons = TRUE,
        dragNodes = TRUE,
        dragView = TRUE,
        zoomView = TRUE
      ) %>%
      visLayout(randomSeed = 123) %>%
      visPhysics(
        stabilization = list(iterations = 100),
        barnesHut = list(
          gravitationalConstant = network_data$physics_settings$gravitationalConstant,
          springConstant = 0.001,
          springLength = network_data$physics_settings$springLength
        )
      ) %>%
      visLegend(
        useGroups = TRUE,
        position = "right",
        main = "Node Categories"
      )
  })
  
  # Event response - listen for button click, update network using reactive data
  observeEvent(input$update_sailor_network, {
    output$sailor_network_vis <- renderVisNetwork({
      network_data <- sailor_network_data()
      
      # Create updated network with new settings
      visNetwork(network_data$nodes, network_data$edges, height = "700px", width = "100%") %>%
        visEdges(arrows = "to", color = list(color = "#848484", highlight = "#ff0000")) %>%
        visOptions(
          highlightNearest = TRUE, 
          nodesIdSelection = TRUE,
          selectedBy = "group"
        ) %>%
        visInteraction(
          navigationButtons = TRUE,
          dragNodes = TRUE,
          dragView = TRUE,
          zoomView = TRUE
        ) %>%
        visLayout(randomSeed = sample(1:1000, 1)) %>%  # Random seed for different layout
        visPhysics(
          stabilization = list(iterations = 200),
          barnesHut = list(
            gravitationalConstant = network_data$physics_settings$gravitationalConstant,
            springConstant = 0.002,
            springLength = network_data$physics_settings$springLength
          )
        ) %>%
        visLegend(
          useGroups = TRUE,
          position = "right",
          main = "Node Categories (Updated)"
        )
    })
    
    # Show update notification
    showNotification("Sailor Shift network updated with new settings!", type = "message", duration = 3)
  })
  
  # Network statistics output
  output$sailor_network_stats <- renderText({
    network_data <- sailor_network_data()
    stats <- network_data$stats
    
    paste0(
      "Network: ", stats$total_nodes, " nodes, ", stats$total_edges, " edges\n",
      "Sailor's Works: ", stats$sailor_works, " total, ", stats$notable_works, " notable"
    )
  })
  
  # =============================================================================
  # SIMPLE NETWORK PLOT (ORIGINAL SECOND NETWORK) - KEEP AS IS
  # =============================================================================
  
  # Simple network data for the second visualization
  simple_network_data <- reactive({
    tryCatch({
      # Get edges directly related to Sailor Shift
      edges_sub <- edges_tbl %>% 
        filter(source == sailor_id | target == sailor_id)
      
      # Check if there's data
      if(nrow(edges_sub) == 0) {
        return(list(
          nodes = data.frame(
            id = sailor_id,
            label = "Sailor Shift (No connections)",
            group = "Person",
            title = "No connections found",
            size = 30,
            color = "#ff9999"
          ),
          edges = data.frame(
            from = character(0), 
            to = character(0), 
            arrows = character(0), 
            title = character(0)
          )
        ))
      }
      
      # Get all related node IDs
      node_ids <- unique(c(edges_sub$source, edges_sub$target))
      nodes_sub <- nodes_tbl %>% filter(id %in% node_ids)
      
      # Process node data
      nodes <- nodes_sub %>% 
        mutate(
          label = name,
          group = `Node Type`,
          title = paste0(
            "<b>", name, "</b><br>",
            "Type: ", `Node Type`, "<br>",
            "Genre: ", coalesce(genre, "Unknown"), "<br>",
            "Notable: ", coalesce(notable, FALSE)
          ),
          size = case_when(
            id == sailor_id ~ 40,
            `Node Type` == "Song" ~ 25,
            `Node Type` == "Album" ~ 30,
            `Node Type` == "Person" ~ 20,
            TRUE ~ 15
          ),
          color = case_when(
            id == sailor_id ~ "#1f78b4",
            `Node Type` == "Song" ~ "#33a02c",
            `Node Type` == "Album" ~ "#e31a1c",
            `Node Type` == "Person" ~ "#ff7f00",
            TRUE ~ "#6a3d9a"
          )
        )
      
      # Process edge data
      edges <- edges_sub %>% 
        mutate(
          from = source,
          to = target,
          arrows = "to",
          title = paste0("Relationship: ", `Edge Type`),
          color = case_when(
            `Edge Type` == "PerformerOf" ~ "#1f78b4",
            `Edge Type` == "ComposerOf" ~ "#33a02c",
            `Edge Type` == "LyricistOf" ~ "#e31a1c",
            `Edge Type` == "ProducerOf" ~ "#ff7f00",
            TRUE ~ "#999999"
          ),
          width = case_when(
            `Edge Type` %in% c("PerformerOf", "ComposerOf") ~ 3,
            `Edge Type` %in% c("LyricistOf", "ProducerOf") ~ 2,
            TRUE ~ 1
          )
        )
      
      list(nodes = nodes, edges = edges)
      
    }, error = function(e) {
      cat("Error in simple_network_data:", e$message, "\n")
      list(
        nodes = data.frame(
          id = 1,
          label = paste("Error:", e$message),
          group = "Error",
          title = paste("Error loading network:", e$message),
          size = 30,
          color = "#ff0000"
        ),
        edges = data.frame(
          from = character(0), 
          to = character(0), 
          arrows = character(0), 
          title = character(0)
        )
      )
    })
  })
  
  # Initialize simple network rendering
  output$network_plot <- renderVisNetwork({
    network_data <- simple_network_data()
    
    visNetwork(
      network_data$nodes, 
      network_data$edges, 
      height = "600px", 
      width = "100%"
    ) %>%
      visOptions(
        highlightNearest = TRUE, 
        nodesIdSelection = TRUE,
        selectedBy = "group"
      ) %>%
      visInteraction(
        navigationButtons = TRUE,
        dragNodes = TRUE,
        dragView = TRUE,
        zoomView = TRUE
      ) %>%
      visLayout(randomSeed = 42) %>%
      visPhysics(
        stabilization = list(iterations = 100),
        barnesHut = list(gravitationalConstant = -8000, springConstant = 0.001, springLength = 200)
      ) %>%
      visLegend(
        useGroups = TRUE,
        position = "right",
        main = "Node Types"
      )
  })
  
  # =============================================================================
  # TOP INFLUENCERS - 方法1：响应式数据 + 事件响应 (修复版本)
  # =============================================================================
  
  # 1. 创建响应式数据函数，提供默认值
  influencers_data <- reactive({
    # 提供默认值以防输入为NULL
    top_n <- if(is.null(input$influencers_top_n)) 10 else input$influencers_top_n
    include_samples <- if(is.null(input$influencers_include_samples)) TRUE else input$influencers_include_samples
    include_style <- if(is.null(input$influencers_include_style)) TRUE else input$influencers_include_style
    
    tryCatch({
      # Use the same logic as the first network to get sailor's works
      sailor_works <- edges_tbl %>%
        filter(source == sailor_id, `Edge Type` == "PerformerOf") %>%
        pull(target)
      
      if(length(sailor_works) == 0) {
        cat("No sailor works found for influencer analysis\n")
        return(data.frame(name = character(0), genre = character(0), times = integer(0)))
      }
      
      cat("Found", length(sailor_works), "sailor works for influencer analysis\n")
      
      # Build edge types based on user selections
      edge_types <- character(0)
      if(include_samples) {
        edge_types <- c(edge_types, "DirectlySamples", "InterpolatesFrom", "CoverOf")
      }
      if(include_style) {
        edge_types <- c(edge_types, "InStyleOf", "LyricalReferenceTo")
      }
      
      if(length(edge_types) == 0) {
        cat("No edge types selected for influencer analysis\n")
        return(data.frame(name = character(0), genre = character(0), times = integer(0)))
      }
      
      cat("Using edge types:", paste(edge_types, collapse = ", "), "\n")
      
      # Method 1: Find works that Sailor's works reference/sample (who influenced Sailor)
      # Look for edges FROM other works TO Sailor's works
      influence_edges <- edges_tbl %>%
        filter(target %in% sailor_works, 
               `Edge Type` %in% edge_types)
      
      cat("Found", nrow(influence_edges), "influence edges TO sailor's works\n")
      
      if(nrow(influence_edges) == 0) {
        # Method 2: Try reverse - find what Sailor's works influenced, then trace back
        outgoing_influence <- edges_tbl %>%
          filter(source %in% sailor_works,
                 `Edge Type` %in% edge_types)
        
        cat("Found", nrow(outgoing_influence), "outgoing influence edges FROM sailor's works\n")
        
        if(nrow(outgoing_influence) == 0) {
          return(data.frame(name = character(0), genre = character(0), times = integer(0)))
        }
        
        # Get who created the works that influenced those influenced by Sailor
        influenced_work_ids <- outgoing_influence$target
        
        # Find creators of works that were influenced by Sailor (reverse influence)
        influenced_creators <- edges_tbl %>%
          filter(target %in% influenced_work_ids,
                 `Edge Type` %in% c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf"))
        
        influencers <- influenced_creators %>%
          count(source, sort = TRUE) %>%
          left_join(nodes_tbl, by = c("source" = "id")) %>%
          filter(!is.na(name), source != sailor_id) %>%
          select(name, genre = genre, times = n) %>%
          head(top_n)
        
        cat("Found", nrow(influencers), "artists influenced BY sailor (reverse analysis)\n")
        return(influencers)
      }
      
      # Get the source works that influenced Sailor
      influencing_work_ids <- influence_edges$source
      
      # Find the creators of those influencing works
      influencer_edges <- edges_tbl %>%
        filter(target %in% influencing_work_ids,
               `Edge Type` %in% c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf"))
      
      cat("Found", nrow(influencer_edges), "creator edges for influencing works\n")
      
      # Count and rank influencers
      influencers <- influencer_edges %>%
        count(source, sort = TRUE) %>%
        left_join(nodes_tbl, by = c("source" = "id")) %>%
        filter(!is.na(name), source != sailor_id) %>%  # Exclude Sailor himself
        select(name, genre = genre, times = n) %>%
        head(top_n)
      
      cat("Final influencers found:", nrow(influencers), "\n")
      influencers
      
    }, error = function(e) {
      cat("Error in influencers_data:", e$message, "\n")
      data.frame(name = character(0), genre = character(0), times = integer(0))
    })
  })
  
  # 2. 初始化渲染 - 页面加载时显示默认图表
  output$plot_top_influencers <- renderPlotly({
    # 使用固定的默认值进行初始化
    data <- tryCatch({
      sailor_works <- edges_tbl %>%
        filter(source == sailor_id, `Edge Type` == "PerformerOf") %>%
        pull(target)
      
      if(length(sailor_works) > 0) {
        # Try multiple methods to find influencers
        
        # Method 1: Direct influence TO sailor's works
        influence_edges <- edges_tbl %>%
          filter(target %in% sailor_works, 
                 `Edge Type` %in% c("DirectlySamples", "InterpolatesFrom", "CoverOf", "InStyleOf", "LyricalReferenceTo"))
        
        if(nrow(influence_edges) > 0) {
          influencing_work_ids <- influence_edges$source
          
          influencer_edges <- edges_tbl %>%
            filter(target %in% influencing_work_ids,
                   `Edge Type` %in% c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf"))
          
          if(nrow(influencer_edges) > 0) {
            result <- influencer_edges %>%
              count(source, sort = TRUE) %>%
              left_join(nodes_tbl, by = c("source" = "id")) %>%
              filter(!is.na(name), source != sailor_id) %>%
              select(name, genre = genre, times = n) %>%
              head(10)
            
            if(nrow(result) > 0) return(result)
          }
        }
        
        # Method 2: Find who Sailor influenced (reverse analysis)
        outgoing_influence <- edges_tbl %>%
          filter(source %in% sailor_works,
                 `Edge Type` %in% c("DirectlySamples", "InterpolatesFrom", "CoverOf", "InStyleOf", "LyricalReferenceTo"))
        
        if(nrow(outgoing_influence) > 0) {
          influenced_work_ids <- outgoing_influence$target
          
          influenced_creators <- edges_tbl %>%
            filter(target %in% influenced_work_ids,
                   `Edge Type` %in% c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf"))
          
          if(nrow(influenced_creators) > 0) {
            result <- influenced_creators %>%
              count(source, sort = TRUE) %>%
              left_join(nodes_tbl, by = c("source" = "id")) %>%
              filter(!is.na(name), source != sailor_id) %>%
              select(name, genre = genre, times = n) %>%
              head(10)
            
            if(nrow(result) > 0) return(result)
          }
        }
      }
      
      data.frame(name = character(0), genre = character(0), times = integer(0))
    }, error = function(e) {
      data.frame(name = character(0), genre = character(0), times = integer(0))
    })
    
    if(nrow(data) == 0) {
      p <- ggplot() + 
        theme_void() + 
        labs(title = "No influencer data available") +
        theme(plot.title = element_text(hjust = 0.5, size = 14))
    } else {
      p <- ggplot(data, aes(x = reorder(name, times), y = times, 
                            text = paste("Artist:", name, "<br>Genre:", coalesce(genre, "Unknown"), "<br>Count:", times))) +
        geom_col(fill = "steelblue", alpha = 0.8) +
        coord_flip() +
        labs(title = "Music Network Analysis - Influencers", x = "Artist", y = "Connection Count") +
        theme_minimal() +
        theme(
          text = element_text(size = 12),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 14, hjust = 0.5)
        )
    }
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = TRUE) %>%
      layout(margin = list(l = 120, r = 50, t = 50, b = 50))
  })
  
  output$influencers_table <- renderDT({
    # 使用默认数据进行初始化
    data <- tryCatch({
      sailor_works <- edges_tbl %>%
        filter(source == sailor_id, `Edge Type` == "PerformerOf") %>%
        pull(target)
      
      if(length(sailor_works) > 0) {
        # Try to find any music connections
        all_connections <- edges_tbl %>%
          filter((source %in% sailor_works | target %in% sailor_works),
                 `Edge Type` %in% c("DirectlySamples", "InterpolatesFrom", "CoverOf", "InStyleOf", "LyricalReferenceTo", "ComposerOf", "LyricistOf", "ProducerOf"))
        
        if(nrow(all_connections) > 0) {
          # Get all connected nodes except Sailor
          connected_nodes <- unique(c(all_connections$source, all_connections$target))
          connected_nodes <- connected_nodes[connected_nodes != sailor_id]
          connected_nodes <- connected_nodes[connected_nodes %in% nodes_tbl$id]
          
          if(length(connected_nodes) > 0) {
            result <- nodes_tbl %>%
              filter(id %in% connected_nodes) %>%
              count(name, genre, sort = TRUE) %>%
              rename(times = n) %>%
              head(10)
            
            if(nrow(result) > 0) {
              return(result %>%
                       rename("Artist Name" = name, "Genre" = genre, "Connection Count" = times))
            }
          }
        }
      }
      
      data.frame("Artist Name" = character(0), "Genre" = character(0), "Connection Count" = integer(0))
    }, error = function(e) {
      data.frame("Artist Name" = character(0), "Genre" = character(0), "Connection Count" = integer(0))
    })
    
    data
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # 3. 事件响应 - 监听按钮点击，使用响应式数据更新图表
  observeEvent(input$update_influencers, {
    output$plot_top_influencers <- renderPlotly({
      # 使用响应式数据
      data <- influencers_data()
      
      if(nrow(data) == 0) {
        p <- ggplot() + 
          theme_void() + 
          labs(title = "No influencer data available (Updated)") +
          theme(plot.title = element_text(hjust = 0.5, size = 14))
      } else {
        title_text <- if(input$influencers_include_samples && input$influencers_include_style) {
          "Artists Who Influenced Sailor Shift (Samples + Style)"
        } else if(input$influencers_include_samples) {
          "Artists Who Influenced Sailor Shift (Samples Only)"
        } else if(input$influencers_include_style) {
          "Artists Who Influenced Sailor Shift (Style Only)"
        } else {
          "No Analysis Selected"
        }
        
        p <- ggplot(data, aes(x = reorder(name, times), y = times, 
                              text = paste("Artist:", name, "<br>Genre:", coalesce(genre, "Unknown"), "<br>Influences:", times))) +
          geom_col(fill = "steelblue", alpha = 0.8) +
          coord_flip() +
          labs(title = title_text, x = "Artist", y = "Number of Influences") +
          theme_minimal() +
          theme(
            text = element_text(size = 12),
            axis.text = element_text(size = 10),
            plot.title = element_text(size = 14, hjust = 0.5)
          )
      }
      
      ggplotly(p, tooltip = "text") %>%
        config(displayModeBar = TRUE) %>%
        layout(margin = list(l = 120, r = 50, t = 50, b = 50))
    })
    
    output$influencers_table <- renderDT({
      data <- influencers_data()
      if(nrow(data) > 0) {
        data %>%
          rename("Artist Name" = name, "Genre" = genre, "Influence Count" = times)
      } else {
        data.frame("Artist Name" = character(0), "Genre" = character(0), "Influence Count" = integer(0))
      }
    }, options = list(pageLength = 10, scrollX = TRUE))
    
    # 显示更新提示
    showNotification("Influencers analysis updated!", type = "message", duration = 2)
  })
  
  # =============================================================================
  # TOP INFLUENCERS - 方法1：响应式数据 + 事件响应 (修复版本)
  # =============================================================================
  
  # 1. 创建响应式数据函数，提供默认值
  influencers_data <- reactive({
    # 提供默认值以防输入为NULL
    top_n <- if(is.null(input$influencers_top_n)) 10 else input$influencers_top_n
    include_samples <- if(is.null(input$influencers_include_samples)) TRUE else input$influencers_include_samples
    include_style <- if(is.null(input$influencers_include_style)) TRUE else input$influencers_include_style
    
    tryCatch({
      # Use the same logic as the first network to get sailor's works
      sailor_works <- edges_tbl %>%
        filter(source == sailor_id, `Edge Type` == "PerformerOf") %>%
        pull(target)
      
      if(length(sailor_works) == 0) {
        cat("No sailor works found for influencer analysis\n")
        return(data.frame(name = character(0), genre = character(0), times = integer(0)))
      }
      
      cat("Found", length(sailor_works), "sailor works for influencer analysis\n")
      
      # Build edge types based on user selections
      edge_types <- character(0)
      if(include_samples) {
        edge_types <- c(edge_types, "DirectlySamples", "InterpolatesFrom", "CoverOf")
      }
      if(include_style) {
        edge_types <- c(edge_types, "InStyleOf", "LyricalReferenceTo")
      }
      
      if(length(edge_types) == 0) {
        cat("No edge types selected for influencer analysis\n")
        return(data.frame(name = character(0), genre = character(0), times = integer(0)))
      }
      
      cat("Using edge types:", paste(edge_types, collapse = ", "), "\n")
      
      # Method 1: Find works that Sailor's works reference/sample (who influenced Sailor)
      # Look for edges FROM other works TO Sailor's works
      influence_edges <- edges_tbl %>%
        filter(target %in% sailor_works, 
               `Edge Type` %in% edge_types)
      
      cat("Found", nrow(influence_edges), "influence edges TO sailor's works\n")
      
      if(nrow(influence_edges) == 0) {
        # Method 2: Try reverse - find what Sailor's works influenced, then trace back
        outgoing_influence <- edges_tbl %>%
          filter(source %in% sailor_works,
                 `Edge Type` %in% edge_types)
        
        cat("Found", nrow(outgoing_influence), "outgoing influence edges FROM sailor's works\n")
        
        if(nrow(outgoing_influence) == 0) {
          return(data.frame(name = character(0), genre = character(0), times = integer(0)))
        }
        
        # Get who created the works that influenced those influenced by Sailor
        influenced_work_ids <- outgoing_influence$target
        
        # Find creators of works that were influenced by Sailor (reverse influence)
        influenced_creators <- edges_tbl %>%
          filter(target %in% influenced_work_ids,
                 `Edge Type` %in% c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf"))
        
        influencers <- influenced_creators %>%
          count(source, sort = TRUE) %>%
          left_join(nodes_tbl, by = c("source" = "id")) %>%
          filter(!is.na(name), source != sailor_id) %>%
          select(name, genre = genre, times = n) %>%
          head(top_n)
        
        cat("Found", nrow(influencers), "artists influenced BY sailor (reverse analysis)\n")
        return(influencers)
      }
      
      # Get the source works that influenced Sailor
      influencing_work_ids <- influence_edges$source
      
      # Find the creators of those influencing works
      influencer_edges <- edges_tbl %>%
        filter(target %in% influencing_work_ids,
               `Edge Type` %in% c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf"))
      
      cat("Found", nrow(influencer_edges), "creator edges for influencing works\n")
      
      # Count and rank influencers
      influencers <- influencer_edges %>%
        count(source, sort = TRUE) %>%
        left_join(nodes_tbl, by = c("source" = "id")) %>%
        filter(!is.na(name), source != sailor_id) %>%  # Exclude Sailor himself
        select(name, genre = genre, times = n) %>%
        head(top_n)
      
      cat("Final influencers found:", nrow(influencers), "\n")
      influencers
      
    }, error = function(e) {
      cat("Error in influencers_data:", e$message, "\n")
      data.frame(name = character(0), genre = character(0), times = integer(0))
    })
  })
  
  # 2. 初始化渲染 - 页面加载时显示默认图表
  output$plot_top_influencers <- renderPlotly({
    # 使用固定的默认值进行初始化
    data <- tryCatch({
      sailor_works <- edges_tbl %>%
        filter(source == sailor_id, `Edge Type` == "PerformerOf") %>%
        pull(target)
      
      if(length(sailor_works) > 0) {
        # Try multiple methods to find influencers
        
        # Method 1: Direct influence TO sailor's works
        influence_edges <- edges_tbl %>%
          filter(target %in% sailor_works, 
                 `Edge Type` %in% c("DirectlySamples", "InterpolatesFrom", "CoverOf", "InStyleOf", "LyricalReferenceTo"))
        
        if(nrow(influence_edges) > 0) {
          influencing_work_ids <- influence_edges$source
          
          influencer_edges <- edges_tbl %>%
            filter(target %in% influencing_work_ids,
                   `Edge Type` %in% c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf"))
          
          if(nrow(influencer_edges) > 0) {
            result <- influencer_edges %>%
              count(source, sort = TRUE) %>%
              left_join(nodes_tbl, by = c("source" = "id")) %>%
              filter(!is.na(name), source != sailor_id) %>%
              select(name, genre = genre, times = n) %>%
              head(10)
            
            if(nrow(result) > 0) return(result)
          }
        }
        
        # Method 2: Find who Sailor influenced (reverse analysis)
        outgoing_influence <- edges_tbl %>%
          filter(source %in% sailor_works,
                 `Edge Type` %in% c("DirectlySamples", "InterpolatesFrom", "CoverOf", "InStyleOf", "LyricalReferenceTo"))
        
        if(nrow(outgoing_influence) > 0) {
          influenced_work_ids <- outgoing_influence$target
          
          influenced_creators <- edges_tbl %>%
            filter(target %in% influenced_work_ids,
                   `Edge Type` %in% c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf"))
          
          if(nrow(influenced_creators) > 0) {
            result <- influenced_creators %>%
              count(source, sort = TRUE) %>%
              left_join(nodes_tbl, by = c("source" = "id")) %>%
              filter(!is.na(name), source != sailor_id) %>%
              select(name, genre = genre, times = n) %>%
              head(10)
            
            if(nrow(result) > 0) return(result)
          }
        }
      }
      
      data.frame(name = character(0), genre = character(0), times = integer(0))
    }, error = function(e) {
      data.frame(name = character(0), genre = character(0), times = integer(0))
    })
    
    if(nrow(data) == 0) {
      p <- ggplot() + 
        theme_void() + 
        labs(title = "No influencer data available") +
        theme(plot.title = element_text(hjust = 0.5, size = 14))
    } else {
      p <- ggplot(data, aes(x = reorder(name, times), y = times, 
                            text = paste("Artist:", name, "<br>Genre:", coalesce(genre, "Unknown"), "<br>Count:", times))) +
        geom_col(fill = "steelblue", alpha = 0.8) +
        coord_flip() +
        labs(title = "Music Network Analysis - Influencers", x = "Artist", y = "Connection Count") +
        theme_minimal() +
        theme(
          text = element_text(size = 12),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 14, hjust = 0.5)
        )
    }
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = TRUE) %>%
      layout(margin = list(l = 120, r = 50, t = 50, b = 50))
  })
  
  output$influencers_table <- renderDT({
    # 使用默认数据进行初始化
    data <- tryCatch({
      sailor_works <- edges_tbl %>%
        filter(source == sailor_id, `Edge Type` == "PerformerOf") %>%
        pull(target)
      
      if(length(sailor_works) > 0) {
        # Try to find any music connections
        all_connections <- edges_tbl %>%
          filter((source %in% sailor_works | target %in% sailor_works),
                 `Edge Type` %in% c("DirectlySamples", "InterpolatesFrom", "CoverOf", "InStyleOf", "LyricalReferenceTo", "ComposerOf", "LyricistOf", "ProducerOf"))
        
        if(nrow(all_connections) > 0) {
          # Get all connected nodes except Sailor
          connected_nodes <- unique(c(all_connections$source, all_connections$target))
          connected_nodes <- connected_nodes[connected_nodes != sailor_id]
          connected_nodes <- connected_nodes[connected_nodes %in% nodes_tbl$id]
          
          if(length(connected_nodes) > 0) {
            result <- nodes_tbl %>%
              filter(id %in% connected_nodes) %>%
              count(name, genre, sort = TRUE) %>%
              rename(times = n) %>%
              head(10)
            
            if(nrow(result) > 0) {
              return(result %>%
                       rename("Artist Name" = name, "Genre" = genre, "Connection Count" = times))
            }
          }
        }
      }
      
      data.frame("Artist Name" = character(0), "Genre" = character(0), "Connection Count" = integer(0))
    }, error = function(e) {
      data.frame("Artist Name" = character(0), "Genre" = character(0), "Connection Count" = integer(0))
    })
    
    data
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # 3. 事件响应 - 监听按钮点击，使用响应式数据更新图表
  observeEvent(input$update_influencers, {
    output$plot_top_influencers <- renderPlotly({
      # 使用响应式数据
      data <- influencers_data()
      
      if(nrow(data) == 0) {
        p <- ggplot() + 
          theme_void() + 
          labs(title = "No influencer data available (Updated)") +
          theme(plot.title = element_text(hjust = 0.5, size = 14))
      } else {
        title_text <- if(input$influencers_include_samples && input$influencers_include_style) {
          "Artists Who Influenced Sailor Shift (Samples + Style)"
        } else if(input$influencers_include_samples) {
          "Artists Who Influenced Sailor Shift (Samples Only)"
        } else if(input$influencers_include_style) {
          "Artists Who Influenced Sailor Shift (Style Only)"
        } else {
          "No Analysis Selected"
        }
        
        p <- ggplot(data, aes(x = reorder(name, times), y = times, 
                              text = paste("Artist:", name, "<br>Genre:", coalesce(genre, "Unknown"), "<br>Influences:", times))) +
          geom_col(fill = "steelblue", alpha = 0.8) +
          coord_flip() +
          labs(title = title_text, x = "Artist", y = "Number of Influences") +
          theme_minimal() +
          theme(
            text = element_text(size = 12),
            axis.text = element_text(size = 10),
            plot.title = element_text(size = 14, hjust = 0.5)
          )
      }
      
      ggplotly(p, tooltip = "text") %>%
        config(displayModeBar = TRUE) %>%
        layout(margin = list(l = 120, r = 50, t = 50, b = 50))
    })
    
    output$influencers_table <- renderDT({
      data <- influencers_data()
      if(nrow(data) > 0) {
        data %>%
          rename("Artist Name" = name, "Genre" = genre, "Influence Count" = times)
      } else {
        data.frame("Artist Name" = character(0), "Genre" = character(0), "Influence Count" = integer(0))
      }
    }, options = list(pageLength = 10, scrollX = TRUE))
    
    # 显示更新提示
    showNotification("Influencers analysis updated!", type = "message", duration = 2)
  })
  
  # =============================================================================
  # TOP COLLABORATORS - BASED ON FIRST NETWORK'S SUCCESSFUL DATA PROCESSING
  # =============================================================================
  
  collaborators_data <- reactive({
    tryCatch({
      # Use the same logic as the first network to get sailor's works
      sailor_works <- edges_tbl %>%
        filter(source == sailor_id, `Edge Type` == "PerformerOf") %>%
        pull(target)
      
      if(length(sailor_works) == 0) {
        return(data.frame(name = character(0), genre = character(0), times = integer(0)))
      }
      
      # Find all people who worked on Sailor's works (collaborators)
      collaboration_edges <- edges_tbl %>%
        filter(target %in% sailor_works, 
               source != sailor_id,  # Exclude Sailor himself
               `Edge Type` %in% c("ComposerOf", "LyricistOf", "ProducerOf", "PerformerOf", "RecordedBy"))
      
      if(nrow(collaboration_edges) == 0) {
        return(data.frame(name = character(0), genre = character(0), times = integer(0)))
      }
      
      # Count and rank collaborators
      collaborators <- collaboration_edges %>%
        count(source, sort = TRUE) %>%
        left_join(nodes_tbl, by = c("source" = "id")) %>%
        filter(!is.na(name)) %>%
        select(name, genre = genre, times = n) %>%
        head(input$collaborators_top_n)
      
      # Debug output
      cat("Found", nrow(collaboration_edges), "collaboration edges on Sailor's works\n")
      cat("Found", nrow(collaborators), "collaborators\n")
      
      collaborators
      
    }, error = function(e) {
      cat("Error in collaborators_data:", e$message, "\n")
      data.frame(name = character(0), genre = character(0), times = integer(0))
    })
  })
  
  output$plot_top_collaborators <- renderPlotly({
    data <- collaborators_data()
    if(nrow(data) == 0) {
      p <- ggplot() + 
        theme_void() + 
        labs(title = "No collaborator data available") +
        theme(plot.title = element_text(hjust = 0.5, size = 14))
    } else {
      p <- ggplot(data, aes(x = reorder(name, times), y = times,
                            text = paste("Artist:", name, "<br>Genre:", genre, "<br>Collaborations:", times))) +
        geom_col(fill = "darkorange", alpha = 0.8) +
        coord_flip() +
        labs(title = "Top Collaborators with Sailor Shift", x = "Artist", y = "Number of Collaborations") +
        theme_minimal() +
        theme(
          text = element_text(size = 12),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 14, hjust = 0.5)
        )
    }
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = TRUE) %>%
      layout(margin = list(l = 120, r = 50, t = 50, b = 50))
  })
  
  output$collaborators_table <- renderDT({
    data <- collaborators_data()
    if(nrow(data) > 0) {
      data %>%
        rename("Artist Name" = name, "Genre" = genre, "Collaboration Count" = times)
    } else {
      data.frame("Artist Name" = character(0), "Genre" = character(0), "Collaboration Count" = integer(0))
    }
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # =============================================================================
  # OCEANUS FOLK INFLUENCE NETWORK - 方法1：响应式数据 + 事件响应
  # =============================================================================
  
  # 1. 创建响应式数据函数，提供默认值
  oceanus_network_data <- reactive({
    # 提供默认值以防输入为NULL
    include_covers <- if(is.null(input$oceanus_include_covers)) TRUE else input$oceanus_include_covers
    include_samples <- if(is.null(input$oceanus_include_samples)) TRUE else input$oceanus_include_samples
    include_style <- if(is.null(input$oceanus_include_style)) TRUE else input$oceanus_include_style
    
    tryCatch({
      # 查找 Oceanus Folk 节点
      oceanus_ids <- nodes_tbl %>% 
        filter(str_detect(tolower(genre), "oceanus folk")) %>% 
        pull(id)
      
      cat("Found", length(oceanus_ids), "Oceanus Folk nodes\n")
      
      if(length(oceanus_ids) == 0) {
        return(list(
          nodes = data.frame(id = 1, label = "No Oceanus Folk Data", group = "Unknown", 
                             title = "No Oceanus Folk data found", size = 30, color = "#ff9999"),
          edges = data.frame(from = integer(0), to = integer(0), title = character(0), color = character(0))
        ))
      }
      
      # 根据用户选择构建边类型
      edge_types <- character(0)
      if(include_covers) {
        edge_types <- c(edge_types, "CoverOf")
      }
      if(include_samples) {
        edge_types <- c(edge_types, "DirectlySamples", "InterpolatesFrom")
      }
      if(include_style) {
        edge_types <- c(edge_types, "InStyleOf", "LyricalReferenceTo")
      }
      
      if(length(edge_types) == 0) {
        return(list(
          nodes = data.frame(id = 1, label = "No Edge Types Selected", group = "Error", 
                             title = "Please select at least one influence type", size = 30, color = "#ff9999"),
          edges = data.frame(from = integer(0), to = integer(0), title = character(0), color = character(0))
        ))
      }
      
      cat("Using edge types:", paste(edge_types, collapse = ", "), "\n")
      
      # 查找影响边
      influence_edges <- edges_tbl %>%
        filter(source %in% oceanus_ids | target %in% oceanus_ids) %>%
        filter(`Edge Type` %in% edge_types)
      
      cat("Found", nrow(influence_edges), "influence edges\n")
      
      if(nrow(influence_edges) == 0) {
        return(list(
          nodes = data.frame(id = oceanus_ids[1], label = "Oceanus Folk (No Connections)", 
                             group = "Oceanus Folk", title = "No influence connections found", 
                             size = 30, color = "#a6cee3"),
          edges = data.frame(from = integer(0), to = integer(0), title = character(0), color = character(0))
        ))
      }
      
      # 获取所有相关节点
      related_node_ids <- unique(c(influence_edges$source, influence_edges$target))
      
      sub_nodes <- nodes_tbl %>%
        filter(id %in% related_node_ids) %>%
        mutate(
          index = row_number(),
          label = name,
          group = coalesce(genre, `Node Type`),
          title = paste0("<b>", name, "</b><br>Type: ", `Node Type`, 
                         "<br>Genre: ", coalesce(genre, "Unknown"),
                         "<br>Notable: ", coalesce(notable, FALSE)),
          size = case_when(
            str_detect(tolower(coalesce(genre, "")), "oceanus folk") ~ 25,
            `Node Type` == "Song" ~ 20,
            `Node Type` == "Album" ~ 22,
            `Node Type` == "Person" ~ 18,
            TRUE ~ 15
          ),
          color = case_when(
            str_detect(tolower(coalesce(genre, "")), "oceanus folk") ~ "#1f78b4",  # 蓝色 - Oceanus Folk
            `Node Type` == "Song" ~ "#33a02c",   # 绿色 - 歌曲
            `Node Type` == "Album" ~ "#e31a1c",  # 红色 - 专辑
            `Node Type` == "Person" ~ "#ff7f00", # 橙色 - 人物
            TRUE ~ "#6a3d9a"  # 紫色 - 其他
          )
        )
      
      cat("Found", nrow(sub_nodes), "related nodes\n")
      
      if(nrow(sub_nodes) == 0) {
        return(list(
          nodes = data.frame(id = 1, label = "No Network Data", group = "Unknown", 
                             title = "No related nodes found", size = 30, color = "#ff9999"),
          edges = data.frame(from = integer(0), to = integer(0), title = character(0), color = character(0))
        ))
      }
      
      # 创建ID映射
      id_map <- sub_nodes %>% select(id, index)
      
      # 构建可视化边
      edges_vis <- influence_edges %>%
        left_join(id_map, by = c("source" = "id")) %>%
        rename(from = index) %>%
        left_join(id_map, by = c("target" = "id")) %>%
        rename(to = index) %>%
        filter(!is.na(from), !is.na(to)) %>%
        mutate(
          title = paste("Relationship:", `Edge Type`),
          color = case_when(
            `Edge Type` == "CoverOf" ~ "#1f78b4",
            `Edge Type` == "DirectlySamples" ~ "#33a02c", 
            `Edge Type` == "InterpolatesFrom" ~ "#e31a1c",
            `Edge Type` == "InStyleOf" ~ "#ff7f00",
            `Edge Type` == "LyricalReferenceTo" ~ "#6a3d9a",
            TRUE ~ "#999999"
          )
        ) %>%
        select(from, to, title, color)
      
      # 更新节点ID为索引
      final_nodes <- sub_nodes %>%
        mutate(id = index) %>%
        select(id, label, group, title, size, color)
      
      cat("Final network: ", nrow(final_nodes), "nodes,", nrow(edges_vis), "edges\n")
      
      list(
        nodes = final_nodes,
        edges = edges_vis
      )
      
    }, error = function(e) {
      cat("Error in oceanus_network_data:", e$message, "\n")
      list(
        nodes = data.frame(id = "error", label = paste("Error:", e$message), group = "Error", 
                           title = paste("Error:", e$message), size = 30, color = "#ff0000"),
        edges = data.frame(from = character(0), to = character(0), title = character(0), color = character(0))
      )
    })
  })
  
  # 2. 初始化渲染 - 页面加载时显示默认网络
  output$oceanus_network <- renderVisNetwork({
    # 使用固定的默认值进行初始化
    network_data <- tryCatch({
      oceanus_ids <- nodes_tbl %>% 
        filter(str_detect(tolower(genre), "oceanus folk")) %>% 
        pull(id)
      
      if(length(oceanus_ids) > 0) {
        influence_edges <- edges_tbl %>%
          filter(source %in% oceanus_ids | target %in% oceanus_ids) %>%
          filter(`Edge Type` %in% c("InStyleOf", "LyricalReferenceTo", "CoverOf", "InterpolatesFrom", "DirectlySamples"))
        
        if(nrow(influence_edges) > 0) {
          related_node_ids <- unique(c(influence_edges$source, influence_edges$target))
          
          sub_nodes <- nodes_tbl %>%
            filter(id %in% related_node_ids) %>%
            mutate(
              index = row_number(),
              label = name,
              group = coalesce(genre, `Node Type`),
              title = paste0("<b>", name, "</b><br>Type: ", `Node Type`, "<br>Genre: ", coalesce(genre, "Unknown")),
              size = case_when(
                str_detect(tolower(coalesce(genre, "")), "oceanus folk") ~ 25,
                `Node Type` == "Song" ~ 20,
                TRUE ~ 15
              ),
              color = case_when(
                str_detect(tolower(coalesce(genre, "")), "oceanus folk") ~ "#1f78b4",
                `Node Type` == "Song" ~ "#33a02c",
                TRUE ~ "#6a3d9a"
              )
            )
          
          if(nrow(sub_nodes) > 0) {
            id_map <- sub_nodes %>% select(id, index)
            
            edges_vis <- influence_edges %>%
              left_join(id_map, by = c("source" = "id")) %>%
              rename(from = index) %>%
              left_join(id_map, by = c("target" = "id")) %>%
              rename(to = index) %>%
              filter(!is.na(from), !is.na(to)) %>%
              mutate(title = paste("Relationship:", `Edge Type`), color = "#999999") %>%
              select(from, to, title, color)
            
            final_nodes <- sub_nodes %>%
              mutate(id = index) %>%
              select(id, label, group, title, size, color)
            
            list(nodes = final_nodes, edges = edges_vis)
          } else {
            list(
              nodes = data.frame(id = 1, label = "No Network Data", group = "Unknown", 
                                 title = "No nodes found", size = 30, color = "#ff9999"),
              edges = data.frame(from = integer(0), to = integer(0), title = character(0), color = character(0))
            )
          }
        } else {
          list(
            nodes = data.frame(id = 1, label = "No Oceanus Folk Connections", group = "Oceanus Folk", 
                               title = "No influence connections found", size = 30, color = "#a6cee3"),
            edges = data.frame(from = integer(0), to = integer(0), title = character(0), color = character(0))
          )
        }
      } else {
        list(
          nodes = data.frame(id = 1, label = "No Oceanus Folk Data", group = "Unknown", 
                             title = "No Oceanus Folk nodes found", size = 30, color = "#ff9999"),
          edges = data.frame(from = integer(0), to = integer(0), title = character(0), color = character(0))
        )
      }
    }, error = function(e) {
      list(
        nodes = data.frame(id = 1, label = paste("Error:", e$message), group = "Error", 
                           title = paste("Error:", e$message), size = 30, color = "#ff0000"),
        edges = data.frame(from = integer(0), to = integer(0), title = character(0), color = character(0))
      )
    })
    
    visNetwork(network_data$nodes, network_data$edges) %>%
      visEdges(arrows = "to", color = list(color = "#999999", highlight = "#ff0000")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visInteraction(navigationButtons = TRUE, dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) %>%
      visLegend(useGroups = TRUE, position = "right", main = "Node Types") %>%
      visLayout(randomSeed = 42) %>%
      visPhysics(stabilization = list(iterations = 100))
  })
  
  # 3. 事件响应 - 监听按钮点击，使用响应式数据更新网络
  observeEvent(input$update_oceanus_network, {
    output$oceanus_network <- renderVisNetwork({
      # 使用响应式数据
      network_data <- oceanus_network_data()
      
      # 创建更新后的网络，可以有不同的配置
      visNetwork(network_data$nodes, network_data$edges) %>%
        visEdges(arrows = "to", color = list(color = "#999999", highlight = "#ff0000")) %>%
        visOptions(
          highlightNearest = TRUE, 
          nodesIdSelection = TRUE,
          selectedBy = "group"
        ) %>%
        visInteraction(
          navigationButtons = TRUE,
          dragNodes = TRUE,
          dragView = TRUE,
          zoomView = TRUE
        ) %>%
        visLayout(randomSeed = sample(1:1000, 1)) %>%  # 随机种子，获得不同布局
        visPhysics(
          stabilization = list(iterations = 200),
          barnesHut = list(
            gravitationalConstant = -8000, 
            springConstant = 0.002, 
            springLength = 200
          )
        ) %>%
        visLegend(
          useGroups = TRUE,
          position = "right",
          main = "Node Types (Updated)"
        )
    })
    
    # 显示更新提示
    showNotification("Oceanus Folk network updated!", type = "message", duration = 3)
  })
  
  # =============================================================================
  # DATA SOURCE - 数据展示功能
  # =============================================================================
  
  # 1. 创建响应式数据函数
  data_view_reactive <- reactive({
    # 提供默认值以防输入为NULL
    view_type <- if(is.null(input$data_view_type)) "nodes" else input$data_view_type
    rows_display <- if(is.null(input$data_rows_display)) 20 else input$data_rows_display
    
    if(view_type == "nodes") {
      data <- nodes_tbl %>%
        head(rows_display) %>%
        select(id, name, `Node Type`, genre, notable, release_date, written_date)
    } else {
      data <- edges_tbl %>%
        head(rows_display) %>%
        select(source, target, `Edge Type`)
    }
    
    data
  })
  
  # 2. 初始化渲染 - 页面加载时显示默认数据
  output$data_table <- renderDT({
    # 使用固定的默认值进行初始化
    default_data <- nodes_tbl %>%
      head(20) %>%
      select(id, name, `Node Type`, genre, notable, release_date, written_date)
    
    datatable(
      default_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searching = TRUE,
        ordering = TRUE,
        info = TRUE,
        lengthMenu = c(5, 10, 15, 20),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = 'cell-border stripe',
      rownames = FALSE,
      caption = "MC1 Graph - Nodes Data"
    )
  })
  
  output$data_summary <- renderText({
    nodes_count <- nrow(nodes_tbl)
    edges_count <- nrow(edges_tbl)
    
    paste0(
      "Total Nodes: ", nodes_count, "\n",
      "Total Edges: ", edges_count, "\n",
      "Data loaded successfully from MC1_graph.json"
    )
  })
  
  # 3. 事件响应 - 监听按钮点击，使用响应式数据更新表格
  observeEvent(input$update_data_view, {
    output$data_table <- renderDT({
      # 使用响应式数据
      data <- data_view_reactive()
      
      # 根据数据类型设置不同的标题
      caption_text <- if(input$data_view_type == "nodes") {
        paste0("MC1 Graph - Nodes Data (Showing ", nrow(data), " rows)")
      } else {
        paste0("MC1 Graph - Edges Data (Showing ", nrow(data), " rows)")
      }
      
      datatable(
        data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          info = TRUE,
          lengthMenu = c(5, 10, 15, 20),
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = caption_text
      )
    })
    
    # 更新数据摘要
    output$data_summary <- renderText({
      nodes_count <- nrow(nodes_tbl)
      edges_count <- nrow(edges_tbl)
      view_type <- input$data_view_type
      rows_display <- input$data_rows_display
      
      if(view_type == "nodes") {
        paste0(
          "Viewing: Nodes\n",
          "Displaying: ", min(rows_display, nodes_count), " of ", nodes_count, " total nodes\n",
          "Total Edges: ", edges_count
        )
      } else {
        paste0(
          "Viewing: Edges\n", 
          "Displaying: ", min(rows_display, edges_count), " of ", edges_count, " total edges\n",
          "Total Nodes: ", nodes_count
        )
      }
    })
    
    # 显示更新提示
    showNotification("Data view updated!", type = "message", duration = 2)
  })
  
  output$plot_oceanus_timeline <- renderPlotly({
    # 数据准备
    all_folk <- nodes_tbl %>%
      filter(`Node Type` == "Song", genre == "Oceanus Folk", !is.na(release_date)) %>%
      mutate(release_year = as.integer(release_date),
             notable = FALSE) %>%
      count(release_year, notable, name = "count")
    
    notable_folk <- nodes_tbl %>%
      filter(`Node Type` == "Song", genre == "Oceanus Folk", notable == TRUE, !is.na(release_date)) %>%
      mutate(release_year = as.integer(release_date),
             notable = TRUE) %>%
      count(release_year, notable, name = "count")
    
    # 合并数据
    trend_data <- bind_rows(all_folk, notable_folk) %>%
      mutate(type = ifelse(notable, "Notable Oceanus Folk", "All Oceanus Folk"))
    
    # 根据时间范围过滤数据
    if(!is.null(input$oceanus_time_range)) {
      trend_data <- trend_data %>%
        filter(release_year >= input$oceanus_time_range[1],
               release_year <= input$oceanus_time_range[2])
    }
    
    # 检查是否有数据
    if(nrow(trend_data) == 0) {
      p <- ggplot() + 
        theme_void() + 
        labs(title = "No Oceanus Folk data available in selected range") +
        theme(plot.title = element_text(hjust = 0.5, size = 14))
    } else {
      # 创建 ggplot 图，加 group
      p <- ggplot(trend_data, aes(x = release_year, y = count, color = type, group = type,
                                  text = paste0("Year: ", release_year,
                                                "<br>Count: ", count,
                                                "<br>Type: ", type))) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        geom_vline(xintercept = 2028, linetype = "dashed", color = "gray40", linewidth = 1) +
        annotate("text", x = 2028, y = max(trend_data$count), label = "Sailor viral", vjust = -0.5, hjust = 1.1) +
        labs(title = "Interactive Trend of Oceanus Folk Songs",
             x = "Release Year", y = "Song Count", color = "Type") +
        theme_minimal() +
        theme(
          text = element_text(size = 12),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 14, hjust = 0.5)
        )
    }
    
    # 转为交互式
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = TRUE) %>%
      layout(margin = list(l = 50, r = 50, t = 50, b = 50))
  })
}

# Run the app
shinyApp(ui = ui, server = server)