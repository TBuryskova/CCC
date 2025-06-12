library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggraph)
library(tidygraph)
library(igraph)

# 1. Load data ----------------------------------------------------
edges_cumulative_clean <- read_rds("edges_cumulative_clean.rds")
all_months_clean       <- read_rds("all_months_clean.rds")

base_theme <- theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "none"
  )

node_point_size  <- 3
node_text_size   <- 5
edge_alpha       <- 0.40
edge_width_range <- c(0.3, 2)
node_color       <- "black"

nodes_clean <- tibble(name = unique(c(edges_cumulative_clean$j1, edges_cumulative_clean$j2)))

# Precompute full graph and layout
g_clean     <- tbl_graph(nodes = nodes_clean, edges = edges_cumulative_clean, directed = FALSE)
# 1. Precompute layout for full graph
layout_clean <- create_layout(g_clean, layout = "stress")

# 2. Extract node positions
node_coords <- layout_clean %>%
  as_tibble() %>%
  select(name, x, y) %>%
  mutate(.ggraph.index = 1:n())  # required by ggraph layout


edges_cumulative<- read_rds("edges_cumulative.rds")
all_months      <- read_rds("all_months.rds")


nodes<- tibble(name = unique(c(edges_cumulative$j1, edges_cumulative$j2)))

# Precompute full graph and layout
g <- tbl_graph(nodes = nodes, edges = edges_cumulative, directed = FALSE)
# 1. Precompute layout for full graph
layout<- create_layout(g, layout = "stress")

# 2. Extract node positions
node_coords <- layout %>%
  as_tibble() %>%
  select(name, x, y) %>%
  mutate(.ggraph.index = 1:n())  # required by ggraph layout

# Get global edge width range
global_edge_range <- range(edges_cumulative$n_cases_together, na.rm = TRUE)




# 2. UI ----------------------------------------------------------
ui <- fluidPage(
  tags$style(HTML("
    .irs-grid-text {
      transform: rotate(45deg);
      transform-origin: top left;
      font-size: 10px;
      white-space: nowrap;
    }
  ")),
  sidebarLayout(
    sidebarPanel(
      sliderTextInput("month", "Select Month:",
                      choices = format(all_months_clean, "%b %Y"),
                      selected = format(all_months_clean[1], "%b %Y"),
                      grid = TRUE,
                      animate = animationOptions(interval = 500, loop = TRUE) ),
      
      radioButtons("graph_choice", "Select:",
                   choices = c("Clean sample" = "clean", "Basic sample" = "basic"),
                   selected = "clean")
                      
     
    ),
    mainPanel(
      uiOutput("graphOutput")
      
      
    )
  )
)

# 3. Server -------------------------------------------------------
server <- function(input, output, session) {
  
  selected_month <- reactive({
    parse_date_time(input$month, orders = "b Y")  # from lubridate
  })
  output$graphOutput <- renderUI({
    req(input$graph_choice)  # wait until input$graph_choice is available
    if (input$graph_choice == "clean") {
      plotOutput("networkPlot", height = "400px")
    } else {
      plotOutput("networkPlot2", height = "400px")
    }
  })
  
  output$networkPlot <- renderPlot({
    req(input$graph_choice == "clean")
    # Filter edges by month
    e <- edges_cumulative_clean %>%
      filter(month <= selected_month())
    
    # Create filtered graph
    g_filtered <- tbl_graph(nodes = nodes_clean, edges = e, directed = FALSE)
    
    # Keep only node coords for nodes in filtered graph
    active_names <- g_filtered %>% activate(nodes) %>% pull(name)
    layout_filtered <- node_coords %>%
      filter(name %in% active_names) %>%
      mutate(.ggraph.index = match(name, active_names))  # index must match node order
    
    # Add manual layout
    ggraph(g_filtered, layout = "manual", x = layout_filtered$x, y = layout_filtered$y) +
      geom_edge_link(aes(width = n_cases_together), alpha = edge_alpha) +
      geom_node_point(size = node_point_size, color = node_color) +
      geom_node_text(aes(label = name), size = node_text_size, vjust = 2, hjust = 0.45) +
      scale_edge_width(
        limits = range(edges_cumulative_clean$n_cases_together, na.rm = TRUE),
        range = edge_width_range
      ) +
      base_theme +
      labs(title = paste("Judicial Co-decision Network (clean sample):", format(selected_month(), "%Y-%m")))
  })
  
  
  output$networkPlot2 <- renderPlot({
    req(input$graph_choice == "basic") 
    # Filter edges by month
    e <- edges_cumulative %>%
      filter(month <= selected_month())
    
    # Create filtered graph
    g_filtered <- tbl_graph(nodes = nodes, edges = e, directed = FALSE)
    
    # Keep only node coords for nodes in filtered graph
    active_names <- g_filtered %>% activate(nodes) %>% pull(name)
    layout_filtered <- node_coords %>%
      filter(name %in% active_names) %>%
      mutate(.ggraph.index = match(name, active_names))  # index must match node order
    
    # Add manual layout
    ggraph(g_filtered, layout = "manual", x = layout_filtered$x, y = layout_filtered$y) +
      geom_edge_link(aes(width = n_cases_together), alpha = edge_alpha) +
      geom_node_point(size = node_point_size, color = node_color) +
      geom_node_text(aes(label = name), size = node_text_size, vjust = 2, hjust = 0.45) +
      scale_edge_width(
        limits = range(edges_cumulative$n_cases_together, na.rm = TRUE),
        range = edge_width_range
      ) +
      base_theme +
      labs(title = paste("Judicial Co-decision Network (basic sample):", format(selected_month(), "%Y-%m")))
  })
}

# 4. Run ----------------------------------------------------------
shinyApp(ui, server)