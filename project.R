# =============================================================================
# MINI DIAMONDS EXPLORER - Shiny App
# =============================================================================

# Install packages if needed
# install.packages(c("shiny", "ggplot2", "dplyr"))

library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  
  # App title
  titlePanel("💎 Multivariate Analysis of Diamond Pricing Factors", windowTitle = "Diamonds EDA"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Data Filters"),
      
      # Carat filter
      sliderInput("carat", "Carat Range:",
                  min = 0.2, max = 5, value = c(0.5, 2)),
      
      # Price filter
      sliderInput("price", "Price Range ($):",
                  min = 326, max = 18823, value = c(500, 5000)),
      
      # Cut filter
      checkboxGroupInput("cut", "Cut Quality:",
                         choices = c("Fair", "Good", "Very Good", "Premium", "Ideal"),
                         selected = c("Fair", "Good", "Very Good", "Premium", "Ideal")),
      
      # Plot type selector
      selectInput("plot_type", "Choose Visualization:",
                  choices = c("Price vs Carat" = "scatter",
                              "Price Distribution" = "histogram",
                              "Price by Cut" = "boxplot",
                              "Price per Carat" = "price_per_carat")),
      
      # Color by selector
      selectInput("color_by", "Color Points By:",
                  choices = c("Cut" = "cut", 
                              "Color" = "color", 
                              "Clarity" = "clarity",
                              "None" = "none")),
      
      # Sample size for performance
      numericInput("sample_size", "Sample Size:", 
                   value = 2000, min = 500, max = 10000),
      
      # Info text
      tags$div(
        style = "margin-top: 20px; font-size: 12px; color: #666;",
        "💡 Tip: Notice how Ideal cut diamonds have lower prices? Check 'Price per Carat'!"
      )
    ),
    
    # Main panel
    mainPanel(
      width = 9,
      
      # Value boxes row
      fluidRow(
        column(3, 
               wellPanel(style = "text-align: center; background-color: #e8f4f8;",
                         h4("Diamonds"),
                         textOutput("count_diamonds"))),
        column(3,
               wellPanel(style = "text-align: center; background-color: #e8f8f0;",
                         h4("Avg Price"),
                         textOutput("avg_price"))),
        column(3,
               wellPanel(style = "text-align: center; background-color: #f8f0e8;",
                         h4("Avg Carat"),
                         textOutput("avg_carat"))),
        column(3,
               wellPanel(style = "text-align: center; background-color: #f8e8f8;",
                         h4("Avg $/Carat"),
                         textOutput("avg_ppc")))
      ),
      
      # Plot
      plotOutput("diamonds_plot", height = "500px"),
      
      # Summary stats
      verbatimTextOutput("summary_stats")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    data <- diamonds %>%
      filter(carat >= input$carat[1], carat <= input$carat[2],
             price >= input$price[1], price <= input$price[2],
             cut %in% input$cut)
    
    # Sample for performance
    if (nrow(data) > input$sample_size) {
      set.seed(123)
      data <- sample_n(data, input$sample_size)
    }
    
    # Add price per carat
    data$price_per_carat <- data$price / data$carat
    
    return(data)
  })
  
  # Value box outputs
  output$count_diamonds <- renderText({
    format(nrow(filtered_data()), big.mark = ",")
  })
  
  output$avg_price <- renderText({
    paste0("$", format(round(mean(filtered_data()$price)), big.mark = ","))
  })
  
  output$avg_carat <- renderText({
    round(mean(filtered_data()$carat), 2)
  })
  
  output$avg_ppc <- renderText({
    paste0("$", format(round(mean(filtered_data()$price / filtered_data()$carat)), big.mark = ","))
  })
  
  # Main plot
  output$diamonds_plot <- renderPlot({
    data <- filtered_data()
    
    # Base plot
    if (input$plot_type == "scatter") {
      p <- ggplot(data, aes(x = carat, y = price))
      
      if (input$color_by == "none") {
        p <- p + geom_point(alpha = 0.6, color = "steelblue", size = 2)
      } else {
        p <- p + geom_point(alpha = 0.6, aes_string(color = input$color_by), size = 2)
      }
      
      p <- p + 
        labs(title = "Diamond Price vs Carat",
             subtitle = "Explore the relationship between size and price",
             x = "Carat", y = "Price (USD)") +
        scale_y_continuous(labels = scales::dollar)
      
    } else if (input$plot_type == "histogram") {
      p <- ggplot(data, aes(x = price)) +
        geom_histogram(binwidth = 500, fill = "coral", alpha = 0.7, color = "white") +
        labs(title = "Distribution of Diamond Prices",
             x = "Price (USD)", y = "Count") +
        scale_x_continuous(labels = scales::dollar)
      
    } else if (input$plot_type == "boxplot") {
      p <- ggplot(data, aes(x = cut, y = price, fill = cut)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = "Diamond Prices by Cut Quality",
             subtitle = "Notice: Fair cut diamonds have higher prices than Ideal cuts?",
             x = "Cut Quality", y = "Price (USD)") +
        scale_y_continuous(labels = scales::dollar) +
        scale_fill_brewer(palette = "Set2") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$plot_type == "price_per_carat") {
      p <- ggplot(data, aes(x = cut, y = price_per_carat, fill = cut)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = "Price per Carat by Cut Quality",
             subtitle = "Revealing the true value: Ideal cuts cost more per carat!",
             x = "Cut Quality", y = "Price per Carat (USD)") +
        scale_y_continuous(labels = scales::dollar) +
        scale_fill_brewer(palette = "Set2") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    # Common theme elements
    p + 
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(color = "gray40"),
            legend.position = "right")
  })
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    data <- filtered_data()
    cat("=== QUICK SUMMARY ===\n")
    cat("Total diamonds:", format(nrow(data), big.mark = ","), "\n")
    cat("Price range: $", format(min(data$price), big.mark = ","), 
        " - $", format(max(data$price), big.mark = ","), "\n", sep = "")
    cat("Carat range: ", round(min(data$carat), 2), 
        " - ", round(max(data$carat), 2), "\n", sep = "")
    cat("Avg price per carat: $", round(mean(data$price/data$carat)), "\n", sep = "")
    
    # The key insight
    if (input$plot_type %in% c("boxplot", "price_per_carat")) {
      cat("\n💡 KEY INSIGHT:\n")
      cat("Fair cut diamonds appear more expensive in raw prices,\n")
      cat("but Ideal cuts have higher value when comparing price per carat!\n")
      cat("This shows why we need to control for carat size in our analysis.")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)