#########################################################
## ANOVA and Analysis for Symphylan Treatments at Steinke Farm  Dashboard Visualization
## By: Connor Eck
#########################################################


library(reshape2)
library(ggplot2)
library(plotly)
library(ggpubr)
library(gridExtra)

library(renv)
library(shiny)
library(rsconnect)


# Define UI
ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel("Scatterplot",
               plotlyOutput("scatterplot")),
      tabPanel("Confidence Intervals",
               selectInput("treatment", "Treatment:", choices = c("Control", "Bifenture", "Torac", "Brigade", "Capture", "WarriorII", "Bifender FC")),
               verbatimTextOutput("result")),
      tabPanel("ANOVA",
               selectInput("anova_graph", "Select ANOVA Graph:",
                           choices = c("Bait.1", "Bait.2", "Bait.3", "Bait.1.1", "Bait.2.1", "Bait.3.1",
                                       "Bait.1.2", "Bait.2.2", "Bait.3.2")),
               plotlyOutput("anova_plot")),
      tabPanel("Correlation",
               selectInput("correlation_plot", "Select Correlation Plot:",
                           choices = c("Boxplot", "Barplot", "Scatterplot")),
               plotOutput("correlation_output"))
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Read in the data
  symph_data <- read.csv("filtered_symph_data.csv")
  
  # Included columns for melting and analysis
  included_columns <- c("Bait.1", "Bait.2", "Bait.3", "Bait.1.1", "Bait.2.1", "Bait.3.1", "Bait.1.2", "Bait.2.2", "Bait.3.2")
  
  # Melt the data for the scatterplot
  melted_data <- reshape2::melt(symph_data, id.vars = "Treatment", measure.vars = included_columns)
  
  # Function to calculate the confidence intervals
  calculateConfidenceIntervals <- function(data, treatment) {
    included_columns <- c("Bait.1", "Bait.2", "Bait.3", "Bait.1.1", "Bait.2.1", "Bait.3.1", "Bait.1.2", "Bait.2.2", "Bait.3.2")
    
    # Subset the data for the specified treatment and include the specified columns
    treatment_data <- data[data$Treatment == treatment, included_columns]
    
    # Calculate the means of the "Bait" columns within the specified treatment group
    means <- colMeans(treatment_data)
    
    # Calculate the standard errors for each "Bait" column
    standard_errors <- apply(treatment_data, 2, function(x) sd(x) / sqrt(length(x)))
    
    # Calculate the critical value for a 95% confidence level
    critical_value <- qt(0.975, nrow(treatment_data) - 1)
    
    # Calculate the margins of error for each "Bait" column
    margins_of_error <- critical_value * standard_errors
    
    # Calculate the lower and upper bounds of the confidence intervals for each "Bait" column
    lower_bounds <- means - margins_of_error
    upper_bounds <- means + margins_of_error
    
    # Create a data frame to store the confidence intervals
    confidence_intervals <- data.frame(Bait = included_columns, LowerBound = lower_bounds, UpperBound = upper_bounds)
    
    return(confidence_intervals)
  }
  
  # Update the result based on the selected confidence interval
  output$result <- renderPrint({
    treatment <- input$treatment
    confidence_intervals <- calculateConfidenceIntervals(symph_data, treatment)
    means <- colMeans(symph_data[symph_data$Treatment == treatment, included_columns])
    
    paste("Treatment:", treatment, "\n")
    print(confidence_intervals)
    paste("\nMeans:", "\n")
    print(means)
  })
  
  # Render the scatterplot
  output$scatterplot <- renderPlotly({
    ggplotly(ggplot(melted_data, aes(x = Treatment, y = value, color = variable)) +
               geom_point() +
               geom_smooth(method = "lm", se = FALSE) +
               labs(x = "Treatment", y = "Value", color = "Variable") +
               ggtitle("Treatment vs. Variables"))
  })
  
  # Render the ANOVA plot
  output$anova_plot <- renderPlotly({
    variable <- input$anova_graph
    
    # Define a vector of unique colors for the ANOVA graphs
    anova_colors <- c("blue", "red", "purple", "orange", "yellow", "green", "cyan", "black", "pink")
    
    # Visualize the selected variable by Treatment with color grouping by Treatment
    p <- ggplot(symph_data, aes(x = Treatment, y = symph_data[[variable]], fill = Treatment)) +
      geom_boxplot() +
      stat_compare_means(method = "anova") +
      scale_fill_manual(values = anova_colors) +
      ggtitle(paste("Comparison of", variable, "by Treatment")) +
      xlab("Treatment") +
      ylab(paste(variable, "Abundance"))
    
    ggplotly(p)
  })
  
  # Render the correlation plot
  output$correlation_output <- renderPlot({
    plot_type <- input$correlation_plot
    
    if (plot_type == "Boxplot") {
      boxplot(Germination ~ Treatment, data = symph_data,
              xlab = "Treatment", ylab = "Germination Count",
              main = "Treatment vs. Germination",
              col = "red")
    } else if (plot_type == "Barplot") {
      barplot(symph_data$Germination, names.arg = symph_data$Treatment,
              xlab = "Treatment", ylab = "Germination",
              main = "Treatment vs. Germination",
              col = "purple")
    } else if (plot_type == "Scatterplot") {
      # Assigns numbers to treatments
      treatment_numeric <- as.numeric(factor(symph_data$Treatment))
      
      # Creating a scatter plot with a line of best fit
      ggplot(data = symph_data, aes(x = treatment_numeric, y = Germination)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        xlab("Treatment") +
        ylab("Germination") +
        ggtitle("Treatment vs. Germination")
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
