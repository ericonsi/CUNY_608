#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

set.seed(042760)
library(boot)
library(tidyverse)

home_sales <- read.csv("D:\\RStudio\\605_Final\\Final\\housing\\train.csv", stringsAsFactors=TRUE, header = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Learning about bootstrap using home sale price dataset"),

    # Sidebar with a slider input for number of bins 
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        sliderInput("rows",
                    "rows",
                    min = 2,
                    max = 1460,
                    value = 100),
        
        sliderInput("reps",
                    "reps",
                    min = 5,
                    max = 2000,
                    value = 500)
        
      ),
      
      
    sidebarLayout(
        sidebarPanel(),
        
        
        # Show a plot of the generated distribution
        mainPanel("Normal curve generated from sample of n rows of dataset",
                  fluidRow(
                    splitLayout(cellWidths = c("33%", "67%"), plotOutput("distPlot"), plotOutput("distPlot2"))
                  ),
                  tableOutput("distPlot3"),
                  tableOutput("distPlot4")
        )  
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
    
    zz <- home_sales$SalePrice[1:input$rows]
    
    qmean <- mean(zz)
    qn <- input$rows
    qsd <- sd(x)
      
    p1 <- ggplot(data = data.frame(x = c(-3*qmean, 3*qmean)), aes(x)) +
      stat_function(fun = dnorm, n = qn, args = list(mean = qmean, sd = qsd)) + ylab("") +
      scale_y_continuous(breaks = NULL)
    p1

    })
    
    output$distPlot2 <- renderPlot({
      
      x <- home_sales$SalePrice[1:input$rows]
      library(boot)
      
      boot_mean <- function(original_vector, resample_vector) {
        mean(original_vector[resample_vector])
      }
      
      #Number of reps must be more than number of data rows
      reps <- boot(x, boot_mean, R = input$reps)
      plot(reps)

    })
    output$distPlot3 <- renderTable({
      
      x <- home_sales$SalePrice[1:input$rows]

      library(broom)
      library(purrr)
      
      b <- t.test(x, conf.level=0.95)
      tab <- map_df(list(b), tidy)
      tab
      
      
    })
    output$distPlot4 <- renderTable({
      
      x <- home_sales$SalePrice[1:input$rows]
      library(boot)
      
      boot_mean <- function(original_vector, resample_vector) {
        mean(original_vector[resample_vector])
      }
      
      #Number of reps must be more than number of data rows
      reps <- boot(x, boot_mean, R = input$reps)
      
      c <- boot.ci(reps)
      d <- as.data.frame(c$t0, c$bca)
      e <- as.data.frame(cbind(d, c$bca))
      e
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
