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
    titlePanel("Examining a normal curve"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "n",
                        min = 1,
                        max = 1000,
                        value = 1000),
            
            sliderInput("mean",
                        "mean",
                        min = 1,
                        max = 1000,
                        value = 500),
            
            sliderInput("sd",
                        "sd",
                        min = 1,
                        max = 1000,
                        value = 500)
      
        ),
        
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
                        value = 500),
            
            sliderInput("x",
                        "x",
                        min = 1,
                        max = 1000,
                        value = 500)
            
          ),
          
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot2"),
           tableOutput("distPlot3"),
           tableOutput("distPlot4"),
           plotOutput("distPlot")
        )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
    q <- rnorm(n=input$n, mean=input$mean, sd=input$sd/10)
    p1 <- ggplot(data = data.frame(x = c(-6*input$n, 6*input$n)), aes(x)) +
      stat_function(fun = dnorm, n = input$n, args = list(mean = input$mean, sd = input$sd)) + ylab("") +
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
      library(boot)
      
      boot_mean <- function(original_vector, resample_vector) {
        mean(original_vector[resample_vector])
      }
      
      #Number of reps must be more than number of data rows
      reps <- boot(x, boot_mean, R = input$reps)
      
      boot.ci(reps)

      library(broom)
      library(purrr)
      
      b <- t.test(home_sales$SalePrice, conf.level=0.95)
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
      d <- as.data.frame(c$t0, c$normal)
      e <- as.data.frame(cbind(d, c$normal))
      e
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
