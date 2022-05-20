#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(dplyr)
library(shiny)
library(plotly)
library(tidyr)
library(ggsci)
library(bbplot)

EHTheme <- function(){
    
    x <- theme(axis.text.y = element_text(size=10), axis.text.x = element_text(size=10), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="black"), panel.background = element_rect(fill = "white", color="slategray"))
    
    return (x)
}


df <- read.csv('https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv')


ui <- fluidPage(
    headerPanel('Change in Mortality Rate By State and Cause'),
    sidebarPanel(
        selectInput('cause', 'Cause of Death', unique(df$ICD.Chapter)),
        selectInput('state', 'State', unique(df$State)),
    ),
    mainPanel(
        plotlyOutput('plot1'),
        plotlyOutput('plot2')
    )
)

server <- shinyServer(function(input, output, session) {
    
    output$plot1 <- renderPlotly({
        
        dfx <- df %>%
            filter(ICD.Chapter == input$cause, State==input$state ) %>%
            rename(State_Rate = Crude.Rate)
        
        dfx2 <- df %>%
            filter(ICD.Chapter == input$cause) %>%
            group_by(Year, ICD.Chapter) %>%
            summarize(National_Rate= sum(Deaths)/sum(Population)*100000)
        
        dfx3 = merge(x=dfx,y=dfx2,by=c("ICD.Chapter", "Year"))
        
        dfx4 <- gather(dfx3, Average_Rate, measurement, State_Rate:National_Rate, factor_key=TRUE)
        
        p1 <- ggplot(data = dfx4, aes(x = Year, y=measurement, color=Average_Rate)) +
            geom_line(size=1.2) + 
            scale_color_d3() +
            ylab("Mortality per 100,000") +
            EHTheme() + ggtitle("State and National Mortality Rates Over time") +
            theme(
                legend.title = element_blank(),
                legend.text = element_text(size = 12),
                legend.position = "bottom"
            ) 
        
        q <- ggplotly(p1)
        q
        
    })
    
    output$plot2 <- renderPlotly({
        
        dfx <- df %>%
            filter(ICD.Chapter == input$cause, State==input$state ) %>%
            rename(State_Rate = Crude.Rate)
        
        dfx2 <- df %>%
            filter(ICD.Chapter == input$cause) %>%
            group_by(Year, ICD.Chapter) %>%
            summarize(National_Rate= sum(Deaths)/sum(Population)*100000)
        
        dfx3 = merge(x=dfx,y=dfx2,by=c("ICD.Chapter", "Year"))
        
        dfx4 <- gather(dfx3, Average_Rate, measurement, State_Rate:National_Rate, factor_key=TRUE)
        
        dfx5 <- dfx3 %>%
            mutate(Diff = State_Rate-National_Rate)
        
        p1 <- ggplot(data = dfx5, aes(x = Year, y=Diff)) +
            geom_line(size=1, color = "darkgreen") + 
            ylab("State Rate Minus National Rate") +
            ggtitle("Difference Between State and National Mortality Rates Over Time") +
            EHTheme() 
        
        q <- ggplotly(p1)
        q
        
    })
    
    
})

shinyApp(ui = ui, server = server)