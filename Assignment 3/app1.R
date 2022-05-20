library(ggplot2)
library(dplyr)
library(shiny)


df2 <- read.csv('https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv')
df2010 <- df2 %>%
    filter(Year == 2010)


df <- read.csv('https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/Sample%20Code/hpi.csv')
df$DATE <- as.POSIXct(strptime(df$DATE, format = '%m/%d/%y'))

ui <- fluidPage(
    headerPanel('State Mortality by Cause of Death'),
    sidebarPanel(
        selectInput('cause', 'Cause of Death', unique(df2010$ICD.Chapter))
    ),
    mainPanel(
        tableOutput('table2')
    )
)

server <- shinyServer(function(input, output, session) {
    
    selectedData <- reactive({
        dfSlice <- df2010 %>%
            filter(ICD.Chapter == input$cause)
    })
    
    output$table2 <- renderTable({
        
        dfx <- df2010 %>%
            filter(ICD.Chapter == input$cause) %>%
            select(State, Crude.Rate) %>%
            arrange(Crude.Rate)
        dfx

    })
    
    output$plot2 <- renderPlot({
        
        dfSlice <- df2010 %>%
            filter(ICD.Chapter == input$cause)
        
        ggplot(selectedData(), aes(x = State, y = Crude.Rate)) +
            geom_bar()
    })
    
})

shinyApp(ui = ui, server = server)