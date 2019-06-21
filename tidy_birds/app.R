#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

bird_counts <- read_csv("data/bird_counts.csv")

birds = list(bird_counts%>%select(species)%>%unique())[[1]]
year = list(bird_counts%>%select(year)%>%unique())[[1]]

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Analyzing the canadian birds data set"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Birds",
                        "Birds:",
                        birds$species,
                        selected='Canada Goose',
                        multiple = TRUE),
            selectInput("Year",
                        "Year:",
                        year$year,
                        selected = 2017)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("timeline",height = "50%"),
           plotlyOutput("bar",height = "50%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$timeline <- renderPlotly({
        # generate bins based on input$bins from ui.R
        ggplotly(bird_counts%>%
                     filter(species %in% c(input$Birds))%>%
                     group_by(year,species)%>%
                     summarise(birds=sum(how_many_counted))%>%
                     ggplot(aes(x=year,y=birds,group=species,color=species))+
                     geom_line()+ theme(plot.subtitle = element_text(vjust = 1), 
                                        plot.caption = element_text(vjust = 1), 
                                        panel.background = element_rect(fill = NA), 
                                        legend.position = "bottom", legend.direction = "horizontal") +labs(title = "Change in bird sitings over time", 
                                                                                                           y = "Number of birst", colour = "Species of bird")
            )
    
                 })
    output$bar <- renderPlotly({
        # generate bins based on input$bins from ui.R
        ggplotly(bird_counts%>%
                     filter(year == input$Year)%>%
                     group_by(species)%>%
                     summarise(birds=sum(how_many_counted))%>%
                     top_n(25)%>%
                     ggplot(aes(x=reorder(species,birds),y=birds))+
                     geom_bar(stat='identity')+
                     coord_flip()+ theme(plot.subtitle = element_text(vjust = 1), 
                                         plot.caption = element_text(vjust = 1), 
                                         panel.background = element_rect(fill = NA)) +labs(title = "Top 25 birds in selected year", 
                                                                                           x = "Top birds (highest counted to lowest)", 
                                                                                           y = "Birds sighted")
        )
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
