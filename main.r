install.packages('rsconnect')
library(rsconnect)
library(DT)
library(dplyr)
library(ggplot2)
install.packages("shiny")
library(shiny)
library(shinythemes)
#library(shinydashboard)
#library(autoshiny)

rsconnect::setAccountInfo(name='clairebanks', token='6BE220DAA0D229ACE43FAF89E6C0F841', secret='Nwwm5zZ5UD/rnRzkRBWciRbX8d5XugACyWX6FQ7t')

data <- read.delim("Underlying Cause of Death, 2018-2021, Single Race (2).txt")
df <- data %>% 
  select(-State.Code, -Population, -Crude.Rate, -Notes)

# Remove rows with missing values
df <- df %>% 
  filter(!is.na(Gender) & Gender != "",
         !is.na(Year),
         Place.of.Death != "",
         Cause.of.death != "",
         Deaths != 9211)

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("Place and Underlying Cause of Death in Florida",
             strong("An In-Depth Look")),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "Year.Code",
                  label = "Death Year",
                  min = 2018, max = 2021, value = 2018),
      
      selectInput(inputId = "Place.of.Death.Code",
                  label = "Death Place",
                  choices = unique(df$Place.of.Death) %>% sort())
    ),
    h4(style = "font-weight:bold; font-style:italic;",
       "This interactive website allows you to explore data from Florida, USA pertaining to places deaths have taken place and causes of said deaths. Browse data pertaining to deaths in Florida using the slider bar to select the year (2018-2021) and the drop-down menu to select the place of death! The data presented in the histogram and table will be based on the parameters you have selected. On the 'Histogram' tab, you will see a plot of death counts by cause and gender, and on the 'Table' tab you will see a table of the same data. The 'Data' tab provides the original dataset.")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Histogram",
               plotOutput("hist"),
               br(),
      ),
      tabPanel("Table",
               DTOutput("table")
      ),
      tabPanel("Data",
               dataTableOutput("data_table")
      )
    )
  )
)

server <- function(input, output) {
  dat_filtered <- reactive({
    df %>% 
      filter(Year == input$Year.Code,
             Place.of.Death == input$Place.of.Death.Code)
  })
  
  output$hist <- renderPlot({
    ggplot(dat_filtered(), aes(x = Cause.of.death, y = Deaths, fill = Gender)) +
      geom_col(position = "dodge") 
      labs(x = "Cause of Death", y = "Number of Deaths", title = "Death count by Cause and Gender") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$table <- renderDT({
    dat_filtered() %>% 
      select(Cause.of.death, Deaths, Gender) %>% 
      datatable(options = list(pageLength = 10))
  })
  
  output$data_table <- renderDataTable({
    df
  })
}

shinyApp(ui = ui, server = server)

runApp()
