#library(DT)
#library(ggplot2)
library(graphics)
library(grDevices)
library(methods)
#library(readr)
#library(stringr)
#library(readr)
library(stats)
library(utils)
library(dplyr)
#library(DT)
#library(ggplot2)
library(graphics)
library(grDevices)
library(methods)
#library(readr)
#library(stringr)
#library(readr)
library(stats)
library(utils)
read.delim("Underlying Cause of Death, 2018-2021, Single Race (2).txt")
data<-read.delim("Underlying Cause of Death, 2018-2021, Single Race (2).txt")
tidy_data<-data%>%
  dplyr::select(-State.Code,-Population,-Crude.Rate,-Notes)
df<-tidy_data

#checking Gender column
unique(df$Gender)
df<-subset(df, !is.na(Gender)& Gender!="")
df<-tidy_data[complete.cases(tidy_data),]#removing rows that are empty at end of data set


#checking Year column
unique(df$Year)
df<-df%>%
  filter(!is.na(Year))

#checking/fixing Place.of.Death column
unique(df$Place.of.Death)
df<-subset(df,Place.of.Death !="")

#checking Cause.of.death/fixing column
unique(df$Cause.of.death)
df<-subset(df,Cause.of.death !="")

#checking Deaths column
unique(df$Deaths)
df<-subset(df,Deaths!=9211) #removing particular row that does not have valuable data

range(df$Deaths)
write.csv(df, file = "final_data.csv", row.names = FALSE)
final_data<-write.csv(df, file = "final_data.csv", row.names = FALSE)


ui<-fluidPage(
  
  theme = shinytheme("sandstone"),
  titlePanel("Place and Underlying Cause of Death in Florida",
             strong("An In-Depth Look")),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "Year.Code",
                  label = "Death Year",
                  min = 2018, max=2021,value=2018
      ),
      
      selectInput(inputId = "Place.of.Death.Code",
                  label = "Death Place",
                  choices = unique(df$Place.of.Death)%>%sort())
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
  dat_filtered<-reactive({
    df%>%
      filter(Year==input$Year.Code,
             Place.of.Death==input$Place.of.Death.Code)
  })
  
  output$hist <- renderPlot({
    ggplot(dat_filtered(), aes(x=Cause.of.death, y=Deaths, fill=Gender)) +
      geom_col(position="dodge") +
      labs(x="Cause of Death", y="Number of Deaths", title="Death count by Cause and Gender") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$table <- renderDT({
    dat_filtered() %>%
      dplyr::select(Cause.of.death, Deaths,Gender) %>%
      datatable(options = list(pageLength = 10))
  })
  
  
  output$data_table <- renderDataTable({
    df
  })
}

shinyApp(ui = ui, server = server)
