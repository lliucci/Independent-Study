library(shiny)

Courses <- c("M105Q", "M121Q", "M151Q", "M161Q", "M165Q", "M171Q", "M172Q", "M182Q", "M221", "M273Q", "M274", "STAT 216Q", "STAT 217Q")

Questions <- c("Trends by Major",
               "Busiest Times by Class")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MSC Usage"),
navlistPanel(
  tabPanel("Class Density",
           sidebarLayout(
             sidebarPanel(
               selectInput("class_specified", "Class of Interest", choices = Courses)
             ),
             mainPanel()
           )),
  tabPanel("Overall Density",
           sidebarLayout(
             sidebarPanel(
               selectInput("density_type", "Density of Interest", choices = c("Hourly", "Daily", "Weekly"))
             ),
             mainPanel()
           )),
  tabPanel("Other",
           sidebarLayout(
             sidebarPanel(
               radioButtons("question_specified", "Question of Interest", choices = Questions)
             ),
             mainPanel()
           ))
  
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
