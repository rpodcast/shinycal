#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(toastui)
library(calendar)

ics_df <- ic_read("../r_code/wimpys-world-of-streamers-2021-06-08.ics")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    fluidRow(
        column(
            width = 12,
            calendarOutput("testout", width = "100%", height = "600px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$testout <- renderCalendar({
        calendar(cal_demo_data(), useNavigation = TRUE) %>%
            cal_props(
                list(
                    id = 1,
                    name = "PERSO",
                    color = "white",
                    bgColor = "firebrick",
                    borderColor = "firebrick"
                ),
                list(
                    id = 2,
                    name = "WORK",
                    color = "white",
                    bgColor = "forestgreen",
                    borderColor = "forestgreen"
                )
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
