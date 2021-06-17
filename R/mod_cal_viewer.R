#' cal_viewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom toastui calendarOutput renderCalendar calendar cal_props
mod_cal_viewer_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_12(
        calendarOutput(ns("calui"))
      )
    )
  )
}
    
#' cal_viewer Server Functions
#'
#' @noRd 
mod_cal_viewer_server <- function(id, cal_df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$calui <- renderCalendar({
      
      # process cal_df to conform to the proper structure
      # coalesce(contains(dtstart()))

      # `YYYY-MM-DD XX:YY:ZZ`
      
      cal_sub <- cal_df %>%
        mutate(start_clock = as_naive_time(start_clock), end_clock = as_naive_time(end_clock)) %>%
        mutate(start = as.character(start_clock), end = as.character(end_clock)) %>%
        select(., -start_clock, -end_clock)
      
      toastui::calendar(
        #toastui::cal_demo_data(), 
        cal_sub,
        useNavigation = TRUE
      ) %>%
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
 
  })
}
    
## To be copied in the UI
# mod_cal_viewer_ui("cal_viewer_ui_1")
    
## To be copied in the server
# mod_cal_viewer_server("cal_viewer_ui_1")
