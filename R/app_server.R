#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  output$server <- renderText({ c("Server time:", as.character(Sys.time()), as.character(Sys.timezone())) })
  
  session$userData$time <- reactive({
    format(lubridate::mdy_hms(as.character(input$clientTime)), "%d/%m/%Y; %H:%M:%S")
  })

  session$userData$offset <- reactive({
    input$clientTimeOffset
  })

  session$userData$zone <- reactive({
    input$clientZone
  })

  # output$local <- renderText({
  #   session$userData$time()
  # })

  # output$local2 <- renderText({
  #   session$userData$offset()
  # })

  # output$local3 <- renderText({
  #   session$userData$zone()
  # })
  # specific to shinylogs
  #shinylogs::track_usage(storage_mode = store_null())
  
  # Your application server logic 
  mod_cal_viewer_server("cal_viewer_ui_1")
  #mod_cal_entry_server("cal_entry_ui_1")
}
