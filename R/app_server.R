#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # import calendar from server
  cal_df <- import_cal()
  
  # Your application server logic 
  mod_cal_viewer_server("cal_viewer_ui_1", cal_df)
}
