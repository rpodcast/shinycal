#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  # specific to shinylogs
  #shinylogs::track_usage(storage_mode = store_null())
  
  # Your application server logic 
  mod_cal_viewer_server("cal_viewer_ui_1")
}
