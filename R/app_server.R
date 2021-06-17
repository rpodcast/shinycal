#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # import calendar from server
  cal_slug <- "data-science-streams"
  cal_base_url <- "https://nextcloud.r-podcastdev.link/remote.php/dav/calendars/shinycal"
  cal_df <- import_cal(cal_base_url = cal_base_url, cal_slug = cal_slug) %>%
    process_cal(.)
  
  # Your application server logic 
  mod_cal_viewer_server("cal_viewer_ui_1", cal_df)
}
