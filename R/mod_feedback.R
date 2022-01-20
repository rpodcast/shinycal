#' feedback UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_feedback_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' feedback Server Functions
#'
#' @noRd 
mod_feedback_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_feedback_ui("feedback_ui_1")
    
## To be copied in the server
# mod_feedback_server("feedback_ui_1")
