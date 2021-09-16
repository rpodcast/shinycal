#' cal_entry UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyvalidate
mod_cal_entry_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_6(
        textInput(
          ns("user_id"),
          with_red_star("Twitch user ID"),
          value = "",
          placeholder = "Please enter an ID"
        ),
        textInput(
          ns("user_name"),
          with_red_star("Enter name"),
          value = ""
        ),
        textInput(
          ns("twitter_id"),
          "Twitter ID (Optional)",
          value = ""
        ),
        shinyWidgets::prettyRadioButtons(
          ns("platform"),
          label = "Choose Streaming Platform",
          choices = c("twitch", "youtube"),
          selected = "twitch"
        ),
        shinyWidgets::pickerInput(
          ns("categories"),
          label = with_red_star("Select categories"), 
          choices = c("R", "datascience", "shiny", "sports", "gaming", "linux", "julia", "python"),
          selected = "R",
          options = list(
            `actions-box` = TRUE),
            multiple = TRUE
        ),
        shinyWidgets::actionBttn(
          ns("submit"),
          label = "Submit!",
          icon = "rocket",
          style = "jelly",
          color = "success",
          size = "lg"
        ),
        verbatimTextOutput(
          ns("debugging")
        )
      )
    )
  )
}
    
#' cal_entry Server Functions
#'
#' @noRd 
mod_cal_entry_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # reactive vals
    entry_yaml <- reactiveVal(NULL)

    # establish form validation processing and rules
    iv <- InputValidator$new()

    iv$add_rule("user_id", sv_required())
    iv$add_rule("user_name", sv_required())
    iv$add_rule("categories", sv_required())

    # process form submission
    observeEvent(input$submit, {
      # ensure form is valid before processing
      iv$enable()
  
      if (!iv$is_valid()) {
        shinyWidgets::show_alert(
          title = "Oops",
          text = "Please complete the form"
        )
        return(NULL)
      }

      req(iv$is_valid())
      
      # assemble form inputs and create yaml structure
      entry_list <- list(
        user_id = input$user_id,
        user_name = input$user_name,
        twitter_id = input$twitter_id,
        platform = input$platform,
        categories = input$categories
      )

      en_yaml <- yaml::as.yaml(entry_list)
      entry_yaml(en_yaml)
    })

    output$debugging <- renderPrint({
      req(entry_yaml())
      cat(entry_yaml())
    })
 
  })
}
    
## To be copied in the UI
# mod_cal_entry_ui("cal_entry_ui_1")
    
## To be copied in the server
# mod_cal_entry_server("cal_entry_ui_1")
