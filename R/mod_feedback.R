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
    fluidRow(
      col_12(
        h2("Submit Feedback!"),
        p("Use the form below to provide feedback to the author of shinycal directly on the issue tracker.")
      )
    ),
    fluidRow(
      col_6(
        textInput(
          ns("issue_title"),
          with_red_star("Title"),
          value = "",
          placeholder = "enter short title",
          width = "100%"
        )
      ),
      col_6(
        shinyWidgets::pickerInput(
          ns("issue_labels"),
          label = with_red_star("Choose one or more labels"),
          choices = c("silly"),
          multiple = TRUE
        )
      )
    ),
    fluidRow(
      col_12(
        textAreaInput(
          ns("issue_description"),
          label = with_red_star("Enter issue description"),
          value = "",
          width = "300%",
          cols = 80,
          rows = 10,
          placeholder = "markdown format supported",
          resize = "vertical"
        )
      )
    ),
    fluidRow(
      col_2(
        shinyWidgets::actionBttn(
          ns("submit_issue"),
          "Submit!",
          icon = icon("save"),
          style = "jelly",
          color = "success",
          size = "sm"
        )
      )
    )
  )
}
    
#' feedback Server Functions
#'
#' @noRd 
mod_feedback_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # update issue label choices
    shinyWidgets::updatePickerInput(
      session,
      "issue_labels",
      choices = get_issue_labels(),
      selected = NULL
    )

    # process submit click
    observeEvent(input$submit_issue, {
      # perform mandatory input checks
      if (!shiny::isTruthy(input$issue_title)) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Oops!",
          text = "Please enter a title for your issue",
          type = "error"
        )

        return(NULL)
      }

      if (!shiny::isTruthy(input$issue_labels)) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Oops!",
          text = "Please choose at least one label",
          type = "error"
        )

        return(NULL)
      }

      if (!shiny::isTruthy(input$issue_description)) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Oops!",
          text = "Please enter an issue description",
          type = "error"
        )

        return(NULL)
      }

      # process submit
      res <- submit_issue(
        input$issue_title,
        input$issue_labels,
        input$issue_description
      )

      # show confirmation
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Issue submitted!",
        text = "Thank you for your feedback on shinycal! Your issue has been submitted to the shinycal GitHub issue tracker.",
        type = "success"
      )

      # reset the form inputs
      updateTextInput(
        session,
        "issue_title",
        value = ""
      )

      updateTextAreaInput(
        session,
        "issue_description",
        value = ""
      )

      shinyWidgets::updatePickerInput(
        session,
        "issue_labels",
        selected = NULL
      )
    })
  })
}
    
## To be copied in the UI
# mod_feedback_ui("feedback_ui_1")
    
## To be copied in the server
# mod_feedback_server("feedback_ui_1")
