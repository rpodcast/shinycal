#' cal_viewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import toastui
#' @import shinylogs
mod_cal_viewer_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_8(
        calendarOutput(ns("calui")),
        uiOutput(ns("vid"))
      ),
      col_4(
        h2("More Info Here"),
        textInput(ns("blah"), "Enter something"),
        verbatimTextOutput(ns("last_changed"))
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

    observeEvent(input$calui_click, {
      message("hello")
    })

    output$vid <- renderUI({
      js_snippet <- glue::glue("
      <script type='text/javascript'>
          var options = {
            width: '800',
            height: '600',
            video: '1060733999',
            autoplay: false
            //parent: 'localhost'
          };
          var player = new Twitch.Player('<<<divid>>>', options);
          player.setVolume(0.5);
        </script>",
        divid = ns("testvid"),
        .open = "<<<",
        .close = ">>>")
      
      tags$div(
        id = ns("testvid"),
        h2("vid was here")
        #htmltools::HTML(js_snippet)
      )
    })

    output$last_changed <- renderPrint({
      
      req(input$calui_schedules)

      input_lst <- reactiveValuesToList(input)
      print(input_lst)
      # list(
      #   click = input$calui_click,
      #   dates = input$calui_dates,
      #   schedules = input$calui_schedules
      # )
      #input$`.shinylogs_lastInput`
      # Shiny.setInputValue(el.id + "_dates"
      # _add
      # _schedules
      # _click
      # _delete
      # _update
      # _dates
    })
    
    output$calui <- renderCalendar({
      
      # process cal_df to conform to the proper structure
      # coalesce(contains(dtstart()))

      # `YYYY-MM-DD XX:YY:ZZ`
      
      my_id <- ns("fancy")
      
      # TODO: Change this to use the streamer_data frame

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
        ) %>%
        cal_events(
          # schedule elements that we could grab
          # - location
          # - recurrenceRule
          # - body
          # - title
          cal = .,
          clickSchedule = JS(glue::glue('function(event) {console.log(event.schedule.id); Shiny.setInputValue("<<my_id>>", event.schedule.raw);}', .open = "<<", .close = ">>"))
          #clickSchedule = JS("function(event) {alert(event.schedule.id);}")
        )
    })
 
  })
}
    
## To be copied in the UI
# mod_cal_viewer_ui("cal_viewer_ui_1")
    
## To be copied in the server
# mod_cal_viewer_server("cal_viewer_ui_1")
