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
      col_4(
        radioButtons(
          ns("cal_view"),
          label = "View Type",
          choices = c("day", "week", "month"),
          selected = "week",
          inline = TRUE
        )
      ),
      col_8(
        selectInput(
          ns("streamer_select"),
          label = "Streamer",
          choices = c("all")
        )
      )
    ),
    fluidRow(
      col_12(
        calendarOutput(ns("calui")),
        uiOutput(ns("vid"))
      )
    )
  )
}
    
#' cal_viewer Server Functions
#'
#' @noRd 
mod_cal_viewer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    cal_df <- streamer_data

    # reactive value for clicked schedule item
    schedule_click <- reactiveVal(NULL)

    observeEvent(input$fancy, {
      schedule_click(input$fancy)
    })

    output$vid <- renderUI({
      req(cal_processed())
      req(schedule_click())

      video_id <- cal_processed() %>%
        dplyr::filter(id == schedule_click()$id) %>%
        tidyr::unnest(cols = videos_data) %>%
        dplyr::pull(videos_data)

      js_snippet <- glue::glue("
      <script type='text/javascript'>
          var options = {
            width: '800',
            height: '600',
            video: '<<<video_id>>>',
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
        #h2("vid was here")
        htmltools::HTML(js_snippet)
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

    cal_processed <- reactive({
      shiny::updateSelectInput(
        session = session,
        inputId = "streamer_select",
        choices = c("all", cal_df$user_id),
        selected = "all"
      )

      cal_sub <- cal_df %>%
              tidyr::unnest(schedule_data) %>%
              mutate(calendarId = 1) %>%
              mutate(id = seq_len(dplyr::n())) %>%
              #mutate(start = as.character(start), end = as.character(end)) %>%
              mutate(raw = map2(title, categories, ~list(title = .x, categories = .y)))

        return(cal_sub)
    })

    cal_display_df <- reactive({
      req(cal_processed())
      #req(input$streamer_select)

      cal_sub2 <- cal_processed() %>%
        select(., -videos_data, -start_time, -end_time, -category)

      if (input$streamer_select != "all") {
        cal_sub2 <- cal_sub2 %>%
          filter(user_id == input$streamer_select)   
      }

      return(cal_sub2)
    })
    
    output$calui <- renderCalendar({
      req(cal_display_df())
      

      
      # process cal_df to conform to the proper structure
      # coalesce(contains(dtstart()))

      # `YYYY-MM-DD XX:YY:ZZ`
      #browser()
      my_id <- ns("fancy")

      toastui::calendar(
        #toastui::cal_demo_data(), 
        cal_display_df(),
        view = "week",
        scheduleView = list('time'),
        useNavigation = TRUE,
        useDetailPopup = FALSE,
      ) %>%
        cal_events(
          # schedule elements that we could grab
          # - location
          # - recurrenceRule
          # - body
          # - title
          cal = .,
          clickSchedule = JS(glue::glue('function(event) {console.log(event.schedule.id); Shiny.setInputValue("<<my_id>>", {raw: event.schedule.raw, id: event.schedule.id, x: event.event.clientX, y: event.event.clientY});}', .open = "<<", .close = ">>"))
          #clickSchedule = JS("function(event) {alert(event.schedule.id);}")
        ) %>%
        cal_timezone(
          timezoneName = "America/New_York",
          displayLabel = "UTC-05:00",
          tooltip = "New York",
          extra_zones = list(
            list(
              timezoneName = "UTC",
              displayLabel = "UTC+00:00",
              tooltip = "UTC"
            )
          )
        ) %>%
        cal_week_options(
          showTimezoneCollapseButton = TRUE,
          timezonesCollapsed = FALSE
        )
    })

    observeEvent(
      input$cal_view,
      cal_proxy_view(ns("calui"), input$cal_view),
      ignoreInit = TRUE
    )

    observeEvent(input$fancy, {
      removeUI(selector = paste0("#", ns("custom_popup")))
      id <- as.numeric(input$fancy$id)
      # Get the appropriate line clicked
      sched <- cal_processed()[cal_processed()$id == id, ]

      start_time <- lubridate::as_datetime(sched$start) %>% hms::as_hms()  
      end_time <- lubridate::as_datetime(sched$end) %>% hms::as_hms()  

      insertUI(
        selector = "body",
        #selector = paste0("#", ns("add")),
        ui = absolutePanel(
          id = ns("custom_popup"),
          top = input$fancy$y,
          left = input$fancy$x, 
          draggable = FALSE,
          width = "300px",
          tags$div(
            style = "background: #FFF; padding: 10px; box-shadow: 0px 0.2em 0.4em rgb(0, 0, 0, 0.8); border-radius: 5px;",
            actionLink(
              inputId = ns("close_calendar_panel"), 
              label = NULL, 
              icon = icon("close"), 
              style = "position: absolute; top: 5px; right: 5px;"
            ),
            tags$br(),
            tags$div(
              style = "text-align: left;",
              tags$p(
                glue::glue("{user_id} is streaming from {start_time} to {end_time}",
                  user_id = sched$user_id)
              ),
              tags$p(
                sched$description,
              ),
              tags$p(
                "Categories", list_to_li(sched$raw[[1]]$categories)
              )
            )
          )
        )
    )
    })

    observeEvent(input$close_calendar_panel, {
      removeUI(selector = paste0("#", ns("custom_popup")))
    })
  })


}
    
## To be copied in the UI
# mod_cal_viewer_ui("cal_viewer_ui_1")
    
## To be copied in the server
# mod_cal_viewer_server("cal_viewer_ui_1")
