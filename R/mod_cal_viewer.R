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
      col_3(
        radioButtons(
          ns("cal_view"),
          label = "View Type",
          choices = c("day", "week", "month"),
          selected = "week",
          inline = TRUE
        )
      ),
      col_3(
        selectInput(
          ns("streamer_select"),
          label = "Streamer",
          choices = c("nothing")
        )
      ),
      col_3(
        colourpicker::colourInput(
          ns("entry_color"),
          label = "Entry Color",
          value = "#0ff1a6"
        )       
      ),
      col_3(
        colourpicker::colourInput(
          ns("entry_font_color"),
          label = "Entry Font Color",
          palette = "limited",
          value = "black",
          allowedCols = c("white", "black")
        )
      )
    ),
    fluidRow(
      col_4(
        shinyWidgets::pickerInput(
          ns("time_zone"),
          label = "Select Time Zone",
          choices = c("nothing"),
          options = shinyWidgets::pickerOptions(liveSearch = TRUE)
        )
      )
    ),
    fluidRow(
      col_8(
        calendarOutput(ns("calui"))
      ),
      col_4(
        h4("Click an entry to view the streamer's latest VOD!"),
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
    
    # load data set from online storage if running app on production
    if (getOption("golem.app.prod")) {
      cal_df <- readRDS(url("https://sds-streamer-data.us-east-1.linodeobjects.com/streamer_data_current.rds", "rb"))
    } else {
      cal_df <- readRDS("prototyping/streamer_data_current.rds")
    }

    # perform schedule munging that does not depend on UI
    cal_sub <- cal_df %>%
      tidyr::unnest(schedule_data) %>%
      mutate(calendarId = 1) %>%
      mutate(id = seq_len(dplyr::n())) %>%
      mutate(raw = map2(title, categories, ~list(title = .x, categories = .y)))
    
    # reactive value for clicked schedule item
    schedule_click <- reactiveVal(NULL)

    # reactive for source schedule data
    cal_display_df <- reactiveVal(NULL)

    # reactive for time zone selected
    cal_prev_timezone <- reactiveVal(NULL)

    observeEvent(input$fancy, {
      schedule_click(input$fancy)
    })

    output$vid <- renderUI({
      #req(cal_processed())
      req(schedule_click())

      video_id <- cal_sub %>%
        dplyr::filter(id == schedule_click()$id) %>%
        tidyr::unnest(cols = video_data) %>%
        dplyr::pull(video_data)

      js_snippet <- glue::glue("
      <script type='text/javascript'>
          var options = {
            width: '600',
            height: '400',
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

    # on initial load, ensure calendar data matches user's time zone
    observeEvent(session$userData$zone, {
      message("entered zone observe")


      # update choices for streamer selection input
      shiny::updateSelectInput(
        session = session,
        inputId = "streamer_select",
        choices = c("all", cal_df$user_id),
        selected = "all"
      )

      # set default in picker input to user's time zone
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "time_zone",
        choices = process_raw_timezones(),
        selected = session$userData$zone()
      )

      # apply client time zone to schedule data
      new_zone <- session$userData$zone()
      
      cal_sub2 <- cal_sub %>%
        mutate(start = purrr::map_chr(start, ~time_parser(.x, orig_zone = "America/New_York", new_zone = new_zone, format = "%Y-%m-%dT%H:%M:%S", convert_to_char = TRUE))) %>%
        mutate(end = purrr::map_chr(end, ~time_parser(.x, orig_zone = "America/New_York", new_zone = new_zone, format = "%Y-%m-%dT%H:%M:%S", convert_to_char = TRUE))) %>%
        select(., -video_data, -start_time, -end_time, -category)

      cal_display_df(cal_sub2)
    })

    observeEvent(input$time_zone, {
      req(session$userData$zone())
      req(cal_display_df())
      
      # on default render (when no choices have been updated), do nothing
      if (input$time_zone == "nothing") return(NULL)

      # update schedule data with new time zone
      prev_zone <- session$userData$zone()

      if (!is.null(cal_prev_timezone())) {
        prev_zone <- cal_prev_timezone()
      }

      # apply selected time zone to schedule data
      if (input$time_zone != prev_zone) {

        new_zone <- input$time_zone
        cal_prev_timezone(new_zone)

        cal_sub2 <- cal_display_df() %>%
          mutate(start = purrr::map_chr(start, ~time_parser(.x, orig_zone = prev_zone, new_zone = new_zone, format = "%Y-%m-%d %H:%M:%S", convert_to_char = TRUE))) %>%
          mutate(end = purrr::map_chr(end, ~time_parser(.x, orig_zone = prev_zone, new_zone = new_zone, format = "%Y-%m-%d %H:%M:%S", convert_to_char = TRUE)))

        cal_display_df(cal_sub2)
      }
    })

    # reactive for calendar object
    cal_display_obj <- reactive({
      req(cal_display_df())
      req(input$entry_color)
      req(input$entry_font_color)

      # apply background and font colors to calendar entries
      cal_tmp <- cal_display_df() %>%
        mutate(bgColor = ifelse(is.na(bgColor), input$entry_color, bgColor)) %>%
        mutate(color = ifelse(is.na(color), input$entry_font_color, color))

      # filter for selected streamers if not "all"
      if (input$streamer_select != "nothing") {
        if (input$streamer_select != "all") {
          cal_tmp <- cal_display_df() %>%
            filter(user_id == input$streamer_select)
        }
      }

      cal_zone <- session$userData$zone()
      offsetCalculator <- NULL

      #----------------------------------------------------------------------------
      # I tried this to get the current time shown on the calendar to match
      # the selected time zone, but I am clearly doing it wrong
      #----------------------------------------------------------------------------
      # if (input$time_zone == "nothing") {
      #   cal_zone <- session$userData$zone()
      #   offsetCalculator <- NULL
      # } else {
      #   cal_zone <- input$time_zone
      #   offsetCalculator <- 'function(timezoneName, timestamp){ return moment.tz.zone(timezoneName).utcOffset(timestamp); }'
      # }

      my_id <- ns("fancy")
      toastui::calendar(
        #toastui::cal_demo_data(), 
        cal_tmp,
        view = input$cal_view,
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
        ) %>%
        cal_timezone(
          timezoneName = cal_zone,
          displayLabel = NULL,
          offsetCalculator = offsetCalculator
        ) %>%
        cal_week_options(
          showTimezoneCollapseButton = TRUE,
          timezonesCollapsed = FALSE
        )
        # cal_theme(
        #   common.border = "2px solid #E5E9F0",
        #   month.dayname.borderLeft = "2px solid #E5E9F0",
        #   common.backgroundColor = "#2E3440",
        #   #month.dayExceptThisMonth.color = input$entry_color,
        #   #common.backgroundColor = input$entry_color,
        #   common.holiday.color = "#88C0D0",
        #   common.saturday.color = "#88C0D0",
        #   common.dayname.color = "#ECEFF4",
        #   common.today.color = "#333"
        # )
    })
    
    output$calui <- renderCalendar({
      req(cal_display_obj())
      cal_display_obj()
    })

    # update calendar cell colors when toggled
    observeEvent(input$entry_color, {
      # TODO: Keep this in case I need it later
      #message(glue::glue("color is {color}", color = input$entry_color))
    })

    observeEvent(input$fancy, {
      removeUI(selector = paste0("#", ns("custom_popup")))
      id <- as.numeric(input$fancy$id)
      # Get the appropriate line clicked
      sched <- cal_sub[cal_sub$id == id, ]

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
