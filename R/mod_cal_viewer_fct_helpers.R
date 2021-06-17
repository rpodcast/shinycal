#' Import calendar directly from server
#'
#' @param cal_slug string for URL slug of calendar.
#'
#' @return data frame with calendar event contents
#' @export
#' @import caldav
#' @importFrom calendar ic_read ical
import_cal <- function(cal_slug = "wimpys-world-of-streamers", cal_base_url = NULL) {
  if (is.null(cal_base_url)) {
    cal_base_url <- get_golem_config("cal_base_url")
  }
  
  caldav_url = glue::glue("{cal_base_url}/{cal_slug}")
  cal_data <- 
    caldav::caldav_get_all_simple_auth(
      url = caldav_url,
      user = Sys.getenv("NEXTCLOUD_USER"),
      password =  Sys.getenv("NEXTCLOUD_PASSWORD")
    )

  x <- cal_data$calendar
  
  res <- withr::with_tempfile("tf", {
    cat(x, file = tf)
    ic_read(tf)
  })
  
  return(res)
}

time_parser <- function(x, zone, format = "%Y%m%dT%H%M%S", convert_to_char = FALSE) {
  x <- clock::date_time_parse(x, zone, format = format) %>%
    clock::as_naive_time() %>%
    clock::as_zoned_time(., zone)
  
  if (convert_to_char) {
    x <- as.character(x)
  }
  return(x)
}

#' @importFrom dplyr mutate select left_join filter case_when
#' @importFrom clock date_time_parse as_naive_time as_zoned_time zoned_time_set_zone
#' @importFrom purrr map map2
#' @noRd
process_cal <- function(raw_df) {
  dt_df <- raw_df %>%
    mutate(uid = UID) %>%
    select(uid, starts_with("DTSTART"), starts_with("DTEND")) %>%
    tidyr::pivot_longer(!uid, names_to = c("dt_type", "timezone"), names_sep = ";", values_to = "time") %>%
    filter(!is.na(time)) %>%
    tidyr::pivot_wider(names_from = c(dt_type), values_from = time)

  dt_df2 <- dt_df %>%
    mutate(
      timezone = stringr::str_remove_all(timezone, "TZID="),
      start_clock = purrr::map2(DTSTART, timezone, ~time_parser(.x, .y)),
      #start = as.character(start_clock),
      end_clock =  purrr::map2(DTEND, timezone, ~time_parser(.x, .y)),
      #end = as.character(end_clock)
    ) %>%
    select(uid, start_clock, end_clock, timezone) %>%
    tidyr::unnest(cols = c(start_clock, end_clock))

  rec_df <- raw_df %>%
    mutate(uid = UID) %>%
    mutate(recurrenceRule = case_when(
      stringr::str_detect(RRULE, "FREQ=WEEKLY") ~ "Every week",
      TRUE ~ RRULE
    )) %>%
    select(uid, recurrenceRule)

  final_df <- raw_df %>%
    mutate(uid = UID) %>%
    select(uid, title = SUMMARY, location = LOCATION) %>%
    left_join(dt_df2, by = "uid") %>%
    left_join(rec_df, by = "uid")

  return(final_df)
}