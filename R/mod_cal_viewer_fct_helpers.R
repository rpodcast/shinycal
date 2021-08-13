#' @importFrom twitchr get_users
#' @importFrom dplyr filter
#' @noRd 
get_twitch_id <- function(user_name) {
  user <- get_users(login = user_name)
  message(glue::glue("user_name: {user_name} - id: {x}", x = user$id))
  res <- dplyr::select(user, id, description, profile_image_url)
  return(res)
}

parse_duration <- function(x, 
                           time_unit = c("seconds", "minutes", "hours"), 
                           dur_regex = "([0-9]{1,2}h)?([0-9]{1,2}m)?([0-9]{1,2}(\\.[0-9]{1,3})?s)?") {
  # process to reverse engineer the starting time of each video
  # clever regex found at https://stackoverflow.com/a/11293491
  # we get a matrix back with 2 rows (second row is meaningless)
  # columns are the following
  # - col 1: the raw duration string
  # - col 2: the "hour" part of the duration string (ex "1h")
  # - col 3: the "minute" part of the duration string (ex "1m")
  # - col 4: the "second" part of the duration string (ex "1s")
  # - col 5: meaningless
  time_unit <- match.arg(time_unit)
  dur_parsed <- stringr::str_match_all(x, dur_regex)[[1]]

  # set up final duration
  dur_final <- 0

  # extract relevant parts of duration matrix
  dur_vec <- readr::parse_number(dur_parsed[1, c(2,3,4)])

  names(dur_vec) <- c("hours", "minutes", "seconds")

  time_mult <- switch(
    time_unit,
    hours = c(1, (1/60), (1/3600)),
    minutes = c(60, 1, (1/60)),
    seconds = c(3600, 60, 1)
  )

  dur_vec <- sum(dur_vec * time_mult)

  return(dur_vec)

}

# https://stackoverflow.com/questions/27397332/find-round-the-time-to-nearest-half-an-hour-from-a-single-file
round_time <- function(x) {
  x <- as.POSIXlt(x)

  x$min <- round(x$min / 30) * 30
  x$sec <- floor(x$sec / 60)

  x <- as.POSIXct(x)
  return(x)
}

shift_week <- function(x, convert = TRUE, which = "next") {
  if (!convert) {
    return(x)
  } else {
    # get current day of week from supplied date
    x_weekday <- clock::as_year_month_weekday(x) %>% clock::get_day()

    res <- clock::date_shift(x, target = clock::weekday(x_weekday), which = which, boundary = "advance")

    return(res)
  }
}

compute_end_clock <- function(start_clock, stream_length, precision = "hour") {
  # truncate the stream length to floor of nearest hour
  new_length <- clock::duration_round(clock::duration_seconds(stream_length), precision = precision)
  end_clock <- clock::add_hours(start_clock, new_length)
  return(end_clock)
}

time_parser <- function(x, orig_zone = "UTC", new_zone = "America/New_York", format = "%Y-%m-%dT%H:%M:%SZ", convert_to_char = TRUE) {
  # was format = "%Y%m%dT%H%M%S" for ical

  x <- clock::date_time_parse(x, orig_zone, format = format)
  x_z <- clock::as_zoned_time(x)

  # change to the desired time zone
  x_final <- clock::zoned_time_set_zone(x_z, new_zone) %>% clock::as_naive_time()
  
  if (convert_to_char) {
    x_final <- as.character(x_final)
  }
  return(x_final)
}


get_twitch_schedule <- function(id) {
  r <- httr::GET("https://api.twitch.tv/helix/schedule", query = list(broadcaster_id = id))
  status <- httr::status_code(r)

  if (status != 200) {
    warning(glue::glue("User {id} does not have valid schedule data. Proceeding to infer a schedule based on videos uploaded (status code {status})"))
    r <- httr::GET("https://api.twitch.tv/helix/videos", query = list(user_id = id, period = "week"))
    status <- httr::status_code(r)

    if (status != 200) {
      warning(glue::glue("User {id} does not have any videos! Skipping ..."))
      return(NULL)
    } else {
      current_weekday <- clock::date_now("America/New_York") %>%  
        clock::as_year_month_weekday() %>% 
        clock::get_day()

      prev_week_date <- clock::date_now("America/New_York") %>% 
        clock::date_shift(target = clock::weekday(current_weekday), which = "previous", boundary = "advance")

      current_sunday <- clock::date_now("America/New_York") %>% 
        clock::date_shift(target = clock::weekday(clock::clock_weekdays$sunday), which = "previous")

      res <- httr::content(r, "parsed") %>%
        purrr::pluck("data") %>%
        tibble::tibble() %>%
        tidyr::unnest_wider(1)

      res_int <- res %>%
        mutate(start = purrr::map(created_at, ~time_parser(.x, convert_to_char = FALSE))) %>%
        mutate(start = purrr::map(start, ~clock::as_date_time(.x, zone = "America/New_York"))) %>%
        mutate(duration2 = purrr::map_dbl(duration, ~parse_duration(.x, "seconds"))) %>%
        tidyr::unnest(cols = c(start)) %>%
        mutate(start = purrr::map(start, ~round_time(.x))) %>%
        mutate(end = purrr::map2(start, duration2, ~compute_end_clock(.x, .y))) %>%
        mutate(category = "time", 
               recurrenceRule = "Every week",
               start_time = NA, 
               end_time = NA) %>%
        tidyr::unnest(cols = c(start, end)) %>%
        filter(start > prev_week_date)
        
      if (nrow(res_int) < 1) {
        return(NULL)
      } else {
        res_final <- res_int %>%
          mutate(before_week_ind = start < current_sunday) %>%
          mutate(start = purrr::map2(start, before_week_ind, ~shift_week(.x, .y))) %>%
          mutate(end = purrr::map2(end, before_week_ind, ~shift_week(.x, .y))) %>%
          tidyr::unnest(cols = c(start, end)) %>%
          mutate(start = as.character(start), end = as.character(end)) %>%
          dplyr::select(start_time, start, end_time, end, title, category, recurrenceRule)
      }
    }
  } else {
    res <- httr::content(r, "parsed") %>%
      purrr::pluck("data", "segments") %>%
      tibble::tibble() %>%
      tidyr::unnest_wider(1)

    res_int <- res %>%
      mutate(start = purrr::map(start_time, ~time_parser(.x, convert_to_char = FALSE)),
             end = purrr::map(end_time, ~time_parser(.x, convert_to_char = FALSE)),
             category = "time",
             recurrenceRule = "Every week") %>%
      dplyr::select(start_time, start, end_time, end, title, category, recurrenceRule)
      #tidyr::unnest(cols = c(start, end))

    # grab the first records of each unique stream     
    res_first <- res_int %>%
      dplyr::group_by(title) %>%
      dplyr::arrange(title, start) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      mutate(start = purrr::map(start, ~clock::as_date_time(.x, zone = "America/New_York")),
              end = purrr::map(end, ~clock::as_date_time(.x, zone = "America/New_York"))) %>%
      mutate(start = purrr::map(start, ~shift_week(.x, which = "previous")),
              end = purrr::map(end, ~shift_week(.x, which = "previous"))) %>%
      mutate(start = purrr::map(start, ~clock::as_naive_time(.x)),
             end = purrr::map(end, ~clock::as_naive_time(.x)))

      # bind back together
      res_final <- dplyr::bind_rows(
        tidyr::unnest(res_first, c("start", "end")), 
        tidyr::unnest(res_int, c("start", "end"))
      ) %>%
      mutate(start = as.character(start), end = as.character(end))


  }
  return(res_final)
}


get_twitch_videos <- function(id) {
  message(glue::glue("twitch id {id}"))
  videos <- twitchr::get_videos(user_id = id, first = 100) 

  if (is.null(videos)) {
    # try getting clips instead
    videos <- twitchr::get_all_clips(broadcaster_id = id)
    if (is.null(videos)) {
      warning(glue::glue("There are no videos for user {id}"))
      return(NA)
    } else {
      videos_play <- videos %>%
        dplyr::mutate(video_id = purrr::map_chr(url, ~{
          tmp <- stringr::str_split(.x, "/")
          n_items <- length(tmp[[1]])
          res <- tmp[[1]][n_items]
          return(res)
        })) %>%
        dplyr::slice(1) %>%
        dplyr::pull(video_id)
      return(videos_play)
    }
  }

  videos_play <- videos$data %>%
    #tibble() %>%
    dplyr::mutate(video_id = purrr::map_chr(url, ~{
      tmp <- stringr::str_split(.x, "/")
      n_items <- length(tmp[[1]])
      res <- tmp[[1]][n_items]
      return(res)
    })) %>%
    dplyr::slice(1) %>%
    dplyr::pull(video_id)

  return(videos_play)
}


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
    left_join(rec_df, by = "uid") %>%
    mutate(raw = uid)

  return(final_df)
}