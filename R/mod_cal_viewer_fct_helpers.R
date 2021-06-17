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