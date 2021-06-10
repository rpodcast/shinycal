library(purrr)

# these should not be needed now that I set the python path env var via  .Rprofile
#reticulate::use_python("/usr/bin/python3")
#reticulate::py_config()
#reticulate::py_available()

library(reticulate)

caldav_url = 'https://nextcloud.r-podcastdev.link/remote.php/dav'

caldav <- import("caldav")

client <- caldav$DAVClient(url = caldav_url, username = Sys.getenv("NEXTCLOUD_USER"), password = Sys.getenv("NEXTCLOUD_PASSWORD"))

my_principal <- client$principal()
calendars <- my_principal$calendars()

purrr::walk(calendars, ~{
  message(glue::glue("Name {.x$name} URL {.x$url}"))
})

per_cal_url <- "https://nextcloud.r-podcastdev.link/remote.php/dav/calendars/rpodcast/personal/"
wimpy_cal_url <- "https://nextcloud.r-podcastdev.link/remote.php/dav/calendars/rpodcast/wimpys-world-of-streamers/"
test_cal_url <- "https://nextcloud.r-podcastdev.link/remote.php/dav/calendars/rpodcast/erics-play-calendar/"

wimpy_cal <- client$calendar(url = wimpy_cal_url)
wimpy_events <- wimpy_cal$events()

one_event <- wimpy_events[[1]]


data_obj <- one_event$data

one_event$get_property(prop = "DTSTART")


library(calendar)
ical_example = readLines(system.file("extdata", "example.ics", package = "calendar"))

x <- ic_attributes_vec(x = ical_example)

ics_df <- ic_read(system.file("extdata", "example.ics", package = "calendar"))
ics_df <- ic_read("r_code/wimpys-world-of-streamers-2021-06-08.ics")

library(caldav)

caldav_url = "https://nextcloud.r-podcastdev.link/remote.php/dav/calendars/rpodcast/wimpys-world-of-streamers/"
cal_data <- 
  caldav_get_all_simple_auth(
    url = caldav_url,
    user = Sys.getenv("NEXTCLOUD_USER"),
    password =  Sys.getenv("NEXTCLOUD_PASSWORD")
  )

x <- cal_data$calendar

cat(x, file = "r_code/tmp.ics")


library(purrr)
tidy_cal <- purrr::map(x, ~stringr::str_split(.x, "\\n"))

ic_read("r_code/tmp.ics")


tidy_cal[[1]]
