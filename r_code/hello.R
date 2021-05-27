library(reticulate)
library(purrr)

#use_python("/bin/python3.8.5")

#reticulate::py_available()
caldav_url = 'https://nextcloud.r-podcastdev.link/remote.php/dav'

caldav <- import("caldav")

client <- caldav$DAVClient(url = caldav_url, username = Sys.getenv("NEXTCLOUD_USER"), password = Sys.getenv("NEXTCLOUD_PASSWORD"))

my_principal = client$principal()
calendars = my_principal$calendars()

purrr::walk(calendars, ~{
  message(glue::glue("Name {.x$name} URL {.x$url}"))
})

per_cal_url <- "https://nextcloud.r-podcastdev.link/remote.php/dav/calendars/rpodcast/personal/"
wimpy_cal_url = "https://nextcloud.r-podcastdev.link/remote.php/dav/calendars/rpodcast/wimpys-world-of-streamers/"

per_cal <- client$calendar(url = per_cal_url)
per_events = per_cal$events()
