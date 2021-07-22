## code to prepare `streamer_data` dataset goes here
devtools::load_all()
yml_file <- "data-raw/streamers.yml"
yml_data <- yaml::read_yaml(file = yml_file)

streamer_data <- tibble::tibble(yml_data$streamers) %>%
    tidyr::unnest_wider(1) %>%
    dplyr::filter(platform == "twitch") %>%
    dplyr::mutate(id = purrr::map_chr(user_id, ~get_twitch_id(user_name = .x))) %>%
    dplyr::mutate(schedule_data = purrr::map(id, ~get_twitch_schedule(.x)),
            videos_data = purrr::map(id, ~get_twitch_videos(.x)))

usethis::use_data(streamer_data, overwrite = TRUE)
