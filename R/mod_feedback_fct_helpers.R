get_issue_labels <- function(owner = "rpodcast", repo = "shinycal") {
  issues_raw <- gh::gh("GET /repos/{owner}/{repo}/labels", owner = "rpodcast", repo = "shinycal")

  labels_df <- tibble::tibble(x = issues_raw) %>%
    tidyr::unnest_wider(1)

  labels <- dplyr::pull(labels_df, name)

  return(labels)
}

submit_issue <- function(
  issue_title,
  issue_labels,
  issue_description,
  repo = "shinycal",
  owner = "rpodcast",
  add_issues = "user feedback"
) {
  # append name and date to end of issue description
  version <- golem::get_golem_version()
  issue_date <- Sys.Date()

  end_text <- glue::glue("\n\n\n\nFile from shinycal version {version} on {issue_date}")

  issue_description <- paste0(issue_description, end_text)

  # add additional labels
  issue_labels <- c(issue_labels, add_issues)

  # submit
  issue_post <- gh::gh(
    glue::glue("POST /repos/{owner}/{repo}/issues"),
    title = issue_title,
    body = issue_description,
    assignee = "rpodcast",
    labels = issue_labels
  )

  return(issue_post)
}

