library(magrittr)

client <- benchlingr::benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
                                         api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")
day <- 1
table_position <- 2

example_table <- entry$days[[day]]$notes[[table_position]]

columns <- snakecase::to_snake_case(example_table$table$column_labels)

.extract_rows <- function(rows, from_api) {
  .extract_row <- function(row) {
    if (class(row[[1]])[1] == "benchling_api_client.v2.stable.models.entry_table_row.EntryTableRow") {
      purrr::map(row, ~ lapply(.$cells, function(x) (x['text'])) %>%
                   unlist)
    } else {
      purrr::map(row, ~ .)
    }
  }
  if (from_api) {
    .extract_row(rows)
  } else {
    purrr::map(rows, ~ .[[1]] %>% unlist)
  }
}

res <- example_table$table$rows %>%
  .extract_rows(., TRUE) %>%
  do.call('rbind', .) %>% 
  as.data.frame()

