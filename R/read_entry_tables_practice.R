library(magrittr)

client <- benchlingr::benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
                                         api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")

entry_tables <- list()
return_table_name <- FALSE
for (i in 1:length(entry$days)) {
  entry_tables[[i]] <- list()
  names(entry_tables)[[i]] <- entry$days[[i]]$date
  counter <- 0
  for (j in 1:length(entry$days[[i]]$notes)) {
    if (entry$days[[i]]$notes[[j]]$type$value == "table") {
      counter <- counter + 1
      entry_tables[[i]][[counter]] <- entry$days[[i]]$notes[[j]]
      if (return_table_name) {
        names(entry_tables[[i]])[[counter]] <- entry$days[[i]]$notes[[j]]$table$name
      } else {
        names(entry_tables[[i]])[[counter]] <- "No Name"
      }
    } else {
      next
    }
  }
  if (length(entry_tables[[i]]) == 0) {
    entry_tables[[i]] <- NA
  }
}

