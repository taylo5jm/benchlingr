library(magrittr)

client <- benchlingr::benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
                                         api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")

list_of_tables <- list()
for (i in 1:length(entry$days)) {
  list_of_tables[[i]] <- list()
  names(list_of_tables)[[i]] <- entry$days[[i]]$date
  counter <- 0
  for (j in 1:length(entry$days[[i]]$notes)) {
    if (entry$days[[i]]$notes[[j]]$type$value == "table") {
      counter <- counter + 1
      list_of_tables[[i]][[counter]] <- entry$days[[i]]$notes[[j]]
      names(list_of_tables[[i]])[[counter]] <- entry$days[[i]]$notes[[j]]$table$name
    } else {
      next
    }
  }
  if (length(list_of_tables[[i]]) == 0) {
    list_of_tables[[i]] <- NULL
  }
}

list_of_tables <- list()
for (i in 1:length(entry$days)) {
  list_of_tables[[i]] <- list()
  for (j in 1:length(entry$days[[i]]$notes)) {
    list_of_tables[[i]][[j]] <- entry$days[[i]]$notes[[j]]
  }
}

tables_by_name <- list()
table_name <- "Experimental Conditions"
for (i in 1:length(list_of_tables)) {
  tables_by_name[[i]] <- list()
  counter <- 0
  for (j in 1:length(list_of_tables[[i]])) {
    if (list_of_tables[[i]][[j]]$type$value == "table") {
      if (list_of_tables[[i]][[j]]$table$name == table_name) {
        counter <- counter + 1
        tables_by_name[[i]][[counter]] <- list_of_tables[[i]][[j]]
      } else {
        next
      }
    } else {
      next
    }
  }
  if (length(tables_by_name[[i]]) == 0) {
    tables_by_name[[i]] <- NULL
  }
}

