entity_list <- list("bat" = c("batch", 
                              "https://hemoshear-dev.benchling.com/api/v2/batches?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/batches/ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/batches:bulk-get?batchIds=ENTITY_ID_VARIABLE"), 
                    "bfi" = c("custom_entity", 
                              "https://hemoshear-dev.benchling.com/api/v2/custom-entities?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/custom-entities/ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/custom-entities:bulk-get?customEntityIds=ENTITY_ID_VARIABLE"),
                    "box" = c("box", 
                              "https://hemoshear-dev.benchling.com/api/v2/boxes?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/boxes/ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/boxes:bulk-get?boxIds=ENTITY_ID_VARIABLE"),
                    "con" = c("container", 
                              "https://hemoshear-dev.benchling.com/api/v2/containers?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/containers/ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/containers:bulk-get?containerIds=ENTITY_ID_VARIABLE"),
                    "ent" = c("user", 
                              "https://hemoshear-dev.benchling.com/api/v2/users?ids=eENTITY_ID_VARIABLE&pageSize=50&sort=name",
                              "https://hemoshear-dev.benchling.com/api/v2/users/ENTITY_ID_VARIABLE", 
                              NA),
                    "etr" = c("entry", 
                              "https://hemoshear-dev.benchling.com/api/v2/entries?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/entries/ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/entries:bulk-get?entryIds=ENTITY_ID_VARIABLE"),
                    "loc" = c("location", 
                              "https://hemoshear-dev.benchling.com/api/v2/locations?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/locations/ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/locations:bulk-get?locationIds=ENTITY_ID_VARIABLE"),
                    "mxt"= c("mixture", 
                             "https://hemoshear-dev.benchling.com/api/v2/mixtures?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                             "https://hemoshear-dev.benchling.com/api/v2/mixtures/ENTITY_ID_VARIABLE", 
                             NA),
                    "plt" = c("plate", 
                              "https://hemoshear-dev.benchling.com/api/v2/plates?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/plates/ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/plates:bulk-get?plateIds=ENTITY_ID_VARIABLE"),
                    "prtn" = c("aa_sequence", 
                               "https://hemoshear-dev.benchling.com/api/v2/aa-sequences?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                               "https://hemoshear-dev.benchling.com/api/v2/aa-sequences/ENTITY_ID_VARIABLE",
                               "https://hemoshear-dev.benchling.com/api/v2/aa-sequences:bulk-get?aaSequenceIds=ENTITY_ID_VARIABLE"),
                    "sfs" = c("dropdown", 
                              NA,
                              "https://hemoshear-dev.benchling.com/api/v2/dropdowns/ENTITY_ID_VARIABLE", 
                              NA),
                    "sfso" = c("dropdown_option", 
                               NA,
                               "https://hemoshear-dev.benchling.com/api/v2/dropdowns/ENTITY_ID_VARIABLE", 
                               NA), 
                    "seq" = c("dna_sequence", 
                              "https://hemoshear-dev.benchling.com/api/v2/dna-sequences?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/dna-sequences/ENTITY_ID_VARIABLE",
                              "https://hemoshear-dev.benchling.com/api/v2/dna-sequences:bulk-get?dnaSequenceIds=ENTITY_ID_VARIABLE"))

l <- list("_Asdasd","poasdad","ads_asd","_adas")
any(purrr::map(l, ~ grepl("_", ., fixed = TRUE)) == FALSE)
entity_match <- function(entity_id, entity_list) {
  purrr
  
  listed_entity_ids <- unique(purrr::map(entity_id, ~ unlist(stringr::str_split(., "_"), use.names = FALSE)[1]))
  names(listed_entity_ids) <- c(unlist(listed_entity_ids))
  for (i in 1:length(listed_entity_ids)) {
    listed_entity_ids[[i]] <- unlist(entity_id[which(purrr::map(entity_id, ~ unlist(stringr::str_split(., "_"), use.names = FALSE)[1]) == names(listed_entity_ids)[[i]])])
  }
  
  
}




s1 <- c("bat_5j79k","bfi_78k,uj","bfi_othy7","yot_67o","tbcs_09o_asdasda")


x_1 <- unique(names(x))

s1_2 <- s1[which(purrr::map(s1, ~ unlist(stringr::str_split(., "_"), use.names = FALSE)[1] %in% names(entity_list)) == FALSE)]



s2 <- purrr::map(names(entity_list), ~ unlist(s1[lapply(s1, function(x) unlist(stringr::str_split(x, "_"))[1]) == .]))

y <- purrr::map(unique(names(x)), ~ unlist(x[which(names(x) == .)], use.names = FALSE))

names(y) <- unique(names(x))
s2 <- s2[lapply(s2, length) > 0]

names(s2) <- names(entity_list)[]

s1[which(unlist(stringr::str_split(s1, "_"), use.names = FALSE)[1]) == "bat"]
lapply(names(entity_list), function(x) purrr::map())
purrr::map(names(entity_list), ~ s1[)]
             
s1[which()]
names(s2) <- c(unlist(s2))
for (i in 1:length(s2)) {
  s2[[i]] <- unlist(s1[which(purrr::map(s1, ~ unlist(stringr::str_split(., "_"), use.names = FALSE)[1]) == names(s2)[[i]])])
}




names(s1) <- c(unlist(purrr::map(s1, ~ unlist(stringr::str_split(., "_"), use.names = FALSE)[1])))

sapply(s1, function(x) unlist(stringr::str_split(x, "_"), use.names = FALSE)[1])


for (i in 1:length(s2)) {
  s2[[i]] <- list()
  
}




lapply(unique(names(s1)), function(x) unlist(s1[x== names(s1)], use.names = FALSE))

s1[which(purrr::map(s1, ~ unlist(stringr::str_split(., "_"), 
                                 use.names = FALSE)[1] == s2[[1]]) == TRUE)]

which(unlist(stringr::str_split(s2, "_"), use.names = FALSE)[1] == s2[[1]])
lapply(unique(purrr::map(s, ~ unlist(stringr::str_split(., "_"), use.names = FALSE)[1])),
      function(x) )
substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1)




lapply(unique(purrr::map(s, ~ unlist(stringr::str_split(., "_"), use.names = FALSE)[1])),
       function(x) Filter(function(y) unlist(stringr::str_split(y, "_"), use.names = FALSE)[1] == "pop", s))


.entity_match <- function(listed_ids, entity_list) {
  organized_listed_ids <- purrr::map(unique(names(listed_ids)), ~ unlist(listed_ids[which(names(listed_ids) == .)], use.names = FALSE))
  names(organized_listed_ids) <- unique(names(listed_ids))
}