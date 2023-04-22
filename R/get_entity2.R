get_entity2 <- function(entity_id, entity_list=NULL,
                        benchling_api_key=Sys.getenv("BENCHLING_API_KEY")) {
  
  inferred_entity_ids <- infer_entity_type(entity_id=entity_id,
                                           entity_list=entity_list)
  if (length(inferred_entity_ids[is.na(inferred_entity_ids)]) > 0 & 
      length(inferred_entity_ids[!is.na(inferred_entity_ids)]) == 0) {
    invalid_ids <- list("invalid_entity"=list("entity_identifiers"=names(inferred_entity_id)[is.na(inferred_entity_id)]))
  }
  if (length(inferred_entity_ids[is.na(inferred_entity_ids)]) == 0 &
      length(inferred_entity_ids[!is.na(inferred_entity_ids)]) > 0) {
    
  }
  if (length(inferred_entity_ids[is.na(inferred_entity_ids)]) > 0 &
      length(inferred_entity_ids[!is.na(inferred_entity_ids)]) > 0) {
    invalid_ids1 <- list("invalid_entity"=list("entity_identifiers"=names(inferred_entity_id)[is.na(inferred_entity_id)]))
  }
  
  
  
}




entity_id <- infer_entity_type(entity_id=c("bfi_Ur5DfvGJ", "seq_Gju61mCm", "bfi_Q13AlXkf", 
                                                     "bfi_Ks908uWV", "ent_Ec76qX9f", "ent_sPrxBNOh", 
                                                     "bfi_Oasd1234", "bfi_U1i9mn34", "bfi_Oasd9321",
                                                     "bfi_Oasd1111", "bfi_TdooUW09", "bfi_Okkiui34",
                                                     "bfi_O0000004", "bfi_Oalmc234", "bfi_Oasd1234",
                                                     "bfi_Oaooo0o4", "bfi_10reeee4", "bfi_Oreeds34",
                                                     "bfi_9fKcrORv", "bfi_KsLU5uWV", "bfi_VVamxrKQ",
                                                     "bfi_Q13AlXkf", "bfi_cgIIb43v", "bfi_jGYT0xek",
                                                     "seq_Cuf0bmCm", "seq_iiiiii34", "seq_mnjkL234",
                                                     "seq_MJ0k8kuu", "seq_ii67jm34", "seq_Opmju0m4",
                                                     "seq_Omjo0u94", "seq_O098m234", "seq_OIuum434",
                                                     "sfs_OklI0mbu", "sfs_OkomPojm", "sfs_5Lkuip34",
                                                     "sfs_O2345798", "sfso_O2222222", "sfs_muy8k034",
                                                     "box_K9950IQ8", "dis_89mkooip", "bxo_923aklum",
                                                     "xop_sad90993"),
                                         entity_list=NULL)


entity_id <- infer_entity_type(entity_id=c("bii_Ur5DfvGJ", "dis_89mkooip", "bxo_923aklum",
                                           "xop_sad90993"),
                               entity_list=NULL)

entity_list <- .list_api_contents(contents="all", entity_list=NULL)

API_URLs <- purrr::map(entity_list, ~ .[c(2,3,4)])
names(API_URLs) <- purrr::map_chr(entity_list, ~ .[1])

sorted_entity_ids <- split(names(inferred_entity_ids), inferred_entity_ids)
sorted_id_types <- names(sorted_entity_ids)
sorted_type_check <- sapply(sorted_id_types, function(x) !is.na(API_URLs[[x]][2]) | !is.na(API_URLs[[x]][3])) 

invalid_ids1 <- names(inferred_entity_ids)[is.na(inferred_entity_ids)] 
invalid_ids2 <- unlist(sorted_entity_ids[!sorted_type_check], use.names = FALSE) 
invalid_ids <- c(invalid_ids1, invalid_ids2) 
invalid_ids <- list("invalid_entity"=list("entity_identifiers"=invalid_ids)) 

valid_ids <- sorted_entity_ids[sorted_type_check] 
valid_id_types <- names(valid_ids) 
valid_ids <- purrr::map(valid_id_types, ~ list("entity_identifiers"=valid_ids[[.]],
                                               "api_urls"=API_URLs[[.]])) 
names(valid_ids) <- valid_id_types 

valid_type_list_options <- sapply(valid_id_types, function(x) !is.na(valid_ids[[x]]$api_urls[1])) 

untested_ids <- valid_ids[!valid_type_list_options] 
untested_id_types <- names(untested_ids)

valid_ids <- valid_ids[valid_type_list_options] 
valid_id_types <- names(valid_ids)

x <- purrr::map(valid_id_types, ~ .entity_id_list_check(entity_type=.,
                                                        entity_id=valid_ids[[.]]$entity_identifiers,
                                                        api_url=valid_ids[[.]]$api_urls[1],
                                                        benchling_api_key=Sys.getenv("BENCHLING_DEV_API_KEY")))
purrr::map(x, ~ unlist(.[[1]]$error$invalidIds))
