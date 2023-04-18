# test-.get_api_contents

entity_list <- .list_api_contents.R(contents="all", entity_list=NULL)

# test case 1
test_that("Check that .get_api_contents.R generates a warning when encountering an input with names that are
          not entity schemas", {
  expect_warning(.get_api_contents(entity_id=list("invalid_entity"=c("ROZ_podE231M")),
                                   entity_list=entity_list),
                 "invalid_entity is not a valid entity schema.")
  expect_warning(.get_api_contents(entity_id=list("custom_entity"=c("bfi_sdas9213","bfi_023opasd","bfi_O83oida"),
                                                  "NAs"=c("R31M","lju","pop"),
                                                  "dna_sequence"=c("seq_asd9edsa", "seq_P921asd4")),
                                   entity_list=entity_list),
                 "NAs is not a valid entity schema.")
})

# test case 2
test_that("Check that .get_api_contents.R attaches an entity's information regarding its list API contents,
          single-get API endpoint contents and bulk-get API endpoints contents to its respective schema in the 
          input and generates a named list of lists", {
  expect_equal(.get_api_contents(entity_id=list("dna_sequence"=c("seq_Mh090ews"), 
                                                "custom_entity"=c("bfi_klod8Xkf"),
                                                "user"=c("ent_EuhqX69f"))),
               list("dna_sequence"=list("entity_identifiers"=c("seq_Mh090ews"),
                                        "api_urls"=c("https://hemoshear-dev.benchling.com/api/v2/dna-sequences?pageSize=50&sort=name&ids=ENTITY_IDS",
                                                     "https://hemoshear-dev.benchling.com/api/v2/dna-sequences/ENTITY_ID",
                                                     "https://hemoshear-dev.benchling.com/api/v2/dna-sequences:bulk-get?dnaSequenceIds=ENTITY_IDS")), 
                    "custom_entity"=list("entity_identifiers"=c("bfi_klod8Xkf"),
                                         "api_urls"=c("https://hemoshear-dev.benchling.com/api/v2/custom-entities?pageSize=50&sort=name&ids=ENTITY_IDS",
                                                      "https://hemoshear-dev.benchling.com/api/v2/custom-entities/ENTITY_ID",
                                                      "https://hemoshear-dev.benchling.com/api/v2/custom-entities:bulk-get?customEntityIds=ENTITY_IDS")),
                    "user"=list("entity_identifiers"=c("ent_EuhqX69f"),
                                "api_urls"=c("https://hemoshear-dev.benchling.com/api/v2/users?ids=ENTITY_IDS&pageSize=50&sort=name",
                                             "https://hemoshear-dev.benchling.com/api/v2/users/ENTITY_ID", 
                                             NA))))
  expect_equal(.get_api_contents(entity_id=list("custom_entity"=c("bfi_P9238Xkf","bfi_BF9233asf","bfi_90poeweq"),
                                                "invalid_entity"=c("Rias0231"))),
               list("custom_entity"=list("entity_identifiers"=c("bfi_P9238Xkf","bfi_BF9233asf","bfi_90poeweq"),
                                         "api_urls"=c("https://hemoshear-dev.benchling.com/api/v2/custom-entities?pageSize=50&sort=name&ids=ENTITY_IDS",
                                                      "https://hemoshear-dev.benchling.com/api/v2/custom-entities/ENTITY_ID",
                                                      "https://hemoshear-dev.benchling.com/api/v2/custom-entities:bulk-get?customEntityIds=ENTITY_IDS")),
                    "invalid_entity"=list("entity_identifiers"=c("Rias0231"),
                                          "api_urls"=c(NA,NA,NA))))
  })

