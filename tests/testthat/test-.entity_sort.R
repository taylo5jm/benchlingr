# test-.entity_sort.R

# test case 1
test_that("Verify that .entity_sort.R generates a warning when encountering an input with only NA values
          as its elements", {
  expect_warning(.entity_sort(entity_id=c("Roczsad0"=NA)),
                 "Input contains only NA values")
  expect_warning(.entity_sort(entity_id=c("sqq..uf0bmCm"=NA, "bfu_Q1PMlXkf"=NA, "ver_io98720u"=NA)),
                 "Input contains only NA values")
})

# test case 2
test_that("Verify that .entity_sort.R organizes input accordingly", {
  expect_equal(.entity_sort(entity_id=c("ROZ_podE231M"=NA)),
               list("invalid_entity"=c("ROZ_podE231M")))
  expect_equal(.entity_sort(entity_id=c("seq_Cuf0bmCm"="dna_sequence", 
                                        "bfi_Q1PMlXkf"="custom_entity", 
                                        "z0edll420u"=NA)),
               list("dna_sequence"=c("seq_Cuf0bmCm"),
                    "custom_entity"=c("bfi_Q1PMlXkf"),
                    "invalid_entity"=c("z0edll420u")))
  expect_equal(.entity_sort(entity_id=c("seq_Mh090ews"="dna_sequence", 
                                        "bfi_klod8Xkf"="custom_entity", 
                                        "ve0k998720u"=NA,"dpd_as0d0Oi"=NA,
                                        "bfi_Q9m0ulkf"="custom_entity", 
                                        "bfi_Pokd7uWV"="custom_entity", 
                                        "ent_EuhqX69f"="user")),
               list("dna_sequence"=c("seq_Mh090ews"), 
                    "custom_entity"=c("bfi_klod8Xkf","bfi_Q9m0ulkf","bfi_Pokd7uWV"),
                    "invalid_entity"=c("ve0k998720u","dpd_as0d0Oi"),
                    "user"=c("ent_EuhqX69f")))
  })