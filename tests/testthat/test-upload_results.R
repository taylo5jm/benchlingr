

conn <- benchlingr::warehouse_connect("hemoshear-dev",
                          Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                          Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))

client <- benchlingr::benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
                                         api_key = Sys.getenv("BENCHLING_DEV_API_KEY"))



# Blob link validation -------------
test_that(".validate_blob_link_column_values throws error when file does 
          not exist on local machine", {
    testthat::expect_match(
    benchlingr:::.validate_blob_link_column_values(
      errors=c(), values="filethatdoesnotexist.txt", 
      column_name="MyFileColumn", multi_select=FALSE),
      "does not exist")
  }
  )

test_that(".validate_blob_link_column_values returns nothing when file exists.", {
            testthat::expect_equal(
              benchlingr:::.validate_blob_link_column_values(
                errors=c(), values="test-upload_assay_results.R", 
                column_name="MyFileColumn", multi_select=FALSE),
              c())
          }
)

# Dropdown validation -------------

test_that(".validate_dropdown_column_values passes when all dropdown options
          exist.",
          {
          testthat::expect_equal(
          benchlingr:::.validate_dropdown_column_values(
            conn, errors=c(), values=c("A", "B", "C"),
            column_name="MyDropdownColumn",
            dropdown_id="sfs_VUhew7oD"),
          c())
          }
)


test_that(".validate_dropdown_column_values throws error when not all dropdown options
          exist.",
          {
          testthat::expect_match(
            benchlingr:::.validate_dropdown_column_values(
              conn, errors=c(), values=c("F", "D"),
              column_name="MyDropdownColumn",
              dropdown_id="sfs_VUhew7oD"),
            "Not all values in 'MyDropdownColumn' are valid dropdown")
            }
)

# Entity link validation -------------

test_that(".validate_entity_column_values throws error when not all IDs correspond
          to registered entities.", {
            testthat::expect_match(
            .validate_entity_column_values(
              conn, 
              errors=c(), values=c("bfi_9fKcrORv", "bfi_FakeID"), 
              column_name="MyAnalyteColumn",
              target_schema_id="analyte",
              id_or_name="id"),
            "Not all values in"
            )
          })
  

test_that(".validate_entity_column_values returns nothing when all IDs correspond
          to registered entities.", {
            testthat::expect_equal(
            .validate_entity_column_values(
              conn, 
              errors=c(), values=c("bfi_9fKcrORv", "bfi_KsLU5uWV", "bfi_VVamxrKQ"), 
              column_name="MyAnalyteColumn",
              target_schema_id="analyte",
              id_or_name="id"),
            c()
            )
          })

test_that(".validate_entity_column_values throws error when not all names correspond
          to registered entities.", {
            testthat::expect_match(
            .validate_entity_column_values(
              conn, 
              errors=c(), values=c("12C-Methylmalonic Acid", "FakeAnalyte"), 
              column_name="MyAnalyteColumn",
              target_schema_id="analyte",
              id_or_name="name$"),
            "Not all values in"
            )
          })


test_that(".validate_entity_column_values returns nothing when all names correspond
          to registered entities.", {
            testthat::expect_equal(
            .validate_entity_column_values(
              conn, 
              errors=c(), values=c("12C-Methylmalonic Acid", "13C-Methylmalonic Acid"), 
              column_name="MyAnalyteColumn",
              target_schema_id="analyte",
              id_or_name="name$"),
            c()
            )
          })

# Type-check -------------------------------
benchling_types <- c('text', 'entity_link', 'dropdown', 'long_text', 
                     'storage_link', 'blob_link', 'dna_sequence_link')
test_that(".validate_entity_column_types returns nothing when character
          vector is passed to a text field.", {
  testthat::expect_equal(
    .validate_column_types(errors=c(), values=c("string1", "string2"), 
                           column_name="MyValidStringColumn",
                           benchling_type="text", 
                           multi_select=FALSE),
    c()
    )}
)

test_that(".validate_entity_column_types returns nothing when character
          vector is passed to an entity field.", {
            testthat::expect_equal(
              .validate_column_types(errors=c(),  values=c("bfi_9fKcrORv", "bfi_KsLU5uWV", "bfi_VVamxrKQ"), 
                                     column_name="MyValidEntityColumn",
                                     benchling_type="entity_link", 
                                     multi_select=FALSE),
              c()
            )}
)


test_that(".validate_entity_column_types returns nothing when character
          vector is passed to a dropdown field.", {
            testthat::expect_equal(
              .validate_column_types(errors=c(),  values=c("A", "B", "C"), 
                                     column_name="MyValidDropdownColumn",
                                     benchling_type="dropdown", 
                                     multi_select=FALSE),
              c()
            )}
)

test_that(".validate_entity_column_types returns nothing when character
          vector is passed to a text or long text field.", {
            testthat::expect_equal(
              .validate_column_types(errors=c(),  values=c("A", "B", "C"), 
                                     column_name="MyValidTextColumn",
                                     benchling_type="text", 
                                     multi_select=FALSE),
              c()
            )
            testthat::expect_equal(
              .validate_column_types(errors=c(),  values=c("A", "B", "C"), 
                                     column_name="MyValidTextColumn",
                                     benchling_type="long_text", 
                                     multi_select=FALSE),
              c()
            )
            
            }
)

test_that(".validate_entity_column_types returns nothing when character
          vector is passed to a blob link", {
            testthat::expect_equal(
              .validate_column_types(errors=c(),  values=c("test-upload_assay_results.R"), 
                                     column_name="MyValidBlobLinkColumn",
                                     benchling_type="blob_link", 
                                     multi_select=FALSE),
              c()
            )}
)


# upload_assay_results -------------------------------


res <- data.frame(
  file = "test-upload_assay_results.R",
  plate = 1,
  study_name = "MAC1",
  date = as.character(Sys.Date()),
  datetime = as.character(Sys.time()),
  bool = 1,
  json = RJSONIO::toJSON(list(algorithm="sgd")),
  dna_sequence = "seq_Cuf0bmCm",
  analyte="bfi_KsLU5uWV"
)

#benchlingr::upload_assay_results(conn, client, df=res, project_id=NULL, 
#               schema_id="assaysch_eBsoKyRO", tenant="hemoshear-dev",
#               api_key=Sys.getenv("BENCHLING_DEV_API_KEY"),
#               id_or_name = "id")

test_that("upload_assay_results will stop if a file in a blob link column
          does not exist on the local machine.", {
    res$file <- 'fakefile'
    testthat::expect_error(
    benchlingr::upload_assay_results(
      conn, client, df=res, project_id="src_ZRvTYOgM", 
      schema_id="assaysch_eBsoKyRO",
      tenant="https://hemoshear-dev.benchling.com",
      api_key=Sys.getenv("BENCHLING_DEV_API_KEY"),
      id_or_name = "id")
    )
}
)

test_that("upload_assay_results will succeed with valid input.", {
            res$file <- 'test-upload_results.R'
            testthat::expect_error(
              res <- benchlingr::upload_assay_results(
                conn, client, df=res, project_id="src_ZRvTYOgM", 
                schema_id="assaysch_eBsoKyRO", 
                tenant="https://hemoshear-dev.benchling.com",
                api_key=Sys.getenv("BENCHLING_DEV_API_KEY"),
                id_or_name = "id")
            )
          }
)


# DBI::dbGetQuery(conn, 'SELECT * FROM uploadresulttestschema$raw')
