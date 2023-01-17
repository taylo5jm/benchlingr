

conn <- benchlingr::warehouse_connect(
  "hemoshear-dev",
  Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
  Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))

client <- benchlingr::benchling_api_auth(
  tenant="https://hemoshear-dev.benchling.com",
  api_key = Sys.getenv("BENCHLING_DEV_API_KEY"))


# Blob link validation -------------
test_that(".validate_blob_link_column_values throws error when file does 
          not exist on local machine", {
    testthat::expect_match(
    benchlingr:::.validate_blob_link_column_values(
      client, errors=c(), values="filethatdoesnotexist.txt", 
      column_name="MyFileColumn", multi_select=FALSE,
      fk_type="name"),
      "does not exist")
  }
  )

test_that(".validate_blob_link_column_values returns nothing when file exists.", {
            testthat::expect_equal(
              benchlingr:::.validate_blob_link_column_values(
                client, errors=c(), values="test-upload_results.R", 
                column_name="MyFileColumn", multi_select=FALSE,
                fk_type="name"),
              c())
          }
)

test_that(".validate_blob_link_column_values throws error when blob identifier
          does not exist", {
            testthat::expect_match(
              benchlingr:::.validate_blob_link_column_values(
                client, errors=c(), values='49176d96-42a2-44f2-ae33-d9', 
                column_name="MyFileColumn", multi_select=FALSE,
                fk_type="id"),
              "One or more blob identifiers could not be found")
          }
)

test_that(".validate_blob_link_column_values returns nothing when 
          blob identifier exists", {
            testthat::expect_equal(
              benchlingr:::.validate_blob_link_column_values(
                client, errors=c(), values='49176d96-42a2-44f2-ae33-d97589601b62', 
                column_name="MyFileColumn", multi_select=FALSE,
                fk_type="id"),
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
              fk_type="id"),
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
              fk_type="id"),
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
              fk_type="name$"),
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
              fk_type="name$"),
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

DBI::dbDisconnect(conn)
