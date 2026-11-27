test_that("TogoIDConverter initialization works", {
  converter <- TogoIDConverter$new()
  expect_s3_class(converter, "TogoIDConverter")
  expected_url <- Sys.getenv("TOGOID_API_ENDPOINT", "https://api.togoid.dbcls.jp")
  expect_equal(converter$api_base_url, expected_url)
})

test_that("TogoIDConverter with custom API URL works", {
  converter <- TogoIDConverter$new(api_base_url = "http://example.com")
  expect_equal(converter$api_base_url, "http://example.com")
})

test_that("togoid_convert wrapper function works", {
  skip_on_cran()
  skip_if_offline()

  result <- togoid_convert(
    ids = c("1", "9"),
    route = c("ncbigene", "ensembl_gene"),
    format = "table"
  )

  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("togoid_convert returns dataframe format", {
  skip_on_cran()
  skip_if_offline()

  result <- togoid_convert(
    ids = c("1", "9"),
    route = c("ncbigene", "ensembl_gene"),
    format = "dataframe"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_equal(ncol(result), 2)
})

test_that("TogoIDConverter$search_databases works", {
  skip_on_cran()
  skip_if_offline()

  converter <- TogoIDConverter$new()
  result <- converter$search_databases("uniprot")

  expect_type(result, "list")
})

test_that("TogoIDConverter$config_dataset works", {
  skip_on_cran()
  skip_if_offline()

  converter <- TogoIDConverter$new()
  result <- converter$config_dataset("ncbigene")

  expect_type(result, "list")
  expect_true("label" %in% names(result))
})

test_that("togoid_convert annotates and filters prefixed IDs", {
  skip_on_cran()
  skip_if_offline()

  # /convert returns GO IDs prefixed (GO:0005829); annotations are keyed by
  # the raw local ID
  route <- c("orphanet_gene", "uniprot", "go")
  annotated <- togoid_convert("217124", route, annotate = list(list("go", "label")))
  expect_true(any(grepl("^GO:", annotated$go)))
  expect_false(all(is.na(annotated[["go.label"]])))

  filtered <- togoid_convert(
    "217124", route,
    annotate = list(list("go", "go_aspect")),
    filter = list(list("go", "go_aspect", "molecular_function"))
  )
  expect_true(nrow(filtered) > 0)
  expect_true(all(filtered[["go.go_aspect"]] == "molecular_function"))
})

test_that("togoid_get_ortholog keeps unconvertible IDs as NA rows", {
  skip_on_cran()
  skip_if_offline()

  result <- togoid_get_ortholog(
    ids = c("672", "999999999"),
    route = c("ncbigene", "homologene"),
    target_taxids = "10090"
  )
  expect_true("672" %in% result$source_id)
  missing <- result[result$source_id == "999999999", ]
  expect_equal(nrow(missing), 1)
  expect_true(is.na(missing$target_id))
})
