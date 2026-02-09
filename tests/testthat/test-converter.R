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
