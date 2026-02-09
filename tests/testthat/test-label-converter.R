test_that("LabelConverter initialization works", {
  converter <- LabelConverter$new()
  expect_s3_class(converter, "LabelConverter")
  expected_url <- Sys.getenv("TOGOID_API_ENDPOINT", "https://api.togoid.dbcls.jp")
  expect_equal(converter$api_base_url, expected_url)
})

test_that("LabelConverter with verbose mode works", {
  converter <- LabelConverter$new(verbose = TRUE)
  expect_true(converter$verbose)
})

test_that("togoid_label2id wrapper works with ncbigene", {
  skip_on_cran()
  skip_if_offline()

  result <- togoid_label2id(
    labels = c("BRCA1"),
    dataset = "ncbigene",
    taxonomy = "9606"
  )

  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true("identifier" %in% names(result[[1]]))
})

test_that("LabelConverter$convert detects correct API", {
  skip_on_cran()
  skip_if_offline()

  converter <- LabelConverter$new(verbose = FALSE)

  # ncbigene should use SPARQList
  result <- converter$convert(
    labels = c("BRCA1"),
    dataset = "ncbigene",
    taxonomy = "9606"
  )

  expect_type(result, "list")
  expect_true(length(result) > 0)
})
