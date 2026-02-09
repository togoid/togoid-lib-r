test_that("AnnotationsConverter initialization works", {
  annotator <- AnnotationsConverter$new()
  expect_s3_class(annotator, "AnnotationsConverter")
  expected_url <- Sys.getenv("TOGOID_API_ENDPOINT", "https://api.togoid.dbcls.jp")
  expect_equal(annotator$api_endpoint, expected_url)
  expected_grasp_url <- Sys.getenv("TOGOID_GRASP_ENDPOINT", "https://dx.dbcls.jp/grasp-dev-togoid")
  expect_equal(annotator$grasp_endpoint, expected_grasp_url)
})

test_that("AnnotationsConverter$list_fields works", {
  skip_on_cran()
  skip_if_offline()

  annotator <- AnnotationsConverter$new()
  fields <- annotator$list_fields("ncbigene")

  expect_s3_class(fields, "data.frame")
  expect_true(nrow(fields) > 0)
  expect_true("field_name" %in% names(fields))
  expect_true("label" %in% fields$field_name)
})

test_that("togoid_list_fields wrapper works", {
  skip_on_cran()
  skip_if_offline()

  fields <- togoid_list_fields("ncbigene")

  expect_s3_class(fields, "data.frame")
  expect_true(nrow(fields) > 0)
})

test_that("AnnotationsConverter$execute_query works", {
  skip_on_cran()
  skip_if_offline()

  annotator <- AnnotationsConverter$new()
  result <- annotator$execute_query(
    dataset_name = "ncbigene",
    ids = c("672", "7157"),
    fields = c("label")
  )

  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true("672" %in% names(result) || "7157" %in% names(result))
})

test_that("togoid_annotate wrapper works", {
  skip_on_cran()
  skip_if_offline()

  result <- togoid_annotate(
    dataset = "ncbigene",
    ids = c("672", "7157"),
    fields = c("label")
  )

  expect_type(result, "list")
  expect_true(length(result) > 0)
})
