test_that("IDs construct", {
  expect_equal(construct_id(identifier = 1756, version = 1), "edi.1756.1")
})
test_that("Quoted numbers work", {
  expect_equal(construct_id(identifier = "1756", version = "1"), "edi.1756.1")
})
test_that("Nonexistent versions fail", {
  expect_error(construct_id(identifier = 1756, version = 20))
})
test_that("Unexpected identifiers fail ID construction", {
  expect_error(construct_id(identifier = "words", version = "newest"))
})
test_that("Multiple parameters work", {
  expect_no_error(download_parameters(parameters = c("chla", "tss"), version = "newest"))
})
test_that("Unexpected parameters fail", {
  # Requires "chla" instead of "chlorophyll"
  expect_error(download_parameters(parameters = c("chlorophyll"), version = "newest"))
})
