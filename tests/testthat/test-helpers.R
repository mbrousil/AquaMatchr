test_that("check_edi_auth throws error when unauthenticated", {
  # Temporarily clear all possible EDI environment variables for this test block
  withr::with_envvar(
    new = c("EDI_API_KEY" = "", "EDI_TOKEN" = "", "AUTH_TOKEN" = ""),
    {
      expect_error(
        check_edi_auth(),
        regexp = "Authentication is required to query or download data from EDI."
      )
    }
  )
})

test_that("check_edi_auth succeeds when EDI_API_KEY is present", {
  # Temporarily set a mock API key
  withr::with_envvar(
    new = c("EDI_API_KEY" = "fake_test_key_12345"),
    {
      expect_true(check_edi_auth())
    }
  )
})
