context("test-url_creation")

u0 <- make_comcat_url()

test_that("default works", {
  expect_is(u0, 'character')
  expect_is(u0, 'comcat_url')
})
