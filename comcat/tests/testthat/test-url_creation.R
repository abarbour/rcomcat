context("test-url_creation")

u0 <- make_comcat_url()
c0 <- make_comcat_url(method='count')

ut1 <- make_comcat_url(starttime='2019-05-01', endtime='2019-05-09')
ct1a <- make_comcat_url(method='count', starttime='2019-05-01', endtime='2019-05-09')
ct1b <- make_comcat_url(method='count', starttime='2019-05-01T00:00:00', endtime='2019-05-09T00:00:00')

test_that("default works", {
  expect_is(u0, 'character')
  expect_is(u0, 'comcat_url')

  expect_is(c0, 'character')
  expect_is(c0, 'comcat_url')
})

test_that("specifying time works", {
  expect_is(ut1, 'character')
  expect_is(ut1, 'comcat_url')

  expect_is(ct1a, 'character')
  expect_is(ct1a, 'comcat_url')

  expect_is(ct1b, 'character')
  expect_is(ct1b, 'comcat_url')

  expect_equal(ct1a, ct1b)
})
