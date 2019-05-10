require(testthat)
context("test-url_creation")

# Create
u0 <- make_comcat_url()
c0 <- make_comcat_url(method='count')

ut1 <- make_comcat_url(starttime='2019-05-01', endtime='2019-05-09')
ct1a <- make_comcat_url(method='count', starttime='2019-05-01', endtime='2019-05-09')
ct1b <- make_comcat_url(method='count', starttime='2019-05-01T00:00:00', endtime='2019-05-09T00:00:00')

search_area <- list(latitude=32, longitude=-120, maxradiuskm=400)
search_area_sq <- list(minlatitude=30, minlongitude=-122, maxlatitude=34, maxlongitude=-118)
ud1 <- make_comcat_url(starttime='2019-05-01T00:00:00', endtime='2019-05-09T00:00:00', search_circle = search_area)
cd1a <- make_comcat_url(method='count', starttime='2019-05-01T00:00:00', endtime='2019-05-09T00:00:00', search_circle = search_area)
cd1b <- make_comcat_url(method='count', starttime='2019-05-01T00:00:00', endtime='2019-05-09T00:00:00', search_circle = search_area)

ud2 <- make_comcat_url(starttime='2019-05-01T00:00:00', endtime='2019-05-09T00:00:00', search_box = search_area_sq)
cd2a <- make_comcat_url(method='count', starttime='2019-05-01T00:00:00', endtime='2019-05-09T00:00:00', search_box = search_area_sq)
cd2b <- make_comcat_url(method='count', starttime='2019-05-01T00:00:00', endtime='2019-05-09T00:00:00', search_box = search_area_sq)

# Convert
u0_c <- convert_to(u0, verbose=FALSE)
c0_c <- convert_to(c0, verbose=FALSE)
u0_recovered <- convert_to(u0_c, verbose=FALSE)
c0_recovered <- convert_to(c0_c, verbose=FALSE)


# Test conditions:

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

test_that("specifying time and circular area works", {

  expect_is(ud1, 'character')
  expect_is(ud1, 'comcat_url')

  expect_is(cd1a, 'character')
  expect_is(cd1a, 'comcat_url')

  expect_is(cd1b, 'character')
  expect_is(cd1b, 'comcat_url')

  expect_equal(cd1a, cd1b)
})

test_that("specifying time and rectangular area works", {

  expect_is(ud2, 'character')
  expect_is(ud2, 'comcat_url')

  expect_is(cd2a, 'character')
  expect_is(cd2a, 'comcat_url')

  expect_is(cd2b, 'character')
  expect_is(cd2b, 'comcat_url')

  expect_equal(cd2a, cd2b)
})

test_that("url conversion works", {
	expect_is(u0_c, 'comcat_url')
	expect_is(c0_c, 'comcat_url')
	expect_equal(u0, u0_recovered)
	expect_equal(c0, c0_recovered)
})