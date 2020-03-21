context("scholar tests - online")

test_that("get_profile works", {
    skip_on_cran()
    skip_if_offline()
    expect_is(p <- get_profile('xJaxiEEAAAAJ'), 'list')
    expect_equal(p[['name']], 'Isaac Newton')
})

test_that("get_complete_authors works (single)", {
  skip_on_cran()
  skip_if_offline()
  id = "xJaxiEEAAAAJ"
  pubs = get_publications(id)
  result = get_complete_authors(id, pubs$pubid[1])
  expect_equal(length(result), 1)
})

test_that("get_complete_authors works (vector)", {
  skip_on_cran()
  skip_if_offline()
  id = "xJaxiEEAAAAJ"
  pubs = get_publications(id)
  result = get_complete_authors(id, pubs$pubid[1:2])
  expect_equal(length(result), 2)
})
# Here we could add tests that use cached data
# context("scholar tests - offline")

