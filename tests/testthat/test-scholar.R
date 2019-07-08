context("scholar tests - online")

test_that("get_profile works", {
    skip_on_cran()
    skip_if_offline()
    expect_is(p <- get_profile('xJaxiEEAAAAJ'), 'list')
    expect_equal(p[['name']], 'Isaac Newton')
})

# Here we could add tests that use cached data
# context("scholar tests - offline")

