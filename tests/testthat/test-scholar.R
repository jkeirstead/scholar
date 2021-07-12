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

test_that("get_citation_history works", {
    skip_on_cran()
    skip_if_offline()
    expect_is(h <- get_citation_history("xJaxiEEAAAAJ"), 'data.frame')
    expect_equal(names(h), c("year", "cites"))
})

test_that("get_article_cite_history works", {
  skip_on_cran()
  skip_if_offline()
  expect_is(ach <- get_article_cite_history("B7vSqZsAAAAJ", "hMod-77fHWUC"), 
            'data.frame')
  expect_equal(names(ach), c("year", "cites", "pubid"))
})

test_that("get_profile works", {
    skip_on_cran()
    skip_if_offline()
    id <- 'xJaxiEEAAAAJ'
    authorlist <- scholar::get_publications(id)$author
    author <- scholar::get_profile(id)$name
    author_position(authorlist, author)
    
    expect_is(h <- get_citation_history("xJaxiEEAAAAJ"), 'data.frame')
    expect_equal(names(h), c("year", "cites"))
})


# Here we could add tests that use cached data
# context("scholar tests - offline")

