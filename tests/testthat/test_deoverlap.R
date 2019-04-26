library(testthat)

context('rule deoverlapping algorithm correctness')

test_that('single dimnension overlapped rules are deoverlapped', {
  rules <- data.frame(
    rule_id = c('1', '1', '2', '2', '3', '4', '5'),
    feature = c('x', 'x', 'x', 'x', 'x', 'x', 'x'),
    split = c(1, 3, 2, 4, 1, 2, 3),
    less_than = c(F, T, F, T, F, T, F),
    stringsAsFactors = FALSE
  )
  
  deoverlapped_rules <- xrf_deoverlap_rules(rules) %>%
    select(rule_id, min, max) %>%
    arrange(min, max)
  
  
  expect_equal(sort(deoverlapped_rules$min),
               c(-Inf, 1, 1, 2, 2, 3, 3, 4))
  expect_equal(deoverlapped_rules$max,
               c(1, 2, 2, 3, 3, 4, 4, Inf))
})

test_that('multi dimension overlapped rules are deoverlapped', {
  # these form one bounded square and an non-bounded square (towards positive Inf on both axes)
  # they overlap in the upper right corner of the bounded square:
  #             |
  #             |   
  #             |         y
  # ____________|___
  # |           |   |
  # |           |   |
  # |      x    |___|___________
  # |               |
  # |               |
  # _________________
  rules <- data.frame(
    rule_id = c('1', '1', '1', '1', '2', '2'),
    feature = c('x', 'x', 'y', 'y', 'x', 'y'),
    split = c(0, 2, 0, 2, 1, 1),
    less_than = c(F, T, F, T, F, F),
    stringsAsFactors = FALSE
  )
  
  deoverlapped_rules <- xrf_deoverlap_rules(rules) %>%
    select(rule_id, min, max, dimension)
  
  rule_bounds <- deoverlapped_rules %>%
    group_by(rule_id) %>%
    count() %>%
    pull(n) %>%
    sort
  
  # this isn't an exact check, because unlike the 1d case, the 2d is harder to reason about and has almost 20 rows
  # instead we check for the correct number of regions and number of bounds on each region, which is close
  # we expect a result with structure similar to:
  #             |   |
  #             | a |  
  #             |   |     
  # ____________|___|   b
  # |           |   |
  # |    c      | d |
  # |___________|___|_______
  # |               |
  # |      e        |
  # ________________|
  #
  # note there are 5 regions. 3 have 4 boundaries (splits), 
  # of course there are similar solutions with this structure that are also correct, so we don't make any exact comparisons
  expect_equal(length(rule_bounds), 5)
  expect_equal(rule_bounds, c(2, 3, 4, 4, 4))
  
  # additionally, we test a point from each of the above regions to make sure they are belong to one region
  distinct_deoverlapped_rules <- deoverlapped_rules %>% 
    select(rule_id, min, max, dimension) %>%
    distinct()
  
  test_points <- data.frame(
    x <- rep(c(.5, 1.5, 3), each = 3),
    y <- rep(c(.5, 1.5, 3), 3),
    covered <- c(1, 1, 0, 1, 1, 1, 0, 1, 1)
  )
  correctly_covering <- sapply(1:nrow(test_points), function(ix) {
    row <- test_points[ix, ]
    n_regions <- distinct_deoverlapped_rules %>%
      filter((row$x >= min & row$x <= max & dimension == 'x') | (row$y >= min & row$y <= max & dimension == 'y')) %>%
      group_by(rule_id) %>%
      count() %>%
      filter(n == 2) %>%
      nrow()
    
    n_regions == row$covered
  })
  
  expect_true(all(correctly_covering))
})

test_that('non-overlapped rules are unchanged', {
  rules <- data.frame(
    rule_id = c('1', '1', '2', '2'),
    feature = c('x', 'y', 'x', 'y'),
    split = c(1, 1, 2, 2),
    less_than = c(T, T, F, F),
    stringsAsFactors = FALSE
  )
  
  deoverlapped_rules <- xrf_deoverlap_rules(rules) %>%
    select(min, max, dimension) %>%
    arrange(min, max, dimension)
  
  # the equals dataframe is equivalent to the input dataframe (restructured)
  expect_equal(deoverlapped_rules, data.frame(
    min = c(-Inf, -Inf, 2, 2), 
    max = c(1, 1, Inf, Inf), 
    dimension = c('x', 'y', 'x', 'y'),
    stringsAsFactors = FALSE)
  )
})