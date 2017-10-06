testthat::test_that("fars basic tests", {
 
  # Tests that fars_summarize_years works as expected
  test_that("fars_summarize_years() works as expected", {
    expect_is(fars_summarize_years(c(2014,2015)), "tbl_df")
    expect_error(fars_summarize_years(2049))
  })
  
  # Testing if fars_map_state runs without warnings/errors
  test_that("fars_map_state() is silent", {
    expect_silent(fars_map_state(1,2014))
    expect_error(fars_map_state(75,2015))
  })
})