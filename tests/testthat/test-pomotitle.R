test_that("no argument works", {
  output <- generate_pomotitle()
  expect_equal(1, length(output))
  expect_equal("character", class(output))
})

test_that("zero argument works", {
  output <- generate_pomotitle(0)
  expect_equal(0, length(output))
  expect_equal("character", class(output))
})

test_that("n argument works", {
  for (n in 1:25) {
    output <- generate_pomotitle(n)
    expect_equal(n, length(output))
    expect_equal("character", class(output))
  }
})

test_that("negative argument fails", {
  expect_error(generate_pomotitle(-1))
})

test_that("incorrect argument type fails", {
  expect_error(generate_pomotitle("this shouldn't work"))
})
