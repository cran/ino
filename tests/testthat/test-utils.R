test_that("check for numbers works", {
  expect_false(is_number(0))
  expect_true(is_number(1))
  expect_false(is_number(0.5))
  expect_false(is_number("1"))
})

test_that("trying an expression silently works", {
  expect_equal(try_silent(1 + 1), 2)
  expect_s3_class(try_silent(1 + "1"), "fail")
})

test_that("interruption of long evaluations works", {
  foo <- function(x) {
    Sys.sleep(x)
    return(x)
  }
  expect_equal(timed(foo(0.9), 1), 0.9)
  if (.Platform$OS.type == "windows") expect_null(timed(foo(1.1), 1))
})

test_that("measuring computation time works", {
  what <- function(s) {
    Sys.sleep(s)
    return(s)
  }
  args <- list(s = 1)
  out <- do.call_timed(what = what, args = args, headstart = 1)
  expect_type(out, "list")
  expect_length(out, 2)
  expect_equal(out[["res"]], 1)
  expect_type(out[["time"]], "double")
  expect_true(abs(out[["time"]] - 2) < 1)
})
