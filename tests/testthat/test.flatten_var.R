library(vars)
data(Canada)
var <- VAR(Canada, p = 1)
nvars <- ncol(Canada)

context("Check flatten_var")
test_that("basic flatten_var", {
  r <- flatten_var(var)
  expect_equal(class(r), "list")
  expect_length(r, 3)
  expect_named(r, c("est", "se", "p_value"))

  context("check the components")
  expect_equal(sapply(r, class),
               c(est = "data.frame", se = "data.frame", p_value = "data.frame"))

  expect_equal(sapply(r, nrow), c(est = nvars, se = nvars, p_value = nvars))
  expect_equal(sapply(r, ncol), c(est = nvars + 2, se = nvars + 2,
                                  p_value = nvars + 2))

  exp_vars <- colnames(Canada)
  col_vars <- c("var", paste0(exp_vars, ".l1"), "const")

  expect_equal(lapply(r, colnames),
               list(est = col_vars, se = col_vars, p_value = col_vars))

  expect_equal(lapply(r, `[[`, "var"),
               list(est = exp_vars, se = exp_vars, p_value = exp_vars))
})
