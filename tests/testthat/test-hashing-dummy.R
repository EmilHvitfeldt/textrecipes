context("test-hashing-dummy-variables")

library(textrecipes)
library(recipes)
data(grants, package = "modeldata")

test_data <- grants_test[1:20, "sponsor_code", drop = FALSE]


rec <- recipe(~., data = test_data)

test_that("hashing gives double outputs", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_dummy_hash(sponsor_code)
  
  obj <- rec %>%
    prep()
  
  expect_true(
    bake(obj, new_data = NULL) %>%
      select(contains("hash")) %>%
      lapply(is.double) %>%
      unlist() %>%
      all()
  )
  
  expect_equal(dim(tidy(rec, 1)), c(1, 4))
  expect_equal(dim(tidy(obj, 1)), c(1, 4))
})

test_that("hashing output width changes accordingly with num_terms", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_dummy_hash(sponsor_code, num_terms = 256) %>%
    prep()
  
  expect_equal(
    bake(rec, new_data = NULL) %>%
      select(contains("hash")) %>%
      ncol(),
    256
  )
})

test_that("hashing output width changes accordingly with num_terms", {
  skip_if_not_installed("text2vec")
  
  signed <- recipe(~., data = test_data) %>%
    step_dummy_hash(all_predictors(), num_terms = 2) %>%
    prep() %>%
    bake(new_data = NULL)
  
  unsigned <- recipe(~., data = test_data) %>%
    step_dummy_hash(all_predictors(), num_terms = 2, signed = FALSE) %>%
    prep() %>%
    bake(new_data = NULL)
  
  expect_true(all(unsigned$sponsor_code_hash1 == abs(signed$sponsor_code_hash1)))
  expect_true(all(unsigned$sponsor_code_hash2 == abs(signed$sponsor_code_hash2)))
  expect_false(all(unsigned$sponsor_code_hash1 == signed$sponsor_code_hash1))
  expect_false(all(unsigned$sponsor_code_hash2 == signed$sponsor_code_hash2))
})



test_that("printing", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_dummy_hash(sponsor_code)
  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})
