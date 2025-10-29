library(testthat)
library(dplyr)

context("Testing core pipeline functions")

# Test clean_analytic_dataset()
test_that("clean_analytic_dataset correctly filters and transforms data", {
  # Create a sample merged dataset
  sample_data <- data.frame(
    SEQN = 1:5,
    RIDAGEYR = c(18, 25, 65, 30, 40),
    RIAGENDR = c(1, 2, 1, 2, 1),
    BMXBMI = c(18, 22, 28, 32, 41),
    bodyfat_pct = c(15, 25, 30, 35, 45),
    WTMEC2YR = rep(1000, 5),
    SDMVSTRA = rep(1, 5),
    SDMVPSU = rep(1, 5)
  )
  
  # Create a sample config
  sample_config <- list(
    analysis = list(
      age_range = c(20, 59)
    )
  )
  
  # Run the function
  cleaned <- clean_analytic_dataset(sample_data, sample_config)
  
  # Assertions
  expect_equal(nrow(cleaned), 3) # Should filter out 18 and 65 year olds
  expect_true(all(c("bmi_cat", "sex") %in% names(cleaned)))
  expect_equal(levels(cleaned$bmi_cat), c("Underweight", "Normal", "Overweight", "Obesity I", "Obesity II", "Obesity III"))
  expect_equal(as.character(cleaned$bmi_cat), c("Normal", "Obesity I", "Obesity III"))
  expect_equal(as.character(cleaned$sex), c("Female", "Female", "Male"))
})

# Test merge_nhanes_datasets()
test_that("merge_nhanes_datasets correctly joins data", {
  # Create sample component datasets
  demo <- data.frame(SEQN = 1:2, RIDAGEYR = c(25, 35), RIAGENDR = c(1,2), RIDRETH1 = c(1,2), WTMEC2YR = c(1,1), SDMVSTRA = c(1,1), SDMVPSU = c(1,1))
  bmx <- data.frame(SEQN = 1:2, BMXBMI = c(22, 27))
  dxx <- data.frame(SEQN = 1:2, DUMMYFAT = c(25, 35))
  dxxag <- data.frame(SEQN = 1:2, ANDROID = c(1,2), GYNOID = c(2,3))
  
  # Run the function
  merged <- merge_nhanes_datasets(demo, bmx, dxx, dxxag, "DUMMYFAT", "ANDROID", "GYNOID")
  
  # Assertions
  expect_equal(nrow(merged), 2)
  expect_equal(ncol(merged), 10) # 7 from demo + 1 from bmx + 1 from dxx + 2 from dxxag, minus SEQN
  expect_true(all(c("BMXBMI", "DUMMYFAT", "ANDROID", "GYNOID") %in% names(merged)))
})
