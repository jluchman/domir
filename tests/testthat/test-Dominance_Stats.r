
library(domir); library(datasets)

vs_mgn <- c( 
  cor(mtcars$mpg, predict(lm(mpg ~ vs, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ cyl, data = mtcars)))^2, 
  cor(mtcars$mpg, predict(lm(mpg ~ vs + carb, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ carb, data = mtcars)))^2, 
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + carb, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ cyl + carb, data = mtcars)))^2)

cyl_mgn <- c(
  cor(mtcars$mpg, predict(lm(mpg ~ cyl, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ vs, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ cyl + carb, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ carb, data = mtcars)))^2, 
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + carb, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ vs + carb, data = mtcars)))^2)

carb_mgn <- c( 
  cor(mtcars$mpg, predict(lm(mpg ~ carb, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ vs + carb, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ vs, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ cyl + carb, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ cyl, data = mtcars)))^2, 
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + carb, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl, data = mtcars)))^2)

vs_c <- c(vs_mgn[[1]], mean(vs_mgn[2:3]), vs_mgn[[4]])
cyl_c <- c(cyl_mgn[[1]], mean(cyl_mgn[2:3]), cyl_mgn[[4]])
carb_c <- c(carb_mgn[[1]], mean(carb_mgn[2:3]), carb_mgn[[4]])

cdl_names <- list(c("vs", "cyl", "carb"), paste0("IVs_", 1:3))

cdl_test <- matrix(c(vs_c, cyl_c, carb_c), nrow = 3, ncol = 3, 
                   byrow = TRUE, dimnames = cdl_names)

test_obj <- domin(mpg ~ vs + cyl + carb, "lm", list("summary", "r.squared"), 
                  data = mtcars)

test_that("Test Conditional Dominance Computation", {
  expect_equal(test_obj$Conditional_Dominance, cdl_test
  )}
)

gen_test <- rowMeans(cdl_test)

names(gen_test) <- cdl_names[[1]]

test_that("Test General Dominance Computation", {
  expect_equal(test_obj$General_Dominance, gen_test
  )}
)

cyl_vs_cpt <- switch(sum(c(cyl_mgn[[3]] > vs_mgn[[3]]), 
  c(cyl_mgn[[1]] > vs_mgn[[1]]))+1, FALSE, NA, TRUE) 

carb_vs_cpt <- switch(sum(c(carb_mgn[[3]] > vs_mgn[[2]]), 
  c(carb_mgn[[1]] > vs_mgn[[1]]))+1, FALSE, NA, TRUE)

carb_cyl_cpt <- switch(sum(c(carb_mgn[[2]] > cyl_mgn[[2]]),
  c(carb_mgn[[1]] > cyl_mgn[[1]]))+1, FALSE, NA, TRUE)

cpt_test <- matrix(c(NA, cyl_vs_cpt, carb_vs_cpt, 
                     !cyl_vs_cpt, NA, carb_cyl_cpt, 
                     !carb_vs_cpt, !carb_cyl_cpt, NA),
                   nrow = 3, ncol = 3)

dimnames(cpt_test) <- list(
  paste0("Dmnates_", c("vs", "cyl", "carb")),
  paste0("Dmnated_", c("vs", "cyl", "carb"))
)

test_that("Test Complete Dominance Designation", {
  expect_equal(test_obj$Complete_Dominance, cpt_test
  )})

test_that("Test Overall Fit Statistic Value", {
  expect_equal(test_obj$Fit_Statistic_Overall, 
               cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + carb, data = mtcars)))^2
  )})



