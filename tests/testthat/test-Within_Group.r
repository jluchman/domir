library(pscl)
library(datasets)

# domir.formula ----

value_vec <- 
  c(
    cor(mtcars$mpg, predict(lm(mpg ~ vs, data = mtcars)))^2,
    cor(mtcars$mpg, predict(lm(mpg ~ cyl, data = mtcars)))^2,
    cor(mtcars$mpg, predict(lm(mpg ~ carb, data = mtcars)))^2,
    cor(mtcars$mpg, predict(lm(mpg ~ am, data = mtcars)))^2,
    cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl, data = mtcars)))^2,
    cor(mtcars$mpg, predict(lm(mpg ~ carb + am, data = mtcars)))^2,
    cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + carb, data = mtcars)))^2,
    cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + am, data = mtcars)))^2,
    cor(mtcars$mpg, predict(lm(mpg ~ carb + am + vs, data = mtcars)))^2,
    cor(mtcars$mpg, predict(lm(mpg ~ carb + am + cyl, data = mtcars)))^2,
    cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + carb + am, data = mtcars)))^2
  )

wst1_vec <- 
  c(
    value_vec[[1]]/2 + (value_vec[[5]] - value_vec[[2]])/2,
    value_vec[[2]]/2 + (value_vec[[5]] - value_vec[[1]])/2,
    value_vec[[3]]/2 + (value_vec[[6]] - value_vec[[4]])/2,
    value_vec[[4]]/2 + (value_vec[[6]] - value_vec[[3]])/2
  )

wst2_vec <- 
  c(
    (value_vec[[9]] - value_vec[[6]])/2 + (value_vec[[11]] - value_vec[[10]])/2,
    (value_vec[[10]] - value_vec[[6]])/2 + (value_vec[[11]] - value_vec[[9]])/2,
    (value_vec[[7]] - value_vec[[5]])/2 + (value_vec[[11]] - value_vec[[8]])/2,
    (value_vec[[8]] - value_vec[[5]])/2 + (value_vec[[11]] - value_vec[[7]])/2
  )

cdl_names <- list(c("vs", "cyl", "carb", "am"), paste0("include_at_", 1:2))

cdl_test <- 
  matrix(c(wst1_vec, wst2_vec), nrow = 4, ncol = 2, dimnames = cdl_names)

test_obj <- domir(mpg ~ vs + cyl + carb + am, 
                      function(fml, data) {
                        res <- summary(lm(fml, data = data))
                        return(res[["r.squared"]])
                      },
                      data = mtcars, 
                      .wst = list(~ vs + cyl, ~ carb + am))

test_that("Test Use of W-sets with Conditional Dominance: domir.formula", {
  expect_equal(test_obj$Conditional_Dominance, cdl_test
  )}
)