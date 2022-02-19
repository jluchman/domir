
library(domir); library(datasets)

vs_mgn <- c( 
  cor(mtcars$mpg, predict(lm(mpg ~ vs, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ cyl, data = mtcars)))^2, 
  cor(mtcars$mpg, predict(lm(mpg ~ vs + carb + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ carb + am, data = mtcars)))^2, 
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + carb + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ cyl + carb + am, data = mtcars)))^2)

cyl_mgn <- c(
  cor(mtcars$mpg, predict(lm(mpg ~ cyl, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ vs, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ cyl + carb + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ carb + am, data = mtcars)))^2, 
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + carb + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ vs + carb + am, data = mtcars)))^2)

set_mgn <- c( 
  cor(mtcars$mpg, predict(lm(mpg ~ carb + am, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ vs + carb + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ vs, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ cyl + carb + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ cyl, data = mtcars)))^2, 
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + carb + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl, data = mtcars)))^2)

vs_c <- c(vs_mgn[[1]], mean(vs_mgn[2:3]), vs_mgn[[4]])
cyl_c <- c(cyl_mgn[[1]], mean(cyl_mgn[2:3]), cyl_mgn[[4]])
set_c <- c(set_mgn[[1]], mean(set_mgn[2:3]), set_mgn[[4]])

cdl_names <- list(c("vs", "cyl", "set1"), paste0("IVs_", 1:3))

cdl_test <- matrix(c(vs_c, cyl_c, set_c), nrow = 3, ncol = 3, 
                   byrow = TRUE, dimnames = cdl_names)

test_obj <- domin(mpg ~ vs + cyl, "lm", list("summary", "r.squared"), 
                  data = mtcars, sets = list(c("carb", "am")))

test_that("Test Use of Sets with Conditional Dominance", {
  expect_equal(test_obj$Conditional_Dominance, cdl_test
  )}
)

all_test <- cor(mtcars$mpg, predict(lm(mpg ~ am, data = mtcars)))^2

vs_mgn2 <- c( 
  cor(mtcars$mpg, predict(lm(mpg ~ vs + am, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ cyl + am, data = mtcars)))^2, 
  cor(mtcars$mpg, predict(lm(mpg ~ vs + carb + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ carb + am, data = mtcars)))^2, 
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + carb + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ cyl + carb + am, data = mtcars)))^2)

cyl_mgn2 <- c(
  cor(mtcars$mpg, predict(lm(mpg ~ cyl + am, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ vs + am, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ cyl + carb + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ carb + am, data = mtcars)))^2, 
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + carb + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ vs + carb + am, data = mtcars)))^2)

carb_mgn2 <- c( 
  cor(mtcars$mpg, predict(lm(mpg ~ carb + am, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ vs + carb + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ vs + am, data = mtcars)))^2,
  cor(mtcars$mpg, predict(lm(mpg ~ cyl + carb + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ cyl + am, data = mtcars)))^2, 
  cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + carb + am, data = mtcars)))^2 - 
    cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + am, data = mtcars)))^2)

vs_c2 <- c(vs_mgn2[[1]] - all_test, mean(vs_mgn2[2:3]), vs_mgn2[[4]])
cyl_c2 <- c(cyl_mgn2[[1]] - all_test, mean(cyl_mgn2[2:3]), cyl_mgn2[[4]])
carb_c2 <- c(carb_mgn2[[1]] - all_test, mean(carb_mgn2[2:3]), carb_mgn2[[4]])

cdl_names2 <- list(c("vs", "cyl", "carb"), paste0("IVs_", 1:3))

cdl_test2 <- matrix(c(vs_c2, cyl_c2, carb_c2), nrow = 3, ncol = 3, 
                   byrow = TRUE, dimnames = cdl_names2)

test_obj2 <- domin(mpg ~ vs + cyl + carb, "lm", list("summary", "r.squared"), 
                  data = mtcars, all = c("am"))

test_that("Test Use of All with Conditional Dominance", {
  expect_equal(test_obj2$Conditional_Dominance, cdl_test2
  )}
)

test_that("Test All Subsets Fitstat Value", {
  expect_equal(test_obj2$Fit_Statistic_All_Subsets, all_test
  )}
)

test_obj3 <- domin(mpg ~ vs + cyl, "lm", list("summary", "r.squared"), 
                  data = mtcars, sets = list(c("carb", "am")), complete = FALSE)

test_that("Test Complete Dominance as \"off\"", {
  expect_null(test_obj3$Complete_Dominance
  )})

# here constant model + reverse

vs_cns_mgn <- c( extractAIC(lm(mpg ~ vs, data = mtcars))[[2]] - 
                   extractAIC(lm(mpg ~ 1, data = mtcars))[[2]],
                 extractAIC(lm(mpg ~ vs + cyl, data = mtcars))[[2]] - 
                   extractAIC(lm(mpg ~ cyl, data = mtcars))[[2]], 
                 extractAIC(lm(mpg ~ vs + carb, data = mtcars))[[2]] - 
                   extractAIC(lm(mpg ~ carb, data = mtcars))[[2]], 
                 extractAIC(lm(mpg ~ vs + cyl + carb, data = mtcars))[[2]] - 
                   extractAIC(lm(mpg ~ cyl + carb, data = mtcars))[[2]] )

cyl_cns_mgn <- c( extractAIC(lm(mpg ~ cyl, data = mtcars))[[2]]- 
                    extractAIC(lm(mpg ~ 1, data = mtcars))[[2]],
                 extractAIC(lm(mpg ~ vs + cyl, data = mtcars))[[2]] - 
                   extractAIC(lm(mpg ~ vs, data = mtcars))[[2]], 
                 extractAIC(lm(mpg ~ cyl + carb, data = mtcars))[[2]] - 
                   extractAIC(lm(mpg ~ carb, data = mtcars))[[2]], 
                 extractAIC(lm(mpg ~ vs + cyl + carb, data = mtcars))[[2]] - 
                   extractAIC(lm(mpg ~ vs + carb, data = mtcars))[[2]] )

carb_cns_mgn <- c( extractAIC(lm(mpg ~ carb, data = mtcars))[[2]]- 
                    extractAIC(lm(mpg ~ 1, data = mtcars))[[2]],
                  extractAIC(lm(mpg ~ vs + carb, data = mtcars))[[2]] - 
                    extractAIC(lm(mpg ~ vs, data = mtcars))[[2]], 
                  extractAIC(lm(mpg ~ cyl + carb, data = mtcars))[[2]] - 
                    extractAIC(lm(mpg ~ cyl, data = mtcars))[[2]], 
                  extractAIC(lm(mpg ~ vs + cyl + carb, data = mtcars))[[2]] - 
                   extractAIC(lm(mpg ~ vs + cyl, data = mtcars))[[2]] )

vs_cns_c <- c(vs_cns_mgn[[1]], mean(vs_cns_mgn[2:3]), vs_cns_mgn[[4]])
cyl_cns_c <- c(cyl_cns_mgn[[1]], mean(cyl_cns_mgn[2:3]), cyl_cns_mgn[[4]])
carb_cns_c <- c(carb_cns_mgn[[1]], mean(carb_cns_mgn[2:3]), carb_cns_mgn[[4]])

cdl_cns_names <- list(c("vs", "cyl", "carb"), paste0("IVs_", 1:3))

cdl_cns_test <- matrix(c(vs_cns_c, cyl_cns_c, carb_cns_c), nrow = 3, ncol = 3, 
                   byrow = TRUE, dimnames = cdl_cns_names)

test_obj4 <- domin(mpg ~ vs + cyl + carb, lm, 
                   list(function(x) list(AIC = extractAIC(x)[[2]]), "AIC"), 
                   data=mtcars, reverse = TRUE, consmodel = "1") 

test_that("Test Use of Constant Model with Conditional Dominance", {
  expect_equal(test_obj4$Conditional_Dominance, cdl_cns_test
  )}
)

test_that("Test Constant Model Fitstat Value", {
  expect_equal(test_obj4$Fit_Statistic_Constant_Model, 
               extractAIC(lm(mpg ~ 1, data = mtcars))[[2]]
  )}
)

cyl_vs_cpt <- switch(sum(c(cyl_cns_mgn[[3]] > vs_cns_mgn[[3]]), 
                         c(cyl_cns_mgn[[1]] > vs_cns_mgn[[1]]))+1, TRUE, NA, FALSE) 

carb_vs_cpt <- switch(sum(c(carb_cns_mgn[[3]] > vs_cns_mgn[[2]]), 
                          c(carb_cns_mgn[[1]] > vs_cns_mgn[[1]]))+1, TRUE, NA, FALSE)

carb_cyl_cpt <- switch(sum(c(carb_cns_mgn[[2]] > cyl_cns_mgn[[2]]),
                           c(carb_cns_mgn[[1]] > cyl_cns_mgn[[1]]))+1, TRUE, NA, FALSE)

cpt_rev_test <- matrix(c(NA, cyl_vs_cpt, carb_vs_cpt, 
                     !cyl_vs_cpt, NA, carb_cyl_cpt, 
                     !carb_vs_cpt, !carb_cyl_cpt, NA),
                   nrow = 3, ncol = 3)

dimnames(cpt_rev_test) <- list(
  paste0("Dmnates_", c("vs", "cyl", "carb")),
  paste0("Dmnated_", c("vs", "cyl", "carb"))
)

test_that("Test Reversed Complete Dominance Designation", {
  expect_equal(test_obj4$Complete_Dominance, cpt_rev_test
  )})

gen_rev_test <- rank(rowMeans(cdl_cns_test))

names(gen_rev_test) <- cdl_cns_names[[1]]

test_that("Test Reversed General Dominance Ranking", {
  expect_equal(test_obj4$Ranks, gen_rev_test
  )}
)

test_obj_nocdl <- domin(mpg ~ vs + cyl, "lm", list("summary", "r.squared"), 
                  data = mtcars, sets = list(c("carb", "am")), conditional = FALSE)

test_that("'Conditional is False' General Dominance Statistics", {
  expect_equal(test_obj_nocdl$General_Dominance, test_obj$General_Dominance
  )}
)
