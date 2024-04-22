library(systemfit)
library(datasets)

# domir.formula and domin tests ----

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
cdl_names_new <- list(c("vs", "cyl", "carb"), paste0("include_at_", 1:3))

cdl_test <- matrix(c(vs_c, cyl_c, carb_c), nrow = 3, ncol = 3, 
                   byrow = TRUE, dimnames = cdl_names)

cdl_test_new <- matrix(c(vs_c, cyl_c, carb_c), nrow = 3, ncol = 3, 
                   byrow = TRUE, dimnames = cdl_names_new)

test_obj <- domin(mpg ~ vs + cyl + carb, "lm", list("summary", "r.squared"), 
                  data = mtcars)

test_obj_new <- domir(mpg ~ vs + cyl + carb, 
                      function(fml, data) {
                        res <- summary(lm(fml, data = data))
                        return(res[["r.squared"]])
                      },
                  data = mtcars)

test_that("Test Conditional Dominance Computation: domin", {
  expect_equal(test_obj$Conditional_Dominance, cdl_test
  )}
)

test_that("Test Conditional Dominance Computation: domir", {
  expect_equal(test_obj_new$Conditional_Dominance, cdl_test_new
  )}
)

gen_test <- rowMeans(cdl_test)

names(gen_test) <- cdl_names[[1]]

test_that("Test General Dominance Computation: domin", {
  expect_equal(test_obj$General_Dominance, gen_test
  )}
)

test_that("Test General Dominance Computation: domir", {
  expect_equal(test_obj_new$General_Dominance, gen_test
  )}
)

cyl_vs_cpt <- mean(c(cyl_mgn[[3]] > vs_mgn[[3]], 
                     cyl_mgn[[1]] > vs_mgn[[1]]))

carb_vs_cpt <- mean(c(carb_mgn[[3]] > vs_mgn[[2]], 
                    carb_mgn[[1]] > vs_mgn[[1]]))

carb_cyl_cpt <- mean(c(carb_mgn[[2]] > cyl_mgn[[2]], 
                     carb_mgn[[1]] > cyl_mgn[[1]]))

cpt_test <- matrix(c(NA, cyl_vs_cpt, carb_vs_cpt, 
                     1-cyl_vs_cpt, NA, carb_cyl_cpt, 
                     1-carb_vs_cpt, 1-carb_cyl_cpt, NA),
                   nrow = 3, ncol = 3)

dimnames(cpt_test) <- list(
  paste0(c("vs", "cyl", "carb"), "_>"),
  paste0(">_", c("vs", "cyl", "carb"))
)

dmn_trns <- function(val) {
  ifelse(val == 1, TRUE, ifelse(val == 0, FALSE, NA))
}
cpt_test_dmn <- matrix(c(NA, dmn_trns(cyl_vs_cpt), dmn_trns(carb_vs_cpt), 
                         dmn_trns(1-cyl_vs_cpt), NA, dmn_trns(carb_cyl_cpt), 
                         dmn_trns(1-carb_vs_cpt), dmn_trns(1-carb_cyl_cpt), NA),
                   nrow = 3, ncol = 3)

dimnames(cpt_test_dmn) <- list(
  paste0("Dmnates_", c("vs", "cyl", "carb")),
  paste0("Dmnated_", c("vs", "cyl", "carb"))
)

test_that("Test Complete Dominance Designation: domin", {
  expect_equal(test_obj$Complete_Dominance, cpt_test_dmn
  )})

test_that("Test Complete Dominance Designation: domir", {
  expect_equal(test_obj_new$Complete_Dominance, cpt_test
  )})

test_that("Test Overall Fit Statistic Value: domin", {
  expect_equal(test_obj$Fit_Statistic_Overall, 
               cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + carb, data = mtcars)))^2
  )})

test_that("Test Overall Fit Statistic Value: domir", {
  expect_equal(test_obj_new$Value,
               cor(mtcars$mpg, predict(lm(mpg ~ vs + cyl + carb, data = mtcars)))^2
  )})

# domir.formula_list tests ----

baseline_ll <- 
  logLik(systemfit(list(mpg ~ 1, qsec ~ 1), data = mtcars))[[1]]

vs_fl_mgn <- c( 
  (1 - logLik(systemfit(list(mpg ~ vs, qsec ~ 1), data = mtcars))[[1]]/baseline_ll),
  (1 - logLik(systemfit(list(mpg ~ vs + cyl, qsec ~ 1), data = mtcars))[[1]]/baseline_ll) - 
    (1 - logLik(systemfit(list(mpg ~ cyl, qsec ~ 1), data = mtcars))[[1]]/baseline_ll),
  (1 - logLik(systemfit(list(mpg ~ vs, qsec ~ carb), data = mtcars))[[1]]/baseline_ll) - 
    (1 - logLik(systemfit(list(mpg ~ 1, qsec ~ carb), data = mtcars))[[1]]/baseline_ll) , 
  (1 - logLik(systemfit(list(mpg ~ vs + cyl, qsec ~ carb), data = mtcars))[[1]]/baseline_ll) - 
    (1 - logLik(systemfit(list(mpg ~ cyl, qsec ~ carb), data = mtcars))[[1]]/baseline_ll) )

cyl_fl_mgn <- c( 
  (1 - logLik(systemfit(list(mpg ~ cyl, qsec ~ 1), data = mtcars))[[1]]/baseline_ll),
  (1 - logLik(systemfit(list(mpg ~ vs + cyl, qsec ~ 1), data = mtcars))[[1]]/baseline_ll) - 
    (1 - logLik(systemfit(list(mpg ~ vs, qsec ~ 1), data = mtcars))[[1]]/baseline_ll),
  (1 - logLik(systemfit(list(mpg ~ cyl, qsec ~ carb), data = mtcars))[[1]]/baseline_ll) - 
    (1 - logLik(systemfit(list(mpg ~ 1, qsec ~ carb), data = mtcars))[[1]]/baseline_ll) , 
  (1 - logLik(systemfit(list(mpg ~ vs + cyl, qsec ~ carb), data = mtcars))[[1]]/baseline_ll) - 
    (1 - logLik(systemfit(list(mpg ~ vs, qsec ~ carb), data = mtcars))[[1]]/baseline_ll) )

carb_fl_mgn <- c( 
  (1 - logLik(systemfit(list(mpg ~ 1, qsec ~ carb), data = mtcars))[[1]]/baseline_ll),
  (1 - logLik(systemfit(list(mpg ~ vs, qsec ~ carb), data = mtcars))[[1]]/baseline_ll) - 
    (1 - logLik(systemfit(list(mpg ~ vs, qsec ~ 1), data = mtcars))[[1]]/baseline_ll),
  (1 - logLik(systemfit(list(mpg ~ cyl, qsec ~ carb), data = mtcars))[[1]]/baseline_ll) - 
    (1 - logLik(systemfit(list(mpg ~ cyl, qsec ~ 1), data = mtcars))[[1]]/baseline_ll) , 
  (1 - logLik(systemfit(list(mpg ~ vs + cyl, qsec ~ carb), data = mtcars))[[1]]/baseline_ll) - 
    (1 - logLik(systemfit(list(mpg ~ vs + cyl, qsec ~ 1), data = mtcars))[[1]]/baseline_ll) )

vs_fl_c <- c(vs_fl_mgn[[1]], mean(vs_fl_mgn[2:3]), vs_fl_mgn[[4]])
cyl_fl_c <- c(cyl_fl_mgn[[1]], mean(cyl_fl_mgn[2:3]), cyl_fl_mgn[[4]])
carb_fl_c <- c(carb_fl_mgn[[1]], mean(carb_fl_mgn[2:3]), carb_fl_mgn[[4]])

cdl_fl_names_new <- list(c("mpg~vs", "mpg~cyl", "qsec~carb"), paste0("include_at_", 1:3))

cdl_fl_test_new <- matrix(c(vs_fl_c, cyl_fl_c, carb_fl_c), nrow = 3, ncol = 3, 
                       byrow = TRUE, dimnames = cdl_fl_names_new)

test_fl_obj_new <- 
  domir(formula_list(
    mpg ~ vs + cyl, 
    qsec ~ carb), 
        function(fml, data, b_ll) {
          res <- logLik(
            systemfit(fml, data = data)
          )[[1]]
          (1 - res/b_ll)
        },
        data = mtcars, b_ll = baseline_ll)

test_that("Test Conditional Dominance Computation: domir.formula_list", {
  expect_equal(test_fl_obj_new$Conditional_Dominance, cdl_fl_test_new
  )}
)

gen_fl_test <- rowMeans(cdl_fl_test_new)

names(gen_fl_test) <- cdl_fl_names_new[[1]]

test_that("Test General Dominance Computation: domir.formula_list", {
  expect_equal(test_fl_obj_new$General_Dominance, gen_fl_test
  )}
)

cyl_vs_cpt_fl <- 
  mean(c(cyl_fl_mgn[[3]] > vs_fl_mgn[[3]], 
             cyl_fl_mgn[[1]] > vs_fl_mgn[[1]])) 

carb_vs_cpt_fl <- 
  mean(c(carb_fl_mgn[[3]] > vs_fl_mgn[[2]], 
             carb_fl_mgn[[1]] > vs_fl_mgn[[1]]))

carb_cyl_cpt_fl <- 
  mean(c(carb_fl_mgn[[2]] > cyl_fl_mgn[[2]],
             carb_fl_mgn[[1]] > cyl_fl_mgn[[1]]))

cpt_fl_test <- 
  matrix(c(NA, cyl_vs_cpt_fl, carb_vs_cpt_fl, 
           1-cyl_vs_cpt_fl, NA, carb_cyl_cpt_fl, 
           1-carb_vs_cpt_fl, 1-carb_cyl_cpt_fl, NA),
         nrow = 3, ncol = 3)

dimnames(cpt_fl_test) <- list(
  paste0(cdl_fl_names_new[[1]], "_>"),
  paste0(">_", cdl_fl_names_new[[1]])
)

test_that("Test Complete Dominance Designation: domir.formula_list", {
  expect_equal(test_fl_obj_new$Complete_Dominance, cpt_fl_test
  )})

test_that("Test Overall Fit Statistic Value: domir.formula_list", {
  expect_equal(
    test_fl_obj_new$Value,
    (1 - logLik(systemfit(list(mpg ~ vs + cyl, qsec ~ carb), data = mtcars))[[1]]/baseline_ll)
  )})