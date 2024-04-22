library(pscl)
library(datasets)

# domir.formula and domin tests ----

## with sets ----

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
cdl_names_new <- list(c("vs", "cyl", "set1"), paste0("include_at_", 1:3))

cdl_test <- matrix(c(vs_c, cyl_c, set_c), nrow = 3, ncol = 3, 
                   byrow = TRUE, dimnames = cdl_names)

cdl_test_new <- matrix(c(vs_c, cyl_c, set_c), nrow = 3, ncol = 3, 
                   byrow = TRUE, dimnames = cdl_names_new)

test_obj <- domin(mpg ~ vs + cyl, "lm", list("summary", "r.squared"), 
                  data = mtcars, sets = list(c("carb", "am")))

test_obj_new <- domir(mpg ~ vs + cyl + carb + am, 
                      function(fml, data) {
                        res <- summary(lm(fml, data = data))
                        return(res[["r.squared"]])
                      },
                      data = mtcars, 
                      .set = list(~ carb + am))

test_that("Test Use of Sets with Conditional Dominance: domin", {
  expect_equal(test_obj$Conditional_Dominance, cdl_test
  )}
)

test_that("Test Use of Sets with Conditional Dominance: domir", {
  expect_equal(test_obj_new$Conditional_Dominance, cdl_test_new
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

cdl_names2_new <- list(c("vs", "cyl", "carb"), paste0("include_at_", 1:3))

cdl_test2 <- matrix(c(vs_c2, cyl_c2, carb_c2), nrow = 3, ncol = 3, 
                   byrow = TRUE, dimnames = cdl_names2)

cdl_test2_new <- matrix(c(vs_c2, cyl_c2, carb_c2), nrow = 3, ncol = 3, 
                    byrow = TRUE, dimnames = cdl_names2_new)

test_obj2 <- domin(mpg ~ vs + cyl + carb, "lm", list("summary", "r.squared"), 
                  data = mtcars, all = c("am"))

test_obj2_new <- domir(mpg ~ vs + cyl + carb + am, 
                      function(fml, data) {
                        res <- summary(lm(fml, data = data))
                        return(res[["r.squared"]])
                      },
                      data = mtcars, 
                      .all = ~ am)
## with all subests  ----
test_that("Test Use of All with Conditional Dominance: domin", {
  expect_equal(test_obj2$Conditional_Dominance, cdl_test2
  )}
)

test_that("Test Use of All with Conditional Dominance: domir", {
  expect_equal(test_obj2_new$Conditional_Dominance, cdl_test2_new
  )}
)

test_that("Test All Subsets Fitstat Value: domin", {
  expect_equal(test_obj2$Fit_Statistic_All_Subsets, all_test
  )}
)

test_that("Test All Subsets Fitstat Value: domir", {
  expect_equal(test_obj2_new$Value_All, all_test
  )}
)

test_obj3 <- domin(mpg ~ vs + cyl, "lm", list("summary", "r.squared"), 
                  data = mtcars, sets = list(c("carb", "am")), complete = FALSE)

test_obj3_new <- domir(mpg ~ vs + cyl + carb + am, 
                      function(fml, data) {
                        res <- summary(lm(fml, data = data))
                        return(res[["r.squared"]])
                      },
                      data = mtcars, .cpt = FALSE,
                      .set = list(~ carb + am))

### confirm complete can be nullified ----

test_that("Test Complete Dominance as \"off\": domin", {
  expect_null(test_obj3$Complete_Dominance
  )})

test_that("Test Complete Dominance as \"off\": domir", {
  expect_null(test_obj3_new$Complete_Dominance
  )})

## constant model + reverse ----

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
cdl_cns_names_new <- list(c("vs", "cyl", "carb"), paste0("include_at_", 1:3))

cdl_cns_test <- matrix(c(vs_cns_c, cyl_cns_c, carb_cns_c), nrow = 3, ncol = 3, 
                   byrow = TRUE, dimnames = cdl_cns_names)

cdl_cns_test_new <- matrix(c(vs_cns_c, cyl_cns_c, carb_cns_c), nrow = 3, ncol = 3, 
                       byrow = TRUE, dimnames = cdl_cns_names_new)

test_obj4 <- domin(mpg ~ vs + cyl + carb, lm, 
                   list(function(x) list(AIC = extractAIC(x)[[2]]), "AIC"), 
                   data=mtcars, reverse = TRUE, consmodel = "1") 

test_obj4_new <- domir(mpg ~ vs + cyl + carb, 
                       function(fml, data) {
                         res <- lm(fml, data = data)
                         return(extractAIC(res)[[2]])
                       },
                   data=mtcars, .rev = TRUE, .adj = TRUE) 

test_that("Test Use of Constant Model with Conditional Dominance: domin", {
  expect_equal(test_obj4$Conditional_Dominance, cdl_cns_test
  )}
)

test_that("Test Use of Adjustment with Conditional Dominance: domir", {
  expect_equal(test_obj4_new$Conditional_Dominance, cdl_cns_test_new
  )}
)

test_that("Test Constant Model Fitstat Value: domin", {
  expect_equal(test_obj4$Fit_Statistic_Constant_Model, 
               extractAIC(lm(mpg ~ 1, data = mtcars))[[2]]
  )}
)

test_that("Test Adjustment Value: domir", {
  expect_equal(test_obj4_new$Value_Adjust, 
               extractAIC(lm(mpg ~ 1, data = mtcars))[[2]]
  )}
)

cyl_vs_cpt <- mean(c(cyl_cns_mgn[[3]] < vs_cns_mgn[[3]], 
                         cyl_cns_mgn[[1]] < vs_cns_mgn[[1]]))

carb_vs_cpt <- mean(c(carb_cns_mgn[[3]] < vs_cns_mgn[[2]], 
                          carb_cns_mgn[[1]] < vs_cns_mgn[[1]]))

carb_cyl_cpt <- mean(c(carb_cns_mgn[[2]] < cyl_cns_mgn[[2]],
                           carb_cns_mgn[[1]] < cyl_cns_mgn[[1]]))

cpt_rev_test <- matrix(c(NA, cyl_vs_cpt, carb_vs_cpt, 
                     1-cyl_vs_cpt, NA, carb_cyl_cpt, 
                     1-carb_vs_cpt, 1-carb_cyl_cpt, NA),
                   nrow = 3, ncol = 3)

dimnames(cpt_rev_test) <- list(
  paste0(c("vs", "cyl", "carb"), "_>"),
  paste0(">_", c("vs", "cyl", "carb"))
)

dmn_trns <- function(val) {
  ifelse(val == 1, TRUE, ifelse(val == 0, FALSE, NA))
}
cpt_rev_test_dmn <- matrix(c(NA, dmn_trns(cyl_vs_cpt), dmn_trns(carb_vs_cpt), 
                         dmn_trns(1-cyl_vs_cpt), NA, dmn_trns(carb_cyl_cpt), 
                         dmn_trns(1-carb_vs_cpt), dmn_trns(1-carb_cyl_cpt), NA),
                       nrow = 3, ncol = 3)

dimnames(cpt_rev_test_dmn) <- list(
  paste0("Dmnates_", c("vs", "cyl", "carb")),
  paste0("Dmnated_", c("vs", "cyl", "carb"))
)

test_that("Test Reversed Complete Dominance Designation: domin", {
  expect_equal(test_obj4$Complete_Dominance, cpt_rev_test_dmn
  )})

test_that("Test Reversed Complete Dominance Designation: domir", {
  expect_equal(test_obj4_new$Complete_Dominance, cpt_rev_test
  )})

gen_rev_test <- rank(rowMeans(cdl_cns_test))

names(gen_rev_test) <- cdl_cns_names[[1]]

test_that("Test Reversed General Dominance Ranking: domin", {
  expect_equal(test_obj4$Ranks, gen_rev_test
  )}
)

test_that("Test Reversed General Dominance Ranking: domir", {
  expect_equal(test_obj4_new$Ranks, gen_rev_test
  )}
)

test_obj_nocdl <- domin(mpg ~ vs + cyl, "lm", list("summary", "r.squared"), 
                  data = mtcars, sets = list(c("carb", "am")), conditional = FALSE)

test_obj_nocdl_new <- domir(mpg ~ vs + cyl + carb + am, 
                            function(fml, data) {
                              res <- summary(lm(fml, data = data))
                              return(res[["r.squared"]])
                            },
                            data = mtcars, 
                            .set = list(~ carb + am), 
                            .cdl = FALSE)

### confirm conditional can be nullified ----

test_that("'Conditional is False' General Dominance: domin", {
  expect_equal(test_obj_nocdl$General_Dominance, test_obj$General_Dominance
  )}
)

test_that("'Conditional is False' General Dominance: domir", {
  expect_equal(test_obj_nocdl_new$General_Dominance, test_obj_new$General_Dominance
  )}
)

# domir.formula_list ----

adj_mtcars <- 
  transform(mtcars, 
            zi = am*gear)

adj_val <- 
  logLik(
    zeroinfl(
      fmllst2Fml(formula_list(zi ~ 1, infert ~ 1), drop_lhs = 2L),
      data = adj_mtcars) )[[1]]

all_val <- 
  logLik(
    zeroinfl(
      fmllst2Fml(formula_list(zi ~ wt, infert ~ 1), drop_lhs = 2L),
      data = adj_mtcars) )[[1]]
    
vsdrat_fl_mgn <- c( 
  logLik(
    zeroinfl(
      fmllst2Fml(formula_list(zi ~ vs + drat + wt, infert ~ 1), drop_lhs = 2L),
      data = adj_mtcars))[[1]],
  logLik(
    zeroinfl(
      fmllst2Fml(formula_list(zi ~ vs + drat + cyl + hp + wt, infert ~ 1), drop_lhs = 2L),
      data = adj_mtcars))[[1]] - 
    logLik(
      zeroinfl(
        fmllst2Fml(formula_list(zi ~ cyl + hp + wt, infert ~ 1), drop_lhs = 2L),
        data = adj_mtcars))[[1]],
  logLik(
    zeroinfl(
      fmllst2Fml(formula_list(zi ~ vs + drat + wt, infert ~ carb), drop_lhs = 2L),
      data = adj_mtcars))[[1]] - 
    logLik(
      zeroinfl(
        fmllst2Fml(formula_list(zi ~ wt, infert ~ carb), drop_lhs = 2L),
        data = adj_mtcars))[[1]],
  logLik(
    zeroinfl(
      fmllst2Fml(formula_list(zi ~ vs + drat + cyl + hp + wt, infert ~ carb), drop_lhs = 2L),
      data = adj_mtcars))[[1]] - 
    logLik(
      zeroinfl(
        fmllst2Fml(formula_list(zi ~ cyl + hp + wt, infert ~ carb), drop_lhs = 2L),
        data = adj_mtcars))[[1]] )

cylhp_fl_mgn <- c( 
  logLik(
    zeroinfl(
      fmllst2Fml(formula_list(zi ~ cyl + hp + wt, infert ~ 1), drop_lhs = 2L),
      data = adj_mtcars))[[1]],
  logLik(
    zeroinfl(
      fmllst2Fml(formula_list(zi ~ vs + drat + cyl + hp + wt, infert ~ 1), drop_lhs = 2L),
      data = adj_mtcars))[[1]] - 
    logLik(
      zeroinfl(
        fmllst2Fml(formula_list(zi ~ vs + drat + wt, infert ~ 1), drop_lhs = 2L),
        data = adj_mtcars))[[1]],
  logLik(
    zeroinfl(
      fmllst2Fml(formula_list(zi ~ cyl + hp + wt, infert ~ carb), drop_lhs = 2L),
      data = adj_mtcars))[[1]] - 
    logLik(
      zeroinfl(
        fmllst2Fml(formula_list(zi ~ wt, infert ~ carb), drop_lhs = 2L),
        data = adj_mtcars))[[1]],
  logLik(
    zeroinfl(
      fmllst2Fml(formula_list(zi ~ vs + drat + cyl + hp + wt, infert ~ carb), drop_lhs = 2L),
      data = adj_mtcars))[[1]] - 
    logLik(
      zeroinfl(
        fmllst2Fml(formula_list(zi ~ vs + drat + wt, infert ~ carb), drop_lhs = 2L),
        data = adj_mtcars))[[1]] )

carbi_fl_mgn <- c( 
  logLik(
    zeroinfl(
      fmllst2Fml(formula_list(zi ~ wt, infert ~ carb), drop_lhs = 2L),
      data = adj_mtcars))[[1]],
  logLik(
    zeroinfl(
      fmllst2Fml(formula_list(zi ~ vs + drat + wt, infert ~ carb), drop_lhs = 2L),
      data = adj_mtcars))[[1]] - 
    logLik(
      zeroinfl(
        fmllst2Fml(formula_list(zi ~ vs + drat + wt, infert ~ 1), drop_lhs = 2L),
        data = adj_mtcars))[[1]],
  logLik(
    zeroinfl(
      fmllst2Fml(formula_list(zi ~ cyl + hp + wt, infert ~ carb), drop_lhs = 2L),
      data = adj_mtcars))[[1]] - 
    logLik(
      zeroinfl(
        fmllst2Fml(formula_list(zi ~ cyl + hp + wt, infert ~ 1), drop_lhs = 2L),
        data = adj_mtcars))[[1]],
  logLik(
    zeroinfl(
      fmllst2Fml(formula_list(zi ~ vs + drat + cyl + hp + wt, infert ~ carb), drop_lhs = 2L),
      data = adj_mtcars))[[1]] - 
    logLik(
      zeroinfl(
        fmllst2Fml(formula_list(zi ~ vs + drat + cyl + hp + wt, infert ~ 1), drop_lhs = 2L),
        data = adj_mtcars))[[1]] )

vsdrat_fl_c <- c(vsdrat_fl_mgn[[1]] - all_val, mean(vsdrat_fl_mgn[2:3]), vsdrat_fl_mgn[[4]])
cylhp_fl_c <- c(cylhp_fl_mgn[[1]] - all_val, mean(cylhp_fl_mgn[2:3]), cylhp_fl_mgn[[4]])
carbi_fl_c <- c(carbi_fl_mgn[[1]] - all_val, mean(carbi_fl_mgn[2:3]), carbi_fl_mgn[[4]])

cdl_fl_names_set <- list(c("infert~carb", "set1", "set2"), paste0("include_at_", 1:3))

cdl_fl_test_set <- matrix(c(carbi_fl_c, vsdrat_fl_c, cylhp_fl_c), nrow = 3, ncol = 3, 
                          byrow = TRUE, dimnames = cdl_fl_names_set)

test_fl_obj_set <- 
  domir(formula_list(
    zi ~ vs + drat + cyl + hp + wt, 
    infert ~ carb), 
    function(fml, data) {
      
      Fml <- 
        fmllst2Fml(fml, drop_lhs = 2L)
      
      logLik(
        zeroinfl(Fml, data = data)
      )[[1]]
      
    },
    .set = 
      list(formula_list(zi ~ vs + drat), formula_list(zi ~ cyl + hp)),
    .all = formula_list(zi ~ wt),
    data = adj_mtcars,
    .adj = TRUE)

test_that("Test Use of Sets with Conditional Dominance: domir.formula_list", {
  expect_equal(test_fl_obj_set$Conditional_Dominance, cdl_fl_test_set
  )}
)

test_that("Test All Subsets Fitstat Value: domir.formula_list", {
  expect_equal(test_fl_obj_set$Value_All, (all_val - adj_val)
  )}
)

test_that("Test Adjustment Fitstat Value: domir.formula_list", {
  expect_equal(test_fl_obj_set$Value_Adjust, adj_val
  )}
)
