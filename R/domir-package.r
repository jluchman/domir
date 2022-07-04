#' @name domir-package
#' @docType package
#' @title Tools to Support Relative Importance Analysis
#' @author Joseph Luchman \email{jluchman@gmail.com}
#' 
#' @description 
#' Methods to apply decomposition-based relative importance analysis for 
#' R functions.
#' 
#' @details
#' Determining the relative importance of inputs to (i.e., independent 
#' variables, predictors, features) to a predictive model is topic 
#' of interest to scientists and analysts. Decomposing a returned value, such 
#' as a model fit metric or statistic, into parts attributable to each input 
#' is a commonly applied method for determining relative importance in 
#' predictive models. 
#' 
#' This package supports applying decomposition methods using 
#' [`lapply`]- or [`Map`]-like functions that compute dominance analysis 
#' (Azen & Budescu, 2004; Budescu, 1993)/Shapley value decomposition 
#' (Grömping, 2007; Lipovetsky & Conklin, 2001) based on the values returned 
#' from other, predictive modeling, functions.
#' 
#' The user interface is structured such that {domir} automates the 
#' decomposition of the returned value and comparisons between model inputs 
#' and the user provides the analysis pipeline including model inputs, the 
#' predictive modeling function into which they are entered, and returned 
#' value from the model to decompose.
#' 
#' This package's user interface accepts inputs as names on the right hand 
#' side of a [`formula`] which can be passed on to the predictive model 
#' directly or further processed in the analysis pipeline.  The interface 
#' is also planned to be extended to [`Formula`][Formula::Formula] from the 
#' package `{Formula}` as well as [`list`] types as inputs.
#'
#' @references
#' \itemize{
#' \item Azen, R., & Budescu, D. V. (2003). The dominance analysis approach 
#' for comparing predictors in multiple regression. Psychological Methods, 
#' 8(2), 129-148. doi:10.1037/1082-989X.8.2.129
#' \item Budescu, D. V. (1993). Dominance analysis: A new approach to the 
#' problem of relative importance of predictors in multiple regression. 
#' Psychological Bulletin, 114(3), 542-551. doi:10.1037/0033-2909.114.3.542
#' \item Grömping, U. (2007). Estimators of relative importance in linear 
#' regression based on variance decomposition. The American Statistician, 
#' 61(2), 139-147. doi:10.1198/000313007X188252
#' \item Lipovetsky, S, & and Conklin, M. (2001). Analysis of regression in 
#' game theory approach. Applied Stochastic Models in Business and Industry, 
#' 17(4), 319-330. doi:10.1002/asmb.446
#'}
NULL