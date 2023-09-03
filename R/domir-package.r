#' @name domir-package
#' @docType package
#' @title Tools to Support Relative Importance Analysis
#' @author Joseph Luchman \email{jluchman@gmail.com}
#'
#' @description
#' Methods to apply dominance analysis-based relative importance analysis for
#' predictive modeling functions.
#'
#' @details
#' This package supports relative importance analysis by implementing several
#' functions that compute dominance analysis (Azen & Budescu, 2004;
#' Budescu, 1993). Dominance analysis produces the well-known Shapley value
#' decomposition (e.g., Grömping, 2007; Lipovetsky & Conklin, 2001) as one
#' of its methods called general dominance statistics.
#'
#' Dominance analysis is a method for determining the relative importance of
#' inputs (i.e., independent variables, predictors, features,
#' parameter estimates) to a predictive model that evaluates how a returned
#' value, such as a model fit metric or statistic, is associated with each
#' input. It is also a common, and generally well accepted, method for
#' determining the relative importance of inputs to predictive models that
#' is effective at separating the effects of correlated inputs.
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
