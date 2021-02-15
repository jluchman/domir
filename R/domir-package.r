#' The \code{domir} package provides a set of flexible wrapper and helper functions
#' for conducting dominance analysis with many different statistical and machine learning models. 
#' This package is intended as a port of the Stata SSC modules \code{domin} 
#' and \code{domme} to R.
#'
#' @details
#' Dominance analysis (DA) determines the relative importance of independent 
#' variables (IVs) or parameter estimates (PEs) in an estimation model based 
#' on contribution to an overall model fit statistic (see Groemping, 2007 
#' for a discussion). DA is an ensemble method in which importance 
#' determinations about IV/PEs are made by aggregating results across 
#' multiple models, though the method usually requires the ensemble 
#' contain each possible combination of the IV/PEs in the full model.
#'
#' The all possible combinations ensemble with \code{p} IV/PEs in the 
#' full model results in \code{2^p-1} models estimated. That is, each 
#' combiation of \code{p} IV/PEs alterating between included versus excluded 
#' (i.e., the 2 base to the exponent) where the #' intercept[s]-only 
#' model is omitted (i.e., the -1 representing the #' distinct combination 
#' where no IV/PEs are included; see Budescu, 1993).
#'
#' Currently, the only method to support DA is \code{domin}. \code{domin} is 
#' as a flexible wrapper function that can be used with many modeling 
#' functions that accept a formula that follows the standard 
#' \code{response ~ terms} format; many functions that do not can be 
#' accommodated with a sub-wrapper function.
#'
#' @name domir-package
#' @aliases domir
#' @docType package
#' @title Support for User-defined Dominance Analysis
#' @author Joseph Luchman \email{jluchman_at_gmail_com}
#' @references
#' \itemize{
#' \item Budescu, D. V. (1993). Dominance analysis: A new approach to the problem of relative importance of predictors in multiple regression. Psychological Bulletin, 114(3), 542-551. doi:10.1037/0033-2909.114.3.542
#' \item Azen, R., & Budescu, D. V. (2003). The dominance analysis approach for comparing predictors in multiple regression. Psychological Methods, 8(2), 129-148. doi:10.1037/1082-989X.8.2.129
#' \item Azen, R., & Budescu, D. V. (2006). Comparing Predictors in Multivariate Regression Models: An Extension of Dominance Analysis. Journal of Educational and Behavioral Statistics, 31(2), 157-180. doi:10.3102/10769986031002157
#' \item Azen, R., & Traxel, N. (2009). Using Dominance Analysis to Determine Predictor Importance in Logistic Regression. Journal of Educational and Behavioral Statistics, 34(3), 319-347. doi:10.3102/1076998609332754
#' \item Groemping, U. (2007). Estimators of relative importance in linear regression based on variance decomposition. The American Statistician, 61(2), 139-147. doi:10.1198/000313007X188252
#' \item Luchman, J. N. (2014). Relative Importance Analysis With Multicategory Dependent Variables: An Extension and Review of Best Practices. Organizational Research Methods, 17(4), 452-471. doi:10.1177/1094428114544509
#' \item Luchman, J. N., Lei, X., & Kaplan, S. A. (2020). Relative Importance Analysis With Multivariate Models: Shifting the Focus from Independent Variables to Parameter Estimates. Journal of Applied Structural Equation Modeling, 4(2), 1-20. doi:10.47263/JASEM.4(2)02
#' \item Luo, W., & Azen, R. (2013). Determining Predictor Importance in Hierarchical Linear Models Using Dominance Analysis. Journal of Educational and Behavioral Statistics, 38(1), 3-31. doi:10.3102/1076998612458319
#' }

NULL
