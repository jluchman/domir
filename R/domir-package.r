#' The \code{domir} package provides a set of flexible wrapper and helper functions
#' for conducting relative importance analysis with a focus on dominance
#' analysis. The intention of this package is to provide tools that allow 
#' relative importance analysis across a wide varitey of practical 
#' data analytic situations.
#'
#' @details
#' Relative importance (RI) analysis is a methodology focused on comparing 
#' independent variables/features/predictors as well as parameter estimates 
#' to one another in terms of how they predict some 
#' dependent variable/response/outcome. The methodology implemented in 
#' this initial version of \code{domir} is dominance analysis (DA). 
#'
#' As a RI method, DA determines the relative importance of independent 
#' variables (IVs) or parameter estimates (PEs) in an estimation model based 
#' on contribution to an overall model fit statistic/metric 
#' (see Budescu, 1993; Groemping, 2007 for a discussions). 
#' DA is an ensemble method in which importance 
#' determinations about IV/PEs are made by aggregating results across 
#' multiple models, though the method usually requires the ensemble 
#' contain each possible combination of the IV/PEs in the full model that has 
#' been selected using model selection methods.
#'
#' The all possible combinations ensemble with \code{p} IV/PEs in the 
#' full model results in \code{2^p} models estimated. That is, each 
#' combiation of \code{p} IV/PEs alterating between included versus excluded 
#' (i.e., the base of 2 to the \code{p} exponent number of IV/PEs).
#'
#' Currently, the only tool implemented in \code{domir} is a DA method 
#' \code{domin}. \code{domin} is a flexible wrapper function that can be used 
#' with many modeling functions that accept a formula that follow the standard 
#' \code{response ~ terms} format.  The format used by 
#' \code{domin} can be extended to other functions focused on IV-based RI 
#' can be accommodated with a additional wrapper functions based on the 
#' formula it creates and submits to modeling functions.
#'
#' @name domir-package
#' @aliases domir
#' @docType package
#' @title Tools to Support Relative Importance Analysis
#' @author Joseph Luchman \email{jluchman_at_gmail_com}
#' @references
#' \itemize{
#' \item Budescu, D. V. (1993). Dominance analysis: A new approach to the problem of relative importance of predictors in multiple regression. Psychological Bulletin, 114(3), 542-551. doi:10.1037/0033-2909.114.3.542
#' \item Azen, R., & Budescu, D. V. (2003). The dominance analysis approach for comparing predictors in multiple regression. Psychological Methods, 8(2), 129-148. doi:10.1037/1082-989X.8.2.129
#' \item Groemping, U. (2007). Estimators of relative importance in linear regression based on variance decomposition. The American Statistician, 61(2), 139-147. doi:10.1198/000313007X188252
#' \item Luchman, J. N., Lei, X., & Kaplan, S. A. (2020). Relative Importance Analysis With Multivariate Models: Shifting the Focus from Independent Variables to Parameter Estimates. Journal of Applied Structural Equation Modeling, 4(2), 1-20. doi:10.47263/JASEM.4(2)02
#' }

NULL
