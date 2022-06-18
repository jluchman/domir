#' @name domir-package
#' @aliases hoo
#' @docType package
#' @title Tools to Support Relative Importance Analysis
#' @author Joseph Luchman \email{jluchman@gmail_com}
#' 
#' @description 
#' Methods to apply decomposition-based relative importance analysis for 
#' R functions.
#' 
#' @details
#' Decomposing a returned value into parts attributable to inputs is a 
#' commonly applied method for determining relative importance in predictive 
#' models. 
#' 
#' This package supports the application of decomposition methods by providing 
#' [`lapply`]- or [`Map`]-like meta-functions that compute dominance analysis 
#' (Azen & Budescu, 2004; Budescu, 1993) or Shapley value decomposition 
#' (Groemping, 2007; need better cite) based on the values returned from other
#' functions.
#' 
#' The focus of this package is on predictive models and fit statistics returned 
#' from such models.  Accordingly, the  programming interface/API currently 
#' works only with [`formula`]s.  The API is being extended to [`Formula`]s as 
#' well as [`list`]s.
#'
#' @references
#' \itemize{
#' \item Azen, R., & Budescu, D. V. (2003). The dominance analysis approach 
#' for comparing predictors in multiple regression. Psychological Methods, 
#' 8(2), 129-148. doi:10.1037/1082-989X.8.2.129
#' \item Budescu, D. V. (1993). Dominance analysis: A new approach to the 
#' problem of relative importance of predictors in multiple regression. 
#' Psychological Bulletin, 114(3), 542-551. doi:10.1037/0033-2909.114.3.542
#' \item Groemping, U. (2007). Estimators of relative importance in linear 
#' regression based on variance decomposition. The American Statistician, 
#' 61(2), 139-147. doi:10.1198/000313007X188252
#'}
NULL