#' The \code{domir} package provides flexible wrapper and helper functions for conducting relative importance analysis with a focus on dominance analysis. The intention of this package is to provide tools that allow relative importance analysis across a wide variety of practical data analytic situations.
#' 
#' @details
#' Relative importance analysis is a methodology focused on comparing independent variables (IVs)/features/predictors as well as parameter estimates to one another in terms of how they predict some dependent variable/response/outcome in the context of a predictive model. 
#' 
#' The intention of this package is to focus on what I will call "model evaluation" or post hoc examination of IV predictive utility in the context of a vetted, selected predictive model.  That is, the methods to apply here will assume that the user has previously applied model selection methods and that the IVs of the predictive model for relative importance analysis have non-trivial effects in improving model fit. The methods in this package are not intended for model selection - though I will acknowledge that many importance methods are (at least implicitly) focused on model selection/identifying which IVs have a trivial effect and removing them.
#' 
#' The only method implemented at current in \code{domir} is dominance analysis method \code{domin}. \code{domin} is a flexible wrapper function that can be used with many modeling functions. \code{domin} is an extension of the Stata command by the same name (see Luchman, 2021) to the R environment.
#' 
#' See the \emph{vignettes} for a more extensive discussion of basic concepts for DA and the \href{https://github.com/jluchman/domir#readme}{README} for further examples and a discussion of some of the implementation specifics of \code{domin}.
#'
#' @name domir-package
#' @aliases domir
#' @docType package
#' @title Tools to Support Relative Importance Analysis
#' @author Joseph Luchman \email{jluchman@gmail_com}
#' @references
#' \itemize{
#' \item Azen, R., & Budescu, D. V. (2003). The dominance analysis approach for comparing predictors in multiple regression. Psychological Methods, 8(2), 129-148. doi:10.1037/1082-989X.8.2.129
#' \item Budescu, D. V. (1993). Dominance analysis: A new approach to the problem of relative importance of predictors in multiple regression. Psychological Bulletin, 114(3), 542-551. doi:10.1037/0033-2909.114.3.542
#' \item Groemping, U. (2007). Estimators of relative importance in linear regression based on variance decomposition. The American Statistician, 61(2), 139-147. doi:10.1198/000313007X188252
#' \item Luchman, J. N., Lei, X., & Kaplan, S. A. (2020). Relative Importance Analysis With Multivariate Models: Shifting the Focus from Independent Variables to Parameter Estimates. Journal of Applied Structural Equation Modeling, 4(2), 1-20. doi:10.47263/JASEM.4(2)02
#'  \item Luchman, J. N. (2021). Determining relative importance in Stata using dominance analysis: domin and domme. Stata Journal 21(2), 510-538. doi:10.1177/1536867X211025837
#'}
NULL