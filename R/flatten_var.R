#' Flattens a var-result to a list of data.frames
#'
#' @param x a result of \code{\link[vars]{VAR}}
#' @param se if the standard errors should be reported
#'
#' @return a list of data.frames containing the estimates \code{est} and the
#'  p-values \code{p-value}. If \code{se = TRUE}, the standard errors are also
#'  reported as \code{se}.
#' @export
#'
#' @examples
#' library(vars)
#' data(Canada)
#' res <- VAR(Canada, p = 2, type = "none")
#'
#' summary(res)
#' flatten_var(res)
#' flatten_var(res, se = FALSE)
flatten_var <- function(x, se = TRUE) {
  if (class(x) != "varest")
    stop("x must be of class varest")

  extr_res_col <- function(x, i) {
    r <- lapply(x$varresult, function(xx) {
      s <- summary(xx)[[4]][, i]
      r <- matrix(s, nrow = 1)
      colnames(r) <- names(s)
      as.data.frame(r)
    })
    r <- do.call(rbind, r)
    names_ <- data.frame(var = rownames(r), stringsAsFactors = FALSE)
    rownames(r) <- NULL
    r <- cbind(names_, r)
    return(r)
  }

  est_vals <- extr_res_col(x, 1)
  p_vals <- extr_res_col(x, 4)

  if (se) {
    se_vals <- extr_res_col(x, 2)
    ll <- list(
      est = est_vals,
      se  = se_vals,
      p_value = p_vals
    )
  } else {
    ll <- list(
      est = est_vals,
      p_value = p_vals
    )
  }
  return(ll)
}

