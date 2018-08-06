#' Creates a LaTeX or HTML table for a VAR result
#'
#' @param x a varest restult, as returned from \code{\link[vars]{VAR}}
#' @param format either "html" or "latex" depending on the desired output format
#' @param digits number of digits to display
#' @param p p-values threshold below which the estimate is displayed as bold,
#' if the estimate should not be boldened, use \code{p = 0}.
#' @param se_values if TRUE, the standard errors are reported instead of the
#' p-values, defaults to TRUE
#' @param stars a named vector of the signs, if set to \code{NULL} no stars are
#' shown
#'
#' @return latex or html string depending on the specified format
#' @export
#'
#' @name tabulize
#'
#' @examples
#' library(vars)
#' data(Canada)
#' varres <- VAR(Canada, p = 2, type = "none")
#'
#' # latex table
#' tabulize(varres)
#'
#' # html table
#' tabulize(varres, "html")
#'
#' # if you want to display 5 digits
#' tabulize(varres, digits = 5)
#'
#' # if you want to display 2 digits and a p-value threshold of 0.05
#' tabulize(varres, digits = 2, p = 0.05)
#'
#' # disable bold
#' tabulize(varres, p = 0)
#'
#' # show p-values instead of standard errors in the second row per variable
#' tabulize(varres, se_values = FALSE)
#'
#' # dont show stars
#' tabulize(varres, stars = NULL)
#'
#' # provide custome stars
#' stars_new <- c("***" = 0, "**" = 0.01, "*" = 0.05, " " = 0.1)
#' tabulize(varres, stars = stars_new, se_values = FALSE)
tabulize <- function(x, format = "latex", digits = 3, p = 0.1,
                     se_values = TRUE,
                     stars = c("***" = 0, "**" = 0.001, "*" = 0.01, "." = 0.05,
                               " " = 0.1)) {

  if (class(x) != "varest")
    stop("x must be of class varest, use vars::VAR(...) to obtain it")

  x <- flatten_var(x, se = T)

  # extract the information into one table
  res <- lapply(1:nrow(x[[1]]), function(i) {
    dplyr::bind_rows(
      x$est[i, ],
      x$se[i, ],
      x$p_value[i, ]
    )
  }) %>%
    dplyr::bind_rows()


  if (max(stars) != 1) stars <- c(stars, " " = 1)

  # small helper that formats the data in the right format
  format_num <- function(x) {
    # the first number is bold if the third is lower than p
    # if se_values is TRUE, use second, else third variable
    i <- ifelse(se_values == TRUE, 2, 3)

    c(
      paste(format(round(x[1], digits), nsmall = digits) %>%
        kableExtra::cell_spec(format, bold = (x[3] < p)),
        apply_stars(x[3], stars)),
      sprintf("(%s)", format(round(x[i], digits), nsmall = digits)),
      "NA"
    )
  }


  # round and format the numbers
  res_formatted <- res %>%
    dplyr::group_by(var) %>%
    dplyr::mutate_if(
      is.numeric,
      ~format_num(.x)
    ) %>%
    dplyr::filter_at(dplyr::vars(-var),
                     dplyr::any_vars(!stringr::str_detect(., "NA"))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = ifelse(var == dplyr::lead(var, default = "-"),
                               var,
                               ""))

    names(res_formatted)[1] <- ""

    star_string <- paste(stars, paste0("'", names(stars), "'"), collapse = " ")
    star_string <- substr(star_string, 1, nchar(star_string) - 4)
    star_string <- paste("Signif. codes:", star_string)

    knitr::kable(res_formatted, format,
                 align = c("l", rep("c", ncol(res) - 1)),
                 booktabs = TRUE, escape = F) %>%
    kableExtra::footnote(star_string, general_title = "")
}

#' @rdname tabulize
tabulise <- tabulize
