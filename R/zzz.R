#' @importFrom kableExtra %>%
NULL


# DOESNT NEED TO BE DOCUMENTED OFFICIALLY
# Applies stars to values
#
# @param x a vector of numerics
# @param stars a vector of stars
#
# @return a vector of stars mapped to xx
#
# @examples
# x <- c(0.0000001, 0.0001, 0.001, 0.002, 0.01, 0.02, 0.045, 0.05, 0.06, 0.1)
#
# data.frame(x = round(x, 3), s = apply_stars(x))
apply_stars <- function(x,
                        stars = c("***" = 0, "**" = 0.001, "*" = 0.01,
                                  "." = 0.05, " " = 0.1)) {
  if (max(stars) != 1) stars <- c(stars, " " = 1)
  if (length(stars) == 0 && is.null(stars)) return(NULL)
  cut(x, stars, right = F, labels = names(stars[-length(stars)])) %>%
    as.character()
}
