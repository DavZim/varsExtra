#' Plots a VAR
#'
#' @param x an IRF-result as returned from \code{\link[vars]{irf}}
#' @param single_plot if all facets should be plotted in a single graph,
#' if FALSE, a list of plots for each from-variable is returned
#' @param plot if FALSE only the plotting data is returned instead of actual plots
#'
#' @return a data_frame if \code{plot == FALSE},
#' a single ggplot if \code{plot == TRUE, single_plot == TRUE} or
#' a list of ggplots if \code{plot == TRUE, single_plot == FALSE}
#' @export
#'
#' @examples
#' library(vars)
#' data(Canada)
#'
#' varres <- VAR(Canada, p = 2, type = "none")
#' # plot the VAR result
#' ggfy(varres)
#'
#' # to extract the data only
#' ggfy(varres, plot = FALSE)
#'
#' # to get a list of plots
#' plot_list <- ggfy(varres, single_plot = FALSE)
#'
#' # IRF
#' irfres <- irf(varres)
#' ggfy(irfres)
#'
#' # to extract the data only
#' ggfy(irfres, plot = FALSE)
#'
#' # to get a list of plots
#' plot_list <- ggfy(irfres, single_plot = FALSE)
#'
ggfy <- function(x, single_plot = TRUE, plot = TRUE) {

  if (class(x) == "varest") {
    p <- varsExtra:::ggfy_varest(x, single_plot = single_plot, plot = plot)
  } else if (class(x) == "varirf") {
    p <- varsExtra:::ggfy_varirf(x,  single_plot = single_plot, plot = plot)
  } else {
    stop("x must be of class varest or varirf")
  }
  return(p)
}

ggfy_varest <- function(x, single_plot, plot) {

  vals <- x$datamat %>%
    dplyr::as_data_frame() %>%
    dplyr::select(-dplyr::matches("\\.l[0-9]+")) %>%
    dplyr::mutate(type = "data", t = 1:n()) %>%
    tidyr::gather(key = "var", value = "value", -type, -t)

  ypred <- lapply(1:length(x$varresult), function(i) {
    dplyr::data_frame(value = as.numeric(fitted(x$varresult[[i]]))) %>%
      dplyr::mutate(type = "fitted",
                    t = 1:n(),
                    var = names(x$varresult)[i])
  }) %>% dplyr::bind_rows()

  resid <- lapply(1:length(x$varresult), function(i) {
    dplyr::data_frame(value = as.numeric(resid(x$varresult[[i]]))) %>%
      dplyr::mutate(type = "residuals",
                    t = 1:n(),
                    var = names(x$varresult)[i])
  }) %>% dplyr::bind_rows()

  df <- dplyr::bind_rows(vals, ypred, resid) %>%
    dplyr::select(var, type, t, value) %>%
    dplyr::arrange(var, type)

  if (!plot) return(tidyr::spread(df, key = type, value = value))

  if (single_plot) {
    df %>%
      dplyr::filter(type != "residuals") %>%
      ggplot2::ggplot(ggplot2::aes(x = t, y = value, color = type,
                                   linetype = type)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~var, ncol = 1, scales = "free_y") +
      ggplot2::labs(title = "Diagram of data and fit")
  } else {
    lapply(split(df, df$var), function(xx) {
      xx %>%
        dplyr::filter(type != "residuals") %>%
        ggplot2::ggplot(ggplot2::aes(x = t, y = value, color = type,
                                     linetype = type)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~var, ncol = 1, scales = "free_y") +
        ggplot2::labs(title = sprintf("Diagram of data and fit for %s",
                                      unique(xx$var)))
    })
  }
}

ggfy_varirf <- function(x, single_plot, plot) {
  if (class(x) != "varirf")
    stop("x must be of class varirf, use vars::irf(...) to obtain it")

  df <- lapply(1:length(x$irf), function(i) {
    df_irf <- dplyr::data_frame(from = names(x$irf)[i],
                                lag = 0:(nrow(x$irf[[i]]) - 1),
                                type = "irf",
                                ci = "NA") %>%
      dplyr::bind_cols(dplyr::as_data_frame(x$irf[[i]]))

    df_upper <- dplyr::data_frame(from = names(x$Upper)[i],
                                  lag = 0:(nrow(x$Upper[[i]]) - 1),
                                  type = "ci",
                                  ci = "upper") %>%
      dplyr::bind_cols(dplyr::as_data_frame(x$Upper[[i]]))

    df_lower <- dplyr::data_frame(from = names(x$Lower)[i],
                                  lag = 0:(nrow(x$Lower[[i]]) - 1),
                                  type = "ci",
                                  ci = "lower") %>%
      dplyr::bind_cols(dplyr::as_data_frame(x$Lower[[i]]))

    dplyr::bind_rows(df_irf, df_upper, df_lower)
  }) %>% dplyr::bind_rows()

  df <- df %>%
    tidyr::gather(key = "to", value = "irf", -from, -lag, -type, -ci)

  if (!plot) {
    d <- df %>%
      dplyr::mutate(ci = ifelse(ci == "NA", "irf", ci)) %>%
      dplyr::select(-type) %>%
      tidyr::spread(key = ci, value = irf) %>%
      dplyr::select(from, to, lag, irf, lower, upper)
    return(d)
  }

  if (single_plot) {
    df %>%
      dplyr::mutate(type = factor(type,
                                  levels = c("irf", "ci"),
                                  labels = c("IRF",
                                             sprintf("CI %.0f%%",
                                                     100 * (1 - x$ci))))) %>%
      ggplot2::ggplot(ggplot2::aes(x = lag, y = irf, linetype = type,
                                   group = ci)) +
      ggplot2::geom_hline(yintercept = 0, color = "darkgray") +
      ggplot2::geom_line() +
      ggplot2::facet_grid(to ~ from, switch = "both") +
      ggplot2::scale_x_continuous(labels = function (x) floor(x)) +
      ggplot2::labs(
        title = "Orthogonal Impulse Response",
        subtitle = paste("The impulse is applied to the x-facet.",
                         "The response is shown on the y-facet")
      )
  } else {
    lapply(split(df, df$from), function(xx) {
      xx %>%
        dplyr::mutate(type = factor(type,
                                    levels = c("irf", "ci"),
                                    labels = c("IRF",
                                               sprintf("CI %.0f%%",
                                                       100 * (1 - x$ci))))) %>%
        ggplot2::ggplot(ggplot2::aes(x = lag, y = irf, linetype = type,
                                     group = ci)) +
        ggplot2::geom_hline(yintercept = 0, color = "darkgray") +
        ggplot2::geom_line() +
        ggplot2::facet_grid(to ~ .) +
        ggplot2::facet_grid(to ~ from, switch = "both") +
        ggplot2::scale_x_continuous(labels = function (x) floor(x)) +
        ggplot2::labs(title = sprintf("Orthogonal Impulse Response from %s",
                                      unique(xx$from)))
    })
  }
}
