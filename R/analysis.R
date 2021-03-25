# Posterior class definition

#' @noRd
#' @export
posterior <- setClass(Class = "posterior",
                list(data = "list")
)


#' @noRd
#' @export
posterior_difference <- setClass(Class = "posterior_difference",
                list(data = "list")
)



#' @noRd
#' @export
binom_proportions <- function(successes, trials, shape1 = 1, shape2 = 1, samples = 10000) {


    posterior_fun_ <- function(theta, successes, trials, shape1, shape2) {
        rbeta(theta = theta,
              shape1 = shape1 + successes,
              shape2 = shape2 + trials - successes)
    }

    posterior_random_ <- function(samples, successes, trials, shape1, shape2) {
        set.seed(1922)
        rbeta(n = samples,
              shape1 = shape1 + successes,
              shape2 = shape2 + trials - successes)
    }

    likelihood_fun_ <- function(theta, successes, trials) {
        dbinom(x = successes, size = trials, prob = theta)
    }

    prior_fun_ <- function(theta, shape1, shape2) {
        dbeta(x = theta, shape1 = shape1, shape2 = shape2)
    }



    posterior_fun <- purrr::partial(posterior_fun_,
    successes = successes, trials = trials, shape1 = shape1, shape2 = shape2)

    posterior_random <- purrr::partial(posterior_random_,
    successes = successes, trials = trials, shape1 = shape1, shape2 = shape2)

    prior_fun <- purrr::partial(prior_fun_,
                                shape1 = shape1,
                                shape2 = shape2)

    likelihood_fun <- purrr::partial(likelihood_fun_,
        successes = successes, trials = trials)

    posterior_samples <- posterior_random(samples = samples)
    posterior_summary <- sample_summary(posterior_samples)

    # add some plots
    likelihood_plot <- ggplot2::ggplot() +
        ggplot2::geom_function(fun = likelihood_fun)

    prior_plot <- ggplot2::ggplot() +
        ggplot2::geom_function(fun = prior_fun)

    posterior_histogram <- purrr::partial(posterior_histogram_,
                                          posterior_samples = posterior_samples)

    prior_description <- glue::glue("Beta(alpha = {shape1}, beta = {shape2})")
    likelihood_description <- glue::glue("Binomial(successes = {successes}, trials = {trials})") #nolint

    data <- list(
        posterior_samples = posterior_samples,
        posterior_summary = posterior_summary,
        posterior_histogram = posterior_histogram,
        likelihood_plot = likelihood_plot,
        prior_plot = prior_plot,
        prior_description = prior_description,
        likelihood_description = likelihood_description
    )


    new(Class = "posterior",
    data = data)
}

#' @noRd
#' @export
setMethod(
    "show",
    "posterior",
    function(object) {
    cat("Object of class posterior\n")
    cat("Prior:\n")
    cat(object@data$prior_description, "\n")
    cat("Likelihood:\n")
    cat(object@data$likelihood_description, "\n")
    }
)

#' @noRd
#' @export
setMethod(
    "show",
    "posterior_difference",
    function(object) {
    cat("Object of class posterior difference\n")
    }
)


#' @noRd
#' @export
setMethod("$",
  signature = "posterior",
  function(x, name) {
    returnval <- x@data[[name]]
    return(returnval)
  }
)

#' @noRd
#' @export
setMethod("$",
  signature = "posterior_difference",
  function(x, name) {
    returnval <- x@data[[name]]
    return(returnval)
  }
)

#' @noRd
#' @export
setMethod("names",
  signature = "posterior",
  function(x) {
    return(names(x@data))
  }
)

#' @noRd
#' @export
setMethod("names",
  signature = "posterior_difference",
  function(x) {
    return(names(x@data))
  }
)


#' @noRd
#' @export
summary.posterior <- function(x) {
    x$posterior_summary
}

#' @noRd
#' @export
summary.posterior_difference <- function(x) {
    x$difference_summary
}



sample_summary <- function(samples) {
    widths <- c(.50, .96, .99)
    posterior_summary_w1 <- ggdist::mode_hdi(samples,
                                             .width = widths[1])
    posterior_summary_w2 <- ggdist::mode_hdi(samples,
                                             .width = widths[2])
    posterior_summary_w3 <- ggdist::mode_hdi(samples,
                                             .width = widths[3])
    posterior_summary <- rbind(
    as.data.frame(posterior_summary_w1[c("y", "ymin", "ymax", ".width")]),
    as.data.frame(posterior_summary_w2[c("y", "ymin", "ymax", ".width")]),
    as.data.frame(posterior_summary_w3[c("y", "ymin", "ymax", ".width")]))

    names(posterior_summary) <- c("mode",
                                  "lower_limit",
                                  "upper_limit",
                                  "width")
    return(posterior_summary)
}

posterior_histogram_ <- function(posterior_samples, bins = NULL, binwidth = NULL) { #nolint
    if (is.null(bins) && is.null(binwidth)) {
        stop("You must set bins or binwidth", call. = FALSE)
    } else if (is.null(bins) && is.null(binwidth)) {
        stop("You must set bins or binwidth")
    }

    if (is.null(bins)) {
        return(ggplot2::ggplot(data = data.frame(x = posterior_samples)) +
        ggplot2::geom_histogram(ggplot2::aes(x = x), binwidth = binwidth))
    }

    ggplot2::ggplot(data = data.frame(x = posterior_samples)) +
    ggplot2::geom_histogram(ggplot2::aes(x = x), bins = bins)
}

#' @noRd
#' @export
`-.posterior` <- function(e1, e2) {

    posterior_difference <- e1@data$posterior_samples - e2@data$posterior_samples #nolint
    difference_summary <- sample_summary(posterior_difference)
    difference_histogram <- purrr::partial(posterior_histogram_,
                                           posterior_samples = posterior_difference) #nolint

    data <- list(posterior_samples = posterior_difference,
    difference_summary = difference_summary,
    difference_histogram = difference_histogram,
    posterior1 = e1,
    posterior2 = e2)


    new(Class = "posterior_difference",
    data = data)

}
