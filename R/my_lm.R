#' Linear Model Function
#'
#' This function is used to fit data to a linear model
#'
#' @param formula Formula class object that indicates which classes from the data to use as for the y-variable and which to use for for the x-variable
#' @param data Data frame to build a linear model for
#'
#' @return Table that reports the estimate, standard error, t-value, and probability > t for each coefficient, including the intercept
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats model.response
#' @importFrom stats pt
#'
#' @examples
#' my_lm(lifeExp ~ gdpPercap, my_gapminder)
#' my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)
#'
#' @keywords inference
#'
#' @export
my_lm <- function(formula, data) {
    x <- model.matrix(formula, data)

    y <- model.response(model.frame(formula, data))

    beta <- solve(t(x) %*% x) %*% t(x) %*% y

    df <- nrow(x) - ncol(x)

    variance <- 0
    for(i in 1:nrow(x)) {
        variance <- variance +
            (y[i] - x[i, ] %*% beta)^2 / df
    }

    se <- sqrt(diag(solve(t(x) %*% x) * variance[1,1]))

    t <- beta / se

    pr <- pt(abs(t), 30, lower.tail = FALSE) * 2

    report <- data.frame("estimate" = beta[1],
                         "se" = se[1],
                         "t" = t[1],
                         "pr" = as.character(pr[1]))

    for(i in 2:ncol(x)) {
        report <- rbind(report,
                        data.frame("estimate" = beta[i],
                                   "se" = se[i],
                                   "t" = t[i],
                                   "pr" = as.character(pr[i]))
        )
    }

    return(report)
}
