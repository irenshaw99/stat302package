#' T-test function
#'
#' This function is used for performing a one-sample t-test
#'
#' @param x Numeric vector to perform t-test on
#' @param alternative String that specifies whether alternative hypothesis is "two.sided", "less", or "greater"
#' @param mu Numeric that indicates the null hypothesis value of the mean
#'
#' @return test_stat Numeric test statistic
#' @return df Numeric degrees of freedom
#' @return alternative String value of the parameter "alternative"
#' @return p_val Numeric p-value
#'
#' @import stats
#'
#' @examples
#' my_t.test(c(1:100), "two.sided", 50)
#' my_t.test(c(1:100), "greater", 50)
#' my_t.test(c(1:100), "less", 50)
#'
#' @export

my_t.test <- function(x, alternative, mu) {
    if(!(alternative %in% c("two.sided", "less", "greater")))
        stop("Parameter \"alternative\" must equal \"two.sided\", \"less\", or \"greater\"")
    df <- length(x) - 1
    se <- sd(x) / sqrt(length(x))
    test_stat <- (mean(x) - mu) / se
    p_val <- pt(test_stat, df, lower.tail = TRUE)

    if(alternative == "greater")
        p_val <- 1 - p_val
    else if(alternative == "two.sided")
        p_val <- min(p_val, 1 - p_val) * 2

    return(
        list("test_stat" = test_stat,
             "df" = df,
             "alternative" = alternative,
             "p_val" = p_val)
    )
}
