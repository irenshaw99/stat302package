#' Random Forest Cross-Validation Function
#'
#' This function calculates the cross-validation error of a prediction from the random forest algorithm
#'
#' @param k Numeric input indicating the number of folds for cross-validation
#'
#' @return Numeric with the cross-validation
#'
#' @import dplyr
#' @importFrom randomForest randomForest
#' @importFrom stats complete.cases
#' @importFrom stats predict
#'
#' @examples
#' my_rf_cv(5)
#'
#' @keywords prediction
#'
#' @export

my_rf_cv <- function(k) {
    p_data <- my_penguins[complete.cases(my_penguins), ]
    my_penguins <- NULL
    train <- p_data[, c(3:6)]
    cl <- "body_mass_g"
    fold <- sample(rep(1:k, length = nrow(train)))
    train <- train %>% mutate("split" = fold)
    predict_matrix <- data.frame("cl" = train[, which(names(train) == cl)],
                                 "err" = c(1:nrow(train)*0))
    mse <- c(1:k)

    for(i in 1:k) {
        # define training and test data and predict using random forest algorithm
        data_train <- train %>% dplyr::filter(split != i)
        data_test <- train %>% dplyr::filter(split == i)
        model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data = data_train, ntree = 100)
        prediction <- predict(model, data_test[, 1:3])

        # calculate mean squared error for current fold
        fold_sum <- 0
        for(j in 1:nrow(data_test)) {
            fold_sum <- fold_sum + (data_test[j, ] %>% pull(4) - prediction[[j]])^2
        }
        mse[i] <- fold_sum / nrow(data_test)
    }

    # calculate cv error
    cv <- sum(mse) / k

    return(cv)
}
