#' K-nearest Neighbors Cross-validation Function
#'
#' This function is used to predict a class using the k-nearest neighbors algorithm
#'
#' @param train Data frame of covariate values used to predict a class
#' @param cl String indicating the output class to predict
#' @param k_nn Numeric for the number of neighbors to use in the k-nearest neighbors algorithm
#' @param k_cv Numeric for the number of folds for cross-validation
#'
#' @return list with the cross-validation error and a vector of the predicted class
#'
#' @examples
#' my_knn(penguin_data, "species", 5, 5)
#' my_knn(penguin_data[, 1:3], "species", 10, 10)
#'
#' @export

my_knn_cv <- function(train, cl, k_nn, k_cv) {
    fold <- sample(rep(1:k_cv, length = nrow(train)))
    train <- train %>% mutate("split" = fold)
    predict_matrix <- data.frame("cl" = train[, which(names(train) == cl)],
                                 "err" = c(1:nrow(train)*0))

    for(i in 1:k_cv) {
        # define training and test data and predict using knn algorithm
        data_train <- train %>% filter(split != i)
        data_test <- train %>% filter(split == i)
        predict <- knn(data.frame(data_train[, -which(names(data_train) == cl)]),
                       data.frame(data_test[, -which(names(data_test) == cl)]),
                       as.vector(data_train %>% pull(cl)),
                       k_nn)

        # find which rows of the original dataset the predicted values correspond to
        predict_rows <- match(c(1:nrow(data_test)), match(do.call("paste", train[, colnames(train)]), do.call("paste", data_test[, colnames(train)])))

        # build output class vector and define error
        for(j in 1:length(predict_rows)) {
            predict_matrix[predict_rows[j], 1] <- predict[j]
            if(predict_matrix[predict_rows[j], 1] != train[predict_rows[j], ] %>% pull(cl))
                predict_matrix[predict_rows[j], 2] <- 1
        }
    }
    cv_err <- sum(predict_matrix$err) / nrow(predict_matrix)
    class <- predict_matrix %>% pull(cl)
    return(list(cv_err, class))
}
