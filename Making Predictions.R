## Splitting the data into training and testing sets (Equal Parts)
sample_size <- floor(0.5 * nrow(X))
sample_size

set.seed(48)

train_index <- sample(seq_len(nrow(X)), size = sample_size)

X_train <- X[train_index, ]
X_test <- X[-train_index,]
y_train <- y[train_index]
y_test <- y[-train_index]


# Calling fit with default parameters for w, learning_rate and epochs
my_model <- fit(X_train, y_train)

built_in_model <- glm(y_train ~ horsepower + weight + year + origin2 + origin3,
                      data = as.data.frame(X_train),
                      family = binomial)

# Comparing the coefficients of our model and the inbuilt model

t(my_model$best_params)

built_in_model$coefficients


## Setting the values of the best params after training the model
w <- my_model$best_params

## Making predictions on the training set

# Gives us the predicted probablities for the label being 1
# Rounding them will give us the predicted label where >= 0.5 is 1 and < 0.5 is 0
train_preds <- predict(X_train, w)

train_conf_matrix <- table(Prediction = round(train_preds), Actual = y_train)
train_conf_matrix

train_error_rate <- mean(round(train_preds) != y_train)
train_error_rate

train_accuracy <- 1 - train_error_rate
train_accuracy

## Making predictions on the test set

test_preds <- predict(X_test, w)

test_conf_matrix <- table(Prediction = round(test_preds), Actual = y_test)
test_conf_matrix

test_error_rate <- mean(round(test_preds) != y_test)
test_error_rate

test_accuracy <- 1 - test_error_rate
test_accuracy

