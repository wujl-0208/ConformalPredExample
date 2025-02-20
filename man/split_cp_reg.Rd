% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_cp_reg_example_sin.R
\name{split_cp_reg}
\alias{split_cp_reg}
\title{Examples for split conformal prediction method to a smooth spline regression model}
\usage{
split_cp_reg()
}
\value{
a picture
}
\description{
To illustrate the application of split conformal prediction,
we conducted a numerical experiment using a synthetic dataset.
Specifically, we applied the split conformal prediction method to a smooth spline regression model,
where the input-output pairs \eqn{(X,Y)\in\mathbb{R}^2} are generated by \eqn{Y=0.1X+2\sin(X)} with added Gaussian noise \eqn{\mathcal{N}(0,3^2)}.
The dataset consists of \eqn{n=500} observations, with \eqn{80\%} used for training, \eqn{20\%} for calibration, and a separate test set of 2,000 points used to evaluate the model's performance.

The model is trained using a smooth spline regression on the training data,
and the nonconformity measures are calculated based on the absolute residuals of the model on the calibration set.
These nonconformity measures are then used to compute the \eqn{90\%} prediction interval for the test set.
The prediction interval is derived by adding and subtracting the \eqn{90\%} quantile of the calibration set's nonconformity measures from the predicted values of the test set.
}
\examples{
split_cp_reg()


 library(ggplot2)
 library(tidyr)
 set.seed(2024)

 true_function <- function(x) {
   return(0.1 * x + 2 * sin(x))
 }

 n <- 500
 x <- sort(runif(n, -10, 10))
 y <- true_function(x) + rnorm(n, sd = 3)

 train_size <- floor(0.6 * n)
 train_data <- data.frame(x = x[1:train_size], y = y[1:train_size])
 calib_data <- data.frame(x = x[(train_size + 1):n], y = y[(train_size + 1):n])

 model1 <- smooth.spline(train_data$x, train_data$y)

 nonconformity_measure <- function(model, data) {
   predictions <- predict(model$fit, data$x)$y
   residuals <- data$y - predictions
   return(abs(residuals))
 }

 calib_nc_measures <- nonconformity_measure(model1, calib_data)

 test_n <- 2000
 test_x <- runif(test_n, -10, 10)
 test_data <- data.frame(x = test_x)
 test_data$y_true <- true_function(test_data$x)
 test_data$y <- test_data$y_true + rnorm(test_n, sd = 3)
 test_data$y_pred <- predict(model1$fit, test_data$x)$y

 test_nc_measures <- nonconformity_measure(model1, test_data)

 test_data$upper <- test_data$y_pred + quantile(calib_nc_measures, probs = 0.9, type = 1)
 test_data$lower <- test_data$y_pred - quantile(calib_nc_measures, probs = 0.9, type = 1)

 p2 = ggplot() +
   geom_point(data = test_data, aes(x = x, y = y), color = "blue", size = 2, alpha = 0.6) +
   geom_line(data = test_data, aes(x = x, y = y_pred), color = "red", size = 2, linetype = "dashed") +
   geom_ribbon(data = test_data, aes(x = x, ymin = lower, ymax = upper), fill = "red", alpha = 0.2) +
   labs(title = "Spline Model with SCP Interval",
        x = "x",
        y = "y") +
   theme_minimal() +
   theme(legend.position = "bottom")

 # ggsave("Spline Model with Split Conformal Prediction Interval.pdf", plot = p2, bg = "white")

 print(p2)

 mean((test_data$lower<=test_data$y)&(test_data$y<=test_data$upper))

 model2 <- lm(y ~ x, data = train_data)

 nonconformity_measure <- function(model, data) {
   predictions <- predict(model, newdata = data)
   residuals <- data$y - predictions
   return(abs(residuals))
 }

 calib_nc_measures <- nonconformity_measure(model2, calib_data)

 test_n <- 2000
 test_x <- runif(test_n, -10, 10)
 test_data <- data.frame(x = test_x)
 test_data$y_pred <- predict(model2, newdata = test_data)
 test_data$y_true <- true_function(test_data$x)
 test_data$y <- test_data$y_true + rnorm(test_n, sd = 3)


 test_nc_measures <- nonconformity_measure(model2, test_data)

 test_data$upper <- test_data$y_pred + quantile(calib_nc_measures, probs = 0.9, type = 1)
 test_data$lower <- test_data$y_pred - quantile(calib_nc_measures, probs = 0.9, type = 1)

 p3 = ggplot() +
   geom_point(data = test_data, aes(x = x, y = y), color = "blue", size = 2, alpha = 0.6) +
   geom_line(data = test_data, aes(x = x, y = y_pred), color = "red", size = 2, linetype = "dashed") +
   geom_ribbon(data = test_data, aes(x = x, ymin = lower, ymax = upper), fill = "red", alpha = 0.2) +
   labs(title = "Linear Model with SCP Interval",
        x = "x",
        y = "y") +
   theme_minimal() +
   theme(legend.position = "bottom")

 # ggsave("Linear Model with Split Conformal Prediction Interval.pdf", plot = p3, bg = "white")

 print(p3)

 mean((test_data$lower<=test_data$y)&(test_data$y<=test_data$upper))

 library(patchwork)

 combined_plot <- p2 + p3

 # ggsave("Comparison_Spline_Linear.pdf", plot = combined_plot, bg = "white", width = 6.5, height = 3)

 print(combined_plot)
}
