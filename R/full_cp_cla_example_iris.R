#' @title title
#'
#' @return a picture
#' @export
#' @import caret
#' @import e1071
#' @examples full_cp_cla()
#'
#'
#'  library(caret)
#'  library(e1071)
#'  data(iris)
#'
#'  iris_data <- iris[, c("Sepal.Length", "Petal.Width", "Species")]
#'  iris_data$Species <- as.factor(iris_data$Species)
#'
#'  set.seed(123)
#'  trainIndex <- createDataPartition(iris_data$Species, p = .8,
#'                                    list = FALSE,
#'                                    times = 1)
#'  trainData <- iris_data[ trainIndex,]
#'  testData  <- iris_data[-trainIndex,]
#'
#'  train_svm <- function(train_data) {
#'    model <- svm(Species ~ ., data = train_data, probability = TRUE)
#'    return(model)
#'  }
#'
#'  calculate_nonconformity <- function(model, x) {
#'    pred <- predict(model, x, probability = TRUE)
#'    pred_prob <- attr(pred, "probabilities")
#'    species_levels <- colnames(pred_prob)
#'    x_species_char <- as.character(x$Species)
#'    col_indices <- match(x_species_char, species_levels)
#'    prob_values <- pred_prob[cbind(1:nrow(x), col_indices)]
#'    nonconformity <- 1 - prob_values  # 计算差异
#'    return(nonconformity)
#'  }
#'
#'  svm_model <- train_svm(trainData)
#'
#'  is_in_confidence_set <- function(x, y, train_data, alpha = 0.1) {
#'    test_data = data.frame(x[,-3], Species = as.factor(y))
#'    new_train_data <- rbind(train_data, test_data)
#'
#'    new_model <- svm(Species ~ ., data = new_train_data, probability = TRUE)
#'
#'    nonconformity <- calculate_nonconformity(new_model, test_data)
#'
#'    train_nonconformities <- calculate_nonconformity(new_model, train_data)
#'
#'    sorted_train_nonconformities <- sort(train_nonconformities)
#'    threshold <- sorted_train_nonconformities[ceiling(length(sorted_train_nonconformities) * (1 - alpha))]
#'
#'    return(nonconformity <= threshold)
#'  }
#'
#'  confidence_sets <- lapply(1:nrow(testData), function(i) {
#'    x <- testData[i, ]
#'    results <- sapply(levels(iris_data$Species), function(y) {
#'      is_in_confidence_set(x, y, trainData)
#'    })
#'    names(results) <- levels(iris_data$Species)
#'    return(results)
#'  })
#'
#'  for (i in 1:length(confidence_sets)) {
#'    cat("Test sample", i, ": Confidence set =", names(which(confidence_sets[[i]] == TRUE)), "\n")
#'  }
#'
#'
#'
#'  x_range <- seq(min(iris_data$Sepal.Length), max(iris_data$Sepal.Length), length.out = 20)
#'  y_range <- seq(min(iris_data$Petal.Width), max(iris_data$Petal.Width), length.out = 20)
#'  grid <- expand.grid(Sepal.Length = x_range, Petal.Width = y_range)
#'
#'  confidence_matrix <- lapply(1:nrow(grid), function(i) {
#'    x <- grid[i, ]
#'    results <- sapply(levels(iris_data$Species), function(y) {
#'      is_in_confidence_set(x, y, trainData)
#'    })
#'    names(results) <- levels(iris_data$Species)
#'    return(results)
#'  })
#'
#'  grid$Confident_Species <- NA
#'
#'  for (i in 1:nrow(grid)) {
#'    conf_set <- confidence_matrix[[i]]
#'    conf_species <- names(which(conf_set == TRUE))
#'    if (length(conf_species) > 1) {
#'      grid$Confident_Species[i] <- paste(conf_species, collapse = ',')
#'    } else if (length(conf_species) == 0) {
#'      grid$Confident_Species[i] <- NA
#'    } else {
#'      grid$Confident_Species[i] <- conf_species[1]
#'    }
#'  }
#'
#'  p1 = ggplot() +
#'    geom_tile(data = grid, aes(x = Sepal.Length, y = Petal.Width, fill = Confident_Species), alpha = 0.3) +  # 决策区域
#'    geom_point(data = iris, aes(x = Sepal.Length, y = Petal.Width, color = Species, shape = Species), size = 3) +  # 数据点
#'    labs(title = "SVM 90% Regions",
#'         x = "Sepal Length", y = "Petal Width") +
#'    scale_fill_manual(values = c("setosa" = "red", "versicolor" = "blue", "virginica" = "green", "setosa,versicolor" = "purple", "versicolor,virginica" = "yellow", "setosa,versicolor,virginica" = "lightpink")) +
#'    scale_color_manual(values = c("setosa" = "red", "versicolor" = "blue", "virginica" = "green")) +
#'    scale_shape_manual(values = c("setosa" = 16, "versicolor" = 17, "virginica" = 18)) +
#'    theme_minimal()
#'
#'  # ggsave("SVM 0.90 Regions.pdf", plot = p1, bg = "white")
#'  print(p1)


full_cp_cla <- function(){
library(caret)
library(e1071)
data(iris)

iris_data <- iris[, c("Sepal.Length", "Petal.Width", "Species")]
iris_data$Species <- as.factor(iris_data$Species)

set.seed(123)
trainIndex <- createDataPartition(iris_data$Species, p = .8,
                                  list = FALSE,
                                  times = 1)
trainData <- iris_data[ trainIndex,]
testData  <- iris_data[-trainIndex,]

train_svm <- function(train_data) {
  model <- svm(Species ~ ., data = train_data, probability = TRUE)
  return(model)
}

calculate_nonconformity <- function(model, x) {
  pred <- predict(model, x, probability = TRUE)
  pred_prob <- attr(pred, "probabilities")
  species_levels <- colnames(pred_prob)
  x_species_char <- as.character(x$Species)
  col_indices <- match(x_species_char, species_levels)
  prob_values <- pred_prob[cbind(1:nrow(x), col_indices)]
  nonconformity <- 1 - prob_values  # 计算差异
  return(nonconformity)
}


svm_model <- train_svm(trainData)

is_in_confidence_set <- function(x, y, train_data, alpha = 0.1) {
  test_data = data.frame(x[,-3], Species = as.factor(y))
  new_train_data <- rbind(train_data, test_data)

  new_model <- svm(Species ~ ., data = new_train_data, probability = TRUE)

  nonconformity <- calculate_nonconformity(new_model, test_data)

  train_nonconformities <- calculate_nonconformity(new_model, train_data)

  sorted_train_nonconformities <- sort(train_nonconformities)
  threshold <- sorted_train_nonconformities[ceiling(length(sorted_train_nonconformities) * (1 - alpha))]

  return(nonconformity <= threshold)
}

confidence_sets <- lapply(1:nrow(testData), function(i) {
  x <- testData[i, ]
  results <- sapply(levels(iris_data$Species), function(y) {
    is_in_confidence_set(x, y, trainData)
  })
  names(results) <- levels(iris_data$Species)
  return(results)
})

for (i in 1:length(confidence_sets)) {
  cat("Test sample", i, ": Confidence set =", names(which(confidence_sets[[i]] == TRUE)), "\n")
}




x_range <- seq(min(iris_data$Sepal.Length), max(iris_data$Sepal.Length), length.out = 20)
y_range <- seq(min(iris_data$Petal.Width), max(iris_data$Petal.Width), length.out = 20)
grid <- expand.grid(Sepal.Length = x_range, Petal.Width = y_range)

confidence_matrix <- lapply(1:nrow(grid), function(i) {
  x <- grid[i, ]
  results <- sapply(levels(iris_data$Species), function(y) {
    is_in_confidence_set(x, y, trainData)
  })
  names(results) <- levels(iris_data$Species)
  return(results)
})

grid$Confident_Species <- NA

for (i in 1:nrow(grid)) {
  conf_set <- confidence_matrix[[i]]
  conf_species <- names(which(conf_set == TRUE))
  if (length(conf_species) > 1) {
    grid$Confident_Species[i] <- paste(conf_species, collapse = ',')
  } else if (length(conf_species) == 0) {
    grid$Confident_Species[i] <- NA
  } else {
    grid$Confident_Species[i] <- conf_species[1]
  }
}

p1 = ggplot() +
  geom_tile(data = grid, aes(x = Sepal.Length, y = Petal.Width, fill = Confident_Species), alpha = 0.3) +  # 决策区域
  geom_point(data = iris, aes(x = Sepal.Length, y = Petal.Width, color = Species, shape = Species), size = 3) +  # 数据点
  labs(title = "SVM 90% Regions",
       x = "Sepal Length", y = "Petal Width") +
  scale_fill_manual(values = c("setosa" = "red", "versicolor" = "blue", "virginica" = "green", "setosa,versicolor" = "purple", "versicolor,virginica" = "yellow", "setosa,versicolor,virginica" = "lightpink")) +
  scale_color_manual(values = c("setosa" = "red", "versicolor" = "blue", "virginica" = "green")) +
  scale_shape_manual(values = c("setosa" = 16, "versicolor" = 17, "virginica" = 18)) +
  theme_minimal()

# ggsave("SVM 0.90 Regions.pdf", plot = p1, bg = "white")
print(p1)
}
