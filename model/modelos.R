# model/modelos.R — Los 7 modelos de regresión del curso con su rejilla de hiperparámetros.
#
# Cada modelo define:
#   rejilla:  data.frame con todas las combinaciones que prueba la búsqueda en rejilla (Grid Search)
#   ajustar:  function(X, y, p) -> modelo entrenado (X ya preprocesada, p = una fila de la rejilla)
#   predecir: function(fit, X) -> vector con el índice predicho
#
# Todos reciben la matriz que produce aplicar_pre() (features.R), igual que el Pipeline de Python.

suppressPackageStartupMessages({
  library(glmnet)
  library(rpart)
  library(randomForest)
  library(xgboost)
  library(e1071)
})

knn_predecir <- function(fit, X) {
  # KNN propio (sin paquetes extra) para poder ponderar por distancia como en scikit-learn.
  d2 <- outer(rowSums(X^2), rowSums(fit$X^2), "+") - 2 * X %*% t(fit$X)
  d <- sqrt(pmax(d2, 0))
  apply(d, 1, function(fila) {
    idx <- order(fila)[seq_len(fit$k)]
    if (fit$pesos == "distancia") {
      if (any(fila[idx] == 0)) return(mean(fit$y[idx][fila[idx] == 0]))
      w <- 1 / fila[idx]
      sum(w * fit$y[idx]) / sum(w)
    } else mean(fit$y[idx])
  })
}

MODELOS <- list(
  "Ridge" = list(
    rejilla = expand.grid(lambda = c(1e-4, 1e-3, 1e-2, 1e-1, 1)),
    ajustar = function(X, y, p) glmnet(X, y, alpha = 0, lambda = p$lambda, standardize = FALSE),
    predecir = function(fit, X) as.numeric(predict(fit, newx = X))
  ),
  "Lasso" = list(
    rejilla = expand.grid(lambda = c(1e-4, 1e-3, 1e-2, 1e-1)),
    ajustar = function(X, y, p) glmnet(X, y, alpha = 1, lambda = p$lambda, standardize = FALSE),
    predecir = function(fit, X) as.numeric(predict(fit, newx = X))
  ),
  "KNN" = list(
    rejilla = expand.grid(k = c(3, 5, 10, 15, 20, 30), pesos = c("uniforme", "distancia"),
                          stringsAsFactors = FALSE),
    ajustar = function(X, y, p) list(X = X, y = y, k = min(p$k, nrow(X)), pesos = p$pesos),
    predecir = knn_predecir
  ),
  "Árbol de decisión" = list(
    rejilla = expand.grid(profundidad = c(2, 3, 4, 6), hoja = c(1, 5, 10, 20)),
    ajustar = function(X, y, p) {
      rpart(y ~ ., data = data.frame(X, y = y), method = "anova",
            control = rpart.control(maxdepth = p$profundidad, minbucket = p$hoja, minsplit = 2 * p$hoja,
                                    cp = 0, xval = 0))
    },
    predecir = function(fit, X) as.numeric(predict(fit, newdata = data.frame(X)))
  ),
  "Random Forest" = list(
    # profundidad NA = sin límite; con un límite d se permiten hasta 2^d hojas por árbol
    rejilla = expand.grid(profundidad = c(3, 5, NA), hoja = c(1, 3, 5, 10), variables = c(1, 0.5)),
    ajustar = function(X, y, p) {
      set.seed(SEED)
      randomForest(x = X, y = y, ntree = 300, nodesize = p$hoja,
                   mtry = max(1, floor(p$variables * ncol(X))),
                   maxnodes = if (is.na(p$profundidad)) NULL else 2^p$profundidad)
    },
    predecir = function(fit, X) as.numeric(predict(fit, X))
  ),
  "XGBoost" = list(
    rejilla = expand.grid(profundidad = c(2, 3, 4), tasa = c(0.03, 0.1), submuestra = c(0.8, 1),
                          peso_hijo = c(1, 5)),
    ajustar = function(X, y, p) {
      set.seed(SEED)
      xgb.train(params = list(objective = "reg:squarederror", max_depth = p$profundidad, eta = p$tasa,
                              subsample = p$submuestra, min_child_weight = p$peso_hijo, nthread = 1,
                              seed = SEED),
                data = xgb.DMatrix(X, label = y), nrounds = 300, verbose = 0)
    },
    predecir = function(fit, X) as.numeric(predict(fit, xgb.DMatrix(X)))
  ),
  "SVR" = list(
    # gamma "scale" = 1 / (número de variables × varianza de X), como en scikit-learn
    rejilla = expand.grid(C = c(0.1, 1, 10), epsilon = c(0.05, 0.1, 0.2), gamma = c("scale", "0.05"),
                          stringsAsFactors = FALSE),
    ajustar = function(X, y, p) {
      g <- if (p$gamma == "scale") 1 / (ncol(X) * mean((X - mean(X))^2)) else as.numeric(p$gamma)
      svm(x = X, y = y, type = "eps-regression", kernel = "radial", cost = p$C, epsilon = p$epsilon,
          gamma = g, scale = FALSE)
    },
    predecir = function(fit, X) as.numeric(predict(fit, X))
  )
)

predecir_indice <- function(final, filas) {
  # Predice el índice con el modelo final guardado en model/artifacts/model.rds
  MODELOS[[final$modelo]]$predecir(final$fit, aplicar_pre(final$pre, filas))
}
