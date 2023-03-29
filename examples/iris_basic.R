library(randomForest)
library(caret)

# import data and fit model
data(iris)
mod <- randomForest(Species ~ ., data = iris)

# Fit a decision ensemble to predict the species setosa (vs versicolor and
# virginica): no regularization (no decision pruning, discretization,
# bootstrapping, or decision filtering)
endo_setosa <- model2DE(model = mod, model_type = "rf", data = iris[, -5]
    , target = iris$Species, classPos = "setosa"
    , filter = FALSE, discretize = FALSE, prune = FALSE)

# Only decision pruning (default = TRUE) and discretization (default in 2
# categories, we want 3 categories so we change K); no bootstrapping or
# decision filtering.
endo_setosa <- model2DE(model = mod, model_type = "rf", data = iris[, -5]
    , target = iris$Species, classPos = "setosa"
    , filter = FALSE, discretize = TRUE, K = 3)

# idem but run it in parallel on 2 threads
endo_setosa <- model2DE(model = mod, model_type = "rf", data = iris[, -5]
    , target = iris$Species, classPos = "setosa"
    , filter = FALSE, discretize = TRUE, K = 3
    , in_parallel = TRUE, n_cores = 2)

# Plot the decision ensemble:
# Plants from the setosa species have small petal and narrow long sepals.
plotFeatures(endo_setosa, levels_order = c("Low", "Medium", "High"))
plotNetwork(endo_setosa, hide_isolated_nodes = FALSE, layout = "fr")
