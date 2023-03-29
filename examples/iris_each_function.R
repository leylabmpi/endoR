library(randomForest)
library(caret)
library(data.table)

# import data and fit model
data(iris)
mod <- randomForest(Species ~ ., data = iris)

# Let's get the decision ensemble. One could use the wrapping function
# model2DE() but, we will run each function separately.

# Get the raw decision ensemble
de <- preCluster(model = mod, model_type = "rf", data = iris[, -5]
        , target = iris$Species, classPos = "setosa"
        , times = 1 # number of bootstraps, here just one
        , discretize = FALSE) # we will discretize outside for the example
summary(de)
# exec = the decision ensemble
# partitions = list of sample indexes for boostrapping
# if we had done discretization, the new data would be in data_ctg
de <- de$exec

# Discretize variables in 3 categories - optional
de <- discretizeDecisions(rules = de, data = iris[, -5], target = iris$Species
        , K = 3, classPos = "setosa", mode = "data")
data_ctg <- de$data_ctg
de <- de$rules_ctg

# Homogenize the decision ensemble
de <- de[, condition := sapply(condition, function(x) {
  paste(sort(unlist(strsplit(x, split = " & "))), collapse = " & ")
})]
de <- unique(
          as.data.table(de)[, n := as.numeric(n)][, n := sum(n), by = condition]
          )

# Calculate decision metrics, we don't need the importances yet since we will
# do pruning. Otherwise, set importances = TRUE and skip the next 2 steps.
de_met <- getDecisionsMetrics(de, data = data_ctg, target = iris$Species
            , classPos = "setosa", importances = FALSE)
de <- de[de_met, on = "condition"]

# Pruning - optional
de <- pruneDecisions(rules = de, data = data_ctg, target = iris$Species
        , classPos = "setosa")

# Decision importances
de <- decisionImportance(rules = de, data = data_ctg, target = iris$Species
        , classPos = "setosa")

# Filter out decisions with the lowest importance: min_imp = the minimal
# importance in the decision ensemble compared to the maximal one.
# E.g., if min_imp = 0.5, then at least all decisions with an
# importance > 0.5*max(importance) will be kept.
# This ensures that we don't throw out too much.
# Since the decision ensemble is quite small, we don't need to filter much...
de <- filterDecisionsImportances(rules = de, min_imp = 0.1)

# Get the network
de_net <- getNetwork(rules = de, data = data_ctg, target = iris$Species
            , classPos = "setosa")

# Plot the feature importance/influence and the network
plotFeatures(de_net, levels_order = c("Low", "Medium", "High"))
plotNetwork(de_net, hide_isolated_nodes = FALSE, layout = "fr")
