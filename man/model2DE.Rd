% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model2DE.R
\name{model2DE}
\alias{model2DE}
\title{Extract a decision ensemble from a tree-based model, simplify it and creates an interaction network from it.}
\usage{
model2DE(
  model,
  model_type,
  data,
  target,
  ntree = "all",
  maxdepth = Inf,
  classPos = NULL,
  dummy_var = NULL,
  discretize = FALSE,
  K = 2,
  mode = "data",
  prune = TRUE,
  maxDecay = 0.05,
  typeDecay = 2,
  aggregate_taxa = FALSE,
  taxa = NULL,
  filter = TRUE,
  min_imp = 0.7,
  exec = NULL,
  in_parallel = FALSE,
  n_cores = detectCores() - 1,
  cluster = NULL,
  light = FALSE
)
}
\arguments{
\item{model}{model to extract rules from.}

\item{model_type}{character string: 'RF', 'random forest', 'rf', 'xgboost', 'XGBOOST', 'xgb', 'XGB', 'ranger', 'Ranger', 'gbm' or 'GBM'.}

\item{data}{data with the same columns than data used to fit the model.}

\item{target}{response variable.}

\item{ntree}{number of trees to use from the model (default = all)}

\item{maxdepth}{maximal node depth to use for extracting rules (by default, full branches are used).}

\item{classPos}{the positive class predicted by decisions}

\item{dummy_var}{if multiclass variables were transformed into dummy variables before fitting the model, one can pass their names in a vector here to avoid multiple levels to be used in a same rule (recommended).}

\item{discretize}{should numeric variables be transformed to categorical variables? If TRUE, K categories are created for each variable based on their distribution in data (mode = 'data') or based on the thresholds used in the decision ensemble (mode = 'model')}

\item{K}{numeric, number of categories to create from numeric variables (default: K = 2).}

\item{mode}{whether to discretize variables based on the data distribution (default, mode = 'data') or on the data splits in the model (mode = 'model').}

\item{prune}{should unimportant features be removed from decisions (= pruning)? Features are removed by calculating the difference in prediction error of the decision with versus without the feature. If the difference is small (< maxDecay), then the feature is removed. The difference can be absolute (typeDecay = 1) or relative (typeDecay = 2, default). See pruneDecisions() for details.}

\item{maxDecay}{when pruning, threshold for the increase in error; if maxDecay = -Inf, no pruning is done; if maxDecay = 0, only variables increasing the error are pruned from decisions.}

\item{typeDecay}{if typeDecay = 1, the absolute increase in error is computed, and the relative one is computed if typeDecay = 2 (default).}

\item{aggregate_taxa}{should taxa be aggregated at the genus level (if species have lower importance than their genus) or species level (if a genus is represented by a unique species)}

\item{taxa}{if aggregate_taxa = TRUE, a data.frame with all taxa included in the dataset: columns = taxonomic ranks (with columns f, g, and s)}

\item{filter}{should decisions with low importance be removed from the decision ensemble? If TRUE, then decisions are filtered in a heuristic manner according to their importance and multiplicity (see filterDecisionsImportances() ).}

\item{min_imp}{minimal relative importance of the decisions that must be kept, the threshold to remove decisions is thus going to take lower values than max(imp)*min_imp.}

\item{exec}{if decisions have already been extracted, datatable with a 'condition' column.}

\item{in_parallel}{if TRUE, the function is run in parallel}

\item{n_cores}{if in_parallel = TRUE, and no cluster has been passed: number of cores to use, default is detectCores() - 1}

\item{cluster}{the cluster to use to run the function in parallel}

\item{light}{if FALSE, returns all intermediary decision ensembles; default = TRUE}
}
\value{
A list with the final decision ensemble, if numeric variables were discretized in decisions, the discretized data, edges and nodes to make a network (plotNetwork and plotFeatures).
}
\description{
Wrapper function to extract rules from a tree based model.
It automatically transforms multiclass predictive variables into dummy variables.
Optionally discretizes numeric variables (see discretizeDecisions). Measures decisions and optionally prunes them. Finally, generates a network.
Can be run in parallel.
}
\examples{
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
}
