# endoR
Code and manual of the endoR R-package. A Python version is currently a work in progress and I hope to release it soon - any help welcome ;) 

[![DOI](https://zenodo.org/badge/349814633.svg)](https://zenodo.org/badge/latestdoi/349814633)


- author: Albane Ruaud [albane.ruaud@tuebingen.mpg.de](mailto:albane.ruaud@tuebingen.mpg.de)
- maintainer: Albane Ruaud [albane.ruaud@tuebingen.mpg.de](mailto:albane.ruaud@tuebingen.mpg.de)

Article: Albane Ruaud, Niklas A Pfister, Ruth E Ley, Nicholas D Youngblut. Interpreting tree ensemble machine learning models with endoR. Plos Computational Biology (2022). DOI: [10.1371/journal.pcbi.1010714](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1010714)


# Abstract
**Motivation:** Tree ensemble machine learning models are increasingly used in microbiome science to explore associations between microbes and their environment, as they are compatible with the compositional, high-dimensional, and sparse structure of sequence-based microbiome data. The complex structure of such models enables efficient capture of higher-order interactions to improve predictions, but makes the final model often difficult to interpret. Therefore, while tree ensembles are often the most accurate models for microbiome data, the approach often yields limited insight into how microbial taxa or genomic content may be associated with host phenotypes.

**Procedure:** endoR is a method that extracts and visualizes how predictive variables contribute to tree ensemble model accuracy. The fitted model is simplified into a decision ensemble and then reduced via regularization and bootstrapping to retain only the most essential aspects. Information about predictive variables and their pairwise interactions are extracted from the decision ensemble and displayed as a network for straightforward interpretation. The network and importance scores derived from endoR help understand how tree ensemble models make predictions from variables and interactions between variables.

![endoR overall workflow](figures/Workflow.png)


# Usage
Installation, with dependencies = FALSE should work to avoid updating everything: `devtools::install_github(repo = "aruaud/endoR", dep = FALSE)`

Before: select and fit a machine learning model (regression or classification)
- random forest R-packages: randomForest, ranger 
- gradient boosted model R-packages: GBM, XGBoost 

1. Generate the **stable decision ensemble**:
	- without bootstrapping, opt. in parallel, with `model2DE()`
	- with bootstrapping, opt. in parallel, with `model2DE_resampling()`
	- with boostrapping each ran in parallel (recommended): `preCluster()` followed by `model2DE_cluster()` iterated on partitions in the `Q()` function of the clusterMQ R-package
2. If bootstrapping, perform **stability selection**: `stabilitySelection()`. Alternatively, use `evaluateAlpha()` to perform stability selection with various values of `alpha` and pick the decision ensemble that can predict as many samples as possible for the lowest `alpha`.
3. Plot results with `plotFeatures()` and `plotNetwork()` (created with ggplot2, ggraph and igraph R-packages, arranged with the ggpubr R-package)

Some common issues: 
- if the installation doesn't work, you may need to install R version 4.0.3 and use the inTrees R-package version 1.3 
- endoR uses text patterns: make sure to remove all special characters from your column names before using endoR, `colnames(data) <- compatibleNames(colnames(data))`


# Background

**From tree ensemble to decision ensembles**

![Schema decisions](figures/Decisions_summary.png)

A decision consists of a full branch of a decision tree: 
- all splits along the branch form the *rule* of the decision;
- the average response variable in the subset of observations following the rule corresponds to the *prediction* of the decision;
- the sample support is the set of observations that follow the rule;
- the error is the average deviation of the prediction to the observed resonse variable. 


**Decision importance**

We measure the decision importance by comparing the error of the decision to what could be expected by a random guessing, i.e., the mean response variable across all samples. Respectively for regressions and classifications, this corresponds to calculating the coefficient of determination R<sup>2</sup> and Cohen's kappa. We weight 

<img src="https://latex.codecogs.com/svg.image?\bg_white&space;I_D&space;=&space;\Big(&space;1&space;-&space;\frac{\alpha(S_D,&space;\hat{y}_D)}{\alpha(S_D,\bar{y})}&space;\Big)&space;\cdot&space;|S_D|" title="\bg_white I_D = \Big( 1 - \frac{\alpha(S_D, \hat{y}_D)}{\alpha(S_D,\bar{y})} \Big) \cdot |S_D|" />


**Feature and interaction importance** = how much a feature, or pair of features, contributes to the accuracy of predictions

**Feature and interaction influence** = how a feature, or pair of features, changes precisions (e.g., increases the predicted age or changes the prediction towards "disease" vs "healthy" state)

**Regularization** to obtain a stable decision ensemble that is generalizable
- decision pruning = remove unimportant variables from decision rules (i.e. the ones that do not increase the decision error much than a threshold)
- bootstrapping and stability selection = select decisions measured as important across bootstrap resamples (i.e, across bootstraps of observations/samples)
