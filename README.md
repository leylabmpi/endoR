# endoR
Code and manual of the endoR R-package (Ruaud et al, in preparation).

[![DOI](https://zenodo.org/badge/349814633.svg)](https://zenodo.org/badge/latestdoi/349814633)


- author: Albane Ruaud [albane.ruaud@tuebingen.mpg.de](mailto:albane.ruaud@tuebingen.mpg.de)
- maintainer: Albane Ruaud [albane.ruaud@tuebingen.mpg.de](mailto:albane.ruaud@tuebingen.mpg.de)

# Abstract
**Motivation:** Tree ensemble machine learning models are increasingly used in microbiome science to explore associations between microbes and their environment, as they are compatible with the compositional, high-dimensional, and sparse structure of sequence-based microbiome data. The complex structure of such models enables efficient capture of higher-order interactions to improve predictions, but makes the final model often difficult to interpret. Therefore, while tree ensembles are often the most accurate models for microbiome data, the approach often yields limited insight into how microbial taxa or genomic content may be associated with host phenotypes.

**Procedure:** endoR is a method that extracts and visualizes how predictive variables contribute to tree ensemble model accuracy. The fitted model is simplified into a decision ensemble and then reduced via regularization and bootstrapping to retain only the most essential aspects. Information about predictive variables and their pairwise interactions are extracted from the decision ensemble and displayed as a network for straightforward interpretation. The network and importance scores derived from endoR help understand how tree ensemble models make predictions from variables and interactions between variables.
