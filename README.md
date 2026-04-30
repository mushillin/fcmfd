fcmfd
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# fcmfd

<!-- badges: start -->

<!-- badges: end -->

**Fuzzy C-Means Clustering for Ordinal Data using Triangular Fuzzy
Numbers**

------------------------------------------------------------------------

## Overview

The **fcmfd** package implements fuzzy clustering for ordinal
Likert-type data using **Triangular Fuzzy Numbers (TFNs)**.

It is designed for datasets where responses are measured on discrete
ordinal scales (e.g., 1-5, 1–7, 1-10 or 0–10), providing a robust
alternative to traditional clustering approaches.

------------------------------------------------------------------------

## Features

- Fuzzy C-Means clustering adapted to TFNs
- Automatic selection of the optimal number of clusters
- Xie–Beni validity index
- Support for Likert-type data
- Cluster assignment and prototype extraction
- Visualization tools

------------------------------------------------------------------------

## Installation

``` r
# install.packages("devtools")
devtools::install_github("yourusername/fcmfd")
```

------------------------------------------------------------------------

## Example

``` r
library(fcmfd)

# Load dataset
data(sim_likert_0_10)

# Run clustering
result <- fcmTFN(
  data = sim_likert_0_10,
  option = "B",
  k_values = 2:6
)

# Summary
summary(result)

# Cluster assignment
clusters <- cluster_assignment(result)
table(clusters)

# Plot Xie–Beni index
plot_xb(result)
```

------------------------------------------------------------------------

## Included Datasets

- **sim_likert7** Simulated dataset with a 1–7 Likert scale

- **sim_likert_0_10** Simulated dataset with a 0–10 Likert scale and
  latent cluster structure

------------------------------------------------------------------------

## Methodological Background

The package combines:

- Fuzzy C-Means clustering
- Triangular Fuzzy Numbers representation
- Xie–Beni cluster validity index

to provide a framework tailored for ordinal data.

------------------------------------------------------------------------

## Uses Cases

- Survey analysis
- Social sciences
- Customer satisfaction
- Quality of life studies
- Likert-type data clustering

------------------------------------------------------------------------

## References

Coppi, R., D’Urso, P., & Giordani, P. (2011). Fuzzy clustering of fuzzy
data. *Computational Statistics & Data Analysis*.
<https://doi.org/10.1016/j.csda.2010.09.013>

Xie, X. L., & Beni, G. (1991). A validity measure for fuzzy clustering.
*IEEE Transactions on Pattern Analysis and Machine Intelligence*.
<https://doi.org/10.1109/34.85677>

------------------------------------------------------------------------

## Author

José Ortigas

------------------------------------------------------------------------

## License

MIT
