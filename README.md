# questionlink

## About

The QuestionLink R-package aims to help researchers who want to harmonize survey data on a concept that were gathered with different questions. Specifically, the focus lies on (single) survey questions used to capture a latent construct, such as attitudes, values, interests, or subjective evaluations.

## Installation

Currently, questionlink is available via github.
To install questionlink from github, you can use one of the following commands:

``` r
# Installing questionlink with devtools
devtools::install_github("https://github.com/MatRoth/questionlink")

# Installing questionlink with pak
pak::pak("https://github.com/MatRoth/questionlink")
``` 

## Getting started
We would strongly recommend working through the [questionlink tutorial](https://matroth.github.io/questionlink/articles/questionlink_tutorial.html)before using the package. 
Applying the QuestionLink package is easy, but to ensure valid harmonization solutions you need to understand the methodological underpinnings and the necesary assumptions.

However, before you invest the time to understand our package, note that there may be alternatives for your specific case:

[ ] If you want to harmonize two psychometric multi-item scales, which have at least some identical items in common, consider NEAT Equating (Non-equivalent Groups with Anchor Tests Equating). 
[ ] If you only want to harmonize two single-item instruments and you have a split-half experiment varying the two, simply apply the [Equate Package](https://github.com/talbano/equate) directly.
[ ] If you have a dataset, where all respondents answered both instruments (ideally in random order), then consider applying a [calibrated multiple imputation approach](https://doi.org/10.1002/sim.6562) instead.
