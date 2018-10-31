
<!--- README.md is generated from README.Rmd. Please edit that file -->
Optimal Project Prioritization Protocol
=======================================

[![lifecycle](https://img.shields.io/badge/Lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![Travis Build Status](https://img.shields.io/travis/prioritizr/optimalppp/master.svg?label=Linux%20%26%20Mac%20OSX)](https://travis-ci.org/prioritizr/optimalppp) [![AppVeyor Build Status](https://img.shields.io/appveyor/ci/jeffreyhanson/optimalppp/master.svg?label=Windows)](https://ci.appveyor.com/project/jeffreyhanson/optimalppp) [![Coverage Status](https://codecov.io/github/prioritizr/optimalppp/coverage.svg?branch=master)](https://codecov.io/github/prioritizr/optimalppp?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/optimalppp)](https://CRAN.R-project.org/package=optimalppp)

**This repository is still under active development. It is not yet ready for use.**

The *optimalppp R* package provides methods for prioritizing funding of conservation projects using the 'Protect Prioritization Protocol'. A range of methods are provided for identifying priority projects for funding. These include exact algorithm solvers which can identify optimal solutions, and also stingy heuristic algorithms that have conventionally been used to identify suboptimal solutions. This package also provides the functionality to visualize how well solutions maintain biodiversity.

Installation
------------

The latest development version can be installed using the following code.

``` r
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("prioritizr/optimalppp")
```

Usage
-----

Here we will provide a short example showing how the *optimalppp R* package can be used to prioritize funding for conservation projects. To start off, we will set the seed for the random number generator to ensure you get the same results as shown here, set some default behavior for the *R* session, load the *optimalppp R* package, and load the *ggtree R* package to plot phylogenetic trees. Please note that you will need install the *ggtree* and *treeio* package separately from Bioconductor since they are not on the Comprehensive R Archive Network (CRAN; see the `ppp_plot` help file for [installation instructions](https://prioritizr.github.io/optimalppp/reference/ppp_plot.html)).

``` r
set.seed(500)
options(getClass.msg = FALSE)
library(optimalppp)
library(ggtree)
```

Now we will load some data sets that are distributed with the package. First, we will load the `sim_tree` object. This object describes the evolutionary relationships between 50 simulated species (named S1, S2, S3, ...). The length of each phylogenetic branch corresponds to millions of years of evolutionary history, and the last common ancestor for all these species occurred approximately 2 million years ago.

``` r
# load data
data(sim_tree)

# plot tree
ggtree(sim_tree) +
geom_tiplab(size = 2.5) +
geom_treescale(width = 2, x = 0, offset = 1)
```

    ## Found more than one class "phylo" in cache; using the first, from namespace 'tidytree'

    ## Also defined by 'treeio'

    ## Found more than one class "phylo" in cache; using the first, from namespace 'tidytree'

    ## Also defined by 'treeio'

<img src="man/figures/README-unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

Next, we will load the `sim_project_data` object. This object stores information about various conservation projects in a tabular format (i.e. `tibble`). Each row corresponds to a different project, and each column describes various properties associated with the projects. Importantly, the `"name"` column contains the name of each project, the `"cost"` column contains the cost of each project, the `"success"` column denotes the probability of each project succeeding if it is funded, and the `"S1"`--`"SN"` columns show the enhanced probability of each species persisting if the project is funded. It also contains additional columns for customizing the solutions, but we will ignore them for now. Note that the last project---the `"baseline_project"`---has a zero cost and represents the baseline probability of each species persisting if no other project is funded. Finally, though most projects in this example directly relate to a single species, you can input projects that directly affect the persistence of multiple species.

``` r
# load data
data(sim_project_data)

# print table
print(sim_project_data)
```

    ## # A tibble: 51 x 55
    ##    name   cost success locked_in locked_out    S1   S10   S11   S12   S13
    ##    <chr> <dbl>   <dbl> <lgl>     <lgl>      <dbl> <dbl> <dbl> <dbl> <dbl>
    NA
    NA
    NA
    NA
    NA
    NA
    NA
    NA
    NA
    NA
    ## # ... with 41 more rows, and 45 more variables: S14 <dbl>, S15 <dbl>,
    ## #   S16 <dbl>, S17 <dbl>, S18 <dbl>, S19 <dbl>, S2 <dbl>, S20 <dbl>,
    ## #   S21 <dbl>, S22 <dbl>, S23 <dbl>, S24 <dbl>, S25 <dbl>, S26 <dbl>,
    ## #   S27 <dbl>, S28 <dbl>, S29 <dbl>, S3 <dbl>, S30 <dbl>, S31 <dbl>,
    ## #   S32 <dbl>, S33 <dbl>, S34 <dbl>, S35 <dbl>, S36 <dbl>, S37 <dbl>,
    ## #   S38 <dbl>, S39 <dbl>, S4 <dbl>, S40 <dbl>, S41 <dbl>, S42 <dbl>,
    ## #   S43 <dbl>, S44 <dbl>, S45 <dbl>, S46 <dbl>, S47 <dbl>, S48 <dbl>,
    ## #   S49 <dbl>, S5 <dbl>, S50 <dbl>, S6 <dbl>, S7 <dbl>, S8 <dbl>, S9 <dbl>

Let us assume that our resources are limited such that we can only spend, at most, $500 on funding conservation projects. In other words, our budget is capped at $500. Now, given the project data (`sim_project_data`), the species' evolutionary relationships (`sim_tree`), and this budget (`500`), So, let's cut to the chase and find an optimal solution.

``` r
s1 <- ppp_exact_solution(x = sim_project_data, tree = sim_tree,
                         budget = 500, project_column_name = "name",
                         cost_column_name = "cost",
                         success_column_name = "success")
```

The object `s1` contains the solution and also various statistics associated with the solution in a tabular format (i.e. `tibble`). Here, each row corresponds to a different solution. Specifically, the `"solution"` column contains an identifier for the solution (this is useful for methods that output multiple solutions), the `"objective"` column contains the objective value (i.e. the expected phylogenetic diversity, Faith 2008), the `"budget"` column stores the budget used for generating the solution, the `"cost"` column stores the cost of the solution, the `"optimal"` column indicates if the solution is known to be optimal (`NA` values mean the optimality is unknown), and the `"method"` column contains the name of the method used to generate the solution. The remaining columns (`"S1_project"`, `"S2_project"`, `"S3_project"`, ..., `"S50_project"`, and `"baseline_project"`) indicate if a given project was prioritized for funding in the solution or not.

Here, the objective value (in the `"objective"` column) denotes the amount of evolutionary history that is expected to persist (i.e. 6.696 million years). Put simply, solutions that are expected to result in better conservation outcomes will be associated with a greater objective value. Since tabular data can be difficult to intuit, let's visualize how well this solution would maintain the different branches in the phylogeny. Note that species which receive any funding are denoted with an asterisk.

``` r
# visualize solution
ppp_plot(sim_project_data, sim_tree, s1, project_column_name = "name",
         cost_column_name = "cost", success_column_name = "success")
```

    ## Found more than one class "treedata" in cache; using the first, from namespace 'tidytree'

    ## Also defined by 'treeio'

<img src="man/figures/README-unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

This has just been a taster of the *optimalppp R* package. For more information, see the \[package vignette\]( (<https://prioritizr.github.io/optimalppp/articles/optimalppp.html>).

Citation
--------

**This repository is still under active development. It is not yet ready for use.**

Please use the following citation to cite the *optimalppp R* package in publications:

Hanson JO, Schuster R, Strimas-Mackey M, Bennett J, (2018). optimalppp: Optimal Project Prioritization Protocol. R package version 0.0.0.1. Available at <https://github.com/prioritizr/optimalppp>.
