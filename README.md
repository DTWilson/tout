
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TOut

<!-- badges: start -->
<!-- badges: end -->

The goal of TOut is to optimise the design of Three-Outcome clinical
trials by determining their sample size and their decision rule
thresholds (also known as *progression criteria* in the field of pilot
trials).

## Installation

You can install the development version of TOut from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DTWilson/TOut")
```

## Example

Suppose we want to use a pilot trial with $n$ participants per arm to
decide if a larger, definitive trial will be feasible with respect to
the level of adherence to treatment in the experimental arm. Adherence
is considered to be a binary event for each participant, which will
occur with probability $\rho$. We would like to avoid conducting the
main trial if the adherence rate is less that 0.5, but would like to
proceed when it is greater than 0.7. When adherence is between these two
levels, we would like to make our progression decision based on other
observations made during the pilot trial. Our aim is to find optimal
progression criteria $x_0, x_1$ such that

$$
Decision =
\begin{cases}
 stop    & \hat{\rho} \leq x_0 \\
 pause   & x_0 < \hat{\rho} \leq x_1 \\
 go      & x_1 < \hat{\rho}
 \end{cases}       
$$

To formalise this problem we define a null and alternative hypothesis as
$\rho_0 = 0.5, \rho_1 = 0.7$ respectively, and set up some operating
characteristics which we want to minimise. Specifically, we will use:

-   $\alpha$: the probability of proceeding to the main trial when
    $\rho = \rho_0$;
-   $\beta$: the probability of not proceeding to the main trial when
    $\rho = \rho_1$; and
-   $\gamma$: the probability of *not* obtaining a $pause$ decision when
    $\rho = (\rho_0 + \rho_1)/2$.

The TOut package allows us to specify maximum levels of these three
operating characteristics, and then finds all designs (i.e. all possible
choices of $x_0, x_1$) which will satisfy these constraints. When there
are several such designs, it will suggest the one which minimises the
average of the three operating characteristics.

``` r
library(TOut)

opt_pc(n = 200, rho_0 = 0.5, rho_1 = 0.7,
                     alpha_nom = 0.05, beta_nom = 0.2, gamma_nom = 0.5)
#> [1] 200 109 131
```

This function `opt_pc` returns a vector of the form $(n, x_0, x_1)$. In
this case we find that a pilot trial with $n = 200$ patients in the
intervention arm will satisfy our constraints
$\alpha < 0.05, \beta < 0.2, \gamma < 0.5$ if we $stop$ if observing 109
patients or fewer adhere, $go$ if we observe more that 131 patients
adhere, and $pause$ otherwise.
