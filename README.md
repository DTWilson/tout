
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TOut

<!-- badges: start -->
<!-- badges: end -->

The goal of TOut is to optimise the design of **T**hree-**Out**come
clinical trials by determining their sample size and their decision rule
thresholds (also known as progression criteria in the field of pilot
trials).

## Installation

You can install the development version of TOut from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DTWilson/TOut")
```

## Example - binary outcome

### Fixed sample size

Suppose we want to use a pilot trial with $n$ participants per arm to
decide if a larger, definitive trial will be feasible with respect to
the level of adherence to treatment in the experimental arm. Adherence
is a binary event for each participant, which will occur with
probability $\rho$. We would like to avoid conducting the main trial if
the adherence rate is less that 0.5, but would like to proceed if it is
greater than 0.7. When adherence is between these two levels, we would
ideally like to pause amd make our progression decision based on other
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

To formalise this problem we set up null and alternative hypotheses as
$\rho_0 = 0.5, \rho_1 = 0.7$ respectively, and define some operating
characteristics which we want to minimise. Specifically, we will use:

-   $\alpha$: the probability of proceeding to the main trial when
    $\rho = \rho_0$;
-   $\beta$: the probability of not proceeding to the main trial when
    $\rho = \rho_1$; and
-   $\gamma$: the probability of not obtaining a $pause$ decision when
    $\rho = (\rho_0 + \rho_1)/2$.

The TOut package allows us to specify maximum levels of these three
operating characteristics, and then finds a design which will satisfy
these constraints. When there are several such designs, it will suggest
the one which minimises the average of the three operating
characteristics.

``` r
library(TOut)

opt_pc(n = 100, rho_0 = 0.5, rho_1 = 0.7,
                     alpha_nom = 0.05, beta_nom = 0.2, gamma_nom = 0.5)
#> [1] 100  56  68
```

The function `opt_pc` returns a vector of the form $(n, x_0, x_1)$. In
this case we find that a pilot trial with $n = 200$ patients in the
intervention arm will satisfy our constraints
$\alpha < 0.05, \beta < 0.2, \gamma < 0.5$ if we $stop$ when we observe
109 patients or fewer adhering, $go$ if we observe more that 131
patients adhering, and $pause$ otherwise.

### Optimising sample size

TOut can also find the lowest sample size which can satisfy all
operating characteristic constraints:

``` r
opt_design(rho_0 = 0.5, rho_1 = 0.7, alpha_nom = 0.05, beta_nom = 0.1, gamma_nom = 0.5)
#> [1] 85 48 55
```

Here we find that our earlier choice of $n = 200$ was unnecessarily
large and can be reduced down to $n = 85$, with progression criteria
$x_0 = 48$ and $x_1 = 55$ .
