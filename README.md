
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

## Binary outcome

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

opt_pc_bin(n = 100, rho_0 = 0.5, rho_1 = 0.7,
                     alpha_nom = 0.05, beta_nom = 0.2, gamma_nom = 0.5)
#> [1] 100.00000000  56.00000000  68.00000000   0.04838276   0.18450332
#> [6]   0.27637906
```

The function `opt_pc` returns a vector of the form
$(n, x_0, x_1, \alpha, \beta, \gamma)$. In this case we find that a
pilot trial with $n = 100$ patients in the intervention arm will satisfy
our constraints $\alpha < 0.05, \beta < 0.2, \gamma < 0.5$ if we $stop$
when we observe 56 patients or fewer adhering, $go$ if we observe more
that 68 patients adhering, and $pause$ otherwise.

### Optimising sample size

TOut can also find the lowest sample size which can satisfy all
operating characteristic constraints:

``` r
TOut_design_bin(rho_0 = 0.5, rho_1 = 0.7, alpha_nom = 0.05, beta_nom = 0.1, gamma_nom = 0.5)
#> [1] 83.00000000 47.00000000 54.00000000  0.04787477  0.09992842  0.44729885
```

Here we find that our earlier choice of $n = 100$ was unnecessarily
large and can be reduced down to $n = 83$, with progression criteria
$x_0 = 47$ and $x_1 = 54$.

## Continuous outcome

We can apply the same approach to the case of a continuous outcome. For
example, suppose that our hypotheses are $\rho_0 = 0, \rho_1 = 0.3$ and
our outcome has a standard deviation of $\sigma = 1$. The optimal sample
size and progression criteria are then:

``` r
TOut_design_cont(rho_0 = 0, rho_1 = 0.3, sigma = 1, alpha_nom = 0.05, beta_nom = 0.1, gamma_nom = 0.5)
#> [1] 142.00000000   1.30209343   2.69138749   0.05000000   0.09999743
#> [6]   0.49672543
```

In the continuous case, the progression criteria $x_0$ and $x_1$ are
given on the scale of the z-statistic.

## Making adjustments

A common rationale for including an intermediate decision between $stop$
and $go$ is to allow for adjustments to be made to the intervention
and/or the trial design in an effort to improve the parameter of
interest by some amount $\tau$. This leads to a different framework,
where we will now proceed to the main trial whenever $\hat{\rho} > x_0$
but will do so after making adjustments if $x_0 < \hat{\rho} \leq x_1$.

In this scenario, a type I error will occur when
$x_1 < \hat{\rho} | \rho \leq \rho_0$ (since we will proceed directly
and run a trial under $\rho = \rho_0$), or when
$x_0 < \hat{rho} | \rho \leq \rho_0 - \tau$ (since we will either adjust
but from such a poor starting point that the adjusted rate is still less
than $rho_0$, or we will do even worse and proceed without adjustment).
Since the conditions are mutually exclusive, controlling the type I
error rate means controlling the probability of these two events. Since
the probability of the former will be maximised at $\rho = \rho_0$ we
can easily determine the minimum $x_1$ s.t. $\alpha_1 = \alpha^*$.
Similarly, the probability of the latter will be maximised at
$\rho = \rho_0 - \tau_{min}$ (assuming we have a specified interval for
the adjustment effect, $\tau \in [\tau_{min}, \tau_{max}]$) and so we
can independently determine the minimum $x_0$ s.t.
$\alpha_2 = \alpha^*$. These choices of $x_0, x_1$ will then minimise
the type II error rate, which is now the probability
$\hat{\rho} leq x_0 | \rho \geq \rho_1 - \tau$ (since this means
stopping despite the parameter being capable of reaching $\rho_1$ if the
adjustment is made). We can then find the minimum $n$ such that
$\beta \leq \beta^*$.

Note that this all supports our previous thought that the upper
threshold $x_1$ can be specified in a straightforward way without
worrying about adjustment effects, but that $x_0$ is sensitive to the
true value of $\tau$ and therefore can feel a little arbitrary.

``` r
# No adjustment possible
opt_pc_adjust_cont(n=100, rho_0 = 0, rho_1 = 0.3, sigma = 1, alpha_nom = 0.05, beta_nom = 0.2,  tau_min=0, tau_max=0)
#> [1] 100.00000000   1.64485363   1.64485363   0.05000000   0.08768546

# Adjustment known exactly:
opt_pc_adjust_cont(n=100, rho_0 = 0, rho_1 = 0.3, sigma = 1, alpha_nom = 0.05, beta_nom = 0.2,  tau_min=0.1, tau_max=0.1)
#> [1] 100.00000000   0.64485363   1.64485363   0.05000000   0.08768546

# Only interval of adjustment known:
opt_pc_adjust_cont(n=100, rho_0 = 0, rho_1 = 0.3, sigma = 1, alpha_nom = 0.05, beta_nom = 0.2,  tau_min=0.08, tau_max=0.12)
#> [1] 100.0000000   0.8448536   1.6448536   0.0500000   0.1697518
```

And similarly we can do this for a binary outcome:

``` r
opt_pc_adjust_bin(n=100, rho_0 = 0.3, rho_1 = 0.5,alpha_nom = 0.05, beta_nom = 0.2,  tau_min=0.08, tau_max=0.12)
#> [1] 100.00000000  29.00000000  38.00000000   0.05000000   0.03816388
```

We can find optimal sample sizes as before, but now handing the function
an interval for the adjustment effect $\tau$:

``` r
TOut_design_bin(rho_0 = 0.5, rho_1 = 0.7, alpha_nom = 0.05, beta_nom = 0.1, tau = c(0.05, 0.15))
#> [1] 220.00000000 111.00000000 122.00000000   0.05000000   0.09917233

TOut_design_cont(rho_0 = 0, rho_1 = 0.3, sigma = 1, alpha_nom = 0.05, beta_nom = 0.1, tau = c(0.07, 0.1))
#> [1] 118.00000000   0.88445899   1.64485363   0.05000000   0.09885608
```
