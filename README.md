
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TOut

<!-- badges: start -->
<!-- badges: end -->

The goal of TOut is to optimise the design of **T**hree-**Out**come
clinical trials by determining their sample size and progression
criteria.

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
like to pause and make our progression decision based on other
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

- $\alpha$: the probability of proceeding to the main trial when
  $\rho = \rho_0$;
- $\beta$: the probability of not proceeding to the main trial when
  $\rho = \rho_1$; and
- $\gamma$: the probability of not obtaining a $pause$ decision when
  $\rho = (\rho_0 + \rho_1)/2$.

The TOut package allows us to specify maximum levels of these three
operating characteristics, and then finds a design which will satisfy
these constraints. For example, if we fix $n = 100$ we can find the
values of $x_0$ and $x_1$ which minimise $\gamma$ whilst ensuring
$\alpha$ and $\beta$ are below their nominal levels:

``` r
library(TOut)

opt_pc_bin(n = 100, rho_0 = 0.5, rho_1 = 0.7,
                     alpha_nom = 0.05, beta_nom = 0.2)
#> Three-Outcome design (binary outcome)
#> 
#> Sample size: 100 
#> Decision thresholds: 56 68 
#> 
#> alpha = 0.04838276 
#> beta = 0.1845033 
#> gamma = 0.2763791 
#> 
#> Hypotheses: 0.5 (null), 0.7 (alternative)
#> Modification effect range: 0 0 
#> Error probability following an intermediate result: 0.5
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
#> Three-Outcome design (binary outcome)
#> 
#> Sample size: 83 
#> Decision thresholds: 47 54 
#> 
#> alpha = 0.04787477 
#> beta = 0.09992842 
#> gamma = 0.4472989 
#> 
#> Hypotheses: 0.5 (null), 0.7 (alternative)
#> Modification effect range: 0 0 
#> Error probability following an intermediate result: 0.5
```

Here we find that our earlier choice of $n = 100$ was unnecessarily
large and can be reduced down to $n = 83$, with progression criteria
$x_0 = 47$ and $x_1 = 54$.

## Continuous outcome

We can apply the same approach to the case of a continuous outcome. For
example, with $\rho$ now denoting the expectation of the continuous
outcome suppose that our hypotheses are $\rho_0 = 0, \rho_1 = 0.5$ and
our outcome has a standard deviation of $\sigma = 1$. The optimal sample
size and progression criteria are then:

``` r
TOut_design_cont(rho_0 = 0, rho_1 = 0.5, sigma = 1, alpha_nom = 0.05, beta_nom = 0.1)
#> [1]    0 1712
#> [1]   0 856
#> [1]   0 428
#> [1]   0 214
#> [1]   0 107
#> [1]  0 54
#> [1] 27 54
#> [1] 27 41
#> [1] 34 41
#> [1] 34 38
#> [1] 34 36
#> Three-Outcome design (continuous outcome)
#> 
#> Sample size: 35 
#> Decision thresholds: 1.515367 1.809841 
#> 
#> alpha = 0.05 
#> beta = 0.09999975 
#> gamma = 0.884887 
#> 
#> Hypotheses: 0 (null), 0.5 (alternative)
#> Satndard deviation: 1 
#> Modification effect range: 0 0 
#> Error probability following an intermediate result: 0.5
```

In the continuous case, the progression criteria $x_0$ and $x_1$ are
given on the scale of the z-statistic. If we want a higher probability
of getting an intermediate result, we can tighten our constraint on
$\gamma$ and see how much more sample size this will cost:

``` r
TOut_design_cont(rho_0 = 0, rho_1 = 0.5, sigma = 1, alpha_nom = 0.05, beta_nom = 0.1, gamma_nom = 0.5)
#> [1]    0 1712
#> [1]   0 856
#> [1]   0 428
#> [1]   0 214
#> [1]   0 107
#> [1]  0 54
#> [1] 27 54
#> [1] 41 54
#> [1] 48 54
#> [1] 48 51
#> Three-Outcome design (continuous outcome)
#> 
#> Sample size: 51 
#> Decision thresholds: 1.302388 2.686702 
#> 
#> alpha = 0.05 
#> beta = 0.09999972 
#> gamma = 0.4982613 
#> 
#> Hypotheses: 0 (null), 0.5 (alternative)
#> Satndard deviation: 1 
#> Modification effect range: 0 0 
#> Error probability following an intermediate result: 0.5
```

## Making adjustments

A common rationale for including an intermediate decision between $stop$
and $go$ is to allow for adjustments to be made to the intervention
and/or the trial design in an effort to improve the parameter of
interest by some amount $\tau$. we can use our framework here by taking
an intermediate result as leading to a choice between $stop$ and
$amend then go$. We typically won’t know what $\tau$ is at the design
stage, but might be able to specify an interval
$\tau \in [\tau_{min}, \tau_{max}]$ which we think is plausible and over
which we’d like to control error rates. We will only consider cases
where $\tau_{min} \geq 0$.

In this more general case, the operating characteristic $\alpha$ is the
maximum probability of proceeding to the main trial, either directly of
following an $amend then go$ outcome, when the (possibly amended)
parameter value is less than or equal to $\rho_0$. Formally,

$$
\alpha = \max \left[ \max_{\rho \leq \rho_0} Pr(\hat{\rho} > x_1), \max_{\rho + \tau \leq \rho_0} \eta Pr(x_0 < \hat{\rho} \leq x_1) + Pr(x_1 < \hat{\rho}) \right].
$$ The first term is clearly maximised at $\rho = \rho_0$. The second
can be written as

$$
\eta Pr(\hat{\rho} \leq x_1) - \eta Pr(\hat{\rho} \leq x_0) + 1 - Pr(\hat{\rho} \leq x_0) = 1 + (\eta - 1)Pr(\hat{\rho} \leq x_1) - \eta Pr(\hat{\rho} \leq x_0),
$$ and so is maximised at $\rho = \rho_0 - \tau_{min}$.

To control this OC at a nominal level $\alpha^*$, we first take $x_1$ as
fixed and such that
$Pr(\hat{\rho} > x_1 ~|~ \rho = \rho_0) \leq \alpha^*$. Then We set the
second term equal to $\alpha^*$ and rearrange to get

$$
Pr(\hat{\rho} \leq x_0) = \frac{1}{\eta} + \frac{\eta - 1}{\eta}Pr(\hat{\rho} \leq x_1) - \frac{\alpha^*}{\eta}.
$$ Using the inverse of $\hat{\rho}$’s distribution function, we can
then find the $\x_0$ which gives us $\alpha = \alpha^*$ (or for the
binary case, the $x_0$ which maximises $\alpha$ whilst respecting the
constraint).

Recall this choice of $x_0$ was conditional on a given $x_1$. To choose
$x_1$, we search over candidate values and choose the largest value such
that the corresponding $x_0 \leq x_1$, and $\beta \leq \beta^*$ (since
larger $x_1$ means a larger intermediate zone and therefore a lower
$\gamma$). The OC $\beta$ is

$$
\beta = \max \left[ \max_{\rho > \rho_1} Pr(\hat{\rho} \leq x_0), \max_{\rho + \tau > \rho_1} Pr(\hat{\rho} \leq x_0) + \eta Pr(x_0 < \hat{\rho} \leq x_1) \right].
$$ The first term is maximised at $\rho = \rho_1$. The second term can
be written as

$$
\eta Pr(\hat{\rho} \leq x_1) + (1 - \eta) Pr(\hat{\rho} \leq x_0),
$$ which is maximised at $\rho = \rho_1 - \tau_{max}$. Since
$\tau_{max} > 0$, the second term will never be less than the first and
so things simplify to

$$
\beta = Pr(\hat{\rho} \leq x_0 ~|~ \rho = \rho_1 - \tau_{max}) + \eta Pr(x_0 < \hat{\rho} \leq x_1 ~|~ \rho = \rho_1 - \tau_{max}).
$$

Note that the above examples are just special cases where we set
$\tau_{min} = \tau_{max} = 0$.

``` r
# No adjustment possible
opt_pc_cont(n=110, rho_0 = 0, rho_1 = 0.3, sigma = 1, alpha_nom = 0.05, beta_nom = 0.2,  tau = c(0,0))
#> Three-Outcome design (continuous outcome)
#> 
#> Sample size: 110 
#> Decision thresholds: 1.295845 2.808898 
#> 
#> alpha = 0.05 
#> beta = 0.1999871 
#> gamma = 0.4990365 
#> 
#> Hypotheses: 0 (null), 0.3 (alternative)
#> Satndard deviation: 1 
#> Modification effect range: 0 0 
#> Error probability following an intermediate result: 0.5

# Adjustment known exactly:
opt_pc_cont(n=110, rho_0 = 0, rho_1 = 0.3, sigma = 1, alpha_nom = 0.05, beta_nom = 0.2,  tau = c(0.1, 0.1))
#> Three-Outcome design (continuous outcome)
#> 
#> Sample size: 110 
#> Decision thresholds: 0.2470354 1.760109 
#> 
#> alpha = 0.05 
#> beta = 0.1999908 
#> gamma = 0.4990326 
#> 
#> Hypotheses: 0 (null), 0.3 (alternative)
#> Satndard deviation: 1 
#> Modification effect range: 0.1 0.1 
#> Error probability following an intermediate result: 0.5

# Only interval of adjustment known:
opt_pc_cont(n=128, rho_0 = 0, rho_1 = 0.3, sigma = 1, alpha_nom = 0.05, beta_nom = 0.2,  tau = c(0.08, 0.12))
#> Three-Outcome design (continuous outcome)
#> 
#> Sample size: 128 
#> Decision thresholds: 0.4076158 1.646594 
#> 
#> alpha = 0.05 
#> beta = 0.1999936 
#> gamma = 0.5770699 
#> 
#> Hypotheses: 0 (null), 0.3 (alternative)
#> Satndard deviation: 1 
#> Modification effect range: 0.08 0.12 
#> Error probability following an intermediate result: 0.5
```

Note that the operating characteristics are the same when the adjustment
effect is 0, and when it is known - just the decision thresholds change.
Also note that we need to increase the sample size in the third case to
ensure $\alpha$ and $\beta$ are controlled when the adjustment effect is
not known.

Similarly, we can do all this for a binary outcome:

``` r
# No adjustment possible
opt_pc_bin(n=110, rho_0 = 0.5, rho_1 = 0.7, alpha_nom = 0.05, beta_nom = 0.2,  tau = c(0,0))
#> Three-Outcome design (binary outcome)
#> 
#> Sample size: 110 
#> Decision thresholds: 62 75 
#> 
#> alpha = 0.03810368 
#> beta = 0.1872513 
#> gamma = 0.2776724 
#> 
#> Hypotheses: 0.5 (null), 0.7 (alternative)
#> Modification effect range: 0 0 
#> Error probability following an intermediate result: 0.5

# Adjustment known exactly:
opt_pc_bin(n=110, rho_0 = 0.5, rho_1 = 0.7, alpha_nom = 0.05, beta_nom = 0.2,  tau = c(0.1, 0.1))
#> Three-Outcome design (binary outcome)
#> 
#> Sample size: 110 
#> Decision thresholds: 51 64 
#> 
#> alpha = 0.03652218 
#> beta = 0.1927931 
#> gamma = 0.287125 
#> 
#> Hypotheses: 0.5 (null), 0.7 (alternative)
#> Modification effect range: 0.1 0.1 
#> Error probability following an intermediate result: 0.5

# Only interval of adjustment known:
opt_pc_bin(n=145, rho_0 = 0.5, rho_1 = 0.7, alpha_nom = 0.05, beta_nom = 0.2,  tau = c(0.08, 0.12))
#> Three-Outcome design (binary outcome)
#> 
#> Sample size: 145 
#> Decision thresholds: 69 82 
#> 
#> alpha = 0.04819701 
#> beta = 0.199825 
#> gamma = 0.3574197 
#> 
#> Hypotheses: 0.5 (null), 0.7 (alternative)
#> Modification effect range: 0.08 0.12 
#> Error probability following an intermediate result: 0.5
```

We can find optimal sample sizes as before, but now handing the function
an interval for the adjustment effect $\tau$:

``` r
TOut_design_bin(rho_0 = 0.5, rho_1 = 0.7, alpha_nom = 0.05, beta_nom = 0.2, tau = c(0.05, 0.15))
#> No valid design found. Consider increasing the maximum sample size (max_n).

TOut_design_cont(rho_0 = 0, rho_1 = 0.3, sigma = 1, alpha_nom = 0.05, beta_nom = 0.2, tau = c(0.02, 0.17))
#> [1]    0 3434
#> [1]    0 1717
#> [1]   0 859
#> [1]   0 430
#> [1] 215 430
#> [1] 215 323
#> [1] 269 323
#> [1] 269 296
#> [1] 283 296
#> [1] 290 296
#> [1] 293 296
#> Three-Outcome design (continuous outcome)
#> 
#> Sample size: 296 
#> Decision thresholds: 1.083884 1.644904 
#> 
#> alpha = 0.05 
#> beta = 0.2007691 
#> gamma = 0.7971188 
#> 
#> Hypotheses: 0 (null), 0.3 (alternative)
#> Satndard deviation: 1 
#> Modification effect range: 0.02 0.17 
#> Error probability following an intermediate result: 0.5
```
