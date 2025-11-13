# Find optimal sample size and progression criteria

Given a null and alternative hypothesis, this function finds the lowest
sample size such that a design with optimal progression criteria (as
determined by the function `opt_pc`) satisfies upper constraints on
three operating characteristics.

## Usage

``` r
tout_design(
  rho_0,
  rho_1,
  alpha_nom,
  beta_nom,
  gamma_nom = 1,
  eta_0 = 0.5,
  eta_1 = eta_0,
  tau = c(0, 0),
  max_n = NULL,
  n = NULL,
  x = NULL,
  sigma = NULL
)
```

## Arguments

- rho_0:

  null hypothesis.

- rho_1:

  alternative hypothesis.

- alpha_nom:

  nominal upper constraint on alpha.

- beta_nom:

  nominal upper constraint on beta.

- gamma_nom:

  nominal upper constraint on gamma. Defaults to 1.

- eta_0:

  probability of an incorrect decision under the null hypothesis after
  an intermediate result. Defaults to 0.5.

- eta_1:

  probability of an incorrect decision under the alternative hypothesis
  after an intermediate result. Defaults to eta_0.

- tau:

  two element vector denoting lower and upper limits of the effect of
  adjustment.

- max_n:

  optional upper limit to use in search over sample sizes.

- n:

  optional sample size (optimised if left unspecified).

- x:

  optional vector of decision thresholds (optimised if left
  unspecified).

- sigma:

  standard deviation of outcome. If left unspecified, a binary outcome
  is assumed.

## Value

An object of class `tout`, which is a list containing the following
components:

- `valid`:

  boolean indicating if the nominal constraints are met.

- `n`:

  sample size.

- `thesholds`:

  numeric vector of the two decision thresholds.

- `alpha`:

  attained value of operating characteristic alpha.

- `beta`:

  attained value of operating characteristic beta.

- `gamma`:

  attained value of operating characteristic gamma.

## Examples

``` r
rho_0 <- 0.5
rho_1 <- 0.7
alpha_nom <- 0.05
beta_nom <- 0.2

tout_design(rho_0, rho_1, alpha_nom, beta_nom)
#> Three-outcome design
#> 
#> Sample size: 37 
#> Decision thresholds: 23 23 
#> 
#> alpha = 0.04943587 
#> beta = 0.1929043 
#> gamma = 1 
#> 
#> Hypotheses: 0.5 (null), 0.7 (alternative)
#> Modification effect range: 0 0 
#> Error probability following an intermediate result: 0.5 0.5 

# Allowing for adjustment effects:

tout_design(rho_0, rho_1, alpha_nom, beta_nom, tau = c(0.08, 0.12))
#> Three-outcome design
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
#> Error probability following an intermediate result: 0.5 0.5 

# Allowing for different error probabilities following a pause decision

tout_design(rho_0, rho_1, alpha_nom, beta_nom, eta_0 = 0.3)
#> Three-outcome design
#> 
#> Sample size: 32 
#> Decision thresholds: 19 21 
#> 
#> alpha = 0.04983493 
#> beta = 0.1995952 
#> gamma = 0.7427826 
#> 
#> Hypotheses: 0.5 (null), 0.7 (alternative)
#> Modification effect range: 0 0 
#> Error probability following an intermediate result: 0.3 0.3 

# Designs for continuous outcomes:

tout_design(rho_0 = 0, rho_1 = 0.4, alpha_nom, beta_nom, sigma = 1)
#> Three-outcome design
#> 
#> Sample size: 39 
#> Decision thresholds: 1.55615 1.748788 
#> 
#> alpha = 0.05 
#> beta = 0.2 
#> gamma = 0.9292477 
#> 
#> Hypotheses: 0 (null), 0.4 (alternative)
#> Standard deviation: 1 
#> Modification effect range: 0 0 
#> Error probability following an intermediate result: 0.5 0.5 
```
