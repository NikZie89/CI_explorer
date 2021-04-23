# CI_explorer
A small shiny app that draws random samples from a simulated population in order to compare estimated regression coefficients and CIs to the true population parameter.
I've used it for explaining my bacheor thesis students more about confidence intervals and sample statistics.

The app can display the underlying (standardized regression coefficient) for a y ~ x based on a simulated normaly distributed population of 100,000 member.
Consequently, the user can draw up to 100 random samples from this population and dislay how the regression coefficients of the sample compare to that of the whole population.
The sample size and the type 1 error for each sample can be adjusted confidence interval do demonstrate students how this effects the widths of the Confidence interval.

The app visualized this process in ggplot2.

See: https://nziegler.shinyapps.io/CI_explorer/
