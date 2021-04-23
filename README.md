# CI_explorer
A small shiny app that draws random samples from a simulated population in order to compare estimated regression coefficients and CIs to the true population parameter.
I've used it for explaining my bacheor thesis students more about confidence intervals and sample statistics.

The app can display the underlying (standardized regression coefficient) for a simple linear regression of two variables (y ~ x) based on a simulated normally distributed population with 100,000 members.
The user can draw up to 100 random samples from this population and dislay how the regression coefficients of each sample compare to that of the whole population.
The sample size and the type 1 error for each sample can be adjusted to demonstrate students how this effects the widths of the Confidence interval.

The app visualized this process in ggplot2.

See: https://nziegler.shinyapps.io/CI_explorer/

A small explanation for my students:
https://eur.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=f421b282-70f6-4f8f-929a-ab76010620e8
