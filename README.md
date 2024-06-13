# RESQUER

An R package for analyzing the [RESQUE Research Quality Evaluation](https://resque-framework.github.io/website/) schemes.
This is work in progress, and likely to change.

## How to preview the dashboard:

Install the development versions from GitHub with:

```
install.packages("remotes")
remotes::install_github("nicebread/OAmetrics", auth_token=NULL)
remotes::install_github("RESQUE-Framework/RESQUER")
```

For the interactive dashboard, you need [Quarto](https://quarto.org/docs/get-started/) installed on your system.

Launch the dashboard with the three included demo profiles:

```
library(quarto)
library(RESQUER)
quarto_serve(system.file("dashboard/dashboard_shiny.qmd", package="RESQUER"))
```


