# RESQUER
An R package for analyzing the [RESQUE Research Quality Evaluation](https://nicebread.github.io/RESQUE) schemes.

## How to preview the dashboard:

Install the development version from GitHub with:

```
install.packages("remotes")
remotes::install_github("nicebread/RESQUER")
```

Launch the dashboard with the three included demo profiles:

```
library(quarto)
quarto_serve(system.file("dashboard/dashboard_shiny.qmd", package="RESQUER"))
```

