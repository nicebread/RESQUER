# ====== Custom operators ======

# %and%
# A custom operator that evaluates to TRUE if both of its arguments are TRUE,
# NA and NULL are treated as FALSE
# Not vectorized, only works with logicals
"%and%" <- function(x, y) {
  logical_x <- !is.na(x) && !is.null(x) && is.logical(x) && x
  logical_y <- !is.na(y) && !is.null(y) && is.logical(y) && y

  logical_x && logical_y
}


# %or%
# A custom operator that evaluates to TRUE
# if at least one of its arguments is TRUE,
# NA and NULL are treated as FALSE
# Not vectorized, only works with logicals
"%or%" <- function(x, y) {
  logical_x <- !is.na(x) && !is.null(x) && is.logical(x)  && x
  logical_y <- !is.na(y) && !is.null(y) && is.logical(y) && y

  logical_x || logical_y
}


# exists()
# A custom operator that evaluates to TRUE if the variable exists in the context
exists <- function(variable, context) {
  variable %in% names(context) && if (is.character(context[[variable]])) {
    context[[variable]] != ""
  } else {
    TRUE
  }
}

not <- function(variable, context) {
  if (exists(variable, context)) {
    value <- context[[variable]]
    return(is.na(value) || is.null(value) || (is.logical(value) && length(value) == 1 &&  !value))
  }

  TRUE
}
