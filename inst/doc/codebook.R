## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(assemblykor)

# Helper: summarise a single variable
var_summary <- function(x, varname) {
  n_total <- length(x)
  n_miss  <- sum(is.na(x))
  pct_miss <- sprintf("%.1f%%", 100 * n_miss / n_total)

  if (is.logical(x)) {
    type_str <- "logical"
    vals <- paste0("TRUE: ", sum(x, na.rm = TRUE),
                   ", FALSE: ", sum(!x, na.rm = TRUE))
  } else if (is.numeric(x)) {
    type_str <- "numeric"
    q <- quantile(x, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    vals <- sprintf("min=%.0f, Q1=%.0f, median=%.0f, Q3=%.0f, max=%.0f",
                    q[1], q[2], q[3], q[4], q[5])
  } else if (inherits(x, "Date")) {
    type_str <- "Date"
    vals <- paste(range(x, na.rm = TRUE), collapse = " to ")
  } else {
    type_str <- "character"
    u <- length(unique(x[!is.na(x)]))
    top <- names(sort(table(x), decreasing = TRUE))[1:min(3, u)]
    vals <- paste0(u, " unique; top: ", paste(top, collapse = ", "))
  }

  data.frame(
    Variable = varname,
    Type = type_str,
    Missing = pct_miss,
    Distribution = vals,
    stringsAsFactors = FALSE
  )
}

# Helper: build a summary table for a dataset
codebook_table <- function(df) {
  rows <- lapply(names(df), function(v) var_summary(df[[v]], v))
  do.call(rbind, rows)
}

## ----legislators-codebook, echo = FALSE---------------------------------------
data(legislators)
knitr::kable(codebook_table(legislators), row.names = FALSE)

## ----bills-codebook, echo = FALSE---------------------------------------------
data(bills)
knitr::kable(codebook_table(bills), row.names = FALSE)

## ----wealth-codebook, echo = FALSE--------------------------------------------
data(wealth)
knitr::kable(codebook_table(wealth), row.names = FALSE)

## ----seminars-codebook, echo = FALSE------------------------------------------
data(seminars)
knitr::kable(codebook_table(seminars), row.names = FALSE)

## ----speeches-codebook, echo = FALSE------------------------------------------
data(speeches)
knitr::kable(codebook_table(speeches), row.names = FALSE)

## ----votes-codebook, echo = FALSE---------------------------------------------
data(votes)
knitr::kable(codebook_table(votes), row.names = FALSE)

## ----roll-calls-codebook, echo = FALSE----------------------------------------
data(roll_calls)
knitr::kable(codebook_table(roll_calls), row.names = FALSE)

