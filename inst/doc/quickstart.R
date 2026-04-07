## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4
)

## ----load---------------------------------------------------------------------
library(assemblykor)

## ----overview-----------------------------------------------------------------
# All datasets load lazily - just call their name
head(legislators, 3)
head(bills, 3)
head(wealth, 3)

## ----gender, message = FALSE--------------------------------------------------
library(dplyr)

legislators %>%
  filter(assembly == 22) %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    median_bills_led = median(n_bills_lead, na.rm = TRUE)
  )

## ----bills-survival-----------------------------------------------------------
bills %>%
  count(result, sort = TRUE) %>%
  head(5)

## ----wealth-ineq--------------------------------------------------------------
# Net worth in billion KRW
hist(wealth$net_worth / 1e6, breaks = 50, col = "steelblue",
     main = "Legislator Net Worth", xlab = "Billion KRW")

## ----votes-consensus----------------------------------------------------------
votes$yes_rate <- votes$yes / votes$voted
hist(votes$yes_rate, breaks = 40, col = "coral",
     main = "Distribution of Yes-Vote Share", xlab = "Proportion yes")

## ----join, message = FALSE----------------------------------------------------
# Do wealthier legislators propose more bills?
leg_wealth <- legislators %>%
  filter(assembly == 22) %>%
  inner_join(wealth %>% filter(year == 2024),
             by = "member_id")

cor(leg_wealth$n_bills_lead, leg_wealth$net_worth,
    use = "complete.obs")

## ----tutorials, eval = FALSE--------------------------------------------------
#  list_tutorials()           # See all 9
#  run_tutorial(1)            # Interactive in browser
#  open_tutorial(1)           # Copy Rmd to your directory

