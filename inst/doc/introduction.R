## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4
)

## ----load---------------------------------------------------------------------
library(assemblykor)

## ----legislators--------------------------------------------------------------
data(legislators)
str(legislators)

## ----gender-table-------------------------------------------------------------
gender_tab <- table(legislators$assembly, legislators$gender)
gender_tab
prop.table(gender_tab, margin = 1)

## ----seniority-plot-----------------------------------------------------------
boxplot(n_bills_lead ~ seniority, data = legislators,
        xlab = "Terms served", ylab = "Bills proposed (as lead)",
        main = "Seniority and Legislative Productivity",
        col = "lightblue")

## ----bills--------------------------------------------------------------------
data(bills)

# Top 5 outcomes
outcome_counts <- sort(table(bills$result), decreasing = TRUE)
barplot(outcome_counts[1:5], las = 2, col = "steelblue",
        main = "Most Common Bill Outcomes")

## ----bills-timeline-----------------------------------------------------------
bills$month <- format(bills$propose_date, "%Y-%m")
monthly <- aggregate(bill_id ~ month, data = bills, FUN = length)
names(monthly) <- c("month", "count")
monthly <- monthly[order(monthly$month), ]

plot(seq_len(nrow(monthly)), monthly$count, type = "l",
     xlab = "Month (index)", ylab = "Bills proposed",
     main = "Monthly Bill Proposals (20th-22nd Assembly)")

## ----wealth-------------------------------------------------------------------
data(wealth)

# Distribution of net worth
hist(wealth$net_worth / 1e6, breaks = 50, col = "coral",
     main = "Legislator Net Worth Distribution",
     xlab = "Net Worth (billion KRW)")

## ----re-share-----------------------------------------------------------------
wealth$re_share <- ifelse(wealth$total_assets > 0,
                          wealth$real_estate / wealth$total_assets, NA)

boxplot(re_share ~ year, data = wealth,
        xlab = "Year", ylab = "Real estate / total assets",
        main = "Real Estate as Share of Legislator Wealth",
        col = "lightyellow")

## ----seminars-----------------------------------------------------------------
data(seminars)

# Governing vs opposition party
gov_means <- tapply(seminars$cross_party_ratio,
                    seminars$is_governing, mean, na.rm = TRUE)
barplot(gov_means, names.arg = c("Opposition", "Governing"),
        ylab = "Cross-party ratio", col = c("dodgerblue", "tomato"),
        main = "Cross-Party Seminar Collaboration")

## ----join, message=FALSE------------------------------------------------------
library(dplyr)

# Merge legislators with wealth
leg_wealth <- legislators %>%
  inner_join(wealth, by = "member_id", relationship = "many-to-many")

# Productivity vs wealth
leg_wealth %>%
  group_by(district_type) %>%
  summarise(
    n = n(),
    median_net_worth = median(net_worth / 1e6, na.rm = TRUE),
    median_bills = median(n_bills_lead, na.rm = TRUE)
  )

## ----votes--------------------------------------------------------------------
data(votes)

# Yes-vote share distribution
votes$yes_rate <- votes$yes / votes$voted
hist(votes$yes_rate, breaks = 40, col = "lightgreen",
     main = "Distribution of Yes-Vote Share",
     xlab = "Proportion yes")

## ----roll-calls, message = FALSE----------------------------------------------
data(roll_calls)
library(dplyr)

# Party discipline: how often do members vote with their party majority?
party_votes <- roll_calls %>%
  group_by(bill_id, party) %>%
  mutate(party_majority = names(which.max(table(vote)))) %>%
  ungroup() %>%
  mutate(with_party = vote == party_majority)

party_votes %>%
  group_by(party) %>%
  summarise(
    n_members = n_distinct(member_id),
    discipline = mean(with_party, na.rm = TRUE)
  ) %>%
  filter(n_members >= 5) %>%
  arrange(desc(discipline))

## ----speeches-----------------------------------------------------------------
data(speeches)

# Who speaks most in committee?
leg_speeches <- speeches[speeches$role == "legislator", ]
speaker_counts <- sort(table(leg_speeches$speaker_name), decreasing = TRUE)
barplot(speaker_counts[1:10], las = 2, col = "plum",
        main = "Top 10 Most Active Speakers (Sci & ICT Committee)")

