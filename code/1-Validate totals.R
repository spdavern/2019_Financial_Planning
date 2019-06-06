# 1-Validate totals

#Load Library
library("magrittr")  # Provides piping functionality

# Validate totals = paypal + offering
bad.totals <- df %>% 
  select(-monthly.giving.families) %>%
  mutate(calcd.total=paypal + offering) %>%
  mutate(diff=total - calcd.total) %>%
  filter(!is.na(calcd.total) & diff != 0)
#Test and report the result.
if (nrow(bad.totals) != 0) {
  message("***** WARNING *****")
  message(
    "Some 'total' observations don't equal the sum of 'paypal' and 'offering'!"
  )
  print(bad.totals)
} else {
  message("All weekly totals equal the sum of 'paypal' and 'offering'.")
}