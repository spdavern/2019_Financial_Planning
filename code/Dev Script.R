# Install packages that enable reading an Excel file if not already installed.
required.packages <- c("Rcpp", "tidyverse", "readxl", "dplyr")
for(pkg in required.packages) {
  if (pkg %in% installed.packages()[, 1] == FALSE)
    install.packages(pkg)  # Install packages if not already
  library(pkg, character.only = TRUE) # Load packages into memory
}

# Extract the giving data from the Excel file:
# Set the working directory
setwd('~/CloudStation/Seed Work/2019 Financial Planning')
# Read data from the file.
giving.data <- read_excel("./Giving Data.xlsx")
# Create a copy of the original data to manipulate.
df <- select(
  .data = giving.data,
  week.ending = 'Week Ending',
  month = Month,
  year = Year,
  paypal = Paypal,
  offering = Offering,
  total = Total,
  monthly.giving.families = 'Monthly # families'
)

# Validate totals = paypal + offering
library("magrittr")  # Provides piping functionality
bad.totals <- df %>% select(-monthly.giving.families) %>%
  mutate(calcd.total=paypal + offering) %>%
  mutate(bad.total=!(calcd.total == total)) %>%
  filter(!is.na(calcd.total)) %>%
  filter(bad.total) %>%
  select(-bad.total)
if (nrow(bad.totals) != 0) {
  message("***** WARNING *****")
  message(
    "Some 'total' observations don't equal the sum of 'paypal' and 'offering'!"
  )
  print(bad.totals)
  cat("\n")
} else {
  message("All weekly totals equal the sum of 'paypal' and 'offering'.")
}

#  ***** Need to do something about these week totals.



# Data transformation: Calculate monthly giving totals.
# Make Month a categorical variable with levels in the order that
# months occur in the year otherwise months are sorted alphabetically.
df$month <- factor(df$month, month.name)
# Aggregate the monthly Totals from giving.data in sums for each month.
MonthTotals <- 
  aggregate(df$total, by = list(df$month, df$year), FUN = sum)
# Exclude the months that don't have totals yet.
MonthTotals <- MonthTotals[complete.cases(MonthTotals), ]
# Extract only rows containing 'monthly.giving.families' data.
df <- df[!is.na(df$monthly.giving.families),]
# Now replace Totals (which were weekly totals) with calculated aggregates
df$total <- MonthTotals$x

# Define a function that returns the number of Sundays in a month.
# Source: https://stackoverflow.com/questions/19913244/count-the-number-of-fridays-or-mondays-in-month-in-r
NumOfGivenDayOfWeekInMonth <-
  function(date.within.month.of.interest,
           day.of.the.week) {
    if (missing(day.of.the.week)) {
      numerical.day.of.week <- 0  # Sunday by Default
    } else {
      list.of.days.of.week <- c("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT")
      numerical.day.of.week <-
        match(toupper(substr(day.of.the.week, 1, 3)),
              list.of.days.of.week, nomatch = 1) - 1
    }
    first <- as.Date(cut(date.within.month.of.interest, "month"))
    last <- as.Date(cut(first + 31, "month")) - 1
    # sum(format(seq(first,last,"day"), "%w")==numerical.day.of.week)
    # Using this 2nd alternative vectorizes this function.
    ceiling(as.numeric(last + 1 - numerical.day.of.week + 4) / 7) -
      ceiling(as.numeric(first - numerical.day.of.week + 4) / 7)
  }
# Calculate and add the columns SundaysInMonth with calculated values
# and MonthsGivingPerWeek
df <-  df %>%
  mutate(SundaysInMonth =
           NumOfGivenDayOfWeekInMonth(df$week.ending, "Sunday")) %>%
  mutate(MonthsGivingPerWeek = total / SundaysInMonth)

# Make year a categorical variable so coefficients are easier to interpret.
df$year <- as.factor(df$year)

write.csv(x = df,
          file = "Cleaned and Transformed Giving Data.csv",
          row.names = FALSE)

# Regress the giving model.
mod <- lm(
  formula = df$total ~
    df$year + df$month + df$monthly.giving.families,
  data = df
)

# Model results: Coefficients
mod$coefficients

# Assessing the model fit:
summary(mod)  
anova(mod)

# Assessing the regression assumptions:
par(mfrow=c(2,2)) # Makes the plots window show a grid of 2x2 plots.
plot(mod)
par(mfrow=c(1,1)) # Return plot window to show 1 plot per page.

stop()
# Analysis above suggests point 84 is an outlier so I'll extract it.
excluded.points <- df[84,]
# Exclude it from df:
df <- df[-c(84),]
# The re-regress the model:
mod <-
  lm(
    formula = df$total ~ df$year + df$month + df$Monthly.giving.families,
    data = df
  )
# Model results: Coefficients
mod$coefficients

# Assessing the model fit:
summary(mod)  
anova(mod)

# Assessing the regression assumptions:
par(mfrow=c(2,2)) # Makes the plots window show a grid of 2x2 plots.
plot(mod)
par(mfrow=c(1,1)) # Return plot window to show 1 plot per page.

