# 0-Extract data from Excel.R

library("readxl")
# Extract the giving data from the Excel file that the original data was provide in:
giving.data <- read_excel("../data/Giving Data.xlsx")

library("dplyr", quietly = TRUE)
# Extract a copy of selected columns of the original data to manipulate.
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