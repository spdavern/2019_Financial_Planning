# Extract the giving data from the Excel file that the original data was provide in:
library("readxl")
giving.data <- read_excel("./data/Giving Data.xlsx")

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
