# NumOfGivenDayOfWeekInMonth function
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