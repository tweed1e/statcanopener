

stc_time <- function(date) {
  strftime(date, "%Y-%m-%dT%H:%M", tz = "America/Toronto")
}

# add fumctioms that check product id, vector id, etc. arguments
# and return errors if they're wrong
