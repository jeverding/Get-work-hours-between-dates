# setup ==================================================================================================== # 
library(dplyr)
library(lubridate)


# functions ================================================================================================ # 
# Create function which calculates number of full working hours between two time stamps (note: this function 
# is flexible concerning the regular daily working hours but assigns these as default values. If working 
# hours are always 9-17h, then one might hard code this instead, but I would advise against doing so) 
full.work.hours <- function(set.start, set.end, set.whours.start = 9, set.whours.end = 17) {
  # Use given inputs to code up start and end date 
  date.start <- parse_date_time(set.start, order=c("%Y-%m-%d %H:%M:%S"))
  date.end <- parse_date_time(set.end, order=c("%Y/%m/%d %H:%M:%S"))
  # Use given inputs to code up working hours (here: given as 09:00h to 17:00h) 
  whours.start <- set.whours.start 
  whours.end <- set.whours.end 
  
  # Create sequence of all days in window between set start and end times 
  timeframe <- data.frame(day = c(seq(date.start, date.end, "days"), date.end))
  
#  # Add number of the weekday (as 1: Monday, ..., 7: Suday) 
#  timeframe$weekday.number <- as.numeric(format(timeframe$day,"%u"))
  
  # Code up new variables: 1) number of the weekday (as 1: Monday, ..., 7: Suday); 2) working hours for week days (leave of weekends, that is, set to 0)
  timeframe <- timeframe %>% mutate(weekday.number = as.numeric(format(timeframe$day,"%u")), 
                                    whours.count = ifelse(weekday.number<=5, 
                                                          whours.end - whours.start, 
                                                          0))
  
  # Correct hours on first day, if set start time is later than start of working hours and day is a weekday 
  if (as.difftime(format(date.start,"%H:%M:%S"), units = "hours") > whours.start  & timeframe$weekday.number[1]<=5) {
    timeframe$whours.count[1] <- as.numeric(whours.end-as.difftime(format(date.start,"%H:%M:%S"), units = "hours")) 
  }
  # Correct hours on last day, if set end time is earlier than end of working hours and day is a weekday. If not (> else): Set to 0 in order not to count twice 
  ifelse(as.difftime(format(date.end,"%H:%M:%S"), units = "hours") < whours.end & timeframe$weekday.number[nrow(timeframe)]<=5, 
         timeframe$whours.count[nrow(timeframe)] <- as.numeric(as.difftime(format(date.end,"%H:%M:%S"), units = "hours")-whours.end), 
         timeframe$whours.count[nrow(timeframe)] <- 0)
  # Sum calculated full number of hours across days (if interest is in exact working time, simply remove round function here)
  whours.sum <- round(sum(timeframe$whours.count))
  
  # Print result from calculation 
  print(paste0("Full working hours between ", date.start, 
               " and ", date.end, " (given standard working hours between ", 
               whours.start, "h and ", whours.end, "h) amount to: ", whours.sum, " hours."))
  
  # In addition, return calculated number of hours to allow using this result as part of subsequent estimations/calculations 
  return(whours.sum)
}


# start ==================================================================================================== # 
# example: calculate working hours between two sets of time stamps 
fullwhours1 <- full.work.hours("2019-10-04 10:05:00", "2019-11-28 18:45:00")
print(fullwhours1)

fullwhours2 <- full.work.hours("2019-12-11 10:30:00", "2019-12-11 16:15:00")
print(fullwhours2)
