# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library (tidyverse)
# library call
# library call

# Data Import and Cleaning
week7_tbl <- read.csv("../data/week3.csv", header=TRUE) %>%
  mutate(timeStart = as.POSIXct(timeStart)) %>%
  mutate(timeEnd = as.POSIXct(timeEnd)) %>%
  subset(timeStart>"2017-06-30 23:59:59") %>%
  subset(q6==1) %>%
  select(-q6) %>%
  mutate(timeSpent = difftime(time1 = timeEnd, time2 = timeStart, units = "mins")) %>%
  mutate(gender = recode(gender, F = "Female", M = "Male")) %>%
  mutate(condition = recode(condition, A = "Block A", B = "Block B", C = "Control"))



# Visualization
