# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library (tidyverse)
library(GGally)
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

 

# Visualization (line 21)
ggpairs(week7_tbl[,5:13]) # how ?ggpairs shows it
week7_tbl %>%   # how we did it in class
  select(q1:q10) %>% 
  ggpairs

# Line 25-30: Check for differences in Q1 over time by creating a plot of timeStart on the x-axis and q1 on the y-axis that looks like Fig 1.
# fig1 <- 
ggplot(week7_tbl, aes(x=timeStart, y=q1)) +
  geom_point() +
  labs(x="Date of Experiment", y="Q1 Score")
  ggsave("../figs/fig1.png", ., dpi=600, height = 4, width=3) 



