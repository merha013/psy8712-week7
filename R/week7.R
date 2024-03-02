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

 

# Visualization
week7_tbl %>%  
  select(q1:q10) %>% 
  ggpairs


# Line 25-30: Check for differences in Q1 over time by creating a plot of timeStart on the x-axis and q1 on the y-axis that looks like Fig 1
(week7_tbl %>%
ggplot(aes(x=timeStart, y=q1)) +
  geom_point(shape = 20, size = 2) +
  guides(x=guide_axis("Date of Experiment"), y=guide_axis("Q1 Score"))) %>%
  ggsave("../figs/fig1.png", ., dpi=600, height = 4, width=3)
# scale_x_datetime(date_breaks = 'month', date_labels=c("Jun", "Jul", "Aug", "Sep")) ...to make x-axis match... but not working correctly
  
  
# Line 31-35: Check for gender differences in the Q1/Q2 relationship by creating a figure that looks like Fig 2.
(week7_tbl %>%
  ggplot(aes(x=q1, y=q2, color=gender)) +
  geom_jitter(width=.08, height =.08, shape = 20, size =2) +
  labs(color = "Participant Gender")) %>%
  ggsave("../figs/fig2.png", ., dpi=600, height = 4, width=3)


# Line 36-42: Check for gender differences in the Q1/Q2 relationship again, but this time by creating two side-by-side plots as shown in Fig 3.
(week7_tbl %>%
  ggplot(aes(x=q1, y=q2)) +
  geom_jitter(width=.08, height =.08, shape = 20, size =2) +
  guides(x=guide_axis("Score on Q1"), y=guide_axis("Score on Q2")) +
  facet_grid(.~gender)) %>%  # need to swap position of Male & Female
  ggsave("../figs/fig3.png", ., dpi=600, height = 4, width=3)


# Line 43-48 Using a single pipe, create a plot displaying the experimental time elapsed in minutes split by gender, which looks like Fig 4.
(week7_tbl %>%
    ggplot(aes(x=gender, y=timeSpent)) +
    geom_boxplot() + # need to swap position of Male & Female
    guides(x=guide_axis("Gender"), y=guide_axis("Time Elapsed (mins)"))) %>%
  ggsave("../figs/fig4.png", ., dpi=600, height = 4, width=3)


# Line 49+: Recreate Fig 5. Hint: The background of the legend is 12.5% grey.
(week7_tbl %>%
    ggplot(aes(x=q5, y=q7, color=condition)) +
    geom_jitter(width=.08, height =.08, shape = 20, size =2) +
    labs(color = "Experimental Condition") +
    guides(x=guide_axis("Score on Q5"), y=guide_axis("Score on Q7")) +
    geom_smooth(method = "lm", se=F) +
    theme(legend.position = "bottom", 
          legend.background = element_rect(fill = grey(.125)))) %>%
  ggsave("../figs/fig5.png", ., dpi=600, height = 4, width=3)
# gray color doesn't seem to be correct...