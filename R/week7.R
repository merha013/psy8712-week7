# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library (tidyverse)
library(GGally)


# Data Import and Cleaning
week7_tbl <- read.csv("../data/week3.csv", header=TRUE) %>%
  mutate(timeStart = as.POSIXct(timeStart)) %>%
  mutate(timeEnd = as.POSIXct(timeEnd)) %>%
  filter(q6==1) %>%
  select(-q6) %>%
  mutate(timeSpent = difftime(timeEnd, timeStart, units = "mins")) %>%
  mutate(gender = 
           recode_factor(gender, F = "Female", M = "Male")) %>%
  mutate(condition = 
           recode_factor(condition, A = "Block A", B = "Block B", C = "Control"))


# Visualization
week7_tbl %>%  
  select(q1:q10) %>% 
  ggpairs
(week7_tbl %>%
ggplot(aes(x=timeStart, y=q1)) +
  geom_point(shape = 20, size = 2) +
  guides(x=guide_axis("Date of Experiment"), y=guide_axis("Q1 Score"))) %>%
  ggsave("../figs/fig1.png", ., dpi=600, height = 4, width=8)

(week7_tbl %>%
  ggplot(aes(x=q1, y=q2, color=gender)) +
  geom_jitter(width=.08, height =.08, shape = 20, size =2) +
  labs(color = "Participant Gender")) %>%   # not sure how to list Male first
  ggsave("../figs/fig2.png", ., dpi=600, height = 4, width=8)
(week7_tbl %>%
  ggplot(aes(x=q1, y=q2)) +
  geom_jitter(width=.08, height =.08, shape = 20, size =2) +
  guides(x=guide_axis("Score on Q1"), y=guide_axis("Score on Q2")) +
  facet_grid(.~gender)) %>%  # not sure how to list Male first
  ggsave("../figs/fig3.png", ., dpi=600, height = 4, width=8)

(week7_tbl %>%
    ggplot(aes(x=gender, y=timeSpent)) + # Is it better to have timeSpent be continuous rather than difftime?
    geom_boxplot() + # not sure how to list Male first
    guides(x=guide_axis("Gender"), y=guide_axis("Time Elapsed (mins)"))) %>%
  ggsave("../figs/fig4.png", ., dpi=600, height = 4, width=8)

(week7_tbl %>%
    ggplot(aes(x=q5, y=q7, color=condition)) +
    geom_jitter(width=.08, height =.08, shape = 20, size =2) +
    labs(color = "Experimental Condition") +
    guides(x=guide_axis("Score on Q5"), y=guide_axis("Score on Q7")) +
    geom_smooth(method = "lm", se=F) +
    theme(legend.position = "bottom", 
          legend.background = element_rect(fill = grey(0.875)))) %>% # hex_code "#DFDFDF"
  ggsave("../figs/fig5.png", ., dpi=600, height = 4, width=8)