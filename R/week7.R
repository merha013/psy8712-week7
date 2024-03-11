# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library (tidyverse) # includes ggplot2 and dplyr
library(GGally)



# Data Import and Cleaning
week7_tbl <- read.csv("../data/week3.csv", header=TRUE) %>%
  mutate(timeStart = as.POSIXct(timeStart)) %>% # or use ymd_hms()
  mutate(timeEnd = as.POSIXct(timeEnd)) %>% # or use ymd_hms()
  filter(q6==1) %>%
  select(-q6) %>%
  mutate(timeSpent = as.numeric(timeEnd-timeStart),
         gender = recode_factor(gender,M = "Male",F = "Female"),
         condition = recode_factor(condition, 
                                   A = "Block A", 
                                   B = "Block B", 
                                   C = "Control"))

# Visualization
select(week7_tbl, q1:q10) %>% 
  ggpairs

(week7_tbl %>%
  ggplot(aes(x=timeStart, y=q1)) +
  geom_point(width=.08, height =.08, shape = 20, size =2) + # didn't have to include details
  xlab("Date of Experiment") +
  ylab("Q1 Score")) %>%
  ggsave("../figs/fig1.png", ., dpi=600, height = 4, width=8) # default dpi=300
(week7_tbl %>%
  ggplot(aes(x=q1, y=q2, color=gender)) +
  geom_jitter(width=.08, height =.08, shape = 20, size =2) + # didn't have to include details
  labs(color = "Participant Gender")) %>% # or could use scale_color_discrete()
  ggsave("../figs/fig2.png", ., dpi=600, height = 4, width=8)
(week7_tbl %>%
  ggplot(aes(x=q1, y=q2)) +
  geom_jitter(width=.08, height =.08, shape = 20, size =2) + # didn't have to include details
  xlabs("Score on Q1") +
  ylabs("Score on Q2") +
  facet_grid(.~gender)) %>% # could also use facet_wrap or scale_x_continuous
  ggsave("../figs/fig3.png", ., dpi=600, height = 4, width=8)
(week7_tbl %>%
  ggplot(aes(x=gender, y=timeSpent)) +
  geom_boxplot() +
  xlabs("Gender") +
  ylabs("Time Elapsed (mins)")) %>%
  ggsave("../figs/fig4.png", ., dpi=600, height = 4, width=8) #or height=1080 & width=1920 & units="px"
(week7_tbl %>%
    ggplot(aes(x=q5, y=q7, color=condition)) +
    geom_jitter(width=.08, height =.08, shape = 20, size =2) + # now the details are actually needed
    labs(x = "Score on Q5",
         y = "Score on Q7",
         color = "Experimental Condition") +
    geom_smooth(method = "lm", se=F) +
    theme(
      legend.position = "bottom", 
      legend.background = element_rect(fill = grey(0.875)))) %>% # hex_code = "#EOEOEO" by multiplying 225*.875
  ggsave("../figs/fig5.png", ., dpi=600, height = 4, width=8)


# I had previously used #guides(x=guide_axis("Score on Q5"), y=guide_axis("Score on Q7"))# for all of my axis labeling, which also worked