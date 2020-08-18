library(tidyverse)
library(ggthemes)
library(RColorBrewer)

data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")
view(data)

write.csv(data, file="C:/Users/luisf/Documents/R/TidyTuesdays/Roman Emperors/Roman Emperors Data.csv")

data %>%
  glimpse()

###Data Exploration:
#1.- Which were the longest and shortest reigns?
#2.- Who were the oldest and youngest emperors?
#3.- Top death causes and killers
#4.- Top  rise causes

###################################################################################################################

                                    ################################
                                    ############## #1 ##############
                                    ################################

as.numeric(data$reign_start)

data$reign_start[1] - data$reign_end[1]

# Reigns duration
data_01 <- data %>%
  transmute(
    name, dynasty,
    reign_duration = round((abs(data$reign_start - data$reign_end) / 365), digits = 2),
  )


# Reigns duration by dynasty

data_01$dynasty <- as.factor(data_01$dynasty)

display.brewer.all()

data_01 %>%
  group_by(dynasty) %>%
  summarise(
    total_duration = sum(reign_duration)
  ) %>%
  ungroup() %>%
  mutate(dynasty = fct_reorder(dynasty, total_duration, sum, .desc = F)) %>%
ggplot(aes(dynasty, total_duration)) +
  geom_col(aes(fill=dynasty),width = .8) + coord_flip() +
  scale_fill_brewer(palette = "Set1") +
theme_clean() +
  theme(
    panel.background = element_rect(fill = "black", color = "white"),
    plot.background = element_rect(fill = "black"), 
    plot.caption = element_text(color = "white"),
    plot.title = element_text(size = 15, face = "bold", color = "white", hjust = .5),
    axis.text = element_text(color = "white", size = 11),
    axis.title = element_text(face="bold", color = "white", size = 12),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 10, color = "white"),
    legend.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black", color = "white"),
    legend.position = c(0.77, 0.3)
  ) +
  labs(title="Reigns Duration by Dynasty", y="Years of Reign", caption= "Source: Wikipedia Zonination \n Made by @ShiXiong3 for #TidyTuesday") +
  guides(fill = F)

# Top 10 longest emeperors

data_01$name <- as.factor(data_01$name)

data_01 %>%
  top_n(10, reign_duration) %>%
  mutate(name = fct_reorder(name, reign_duration, .desc = F)) %>%
ggplot(aes(name, reign_duration)) +
  geom_col(aes(fill=dynasty),width = .8) + coord_flip() +
  scale_fill_brewer(palette = "Set1") +
theme_clean() +
  theme(
    panel.background = element_rect(fill = "black", color = "white"),
    plot.background = element_rect(fill = "black"), 
    plot.caption = element_text(color = "white"),
    plot.title = element_text(size = 15, face = "bold", color = "white", hjust = .5),
    axis.text = element_text(color = "white", size = 11),
    axis.title = element_text(face="bold", color = "white", size = 12),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 10, color = "white"),
    legend.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black", color = "white"),
    legend.position = c(0.85, 0.3)
  ) +
  labs(title="Top 10 Emperor Reigns", y="Years of Reign", caption= "Source: Wikipedia Zonination \n Made by @ShiXiong3 for #TidyTuesday",
  fill = "Dynasty")

###################################################################################################################

                                          ################################
                                          ############## #2 ##############
                                          ################################
# Emperors Age at Reign Start
data_02 <- data %>%
  transmute(
    name, rise, birth, reign_start,
    age_reign_start = round((abs(data$reign_start - data$birth) / 365), digits = 2),
  )

#Youngest
data_02 %>%
  top_n(-10, age_reign_start) %>%
  mutate(name = fct_reorder(name, age_reign_start, .desc = T)) %>%
ggplot(aes(name, age_reign_start)) +
  geom_col(aes(fill = rise),width = .8) + coord_flip() +
  scale_fill_brewer(palette = "Accent") +
theme_clean() +
  theme(
    panel.background = element_rect(fill = "black", color = "white"),
    plot.background = element_rect(fill = "black"), 
    plot.caption = element_text(color = "white"),
    plot.title = element_text(size = 15, face = "bold", color = "white", hjust = .5),
    axis.text = element_text(color = "white", size = 11),
    axis.title = element_text(face="bold", color = "white", size = 12),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 10, color = "white"),
    legend.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black", color = "white"),
    legend.position = c(0.85, 0.8)
  ) +
  labs(title="Youngest Emperors", y="Age Crowned", caption= "Source: Wikipedia Zonination \n Made by @ShiXiong3 for #TidyTuesday",
       fill = "Rise Cause")

#Oldest
data_02 %>%
  top_n(10, age_reign_start) %>%
  mutate(name = fct_reorder(name, age_reign_start, .desc = F)) %>%
ggplot(aes(name, age_reign_start)) +
  geom_col(aes(fill = rise),width = .8) + coord_flip() +
  scale_fill_brewer(palette = "Accent") +
theme_clean() +
  theme(
    plot.margin = margin(5,70,5,5),
    panel.background = element_rect(fill = "black", color = "white"),
    plot.background = element_rect(fill = "black"), 
    plot.caption = element_text(color = "white"),
    plot.title = element_text(size = 15, face = "bold", color = "white", hjust = .5),
    axis.text = element_text(color = "white", size = 11),
    axis.title = element_text(face="bold", color = "white", size = 12),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 10, color = "white"),
    legend.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black", color = "white"),
    legend.position = c(.95, 0.25)
  ) +
  labs(title="Oldest Emperors", y="Age Crowned", caption= "Source: Wikipedia Zonination \n Made by @ShiXiong3 for #TidyTuesday",
       fill = "Rise Cause")

###################################################################################################################
                                            
                                            ################################
                                            ############## #3 ##############
                                            ################################

#Deaths and killers

color_palette <- c("#da9100","#a31621","#f5f5f5","#034932","#0026e8","#f46036","#ff0667","#2e294e","#1b998b","#c5d86d","#343434","#2f4858","#f24333","pink")

data %>%
  group_by(cause, killer) %>%
  summarise(n_cause = n()) %>%
  unique() %>%
  ungroup() %>%
  mutate(cause = fct_reorder(cause, n_cause))%>%
  ggplot(aes(cause, n_cause)) +
  geom_col(aes(fill = killer), color ="white") +
  scale_fill_manual(values = color_palette) +
theme_clean() +
  theme(
    panel.background = element_rect(fill = "black", color = "white"),
    plot.background = element_rect(fill = "black"), 
    plot.caption = element_text(color = "white"),
    plot.title = element_text(size = 15, face = "bold", color = "white", hjust = .5),
    axis.text = element_text(color = "white", size = 11),
    axis.title = element_text(face="bold", color = "white", size = 12),
    axis.title.x = element_blank(),
    legend.text = element_text(size = 10, color = "white"),
    legend.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black", color = "white"),
  ) +
  labs(title="Count of Deaths and Killers", y="Total Ocurrences", caption= "Source: Wikipedia Zonination \n Made by @ShiXiong3 for #TidyTuesday",
       fill = "Killer")


###Intento # no se

datax <- data %>%
  group_by(cause) %>%
  summarise(n_cause = n()) %>%
  arrange(-n_cause)

datax$cause <- factor(datax$cause, levels = c("Assassination", "Captivity", "Died in Battle", 
                                 "Execution", "Natural Causes", "Suicide", "Unknown"))
datax %>%
ggplot(aes(cause)) +
  geom_bar(aes(y = n_cause), stat= "identity")

#https://juliasilge.com/blog/reorder-within/ REORDER WITHIN
install.packages("tidytext")
library(tidytext)

#Decade = killer
#n = n_cause
#names = cause

data %>%
  group_by(cause, killer) %>%
  summarise(n_cause = n()) %>%
  unique() %>%
  ungroup() %>%
  mutate(
    killer = as.factor(killer),
    cause = reorder_within(cause, n_cause, killer)) %>%
ggplot(aes(cause, n_cause)) +
  geom_col(color ="white") +
  facet_wrap(~killer, scales = "free_y") +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0))
