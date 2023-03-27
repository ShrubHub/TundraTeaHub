# Tea Water Trial script
# 5th March 2018
# Sleeping Willow has leached 33% of its mass

# packages ----
library(tidyverse)

# Read in data
data <- read.csv("users/hthomas/tea/data/Water_trial.csv")
meta <- read.csv("users/hthomas/tea/data/Water_meta_analysis.csv")

data_sum <-
  data %>% group_by(Tea_Type, Treatment) %>% summarise(mean(Loss))

data <- dplyr::select(data, Tea_Type, Loss, Treatment)
data$Tea_Type <- as.character(data$Tea_Type)
data$Treatment <- as.character(data$Treatment)
data$Loss <- data$Loss * 100

meta <- dplyr::select(meta, Tea_Type, Loss, Treatment)
meta$Tea_Type <- as.character(meta$Tea_Type)

all_data <- rbind(data, meta)

all_data[all_data$Tea_Type == "Green", ]$Tea_Type <- "green tea"
all_data[all_data$Tea_Type == "Rooibos", ]$Tea_Type <- "rooibos tea"

all_data$Tea_Type <-
  factor(all_data$Tea_Type,
         levels = c("Meta-analysis", "green tea", "rooibos tea"))

figure_data <-
  all_data %>% filter(Tea_Type != "Meta-analysis") %>% mutate(Treatment_name = Treatment) %>% mutate(
    Treatment_name = recode(
      Treatment_name,
      "Water_replace" = "2 months, water replaced weekly",
      "Water_2_month" = "2 months",
      "Water_frozen" = "2 months, frozen",
      "Water_24h" = "24 hour"
    )
  )

ggplot(figure_data, aes(Tea_Type, Loss, fill = reorder(factor(Treatment_name), -Loss, median))) +
  geom_boxplot(aes(Tea_Type, Loss, fill = reorder(factor(Treatment_name), -Loss, median))) +
  theme_bw() +
  scale_fill_viridis_d(option = "plasma") +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(y = "Mass Loss (%)", x = "Tea Type") +
  guides(fill = guide_legend(title = "Incubation Treatment"))
# ggtitle("Leaching experiment")

ggsave(
  "users/hthomas/tea/figures/Leaching_incubations.pdf",
  width = 6,
  height = 3
)
