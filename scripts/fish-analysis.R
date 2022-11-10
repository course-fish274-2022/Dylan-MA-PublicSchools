fish_data <- read.csv("data/Gaeta_etal_CLC_data.csv")

install.packages(dplyr)
library(dplyr)
fish_data_cat <- fish_data %>%
  mutate(length_cat = ifelse(length > 200, "big", "small"))

fish_data_cat <- fish_data %>%
  mutate(length_cat = ifelse(length > 300, "big", "small"))


install.packages("ggplot2")
library(ggplot2)

ggplot(data = fish_data_cat, mapping = aes(x = length, y = scalelength, color = lakeid))

#Plot histogram of scale length by fish categorical size
ggplot(fish_data_cat, aes(x = scalelength, fill = length_cat)) +
  geom_histogram()


