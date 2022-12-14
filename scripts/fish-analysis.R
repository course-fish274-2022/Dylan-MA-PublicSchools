fish_data <- read.csv("data/Gaeta_etal_CLC_data_1.csv")

install.packages(dplyr)
library(dplyr)
fish_data_cat <- fish_data %>%
  mutate(length_cat = ifelse(length > 200, "big", "small"))

#this code categorizes data by lengths greater than 300
fish_data_cat <- fish_data %>%
  mutate(length_cat = ifelse(length > 300, "big", "small"))


install.packages("ggplot2")
library(ggplot2)

ggplot(data = fish_data_cat, mapping = aes(x = length, y = scalelength, color = lakeid))+
  geom_point()

#Plot histogram of scale length by fish categorical size
ggplot(fish_data_cat, aes(x = scalelength, fill = length_cat)) +
  geom_histogram(bins = 80)+
  ggsave("scale_hist_by_length.jpg")


