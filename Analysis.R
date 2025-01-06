library(dplyr)
library(tidyr)
library(tidyverse)


dataFrame1 <- read.csv("WICAgencies2014ytd/Food_costs.csv")
dataFrame2 <- read.csv("WICAgencies2015ytd/Food_costs.csv")  
dataFrame3 <- read.csv("WICAgencies2016ytd/Food_costs.csv")

result <- merge(dataFrame1, dataFrame2, by = "State.Agency.or.Indian.Tribal.Organization", all.x = TRUE)
result <- merge(result, dataFrame3, by = "State.Agency.or.Indian.Tribal.Organization", all.x = TRUE)

colnames(result)[1] <- "place"

newDF <- result %>%
  pivot_longer(
    cols = starts_with("X"), # Select date columns
    names_to = "date",
    values_to = "foodCost"
  ) %>%
  mutate(
    year = sub("^X(\\d{4})\\..*", "\\1", date),
    month = sub("^X\\d{4}\\.(\\d{2})\\..*", "\\1", date)
  ) %>%
  select(place, year, month, foodCost)

df <- newDF[!is.na(newDF$foodCost),]
df <- df[df$year %in% c(2014, 2015), ]

wilcox_test <- wilcox.test(foodCost ~ year, data = df)

print(wilcox_test)
