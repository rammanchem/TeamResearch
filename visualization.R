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


boxplot(foodCost ~ year, data = df, 
        main = "Boxplot",
        xlab = "Year", ylab = "Food Cost across all countries")


h <- hist(
  df$foodCost, 
  breaks = 30, 
  xlab = "Food Cost", 
  ylab = "Frequency", 
  main = "Histogram of food cost across all countries in 2014 and 2015")

x <- seq(min(df$foodCost), max(df$foodCost), length = 100) 
y <- dnorm(x, mean = mean(df$foodCost), sd = sd(df$foodCost)) * length(df$foodCost)
box.size <- diff(h$mids[1:2])
y <- y * box.size
lines(x, y, col = "red")