library(tidyverse)

forest_fires <- read_csv("C:/Users/rajes/OneDrive/Desktop/Analyzing Forest Fire/forest+fires/forestfires.csv")

colnames(forest_fires)
forest_fires %>% pull(month) %>% unique

forest_fires %>% pull(day) %>% unique

month_order <- c("jan", "feb", "mar",
                 "apr", "may", "jun",
                 "jul", "aug", "sep",
                 "oct", "nov", "dec")

dow_order <- c("sun", "mon", "tue", "wed", "thu", "fri", "sat")

forest_fires <- forest_fires %>% 
  mutate(
    month = factor(month, levels = month_order),
    day = factor(day, levels = dow_order)
  )


fires_by_month <- forest_fires %>%
  group_by(month) %>%
  summarize(total_fires = n())

fires_by_month %>% 
  ggplot(aes(x = month, y = total_fires)) +
  geom_col() +
  labs(
    title = "Number of forest fires in data by month",
    y = "Fire count",
    x = "Month"
  )
fires_by_dow <- forest_fires %>%
  group_by(day) %>%
  summarize(total_fires = n())

fires_by_dow %>% 
  ggplot(aes(x = day, y = total_fires)) +
  geom_col() +
  labs(
    title = "Number of forest fires in data by day of the week",
    y = "Fire count",
    x = "Day of the week"
  )
forest_fires_long <- forest_fires %>% 
  pivot_longer(
    cols = c("FFMC", "DMC", "DC", 
             "ISI", "temp", "RH", 
             "wind", "rain"),
    names_to = "data_col",
    values_to = "value"
  )

forest_fires_long %>% 
  ggplot(aes(x = month, y = value)) +
  geom_boxplot() +
  facet_wrap(vars(data_col), scale = "free_y") +
  labs(
    title = "Variable changes over month",
    x = "Month",
    y = "Variable value"
  )
forest_fires_long %>% 
  ggplot(aes(x = value, y = area)) +
  geom_point() +
  facet_wrap(vars(data_col), scales = "free_x") +
  labs(
    title = "Relationships between other variables and area burned",
    x = "Value of column",
    y = "Area burned (hectare)"
  )
forest_fires_long %>% 
  filter(area < 300) %>% 
  ggplot(aes(x = value, y = area)) +
  geom_point() +
  facet_wrap(vars(data_col), scales = "free_x") +
  labs(
    title = "Relationships between other variables and area burned (area < 300)",
    x = "Value of column",
    y = "Area burned (hectare)"
  )