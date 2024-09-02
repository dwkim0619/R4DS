install.packages("styler")

library(tidyverse)
library(nycflights13)

flights |> 
  mutate(
    speed      = distance / air_time,
    dep_hour   = dep_time %/% 100,
    dep_minute = dep_time %% 100
  ) |> 
  relocate(speed, dep_hour, dep_minute, dep_time)

flights |> 
  filter(!is.na(arr_delay), !is.na(tailnum)) |> 
  count(dest)

flights |> 
  group_by(tailnum) |> 
  summarise(
    delay = mean(arr_delay, na.rm = T),
    n = n()
  )

flights |> 
  group_by(month) |> 
  summarise(
    delay = mean(arr_delay, na.rm = T)
  ) |> 
  ggplot(aes(month, delay)) +
    geom_col()

flights |> 
  group_by(dest) |> 
  summarise(
    distance = mean(distance),
    speed    = mean(distance / air_time, na.rm = T)
  ) |> 
  ggplot(aes(distance, speed)) +
  geom_smooth(
    method = 'loess',
    span = .5,
    se = FALSE,
    color = 'white',
    linewidth = 4
  ) +
  geom_point()