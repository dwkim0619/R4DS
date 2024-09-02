library(nycflights13)
library(tidyverse)
library(dplyr)

# 3.1 Introduction -------------------------------------------------------------------

glimpse(flights)
# This dataset contains all 336,776 flights that departed from New York City in 2013.
# Rows: 336,776, Columns: 19

?flights

flights |> 
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )


# 3.1 Rows ----------------------------------------------------------------

?filter

flights |> 
  filter(dep_delay > 120 & month == 1 & day == 1) 

flights |> 
  filter(month == 1 | month == 2)

flights |>
  filter(month %in% c(1, 2))

?arrange # Order rows using column values

flights |>
  arrange(year, desc(month), desc(day), dep_time)

?distinct # Keep distinct/unique rows

flights |>
  distinct(origin, dest, .keep_all = TRUE)

flights |>
  count(origin, dest, sort = TRUE)


# 3.2.5 Exercises ---------------------------------------------------------

# 1. In a single pipeline for each condition, find all flights that meet the condition:

#    Had an arrival delay of two or more hours
flights |>
  filter(arr_delay >= 120) |>
  arrange(desc(arr_delay))
#    Flew to Houston (IAH or HOU)
flights |>
  filter(dest %in% c('IAH', 'HOU'))
#    Were operated by United, American, or Delta
airlines
# UA      United Air Lines Inc.    
# AA      American Airlines Inc.
# DL      Delta Air Lines Inc. 
flights |>
  filter(carrier %in% c('UA', 'AA', 'DL'))  
#    Departed in summer (July, August, and September)
flights |>
  filter(month %in% c(7, 8, 9))
#    Arrived more than two hours late, but didn’t leave late
flights |>
  filter(arr_delay >= 120 & dep_delay <= 0) |>
  view()
#    Were delayed by at least an hour, but made up over 30 minutes in flight
flights |>
  filter(dep_delay >= 60 & dep_delay - arr_delay > 30)

# 2. Sort flights to find the flights with longest departure delays. 
#    Find the flights that left earliest in the morning.
flights |>
  arrange(desc(dep_delay)) |>
  arrange(sched_dep_time) |>
  relocate(dep_delay, sched_dep_time)

# 3. Sort flights to find the fastest flights. (Hint: Try including a math calculation inside of your function.)
flights |>
  arrange(desc(distance / air_time))
  glimpse()
  
  
flights %>%
  mutate(speed = distance / (air_time / 60)) %>%
  arrange(desc(speed)) %>%
  relocate(speed)

# 4. Was there a flight on every day of 2013?
flights %>%
  distinct(year, month, day) |>
  nrow()

# 5. Which flights traveled the farthest distance? 
#    Which traveled the least distance?
flights |>
  arrange(desc(distance)) %>%
  relocate(distance)

flights |>
  arrange(distance) %>%
  relocate(distance)

# 6. Does it matter what order you used filter() and arrange() if you’re using both? 
#    Why/why not? Think about the results and how much work the functions would have to do.

# 행 번호가 아닌 조건에 따라 필터링하므로 순서는 중요하지 않습니다.


# ==============================================================================
# 3.3 Column
# ==============================================================================

# mutate   : creates new columns
# select   : changes which columns are present
# rename   : changes the names of the columns, 
# relocate : changes the positions of the columns.

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60
  ) |>
  relocate(gain, speed)

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    #.before = 1
    .after = day
  ) 
# .before : control where new columns should appear
# 특히 유용한 인수는 "used"로, mutate() 단계에서 관련되거나 생성된 열만 유지하도록 지정합니다.

flights %>%
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used" # all, used, unused, none
  )

flights %>%
  select(year, month, day)

flights %>%
  select(year:day)

flights %>%
  select(!year:day)

flights %>%
  select(!where(is.character))

flights %>%
  select(ends_with('time'))

flights %>%
  select(ends_with('delay'))

# starts_with("abc"): matches names that begin with “abc”.
# ends_with("xyz"): matches names that end with “xyz”.
# contains("ijk"): matches names that contain “ijk”.
# num_range("x", 1:3): matches x1, x2 and x3.

flights %>%
  select(tail_num = tailnum)

flights %>%
  rename(tail_num = tailnum)

flights

flights |>
  relocate(year:dep_time, .after = time_hour)

flights |>
  relocate(contains("arr"), .before = dep_time)

# ==============================================================================
# 3.3.5 Exercises

# 1. Compare dep_time, sched_dep_time, and dep_delay. 
#    How would you expect those three numbers to be related?

flights |>
  mutate(
    sched_dep_time, 
    dep_time,
    dep_delay_2 = (dep_time %/% 100 * 60 + dep_time %% 100) - (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100),
    dep_delay,
    .keep = "used"
    ) 

# dep_time = sched_dep_time + dep_delay

# 2. Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.
flights |>
  select(dep_time, dep_delay, arr_time, arr_delay)

# starts_with("abc"): matches names that begin with “abc”.
# ends_with("xyz"): matches names that end with “xyz”.
# contains("ijk"): matches names that contain “ijk”.
# num_range("x", 1:3): matches x1, x2 and x3.

flights |>
  select(starts_with('dep'), starts_with('arr'))

flights

flights |>
  select(dep_time:arr_delay, -starts_with('sched'))

# 3. What happens if you specify the name of the same variable multiple times in a select() call?
flights |>
  select(dep_time, dep_time) # You get the variable just once.

# 4. What does the any_of() function do? Why might it be helpful in conjunction with this vector?
variables <- c("year", "month", "day", "dep_delay", "arr_delay")

?any_of

flights %>%
  select(any_of(variables))

flights %>%
  select("year", "month", "day", "dep_delay", "arr_delay")

# 5. Does the result of running the following code surprise you? 
#    How do the select helpers deal with upper and lower case by default? How can you change that default?

flights |> select(contains("TIME"))

# contains : default ignore.case = TRUE 

# 6. Rename air_time to air_time_min to indicate units of measurement 
#    and move it to the beginning of the data frame.

flights |>
  rename(air_time_min = air_time) |>
  relocate(air_time_min)

# 7. Why doesn’t the following work, and what does the error mean?
flights |> 
  select(tailnum, arr_delay) |> 
  arrange(arr_delay)

# ==============================================================================
# 3.4 The pipe
# ==============================================================================

flights |>
  filter(dest == "IAH") |>
  mutate(speed = distance / air_time * 60) |>
  select(year:day, dep_time, carrier, flight, speed) |>
  arrange(desc(speed))

mtcars %>%
  group_by(cyl) %>%
  summarize(n = n()) 

# summarise() creates a new data frame. It returns one row for each combination of grouping variables;

# ==============================================================================
# 3.5 Groups
# ==============================================================================

# 3.5.1 group_by()

flights |>
  group_by(month) 

# group_by() : doesn’t change the data
#   adds this grouped feature (referred to as class) to the data frame, 
#   which changes the behavior of the subsequent verbs applied to the data.

# 3.5.2 summarize()

flights |>
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  )
# NA -  because some of the observed flights had missing data in the delay column

# 3.5.3 The slice_ functions

flights |> slice_head(n = 1)

flights |> slice_tail(n = 1)

flights |> slice_min(dep_delay, n = 1)

flights |> slice_max(arr_delay, n = 1)

flights |> slice_sample(n = 5)

flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1, with_ties = FALSE) |> # Use FALSE to ignore ties, and return the first n rows.
  relocate(dest, arr_delay)

# 3.5.4 Grouping by multiple variables

daily <- flights |>
    group_by(year, month, day)

daily_flights <- daily |>
  summarise(n = n())

daily_flights <- daily |> 
  summarize(
    n = n(),
    .groups = 'drop_last'
  )
daily_flights  
  
daily |> 
  ungroup() |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    flights = n()
  )

daily |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    flights = n()
  )

flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = c(year, month)
  )

flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = c(origin, dest)
  )

# ==============================================================================
# 3.5.7 Exercises

# 1. 평균 지연이 가장 심한 항공사는 어디인가요? 
#    도전 과제: 나쁜 공항과 나쁜 항공사의 영향을 구분할 수 있나요? 왜/왜 안 될까요?

?flights

flights |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    .by = carrier
  ) |> 
  arrange(desc(avg_delay))

# F9          20.2 

# 2. 각 목적지에서 출발할 때 가장 지연되는 항공편을 찾아보세요.
flights |>
  group_by(dest) |> 
  arrange(dest, desc(dep_delay)) |> 
  slice_head(n = 5) |> 
  relocate(dest, dep_delay)

# 3. 하루 동안 지연은 어떻게 달라지나요? 플롯으로 답을 설명하세요.
flights |> 
  group_by(hour) |> 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) |> 
  ggplot(aes(hour, avg_dep_delay)) +
  geom_point() +
  geom_smooth()

# 4. slice_min()과 친구에 음수 n을 제공하면 어떻게 되나요?
?slice_min()

flights |> 
  slice_max(dep_delay, n = -5) |> 
  relocate(dep_delay)
  
# 음수 값을 입력하면 데이터 프레임이 
#   오름차순(slice_min() 사용) 또는 
#   내림차순(slice_max() 사용) 정렬되지만 
#   실제로 주어진 변수의 최저/최고 값에 대한 데이터 프레임이 분할되지는 않습니다.

# 5. 방금 배운 dplyr 동사의 관점에서 count()가 하는 일을 설명하세요. count()의 sort 인수는 어떤 역할을 하나요?
# count()는 각 그룹의 관찰 수를 계산하며, 
# sort 인수를 TRUE로 설정하면 관찰 수가 많은 순서대로 카테고리가 내림차순으로 정렬됩니다.

# 6.
df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)

df |>
  group_by(y)

df |>
  arrange(y)

df |> 
  group_by(y) |> 
  summarize(mean_x = mean(x))

df |> 
  group_by(y,z) |> 
  summarize(mean_x = mean(x))

# 다음은 Y와 Z로 df를 그룹화한 다음 
# 각 그룹 조합에 대해 x의 평균값을 계산합니다. 결과 데이터 프레임은 그룹화되지 않습니다.

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x), .groups = "drop")

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))

df |>
  group_by(y, z) |>
  mutate(mean_x = mean(x))

# ==============================================================================
# 3.6 Case study: aggregates and sample size
# ==============================================================================

batters <- Lahman::Batting |> 
  group_by(playerID) |> 
  summarize(
    performanace = sum(H, na.rm = T) / sum(AB, na.rm = T),
    n = sum(AB, na.rm = T)
  )

batters |> 
  filter(n > 100) |> 
  ggplot(aes(n, performanace)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = FALSE)

# The variation in performance is larger among players with fewer at-bats. 
# the variation decreases as the sample size increases4.
# There’s a positive correlation between skill (performance) and opportunities to hit the ball (n) 
#   because teams want to give their best batters the most opportunities to hit the ball.

batters |> 
  arrange(desc(performanace))
