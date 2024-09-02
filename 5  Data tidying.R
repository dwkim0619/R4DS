library(tidyverse)

# 5.1 Introduction --------------------------------------------------------

table1 |> 
  mutate(rate = cases / population * 10000)

table1 |> 
  group_by(year) |> 
  summarise(total_cases = sum(cases))

table1

ggplot(table1, aes(year, cases)) +
  geom_line(aes(group =  country), color = 'grey50') +
  geom_point(aes(color = country, shape = country)) +
  scale_x_continuous(breaks = c(1999, 2000))

?geom_line


# 5.2.1 Exercises ---------------------------------------------------------

table1

table2 |> 
  pivot_wider(
    names_from = type,
    values_from = count
  ) |>
  mutate(rate = cases / population * 100)

table3 |> 
  separate_wider_delim(
    cols = rate,
    delim = '/',
    names = c("cases", "population")
  ) |> 
  mutate(
    cases = as.numeric(cases),
    population = as.numeric(population),
    rate = cases / population * 100
  )

# 5.3 Lengthening data ----------------------------------------------------

# 5.3.1 Data in column name -----------------------------------------------

# each observation is a song
billboard_longer <- billboard |> 
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(
    week = parse_number(week)
  )

view(billboard_longer )

billboard_longer |> 
  filter(artist == '2 Pac')

billboard_longer |> 
  filter(track == 'Wobble Wobble' | track == "I Don't Wanna") |> 
  ggplot(aes(week, rank, color = track)) + 
  geom_line(alpha = .25, wid) +
  scale_y_reverse()
  

# 5.3.2 How does pivoting work? -------------------------------------------

df <- tribble(
  ~id, ~bp1, ~bp2,
  "A", 100, 120,
  "B", 140, 115,
  "C", 120, 125
)
df

df |> 
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "values"
  )

?who2
glimpse(who2)

who2 |> 
  pivot_longer(
    cols = sp_m_014:rel_f_65,
    names_to = "group",
    values_to = "value",
    values_drop_na = TRUE
  )

who2 |> 
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count",
    values_drop_na = T
  )

df <- tribble(
  ~id, ~x_1, ~y_2,
  "A", 100, 120,
  "B", 140, 115,
  "C", 120, 125
)

df |> 
  pivot_longer(
    cols = !c("id"),
    names_to = c("name", "number"),
    names_sep = "_",
    values_to = "value"
  )


# 5.3.4 Data and variable names in the column headers ---------------------
household

household |> 
  pivot_longer(
    cols = !family,
    names_to = c(".value", "child"),
    names_sep = "_"
  )

df <- tribble(
  ~id, ~x_1, ~x_2, ~y_1, ~y_2,
  "A", 100, 120, 1, 2,
  "B", 140, 115, 4, 5,
  "C", 120, 125, 7, 8
)

df |> 
  pivot_longer(
    cols = !id,
    names_to = c(".value", "num"),
    names_sep = "_",
    values_to = "values"
  )


# 5.4 Widening data -------------------------------------------------------

cms_patient_experience
glimpse(cms_patient_experience)

cms_patient_experience |> 
  distinct(measure_cd, measure_title)

cms_patient_experience |> 
  pivot_wider(
    id_cols = starts_with("org"), #  A set of columns that uniquely identify each observation.
    names_from = measure_cd,
    values_from = prf_rate
  )


# 5.4.1 How does pivot_wider() work? --------------------------------------

df <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "B",        "bp1",    140,
  "B",        "bp2",    115, 
  "A",        "bp2",    120,
  "A",        "bp3",    105
)
df

df |> 
  pivot_wider(
    names_from = measurement,
    values_from = value
  )

df |> 
  distinct(measurement) |> 
  pull()

df |> 
  select(-measurement, -value) |> 
  distinct() |> 
  mutate(x = NA, y = NA, z = NA)

df <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "A",        "bp1",    102,
  "A",        "bp2",    120,
  "B",        "bp1",    140, 
  "B",        "bp2",    115
)

df |> 
  pivot_wider(
    names_from = measurement,
    values_from = value
  )

df |> 
  group_by(id, measurement) |> 
  summarise(n = n(), .groups = "drop") |> 
  filter(n > 1)
