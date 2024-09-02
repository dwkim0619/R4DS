library(tidyverse)
library(palmerpenguins)
library(ggthemes)

# <목표>
# 펭귄의 종을 고려하여 
# 펭귄의 오리발 길이(flipper_length_mm)와 몸무게(body_mass_g)의 관계를 보여주는 비주얼리제이션

# ==============================================================================
# 1.2 First steps
# ==============================================================================
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) + 
  geom_point()

# <답>
# "플리퍼 길이와 체질량 사이의 관계는 어떤 모습일까요?" 
# 이 관계는 
# 1. 양(플리퍼 길이가 증가할수록 체질량도 증가)이고, 
# 2. 상당히 선형적이며(점들이 곡선이 아닌 선 주위에 모여 있음), 
# 3. 적당히 강한(선 주위에 너무 많은 산란이 없음) 것으로 보입니다.
# => 오리발이 긴 펭귄은 일반적으로 몸무게가 더 큽니다.

# 산점도(Scatterplots)는 
# 두 숫자 변수 간의 관계를 표시하는 데 유용하지만, 
# 항상 두 변수 간의 겉으로 보이는 관계에 회의적인 태도를 취하고 
# 이 겉으로 보이는 관계의 특성을 설명하거나 변화시킬 수 있는 다른 변수가 있는지 확인하는 것이 좋습니다.
# 예) 오리발 길이와 체질량 사이의 관계는 종에 따라 다를까요? 

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) + 
  geom_point() +
  geom_smooth(method = 'lm')


ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) + 
  geom_point(mapping = aes(colour = species, shape = species)) +
  geom_smooth(method = 'lm') +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Species",
    shape = "Species"
  ) +
  scale_color_colorblind()

# ==============================================================================
# 1.2.5 Exercises

# 1. How many rows are in penguins? How many columns?
glimpse(penguins)
# Rows: 344, Columns: 8

# 2. What does the bill_depth_mm variable in the penguins data frame describe? 
#    Read the help for ?penguins to find out.
?penguins
# bill_depth_mm : a number denoting bill depth (millimeters)
# bill : (새의) 부리

# 3. Make a scatterplot of bill_depth_mm vs. bill_length_mm. 
#    That is, make a scatterplot with bill_depth_mm on the y-axis
#    and bill_length_mm on the x-axis. 
#    Describe the relationship between these two variables.
ggplot(
  data = penguins,
  aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point(
    mapping = aes(colour = species, shape = species)
  ) +
  geom_smooth(method = "lm") +
  labs(
    title = "Bill length and bill Depth",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Bill length (mm)",
    y = "Bill depth (mm)",
    color = "Species",
    shape = "Species"
  ) +
  scale_color_colorblind()

# <답>
# 이 관계는 약한 음의 관계를 보이나 비선형적이며 선 주위에 너무 많은 산란이 있음
  
# 4. What happens if you make a scatterplot of species vs. bill_depth_mm? 
#    What might be a better choice of geom?
ggplot(
  data = penguins,
  aes(x = species, y = bill_depth_mm)
) +
  geom_boxplot()

# 5. Why does the following give an error and how would you fix it?

ggplot(
  data = penguins,
  aes(x = flipper_length_mm, y = body_mass_g, color = sex)
) + 
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm")

# 6. What does the na.rm argument do in geom_point()? 
#    What is the default value of the argument? 
#   Create a scatterplot where you successfully use this argument set to TRUE.

?geom_point
# na.rm = FALSE
# If FALSE, the default, missing values are removed with a warning. 
# If TRUE, missing values are silently removed.

ggplot(
  data = penguins,
  aes(x = flipper_length_mm, y = body_mass_g)
) + 
  geom_point(na.rm = TRUE)

# 7. Add the following caption to the plot you made in the previous exercise: 
# “Data come from the palmerpenguins package.” Hint: Take a look at the documentation for labs().
ggplot(
  data = penguins,
  aes(x = flipper_length_mm, y = body_mass_g)
) + 
  geom_point(na.rm = TRUE) +
  labs(
    title = "Flipper length vs Body mass",
    caption = "Data come from the palmerpenguins package."
  )

# 8. Recreate the following visualization. 
# What aesthetic should bill_depth_mm be mapped to? 
# And should it be mapped at the global level or at the geom level?

ggplot(
  data = penguins,
  aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = bill_depth_mm)) +
  geom_smooth()

# 9. Run this code in your head and predict what the output will look like. 
#    Then, run the code in R and check your predictions.

?geom_smooth
# se : Display confidence interval around smooth? 
#      (TRUE by default, see level to control.)
# level	:Level of confidence interval to use (0.95 by default).
# 신뢰구간(CI)은 
#  모수가 실제로 포함될 것으로 예측되는 범위
#  집단 전체를 연구하는 것은 불가능하므로, 샘플링된 데이터를 기반으로 모수의 범위를 추정하기 위해 사용됨
#  샘플링된 표본이 연구중인 모집단을 얼마나 잘 대표하는지 측정하는 방법


ggplot(
  data = penguins,
  aes(x = flipper_length_mm, y = body_mass_g, colour = island)
) + 
  geom_point() +
  geom_smooth(se = FALSE)
  
# 10. Will these two graphs look different? Why/why not?
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) + 
  geom_point() +
  geom_smooth()

ggplot(
) + 
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )

# ==============================================================================
# 1.3 ggplot2 calls
# ==============================================================================

ggplot(penguins, aes(flipper_length_mm, body_mass_g)) +
  geom_point()

penguins |>
  ggplot(aes(flipper_length_mm, body_mass_g)) +
  geom_point()

# ==============================================================================
# 1.4 Visualizing distributions
# ==============================================================================

# To examine the distribution of a categorical variable, you can use a bar chart.
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

?fct_infreq

# Numerical variables can be continuous or discrete.
# One commonly used visualization for distributions of continuous variables is a histogram.

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)

# An alternative visualization for distributions of numerical variables is a density plot.
#  A density plot is a smoothed-out version of a histogram 

ggplot(penguins, aes(body_mass_g)) +
  geom_density()

# ==============================================================================
# 1.4.3 Exercises

# 1. Make a bar plot of species of penguins, where you assign species to the y aesthetic. How is this plot different?
ggplot(penguins, aes(y = species)) +
  geom_bar()

# 2. How are the following two plots different? 
#    Which aesthetic, color or fill, is more useful for changing the color of bars?

ggplot(penguins, aes(species)) +
  geom_bar(color = "red")

ggplot(penguins, aes(species)) +
  geom_bar(fill = "red")

# 3. What does the bins argument in geom_histogram() do?
?geom_histogram

# Number of bins. Overridden by binwidth. Defaults to 30.
# binwidth : The width of the bins. Can be specified as a numeric value or as a function that calculates width from unscaled x. 

# 4. Make a histogram of the carat variable in the diamonds dataset that is available when you load the tidyverse package. 
#    Experiment with different binwidths. What binwidth reveals the most interesting patterns?
?diamonds
# A dataset containing the prices and other attributes of almost 54,000 diamonds
glimpse(diamonds)

ggplot(diamonds, aes(carat)) +
  geom_histogram()

ggplot(diamonds, aes(carat)) +
  geom_histogram(binwidth = 0.01)

# ==============================================================================
# 1.5 Visualizing relationships
# ==============================================================================

# A numerical and a categorical variable -> box plots
ggplot(penguins, aes(species, body_mass_g)) +
  geom_boxplot()
# 데이터의 대략적인 분포와 개별적인 이상치들을 동시 표시
# 서로 다른 데이터 뭉치를 쉽게 비교

ggplot(penguins, aes(body_mass_g, color = species)) +
  geom_density(linewidth = .75)

ggplot(penguins, aes(body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 1/3)

# Two categorical variables -> stacked bar plots
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

?geom_bar

# Two numerical variables
ggplot(penguins, aes(flipper_length_mm, body_mass_g)) +
  geom_point()

# Three or more variables
ggplot(penguins, aes(flipper_length_mm, body_mass_g)) +
  geom_point(aes(color = species)) +
  facet_wrap(~island)

# ==============================================================================
# 1.5.5 Exercises

# 1. Which variables in mpg are categorical? 
#    Which variables are numerical? (Hint: Type ?mpg to read the documentation for the dataset.) 
#    How can you see this information when you run mpg?
glimpse(mpg)
?mpg # Fuel economy data
# categorical <chr> : manufacturer, model, trans, drv, fl, class

# 2. Make a scatterplot of hwy vs. displ using the mpg data frame. 
#    Next, map a third, numerical variable to color, then size, then both color and size, then shape. 
#    How do these aesthetics behave differently for categorical vs. numerical variables?

# hwy : highway miles per gallon
# displ : engine displacement, in litres
ggplot(mpg, aes(displ, hwy, colour = cyl, size = cyl, shape = drv)) + 
  geom_point() +
  geom_smooth(method = 'lm')

ggplot(mpg, aes(hwy, displ, color = cty)) + geom_point()

ggplot(mpg, aes(hwy, displ, size = cty)) + geom_point()

ggplot(mpg, aes(hwy, displ, color = cty, size = cty)) + geom_point()

ggplot(mpg, aes(hwy, displ, color = cty, size = cty, shape = drv)) + geom_point()


# 3. In the scatterplot of hwy vs. displ, what happens if you map a third variable to linewidth?
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(linewidth = 1) 
  
# 4. What happens if you map the same variable to multiple aesthetics?
ggplot(mpg, aes(hwy, hwy, color = hwy)) + 
  geom_point() 

# 5. Make a scatterplot of bill_depth_mm vs. bill_length_mm and color the points by species. 
#    What does adding coloring by species reveal about the relationship between these two variables? What about faceting by species?

ggplot(penguins, aes(bill_depth_mm, bill_length_mm, color = species)) +
  geom_point() 
  
ggplot(penguins, aes(bill_depth_mm, bill_length_mm, color = species)) +
  geom_point() +
  facet_wrap(~species)

# 6. Why does the following yield two separate legends? How would you fix it to combine the two legends?
ggplot(
  data = penguins, 
  mapping = aes(
    x = bill_length_mm, 
    y = bill_depth_mm, 
    color = species, 
    shape = species)
) +
  geom_point() +
  labs(color = "Species", shape = "Species")

# 7. Create the two following stacked bar plots. 
#    Which question can you answer with the first one? 
#    Which question can you answer with the second one?
ggplot(penguins, aes(island, fill = species)) +
  geom_bar(position = "fill")
# 1. Adelie은 3개의 섬에 모두 서식
# 2. Gentoo는 Biscoe 섬에 만 서식
# 3. Chinstrap은 Dream 섬에 만 서식
# 4. Torgersen 섬에는 Adelie 종 만 서식

ggplot(penguins, aes(species, fill = island)) +
  geom_bar(position = 'fill')
# 1. Adelie은 3개의 섬에 모두 서식
# 2. Chinstrap은 Dream 섬에 만 서식
# 3. Gentoo는 Biscoe 섬에 만 서식
# 4. Torgersen 섬에는 Adelie 종 만 서식

# ==============================================================================
# 1.6 Saving your plots
# ==============================================================================
ggplot(penguins, aes(flipper_length_mm, body_mass_g)) + 
  geom_point()
ggsave('penguin-plot.png')

# ==============================================================================
# 1.6.1 Exercises
# 1. Run the following lines of code. Which of the two plots is saved as mpg-plot.png? Why?
ggplot(mpg, aes(class)) +
  geom_bar()
ggplot(mpg, aes(cty, hwy)) +
  geom_point()
ggsave("mpg-plot.png")

# 2. What do you need to change in the code above to save the plot as a PDF instead of a PNG? 
#    How could you find out what types of image files would work in ggsave()?

?ggsave
ggsave("mpg-plot.pdf")

# ==============================================================================
# 1.7 Common problems
# ==============================================================================

ggplot(data = mpg) 
+ geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + # at the end of the line, not the start
  geom_point(mapping = aes(x = displ, y = hwy))