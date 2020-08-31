## Load usual packages into memory
library(gapminder)
library(here)
library(tidyverse)
library(socviz)

## 2.3 Things to Know about R

# Review of Assignment
c(1, 2, 3, 1, 3, 5, 25)
my_numbers <- c(1, 2, 3, 1, 3, 5, 25)
your_numbers <- c(5, 31, 71, 1, 3, 21, 6)
my_numbers

# Review of functions
?mean()
mean()
mean(x = my_numbers)
mean(x = your_numbers)


my_summary <- summary(my_numbers)
my_summary

table(my_numbers)

sd(my_numbers)

my_numbers * 5

my_numbers + 1

my_numbers + my_numbers

# Descriptive functions
class(my_numbers)
class(my_summary)
class(summary)


# Be careful appending values that are of different type
my_new_vector <- c(my_numbers, "Apple")
my_new_vector
class(my_new_vector)


# Data from from socviz library
titanic
class(titanic)
titanic$percent


# Converting to a tidyverse tibble
titanic_tb <- as_tibble(titanic)
titanic_tb
class(titanic_tb$fate)

# Using str

str(my_numbers)
str(my_summary)

## 2.4 Be Patient with R, and with Yourself
# ggplot() simple example
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point()

# How NOT to continue a line...

ggplot(data = mpg, aes(x = displ, y = hwy))
+ geom_point()


## 2.5 Get Data into R

url <- "https://cdn.rawgit.com/kjhealy/viz-organdata/master/organdonation.csv"
 
organs <- read.csv(file = url)

organs <- read_csv(file = "dataviz_course_notes/data/organdonation.csv")

organs

## 2.6 Make Your First Figure
gapminder

# ----first_plot, fig.height=6, fig.width=10, fig.cap="Life expectancy plotted against GDP per capita for a large number of country-years."----

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p
p + geom_point() + 
  geom_smooth(mapping = aes(color = continent, fill = continent)) + 
  scale_x_log10(labels = scales::dollar)

  # That's all


