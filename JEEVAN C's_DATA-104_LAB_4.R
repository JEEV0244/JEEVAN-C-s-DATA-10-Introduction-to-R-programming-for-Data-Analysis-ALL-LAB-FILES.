### LAB-4 
### JEEVAN C
### INTRODUCTION TO R PROGRAM





#################################################################

library(dplyr)
library(tidyr)

###############################################################
# FILTER
###############################################################

# Example 1: Cars with mpg > 25
result <- mtcars %>% filter(mpg > 25)
print(result)

# Example 2: Iris setosa species
result <- iris %>% filter(Species == "setosa")
head(result)

###############################################################
# SLICE
###############################################################

# Example 1: First 5 rows
result <- mtcars %>% slice(1:5)
print(result)

# Example 2: Random 3 rows
result <- iris %>% slice(sample(1:n(), 3))
print(result)

###############################################################
# ARRANGE
###############################################################

# Example 1: Arrange by mpg (ascending)
result <- mtcars %>% arrange(mpg)
head(result)

# Example 2: Arrange by mpg (descending)
result <- mtcars %>% arrange(desc(mpg))
head(result)

###############################################################
# SELECT
###############################################################

# Example 1: Select specific columns
result <- mtcars %>% select(mpg, cyl, hp)
head(result)

# Example 2: Select columns starting with "Sepal"
result <- iris %>% select(starts_with("Sepal"))
head(result)

###############################################################
# RENAME
###############################################################

# Example 1: Rename mpg to mileage
result <- mtcars %>% rename(mileage = mpg)
head(result)

# Example 2: Rename Sepal.Length
result <- iris %>% rename(Sepal_Length = Sepal.Length)
head(result)

###############################################################
# DISTINCT
###############################################################

# Example 1: Unique cylinder values
result <- mtcars %>% distinct(cyl)
print(result)

# Example 2: Unique species
result <- iris %>% distinct(Species)
print(result)

###############################################################
# MUTATE
###############################################################

# Example 1: Add power-to-weight ratio
result <- mtcars %>% mutate(power_weight = hp / wt)
head(result)

# Example 2: Convert mpg to km/l
result <- mtcars %>% mutate(kmpl = mpg * 0.425)
head(result)

###############################################################
# TRANSMUTE
###############################################################

# Example 1: Only new variable
result <- mtcars %>% transmute(power_weight = hp / wt)
head(result)

# Example 2: Sepal area
result <- iris %>% transmute(sepal_area = Sepal.Length * Sepal.Width)
head(result)

###############################################################
# SUMMARISE
###############################################################

# Example 1: Mean mpg
result <- mtcars %>% summarise(mean_mpg = mean(mpg))
print(result)

# Example 2: Maximum temperature
result <- airquality %>% summarise(max_temp = max(Temp, na.rm = TRUE))
print(result)

###############################################################
# SAMPLE_N
###############################################################

# Example 1: Sample 5 rows
result <- mtcars %>% sample_n(5)
print(result)

# Example 2: Sample 10 rows
result <- iris %>% sample_n(10)
print(result)

###############################################################
# SAMPLE_FRAC
###############################################################

# Example 1: Sample 20% of data
result <- mtcars %>% sample_frac(0.2)
print(result)

# Example 2: Sample 30% of data
result <- iris %>% sample_frac(0.3)
print(result)

###############################################################
# PIPE OPERATOR vs WITHOUT PIPE
###############################################################

# Using pipe
result_pipe <- mtcars %>% filter(mpg > 25)
print(result_pipe)

# Without pipe
result_no_pipe <- filter(mtcars, mpg > 25)
print(result_no_pipe)

###############################################################
# GROUP_BY
###############################################################

# Example 1: Mean mpg by cylinders
result <- mtcars %>% group_by(cyl) %>% summarise(mean_mpg = mean(mpg))
print(result)

# Example 2: Mean sepal length by species
result <- iris %>% group_by(Species) %>% summarise(mean_sepal = mean(Sepal.Length))
print(result)

###############################################################
# COUNT
###############################################################

# Example 1: Count cars by cylinder
result <- mtcars %>% count(cyl)
print(result)

# Example 2: Count iris species
result <- iris %>% count(Species)
print(result)

###############################################################
# MIN_RANK
###############################################################

# Example 1: Rank cars by mpg
result <- mtcars %>% mutate(rank_mpg = min_rank(desc(mpg)))
head(result)

# Example 2: Rank flowers by Sepal.Length
result <- iris %>% mutate(rank_sepal = min_rank(Sepal.Length))
head(result)

###############################################################
# CUMSUM
###############################################################

# Example 1: Cumulative sum of mpg
result <- mtcars %>% mutate(cum_mpg = cumsum(mpg))
head(result)

# Example 2: Cumulative temperature
result <- airquality %>% mutate(cum_temp = cumsum(Temp))
head(result)

