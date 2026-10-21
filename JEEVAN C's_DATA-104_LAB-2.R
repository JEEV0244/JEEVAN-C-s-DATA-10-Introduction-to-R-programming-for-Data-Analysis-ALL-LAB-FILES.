###LAB-2 
###JEEVAN C
###LAB-2 OF DATA-104


#install.packages("dplyr")
library(dplyr)

data()

df = LifeCycleSavings
View(df)
str(df)

df_pop75 = filter(df,pop75 >3 )
View(df_pop75)

df_2 = filter(df,pop75 >3 & ddpi >2 )

#%in%

head(filter(mtcars, gear %in% c(3,4) ))

head(filter(mtcars, !gear %in% c(3,4) ))

### The Slice Function 
dff = AirPassengers
str(AirPassengers)
View(AirPassengers)

slice(mtcars, 3:12)

a = iris

slice(a,1,2)

slice_max(a,Sepal.Length, n = 5 )
slice_min(a,Sepal.Length, n = 5)
slice_min(a, Sepal.Length, n = 3)
slice_head(a,prop=0.05)
slice_sample(a,prop = 0.05)

## Arrange
rm(list = ls())
data()
df = LifeCycleSavings
View(LifeCycleSavings)
str(LifeCycleSavings)

head(arrange(df,sr, dpi))

head(arrange(df,desc(sr), dpi))

### Select function 

head(select(mtcars, 2:4))

head(select(mtcars, starts_with("c")))

head(select(mtcars, hp))

df1 = select(mtcars, c("hp","am"))

head(df1)
View(df1)

head(select(mtcars,-wt))

### Rename

# rename(data_set, new_column_name = old_column_name)

head(rename(mtcars, Weight = wt))
head(rename(mtcars, Engine= vs))

head(select(mtcars, contains("m")))
head(select(mtcars, ends_with("t")))


### distinct function 

distinct(select(mtcars, gear))

df2 = distinct(mtcars)

View(mtcars)
df3 = mtcars

sample_n(mtcars,5)

sample_n(BOD,5)


#pipe operator %>%

# data %>% op1 %>% op2 %>% op3 %>% ...................opn

mtcars %>% filter(gear == 4) %>% sample_n(5) %>% arrange(desc(mpg)) %>% select(-hp,-cyl)

### group_by
head(group_by(mtcars, gear))

summarise(group_by(mtcars,gear),mean(mpg))


#### mutate transmute function 
data()

library("nycflights13")
View(flights)
df1 = head(mutate(flights, new_col = arr_delay+dep_delay))
names(df1) ### name of the coulumns

str(df1)

head(transmute(flights, new_col = arr_delay+dep_delay))




