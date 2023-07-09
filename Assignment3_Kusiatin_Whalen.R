while (!is.null(dev.list())) dev.off()
rm(list = ls())
cat('\014')

library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

#1
read.csv(file.choose()) -> imdb
head(imdb)

#2
imdb %>% mutate(Runtime_num = str_remove(Runtime, "min")) -> imdb
class(imdb$Runtime_num)
as.numeric(imdb$Runtime_num) -> imdb$Runtime_num
class(imdb$Runtime_num)

#We used the function "str_remove" to delete "min" from the Runtime column
# After removing "min," we had to change the data type of the column from
# a character to a numeric data type. 

#3
year(as.Date(imdb$Released_Year, format = "%Y")) -> imdb$Released_Year

imdb_plot <- imdb %>% 
              select(Released_Year, Runtime_num) %>% 
              group_by(Released_Year) %>% 
              summarise(avg_runtime = mean(Runtime_num), 
                        med_runtime = median(Runtime_num))


ggplot(imdb_plot, aes(x = Released_Year)) +
  geom_line(aes(y = avg_runtime), color = "red") +
  geom_line(aes(y = med_runtime), color = "blue")

# The Released_Year column was a character data type, so we had to convert it
# into a date type. We did this by using the as.Date function and then the
# year function to only extract the year. The block of code beneath creates
# two columns: avg_runtime, med_runtime. We used these two columns to plot 
# the difference between the median and the avg runtime of films by year.
# Over time, the median and average runtime of movies increased gradually with
# exception of a few outliers. For many of the early year, the median runtime
# and the average runtime were the same.

#4
imdb %>% mutate(isComedy = str_detect(Genre, "Comedy")) -> imdb
# We used str_detect within our mutate function to create a column of 
# true and false values to indicate whether a movie is a comedy or not

#5
imdb%>%
  rowwise() %>%
  mutate(num_words = length(strsplit(Series_Title, " ")[[1]])) -> imdb_strcount

imdb_word_count <- imdb_strcount %>% 
                    group_by(Released_Year) %>% 
                    select(num_words, Released_Year) %>% 
                    filter(num_words == max(num_words)) %>% 
                    arrange(desc(num_words)) %>% distinct()
                    

ggplot(imdb_word_count, aes(x = Released_Year, y = num_words)) + geom_point()

#We used rowwise to compute a count of words on a row-by-row basis. We created
# a column, "num_words", and used the length(strsplit()) function to calculate
# number of words using the spaces in between. The next block of code groups by
# the year and filters by the title with the most amount of words in each year, 
# respectively. We then used the ggplot to plot the max number of words per 
# year. Based on the graph, we identified a trend of movie titles becoming 
# longer over time. The scatter plot trends upwards, especially in the late
# 1900s early 2000s.

#bonus
GenreTable <- as.matrix(imdb$Series_Title)
GenreTable %>% 
  cbind(str_split(imdb$Genre, pattern = ",", simplify = TRUE)) -> GenreTable
as.data.frame(GenreTable) -> GenreTable

GenreTable <- GenreTable %>% 
                mutate(Series_Titles = V1,
                        Genre1 = V2,
                        Genre2 = V3,
                        Genre3 = V4) %>% 
                select(-c(V1, V2, V3, V4))

# We created a matrix, "GenreTable," which is comprised of the Series_Title 
# column from the original data frame. We used str_split and cbind function
# to create 3 genre attribute columns for each movie record. Then used mutate
# to change the names of the columns. 