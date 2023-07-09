while (!is.null(dev.list())) dev.off()
rm(list = ls())
cat('\014')

library(tidyverse)
library(lubridate)
library(stringr)

read.csv(file.choose()) -> yt

#Cleaning data (removing unnecessary columns, parsing dates) *US YT VIDS*
yt %>% mutate(date.trending = 
              as.Date(trending_date, 
                      format = '%y','%d','%m')) %>% 
       select(-trending_date) -> yt

class(yt$date.trending)



yt <- yt %>% select(-c(thumbnail_link, description, video_id))
yt <- yt %>% mutate(video_id = seq(1:nrow(yt)), 
                    publish_date = str_remove_all(publish_time, "[TZ]")) %>% 
             select(-c(publish_time))


class(yt$publish_date)

yt$publish_date <- ymd_hms(yt$publish_date)


# assigning binary values to "True/False" columns
yt <- yt %>% mutate(comments.allowed = 
                      ifelse(comments_disabled == 'True', 1, 0),
                    ratings.allowed = 
                      ifelse(ratings_disabled == 'False', 0, 1),
                    video.error.or.removed = 
                      ifelse(video_error_or_removed == 'False', 0, 1)) %>% 
            select(-c(comments_disabled, 
                      ratings_disabled,
                      video_error_or_removed))

#Creating a column that notates a genre for each category_id
ytUSA <- yt %>% mutate(genre = 
                         ifelse(category_id == 1, "Film", 
                         ifelse(category_id == 2, "Cars",
                         ifelse(category_id == 10, "Music",
                         ifelse(category_id == 17, "Sports",
                         ifelse(category_id == 24, "Entertainment",
                         ifelse(category_id == 15, "Pets/Animals",
                         ifelse(category_id == 19, "Travel",
                         ifelse(category_id == 20, "Gaming",
                         ifelse(category_id == 22, "People Blogs",
                         ifelse(category_id == 23, "Comedy",
                         ifelse(category_id == 25, "News/Politics",
                         ifelse(category_id == 26, "How-to-Style",
                         ifelse(category_id == 27, "Education",
                         ifelse(category_id == 28, "Science/Tech",
                         ifelse(category_id == 29, "Activism/Non-Profit",
                         ifelse(category_id == 43, "Shows", NA)))))))))))))))))

#ytUSA <- ytUSA %>% select(-category_id)
# Finding the time difference between published date and trending date 
# Changing it to a numeric data type
ytUSA <- ytUSA %>% 
            mutate(days.since.trendDate = 
                     round(difftime(date.trending, 
                                    publish_date, units = "days"), digits = 0)) %>%
            arrange(days.since.trendDate)
          

ytUSA$days.since.trendDate <- as.numeric(ytUSA$days.since.trendDate)

# Creating data frames for max and min amount of views per video
# Accounts for a video when it first becomes trending and its last
# appearance trending 
ytUSA_tot <- ytUSA %>% 
                group_by(title) %>% 
                mutate(total_views = max(views),
                          total_likes = max(likes),
                          total_dislikes = max(dislikes),
                          total_comments = max(comment_count)) %>% 
                filter(total_views == views)
                
ytUSA_min <- ytUSA %>% 
              group_by(title) %>% 
              mutate(min_views = min(views),
                     min_likes = min(likes),
                     min_dislikes = min(dislikes),
                     min_comments = min(comment_count)) %>% 
              filter(min_views == views)

ytUSA <- ytUSA_min %>% 
            left_join(ytUSA_tot, by = "title")
ytUSA <- ytUSA %>% 
          select(-c(channel_title.y, category_id.y, tags.y, views.y,
                    likes.y, dislikes.y, comment_count.y, date.trending.y,
                    video_id.y, publish_date.y, comment_count.y, 
                    ratings.allowed.y, video.error.or.removed.y, 
                    genre.y, days.since.trendDate.y, comments.allowed.y))

#Calculating the difference in the min amount of views the video had
# when it first went trending to the total amount of views it had
# when this data was scraped on Dec 2nd, 2018
ytUSA <- ytUSA %>% 
          group_by(title, channel_title.x) %>% 
          mutate(views_gained_from_trend = total_views - min_views,
                likes_gained_from_trend = total_likes - min_likes,
                dislikes_gained_from_trend = total_dislikes - min_dislikes,
                comments_gained_from_trend = total_comments - min_comments) %>% 
          arrange(desc(views_gained_from_trend))

#Taking the absolute value of the days.since.trendDate because 
# data input errors caused issues with the date calculation.
# All days.since.trendDate values under 50 were negative
abs(ytUSA$days.since.trendDate.x) -> ytUSA$days.since.trendDate.x

# Filter out publish_date for 2018/2017 and picked top 10 vids from each
# genre
ytUSA2018 <- ytUSA %>% 
              filter(year(publish_date.x) == 2018) %>% 
              group_by(genre.x) %>% 
              top_n(10, views.x)
?head

ytUSA2017 <- ytUSA %>% 
              filter(year(publish_date.x) == 2017) %>% 
              group_by(genre.x) %>% 
              top_n(10, views.x)

ggplot(data = ytUSA2018, aes(x = publish_date.x, y = total_views, color = genre.x)) +
  geom_point() + geom_text(aes(label = genre.x), size = 3)
