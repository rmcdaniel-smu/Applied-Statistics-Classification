library(pacman)
p_load(ggplot2, dplyr)

df <- read.csv("./modelingKobeData.csv", header=T, sep=",", strip.white=T, stringsAsFactors = F)

# Identify all NAs to impute (or remove) as needed
na_count <- sapply(df, function(cnt) sum(length(which(is.na(cnt)))))
#View(na_count) # there are no NA values

# convert all number values to numeric since there is a mix of num and int for homogeneity
df <- df %>% mutate_if(is.integer, as.numeric) %>% data.frame()

str(df)

# unlist shot data, save into a data frame
shotsTaken <- data.frame(df$loc_x, df$loc_y, df$shot_distance)

colnames(shotsTaken) <- c("loc_x", "loc_y", "shot_distance")

# simple plot using EVENT_TYPE to colour the dots
ggplot(shotsTaken, aes(x=loc_x, y=loc_y)) + 
  geom_point(aes(colour = df$shot_type)) # there is a 2-point shot from location roughly equal to 500. This isn't possible so will remove

#anything beyond loc_y = 300 should be a 3pt field goal:
df[which(df$loc_y > 300),17] <- "3PT Field Goal"

ggplot(shotsTaken, aes(x=loc_x, y=loc_y)) + 
  geom_point(aes(colour = df$shot_type)) +
  theme(panel.background = element_rect(fill = 'ivory1'))

#View(skimr::skim(df))

range(df$shot_distance)
range(df$lat)
range(df$loc_x)
range(df$loc_y)
range(df$lon)

#to give more separation between shot distances, multiply shot_distance, lat, loc_x, loc_y, and lon values by 10 (don't
#square since some are negative). Since response is binary, this won't impact the scale; only the separation between
#predictors, which should allow the model to assess more distinction between predictors. This is especially helpful for
#lon since it is entirely within a range of -118 to -118 and also helpful for lat. But do this for all distance values
#so the entire distance portion of the data set is on like-scale. This idea comes from case study 18.1.1 where weight is
#measured on a scale of height squared (square meters), done for similar purpose (increased predictor distinction)

#multiply by the absolute values to retain direction signs instead of squaring and removing negatives:
df$shot_distance <- df$shot_distance*abs(df$shot_distance)
df$lat <- df$lat*abs(df$lat)
df$loc_x <- df$loc_x*abs(df$loc_x)
df$loc_y <- df$loc_y*abs(df$loc_y)
df$lon <- df$lon*abs(df$lon)

range(df$shot_distance)
range(df$lat)
range(df$loc_x)
range(df$loc_y)
range(df$lon)

ggplot(shotsTaken, aes(x=loc_x, y=loc_y)) + 
  geom_point(aes(colour = df$shot_type)) +
  theme(panel.background = element_rect(fill = 'ivory1'))

