install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("corrplot")
install.packages("factoextra")
install.packages("plyr")
install.packages("RColorBrewer")
install.packages("funModeling")
install.packages("knitr")

install.packages('spotifyr')


library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(corrplot)
library(factoextra)
library(plyr)
library(RColorBrewer)
library(funModeling)
library(knitr)

##data importing
set.seed(13232767)
spotify <- read.csv("spotify_songs.csv")
glimpse(spotify)

##removing NA values
colSums(is.na(spotify))
spotify <- na.omit(spotify)

##filtering
spotify <- spotify[!duplicated(spotify$track_id),]

##transforming variables
spotify <- spotify %>%
  mutate(playlist_genre = as.factor(spotify$playlist_genre),
         playlist_subgenre = as.factor(spotify$playlist_subgenre),
         mode = as.factor(mode),
         key = as.factor(key))

spotify <- spotify %>% mutate(duration_min = duration_ms/60000)

##separating as groups
spotify <- spotify %>% 
  mutate(popularity_group = as.numeric(case_when(
    ((track_popularity > 0) & (track_popularity < 20)) ~ "1",
    ((track_popularity >= 20) & (track_popularity < 40))~ "2",
    ((track_popularity >= 40) & (track_popularity < 60)) ~ "3",
    TRUE ~ "4"))
  )
table(spotify$popularity_group)


##removing variables
spotify <- spotify %>% select(-c(track_id, track_album_id, playlist_id))
summary(spotify)

##correlation plot
df1 <- select(spotify, track_popularity, danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo)
corrplot(cor(df1))

##histogram
spotify_hist <- spotify[,-c(1,2,3,4,5,6,7,8,11,13,20,22)]
plot_num(spotify_hist)

##boxplot energy distribution and genre
boxplot(energy~playlist_genre, data=spotify,
        main = "Variation of energy between genres",
        xlab = "Energy",
        ylab = "Genre",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

##boxplot dancebility and genre
boxplot(danceability~playlist_genre, data=spotify,
        main = "Variation of danceability between genres",
        xlab = "Danceability",
        ylab = "Genre",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

##boxplot liveness and genre
boxplot(liveness~playlist_genre, data=spotify,
        main = "Variation of liveness between genres",
        xlab = "Liveness",
        ylab = "Genre",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

##boxplot valence and genre
boxplot(valence~playlist_genre, data=spotify,
        main = "Variation of valence between genres",
        xlab = "Valence",
        ylab = "Genre",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

##boxplot loudness and genre
boxplot(loudness~playlist_genre, data=spotify,
        main = "Variation of loudness between genres",
        xlab = "Loudness",
        ylab = "Genre",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

##popularity by acousticness
spotify$acousticness.scale <- scale(spotify$acousticness)
spotify %>%
  select(popularity_group, acousticness.scale, playlist_genre) %>%
  group_by(popularity_group)%>%
  filter(!is.na(popularity_group)) %>%
  filter(!is.na(acousticness.scale))%>%
  ggplot(mapping = aes(x = acousticness.scale, y = popularity_group, color = playlist_genre))+
  facet_wrap(~playlist_genre)+
  geom_point()+
  theme_minimal()

##popularity by valence
spotify%>%
  select(popularity_group, valence, playlist_genre) %>%
  group_by(popularity_group)%>%
  filter(!is.na(popularity_group)) %>%
  filter(!is.na(valence))%>%
  ggplot(mapping = aes(x = popularity_group, y = valence, color = playlist_genre, fill = playlist_genre))+
  geom_bar(stat = 'identity')+
  coord_polar()+
  facet_wrap(~playlist_genre)+
  theme_minimal()

##energy distribution
spotify$cut_energy <- cut(spotify$energy, breaks = 10)
spotify %>%
  ggplot( aes(x=cut_energy ))+
  geom_bar(width=0.2) +
  coord_flip() +
  scale_x_discrete(name="Energy")  

##speechness distribution
spotify$cut_spe <- cut(spotify$speechiness, breaks = 10)
spotify %>%
  ggplot( aes(x=cut_spe ))+
  geom_bar(width=0.2) +
  coord_flip() +
  scale_x_discrete(name="Spechiness")  

##Tempo and Liveness Distribution across Genre
spotify$liveness.scale <- scale(spotify$liveness)
spotify$tempo.scale <- scale(spotify$tempo)
spotify %>%
  select(tempo.scale, liveness.scale, playlist_genre) %>%
  group_by(playlist_genre)%>%
  filter(!is.na(tempo.scale)) %>%
  filter(!is.na(liveness.scale))%>%
  ggplot(mapping = aes(x = tempo.scale, y = liveness.scale, color = playlist_genre, fill = playlist_genre))+
  geom_bar(stat = 'identity')+
  coord_polar()+
  theme_minimal()

#top 10 artists and famous tracks
spotify %>%
  select(track_name, track_artist, track_album_name, playlist_genre, track_popularity)%>%
  group_by(track_artist)%>%
  filter(!is.na(track_name))%>%
  filter(!is.na(track_artist))%>%
  filter(!is.na(track_album_name))%>%
  arrange(desc(track_popularity))%>%
  head(n = 10)%>%
  ggplot(mapping = aes(x = track_name, y =  track_artist, color = track_artist, fill = track_artist, size = track_popularity ))+
  geom_point()+
  coord_polar()+
  facet_wrap(~playlist_genre)+
  theme_minimal()+
  labs(x = 'track_name', y = 'track_artist', title = 'Top ten artists of spotify')+
  theme(plot.title = element_text(hjust=0.5),legend.position ='bottom')

#data clustring
str(spotify)

#Scaling the numeric variables required for cluster analysis
spotify_scaled <- scale(spotify[,-c(1,2,3,4,5,6,7,8,11,13,20,22,23,24,25,26,27)])
summary(spotify_scaled)

##elbow method
wss <- function(data, maxCluster = 15) {
  SSw <- (nrow(data) - 1) * sum(apply(data, 2, var))
  SSw <- vector()
  for (i in 2:maxCluster) {
    SSw[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:maxCluster, SSw, type = "o", xlab = "Number of Clusters", ylab = "Within groups sum of squares", pch=19)
}

wss(spotify_scaled)

##k-means clustering

spotify_kmeans <- kmeans(spotify_scaled, centers = 7)
spotify_kmeans$size

spotify_kmeans$centers

spotify$cluster <- spotify_kmeans$cluster
tail(spotify)

fviz_cluster(spotify_kmeans, data=spotify_scaled)

#goodness of fit

#Within Sum of Squares tot.withinss : signifies the ‘length’ from each observation to its centroid in each cluster
spotify_kmeans$tot.withinss

#Total Sum of Squares totss : signifies the ‘length’ from each observation to global sample mean
spotify_kmeans$totss

#Between Sum of Squares betweenss : signifies the ‘length’ from each centroid from each cluster to the global sample mean
spotify_kmeans$betweenss

#Another ‘goodness’ measure can be signified with a value of betweenss/totss closer the value to 1 or 100%, the better): betweenss/tot.withinss
((spotify_kmeans$betweenss)/(spotify_kmeans$totss))*100

#Finding what kind of song characterises each clusters in the optimized model
spotify %>% 
  group_by(cluster) %>% 
  summarise_all(mean) %>% 
  select(cluster, acousticness, danceability, energy, instrumentalness, speechiness, valence, liveness)

#Song recommendation
spotify %>% 
  filter(track_name == "Memories - Dillon Francis Remix", track_artist == "Maroon 5")


spotify %>% 
  filter(cluster == 5, playlist_genre == "r&b") %>% 
  sample_n(5)

