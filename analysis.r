library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

movies_root <- read.csv("Generated-Data/movies_metadata.csv")

# Média de avaliações por ano --------------------------------------------------

average_vote_per_year <- movies_root %>% group_by(release_year) %>% summarise(average=mean(vote_average))
plot(average_vote_per_year$release_year[76:132], average_vote_per_year$average[76:132])
#plot(movies_root$release_year, movies_root$vote_average)

# Gêneros por avaliação --------------------------------------------------------
movies_genres <- read.csv("Generated-Data/movies_genres.csv")

movies_and_genres <- merge(x=movies_root, y=movies_genres, by.x="id", by.y="movie_id")
movies_and_genres <- select(movies_and_genres, id, original_title, genre, vote_average, release_year)

genre_vote <- movies_and_genres %>% 
  select(genre, vote_average) %>% 
  group_by(genre) %>% 
  summarise(average=mean(vote_average, na.rm = T)) %>%
  arrange(desc(average))

#barplot(genre_vote$average[1:10], names.arg=genre_vote$genre[1:10], ylim=c(5,7), col=1:length(genre_vote$average[1:10]))
ggplot(genre_vote[1:10,], aes(genre, average)) + 
  geom_bar(stat="identity") +
  xlab("Genres") + ylab("Average rating") +
  coord_cartesian(ylim=c(5,7))

# Melhores filmes de cada gênero -----------------------------------------------
index <- which(duplicated(movies_and_genres$original_title))
movies_and_genres <- movies_and_genres[-index,]

movies_genre_ratings <- movies_and_genres %>%
  select(title, genre, vote_average, vote_count, release_year) %>%
  group_by(genre) %>%
  filter(vote_count > 800, title!="The Lord of the Rings: The Return of the King") %>%
  mutate(max_score=max(vote_average)) %>%
  ungroup() %>%
  filter(vote_average==max_score)

movies_genre_ratings <- filter(movies_genre_ratings,
                            genre != "Family", genre != "Adventure",
                            genre != "Mystery", genre != "Fantasy",
                            title != "The Good, the Bad and the Ugly",
                            genre != "History", genre != "Thriller")

names_w_years <- paste(movies_genre_ratings$title, " (",
                       movies_genre_ratings$release_year, ")", sep="")

filter(movies_and_genres[movies_and_genres$title == "Saving Private Ryan",])

ggplot(movies_genre_ratings,
       aes(x=as.character(genre),
           y=as.numeric(vote_average),
           fill=names_w_years)) + 
  geom_bar(stat="identity", position = "dodge") +
  xlab("Genres") + ylab("Average rating") + labs(fill="Collection") +
  coord_cartesian(ylim=c(5,9))

# Contagem gênero por ano ------------------------------------------------------
genre_per_year <- movies_and_genres %>% 
  select(genre, release_year) %>% 
  group_by(release_year, genre) %>%
  summarise(count=n())

drama_per_year <- filter(genre_per_year, genre=="Drama")
animation_per_year <- filter(genre_per_year, genre=="Animation")
action_per_year <- filter(genre_per_year, genre=="Action")
romance_per_year <- filter(genre_per_year, genre=="Romance")
crime_per_year <- filter(genre_per_year, genre=="Crime")
western_per_year <- filter(genre_per_year, genre=="Western")
war_per_year <- filter(genre_per_year, genre=="War")
thriller_per_year <- filter(genre_per_year, genre=="Thriller")
fantasy_per_year <- filter(genre_per_year, genre=="Fantasy")
comedy_per_year <- filter(genre_per_year, genre=="Comedy")
scifi_per_year <- filter(genre_per_year, genre=="Science Fiction")
music_per_year <- filter(genre_per_year, genre=="Music")

fill.genre.years <- function(data){
  for(i in 1874:2020){
    aux <- filter(data, release_year == i)
    if(length(aux$release_year) == 0){
      data[nrow(data)+1,] <- list(i, data$genre[1], 0)
    }
  }
  return(arrange(data, release_year))
}

drama_per_year <- fill.genre.years(drama_per_year)[-148,]
animation_per_year <- fill.genre.years(animation_per_year)[-148,]
action_per_year <- fill.genre.years(action_per_year)[-148,]
romance_per_year <- fill.genre.years(romance_per_year)[-148,]
crime_per_year <- fill.genre.years(crime_per_year)[-148,]
western_per_year <- fill.genre.years(western_per_year)[-148,]
scifi_per_year <- fill.genre.years(scifi_per_year)[-148,]
war_per_year <- fill.genre.years(war_per_year)[-148,]
comedy_per_year <- fill.genre.years(comedy_per_year)[-148,]

plot(animation_per_year$release_year,
     animation_per_year$count,
     type="o", col="blue",
     xlim=c(1920,2010),
     ylim=c(1,700), pch=1)
lines(action_per_year$release_year,
      action_per_year$count,
      type="o", col="green", pch=2)
lines(drama_per_year$release_year,
      drama_per_year$count,
      type="o", col="red", pch=3)
#lines(romance_per_year$release_year,
#      romance_per_year$count,
#      type="o", col="pink", pch=4)
#lines(crime_per_year$release_year,
#      crime_per_year$count,
#      type="o", col="brown", pch=5)
lines(scifi_per_year$release_year,
      scifi_per_year$count,
      type="o", col="grey", pch=7)
lines(war_per_year$release_year,
      war_per_year$count,
      type="o", col="orange", pch=8)
lines(comedy_per_year$release_year,
      comedy_per_year$count,
      type="o", col="yellow", pch=9)
#lines(western_per_year$release_year,
#      western_per_year$count,
#      type="o", col="purple", pch=10)















# Melhores avaliações por mês --------------------------------------------------
best_movies_per_month <- movies_root %>%
  filter(vote_average>0) %>%
  select(title, vote_average, release_month) %>%
  group_by(release_month) %>%
  summarise(count=n())

best_movies_per_month <- best_movies_per_month[-13,]
best_movies_per_month$release_month <- as.Date(
  paste0("2020-", best_movies_per_month$release_month, "-1"))

ggplot(best_movies_per_month) + 
  geom_bar(mapping = aes(x = release_month, y = count), stat="identity") +
  scale_x_date(labels=date_format("%b"), breaks="1 month") +
  xlab("Month") +
  ylab("Quantity of high votes")

# Número de coleções ao longo dos anos -----------------------------------------
fill.collection.years <- function(data){
  for(i in 1874:2020){
    aux <- filter(data, release_year == i)
    if(length(aux$release_year) == 0){
      data[nrow(data)+1,] <- list(i, 0)
    }
  }
  return(arrange(data, release_year))
}

cond = is.na(movies_root$belongs_to_collection)
only_collections = movies_root[!cond,]

collection_per_year <- only_collections %>%
  select(title, release_year) %>%
  group_by(release_year) %>%
  summarise(count=n())

movies_per_year <- movies_root %>%
  select(title, release_year) %>%
  group_by(release_year) %>%
  summarise(count=n())

collection_per_year <- fill.collection.years(collection_per_year)
movies_per_year <- fill.collection.years(movies_per_year)

movies_collection <- data.frame(year = collection_per_year$release_year,
                                movies_count = movies_per_year$count,
                                collection_count = collection_per_year$count)

ggplot(movies_collection[1:147,]) + 
  geom_line(mapping = aes(x = year, y = collection_count), stat="identity")

# Maiores coleções -------------------------------------------------------------
cond = is.na(movies_root$belongs_to_collection)
only_collections = movies_root[!cond,]

collections_revenue <- only_collections %>%
  select(belongs_to_collection, revenue, release_year) %>%
  group_by(belongs_to_collection) %>%
  summarise(total_revenue=sum(revenue), year=min(release_year)) %>%
  arrange(desc(total_revenue))

ggplot(collections_revenue[1:10,], aes(total_revenue, belongs_to_collection)) + 
  geom_bar(stat="identity") +
  xlab("Collections") + ylab("Revenue (in billions)")

# Coleções que mais arrecadaram por década -------------------------------------
collections_revenue$total_revenue <- collections_revenue$total_revenue/1000000000
collections_revenue$decade <- as.integer(collections_revenue$year/10) * 10

better_by_decade <- collections_revenue %>%
  select(belongs_to_collection, decade, total_revenue) %>%
  group_by(decade) %>%
  summarise(total_revenue=max(total_revenue))

better_by_decade <- merge(x=better_by_decade, y=collections_revenue, by=c("total_revenue", "decade"))
better_by_decade <- arrange(better_by_decade, decade)

ggplot(better_by_decade[6:14,], aes(x=as.character(decade),y=total_revenue,fill=belongs_to_collection)) + 
  geom_bar(stat="identity") +
  xlab("Year") + ylab("Revenue (in billions)") + labs(fill="Collection")

# Coleções mais bem avaliadas --------------------------------------------------
collections_rating <- only_collections %>%
  select(belongs_to_collection, release_year, vote_average, vote_count) %>%
  group_by(belongs_to_collection) %>%
  summarise(average_rating=mean(vote_average), 
            year=min(release_year),
            average_count=mean(vote_count),
            count=n()) %>%
  arrange(desc(average_rating)) %>%
  filter(count>1, average_count>500, year>1959)

ggplot(collections_rating[1:10,], aes(average_rating, belongs_to_collection)) + 
  geom_bar(stat="identity") +
  xlab("Collections") + ylab("Average rating") +
  coord_cartesian(xlim=c(7,8.5))

# Coleções mais bem avaliadas por década ---------------------------------------
collections_rating$decade <- as.integer(collections_rating$year/10) * 10

better_rating_decade <- collections_rating %>%
  group_by(decade) %>%
  summarise(average_rating=sort(average_rating, decreasing = T)[1:3],
            collection=belongs_to_collection[1:3], year=year[1:3])

better_rating_decade <- better_rating_decade[complete.cases(better_rating_decade),]

names_w_years <- paste(better_rating_decade$collection, " (",
                       better_rating_decade$year, ")", sep="")

ggplot(better_rating_decade[1:nrow(better_rating_decade),],
       aes(x=as.character(decade),
           y=as.numeric(average_rating),
           fill=names_w_years)) + 
  geom_bar(stat="identity", position = "dodge") +
  xlab("Decade") + ylab("Average rating") + labs(fill="Collection") +
  coord_cartesian(ylim=c(5,9))



# Filmes estrangeiros com mais arrecadação -------------------------------------
international_movies <- movies_root %>%
  filter(original_language != "en")

international_revenues <- international_movies %>%
  select(title, release_year, revenue,
         original_language, vote_average, vote_count) %>%
  arrange(desc(revenue))

# Língua estrangeira com melhor avaliação ao longo dos anos --------------------
language_group <- international_movies %>%
  group_by(original_language) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

top_5 <- as.character(language_group$original_language)[1:5]

language_group_year <- international_movies %>%
  filter(original_language %in% top_5, vote_count>100, release_year>2000) %>%
  group_by(release_year, original_language) %>%
  summarise(rating=mean(vote_average))

fr <- filter(language_group_year, original_language=="fr", release_year>2000)
it <- filter(language_group_year, original_language=="it", release_year>2000)
ja <- filter(language_group_year, original_language=="ja", release_year>2000)
de <- filter(language_group_year, original_language=="de", release_year>2000)
es <- filter(language_group_year, original_language=="es", release_year>2000)

ggplot(data=fr, aes(x=release_year, y=rating)) +
  geom_line(color="blue") +
  coord_cartesian(ylim=c(3,9)) +
  geom_line(data=it, aes(x=release_year, y=rating), color="green") +
  geom_line(data=ja, aes(x=release_year, y=rating), color="red") +
  geom_line(data=de, aes(x=release_year, y=rating)) +
  geom_line(data=es, aes(x=release_year, y=rating), color="orange") +
  scale_color_manual()

ggplot(data=language_group_year, aes(x=release_year, y=rating)) +
  geom_line(aes(color=original_language, linetype=original_language)) +
  coord_cartesian(ylim=c(3,9))

# Língua estrangeira com melhor avaliação ao longo dos anos --------------------
language_group <- international_movies %>%
  group_by(original_language) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

top_5 <- as.character(language_group$original_language)[1:5]

language_revenue_year <- international_movies %>%
  filter(original_language %in% top_5, vote_count>100, release_year>2000) %>%
  group_by(release_year, original_language) %>%
  summarise(revenue=sum(revenue))

language_revenue_year$revenue <- language_revenue_year$revenue/1000000000

ggplot(data=language_revenue_year, aes(x=release_year, y=revenue)) +
  geom_line(aes(color=original_language, linetype=original_language))


# Relação avaliação/arrecadação ------------------------------------------------
library(ggrepel)
movies_rate_revenue <- movies_root %>%
  select(title, release_year, vote_average, vote_count, revenue)

rate_revenue <- movies_rate_revenue %>%
  filter(vote_count>1000) %>%
  group_by(vote_average) %>%
  summarise(revenue=mean(revenue), title=first(title))

rate_revenue$revenue <- as.integer(rate_revenue$revenue/1000000)

aux_movies <- movies_root %>% select(title, vote_average, revenue, vote_count, release_year) %>%
  filter(vote_count>1000)
aux_movies$revenue <- as.integer(aux$revenue/1000000)

aux_movies$revenue_2 <- as.integer(aux_movies$revenue/10) * 10
rate_revenue$revenue_2 <- as.integer(rate_revenue$revenue/10) * 10

aux_movies <- merge(x=rate_revenue, y=aux_movies, by=c("vote_average", "revenue_2"))
index = which(duplicated(aux_movies$vote_average))
aux_movies <- aux_movies[-index,]

names_w_years <- paste(aux_movies$title.y, " (", aux_movies$release_year, ")", sep="")

ggplot(data=aux_movies, mapping = aes(x=vote_average, y=revenue.x)) + 
  geom_point(color="blue", size=3) +
  geom_line(data=rate_revenue, mapping=aes(x=vote_average, y=revenue)) +
  #geom_text(aes(label=title.y), colour="blue") +
  geom_label_repel(aes(label=names_w_years),
                   box.padding=0.35,
                   point.padding=0.5,
                   segment.color="grey50") +
  xlab("Rating") +
  ylab("Average Revenue (in millions)")



