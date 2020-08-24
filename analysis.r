library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(tm)
library(wordcloud)
library(readr)
library(ggrepel)

movies_root <- read.csv("generated_data/movies_metadata.csv")

# Média de avaliações por ano --------------------------------------------------

average_vote_per_year <- movies_root %>%
  filter(vote_count > 100) %>%
  group_by(release_year) %>%
  summarise(average=mean(vote_average))
#plot(average_vote_per_year$release_year, average_vote_per_year$average)
#plot(movies_root$release_year, movies_root$vote_average)
ggplot(data = average_vote_per_year, aes(average_vote_per_year$release_year, average_vote_per_year$average)) +
  geom_point() + ylab("Média de avaliação") + xlab("Ano")

# Quantidade de filmes por ano --------------------------------------------------

count_per_year <- movies_root %>%
  group_by(release_year) %>%
  filter(release_year>1920, release_year<2017) %>%
  summarise(count=n()) %>%
  filter(count>20)
#plot(average_vote_per_year$release_year, average_vote_per_year$average)
#plot(movies_root$release_year, movies_root$vote_average)
ggplot(data = count_per_year, aes(release_year, count)) +
  geom_point() + ylab("Quantidade de filmes") + xlab("Ano")

# Gêneros por avaliação --------------------------------------------------------
movies_genres <- read.csv("generated_data/movies_genres.csv")

movies_and_genres <- merge(x=movies_root, y=movies_genres, by.x="id", by.y="movie_id")
movies_and_genres <- select(movies_and_genres, id, original_title, genre,
                            vote_average, release_year, title, vote_count, release_month)

genre_vote <- movies_and_genres %>% 
  filter(vote_count>100) %>% 
  select(genre, vote_average) %>% 
  group_by(genre) %>% 
  summarise(average=mean(vote_average, na.rm = T)) %>%
  arrange(desc(average))

ggplot(genre_vote[1:10,], aes(genre, average)) + 
  geom_bar(stat="identity") +
  xlab("Gênero") + ylab("Média de avaliação")

# Contagem de gênero --------------------------------------------------------
genre_count <- movies_and_genres %>% 
  filter(vote_count>100) %>%  
  group_by(genre) %>% 
  summarise(count=n()) %>%
  arrange(desc(count))

ggplot(genre_count[1:10,], aes(genre, count)) + 
  geom_bar(stat="identity") +
  xlab("Gênero") + ylab("Quantidade")

# Melhores filmes de cada gênero -----------------------------------------------
index <- which(duplicated(movies_and_genres$original_title))
movies_and_genres <- movies_and_genres[-index,]

movies_genre_ratings <- movies_and_genres %>%
  select(title, genre, vote_average, vote_count, release_year) %>%
  group_by(genre) %>%
  filter(vote_count > 800) %>%
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
  coord_cartesian(ylim=c(7,9))

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
  filter(vote_average>6, vote_count>100) %>%
  select(title, vote_average, release_month) %>%
  group_by(release_month) %>%
  summarise(count=n())

best_movies_per_month <- best_movies_per_month[-13,]
best_movies_per_month$release_month <- as.Date(
  paste0("2020-", best_movies_per_month$release_month, "-1"))

ggplot(best_movies_per_month) + 
  geom_bar(mapping = aes(x = release_month, y = count), stat="identity") +
  scale_x_date(labels=date_format("%b"), breaks="1 month") +
  xlab("Mês") +
  ylab("Quantidade de avaliações acima de 6") +
  coord_cartesian(ylim=c(50,600))

# Melhores arrecadações por mês --------------------------------------------------
best_revenue_per_month <- movies_root %>%
  filter(vote_average>0) %>%
  select(title, revenue, release_month) %>%
  group_by(release_month) %>%
  summarise(revenue=sum(revenue))

best_revenue_per_month <- best_revenue_per_month[-13,]
best_revenue_per_month$release_month <- as.Date(
  paste0("2020-", best_revenue_per_month$release_month, "-1"))

best_revenue_per_month$revenue <- best_revenue_per_month$revenue/1000000000

ggplot(best_revenue_per_month) + 
  geom_bar(mapping = aes(x = release_month, y = revenue), stat="identity") +
  scale_x_date(labels=date_format("%b"), breaks="1 month") +
  xlab("Mês") +
  ylab("Arrecadação (em bilhões)")

# Quantidade de filmes por mês --------------------------------------------------
most_per_month <- movies_root %>%
  group_by(release_month) %>%
  summarise(count=n())

most_per_month <- most_per_month[-13,]
most_per_month$release_month <- as.Date(
  paste0("2020-", most_per_month$release_month, "-1"))

ggplot(most_per_month) + 
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

# Porcentagem arrecadação coleções/total ---------------
collection_revenue <- movies_root %>%
    filter(!is.na(belongs_to_collection)) %>%
    group_by(release_year) %>%
    summarise(total_revenue=sum(revenue)) %>%
    filter(total_revenue>0)

total_revenue <- movies_root %>%
  group_by(release_year) %>%
  summarise(total_revenue=sum(revenue)) %>%
  filter(total_revenue>0)

revenue_year <- merge(collection_revenue, total_revenue, by="release_year")
revenue_year$rate <- (revenue_year$total_revenue.x/revenue_year$total_revenue.y)*100

ggplot(revenue_year, aes(x=release_year,y=rate)) +
    geom_line()

# Maiores coleções -------------------------------------------------------------
cond = is.na(movies_root$belongs_to_collection)
only_collections = movies_root[!cond,]

collections_revenue <- only_collections %>%
  select(belongs_to_collection, revenue, release_year) %>%
  group_by(belongs_to_collection) %>%
  summarise(total_revenue=sum(revenue), year=min(release_year)) %>%
  arrange(desc(total_revenue))

collections_revenue$belongs_to_collection <- paste(collections_revenue$belongs_to_collection,
                                                  " (", collections_revenue$year, ")", sep="")
collections_revenue$total_revenue <- collections_revenue$total_revenue/1000000000

ggplot(collections_revenue[1:10,], aes(total_revenue, belongs_to_collection)) + 
  geom_bar(stat="identity") +
  ylab("Coleção") + xlab("Arrecadação (em bilhões)")

# Coleções que mais arrecadaram por década -------------------------------------
cond = is.na(movies_root$belongs_to_collection)
only_collections = movies_root[!cond,]

collections_revenue <- only_collections %>%
  select(belongs_to_collection, revenue, release_year) %>%
  group_by(belongs_to_collection) %>%
  summarise(total_revenue=sum(revenue), year=min(release_year)) %>%
  arrange(desc(total_revenue)) %>%
  filter(year>1950)
collections_revenue$belongs_to_collection <- paste(collections_revenue$belongs_to_collection,
                                                   " (", collections_revenue$year, ")", sep="")
collections_revenue$total_revenue <- collections_revenue$total_revenue/1000000000
collections_revenue$decade <- as.integer(collections_revenue$year/10) * 10

collections_revenue_decade <- collections_revenue[order(collections_revenue$total_revenue, decreasing=T), ]
collections_revenue_decade <- by(collections_revenue_decade, collections_revenue_decade["decade"], head, n=3)
collections_revenue_decade <- Reduce(rbind, collections_revenue_decade)

ggplot(collections_revenue_decade, aes(x=decade,y=total_revenue,fill=belongs_to_collection)) + 
  geom_bar(stat="identity", position = "dodge") + guides(fill=FALSE) +
  xlab("Year") + ylab("Revenue (in billions)") + labs(fill="Collection") +
  geom_label_repel(aes(label=belongs_to_collection),
                   segment.color="grey50")

# Coleções mais bem avaliadas --------------------------------------------------
cond = is.na(movies_root$belongs_to_collection)
only_collections = movies_root[!cond,]

collections_rating <- only_collections %>%
  select(belongs_to_collection, release_year, vote_average, vote_count) %>%
  group_by(belongs_to_collection) %>%
  summarise(average_rating=mean(vote_average), 
            year=min(release_year),
            average_count=mean(vote_count),
            count=n()) %>%
  arrange(desc(average_rating)) %>%
  filter(count>1, average_count>500, year>1959)

collections_rating$belongs_to_collection <- paste(collections_rating$belongs_to_collection,
                                                   " (", collections_rating$year, ")", sep="")

ggplot(collections_rating[1:10,], aes(average_rating, belongs_to_collection, fill=belongs_to_collection)) + 
  geom_bar(stat="identity") + scale_fill_discrete(guide=FALSE) +
  xlab("Collections") + ylab("Average rating") +
  coord_cartesian(xlim=c(7,8.5))

# Coleções mais bem avaliadas por década ---------------------------------------
cond = is.na(movies_root$belongs_to_collection)
only_collections = movies_root[!cond,]

collections_rating <- only_collections %>%
  select(belongs_to_collection, release_year, vote_average, vote_count) %>%
  group_by(belongs_to_collection) %>%
  summarise(average_rating=mean(vote_average), 
            year=min(release_year),
            average_count=mean(vote_count),
            count=n()) %>%
  arrange(desc(average_rating)) %>%
  filter(count>1, average_count>500, year>1959)

collections_rating$belongs_to_collection <- paste(collections_rating$belongs_to_collection,
                                                  " (", collections_rating$year, ")", sep="")
collections_rating$decade <- as.integer(collections_rating$year/10) * 10

better_rating_decade <- collections_rating[order(collections_rating$average_rating, decreasing=T), ]
better_rating_decade <- by(better_rating_decade, better_rating_decade["decade"], head, n=3)
better_rating_decade <- Reduce(rbind, better_rating_decade)

ggplot(better_rating_decade,
       aes(x=as.character(decade),
           y=as.numeric(average_rating),
           fill=belongs_to_collection)) + 
  geom_bar(stat="identity", position = "dodge") +
  xlab("Decade") + ylab("Average rating") + labs(fill="Collection") +
  coord_cartesian(ylim=c(6,8.5)) + 
  geom_label_repel(aes(label=belongs_to_collection),
                   segment.color="grey50") +
  scale_fill_discrete(guide=FALSE)

# Franquias/Filmes com maior arrecadação ---------------------------------------

better_rating_decade <- collections_rating %>%
  group_by(decade) %>%
  summarise(average_rating=sort(average_rating, decreasing = T)[1:3],
            collection=belongs_to_collection[1:3], year=year[1:3])

franchise_revenue <- movies_root %>%
    group_by(release_year) %>%
    mutate(total_revenue=max(revenue)) %>%
    ungroup() %>%
    filter(revenue==total_revenue) %>%
    select(title, release_year, revenue, belongs_to_collection) %>%
    filter(!is.na(belongs_to_collection))

franchise_revenue$decade <- as.integer(franchise_revenue$release_year/10)*10

franchise_revenue_decade <- franchise_revenue %>%
    group_by(decade) %>%
    summarise(count=n())

ggplot(data=franchise_revenue_decade, aes(x=decade,y=count)) +
    geom_line()

franchise_revenue <- movies_root[order(movies_root$revenue, decreasing=T), ]
franchise_revenue <- by(franchise_revenue, franchise_revenue["release_year"], head, n=50)
franchise_revenue <- Reduce(rbind, franchise_revenue)

franchise_revenue <- franchise_revenue %>%
    filter(!is.na(belongs_to_collection), release_year<2017) %>%
    group_by(release_year) %>%
    summarise(count=n())

ggplot(data=franchise_revenue, aes(x=release_year,y=count)) +
  geom_line() +
  xlab("Ano") + ylab("Quantidade de franquias")

# Quantidade de filmes de língua estrangeira por ano ---------------------------
international_movies <- movies_root %>%
  filter(original_language != "en")

international_count_year <- international_movies %>%
    filter(release_year<2017) %>%
    group_by(release_year) %>%
    summarise(count=n())

ggplot(international_count_year, aes(release_year, count)) +
    geom_line()

# Filmes estrangeiros com mais arrecadação -------------------------------------
international_movies <- movies_root %>%
  filter(original_language != "en")

international_revenues <- international_movies %>%
  select(title, release_year, revenue,
         original_language, vote_average, vote_count) %>%
  arrange(desc(revenue))

# Filmes estrangeiros com melhor avaliação -------------------------------------
international_movies <- movies_root %>%
  filter(original_language != "en")

international_rating <- international_movies %>%
  filter(vote_count>20) %>%
  select(title, release_year, revenue,
         original_language, vote_average, vote_count) %>%
  arrange(desc(vote_average))

# Língua estrangeira com melhor avaliação ao longo dos anos --------------------
international_rating_avg <- international_movies %>%
  filter(release_year>2006, release_year<2017, vote_count>20) %>%
  group_by(original_language, release_year) %>%
  summarise(rating=mean(vote_average), count=n()) %>%
  filter(count>5) %>%
  arrange(desc(rating))


international_rating_year <- international_rating_avg[order(international_rating_avg$rating, decreasing=T), ]
international_rating_year <- by(international_rating_year, international_rating_year["release_year"], head, n=3)
international_rating_year <- Reduce(rbind, international_rating_year)

ggplot(international_rating_year,
       aes(x=release_year,
           y=rating,
           fill=original_language)) + 
  geom_bar(stat="identity", position = "dodge") +
  xlab("Ano") + ylab("Arrecadação (em milhões)") +
  geom_label_repel(aes(label=original_language),
                   segment.color="grey50") +
  coord_cartesian(ylim=c(6,7.5)) +
  scale_fill_discrete(name = "Línguas", labels = c("de: Alemão",  "hi: Hindi",
                                                   "ja: Japonês", "ko: Coreano",
                                                   "pt: Português", "zh: Chinês"))

# Língua estrangeira com melhor arrecadação ao longo dos anos --------------------
international_movies[international_movies$original_language == "ta",]$original_language <- "hi"
international_movies[international_movies$original_language == "cn",]$original_language <- "zh"

View(international_movies %>% filter(original_language=="ta"))

str(international_movies)

international_revenue <- international_movies %>%
  filter(release_year>2006, release_year<2017) %>%
  group_by(original_language, release_year) %>%
  summarise(revenue=sum(revenue)) %>%
  arrange(desc(revenue))

international_revenue_year <- international_revenue[order(international_revenue$revenue, decreasing=T), ]
international_revenue_year <- by(international_revenue_year, international_revenue_year["release_year"], head, n=3)
international_revenue_year <- Reduce(rbind, international_revenue_year)

international_revenue_year$revenue <- international_revenue_year$revenue/1000000

ggplot(international_revenue_year,
       aes(x=release_year,
           y=revenue,
           fill=original_language)) + 
  geom_bar(stat="identity", position = "dodge") +
  xlab("Ano") + ylab("Arrecadação (em milhões)") +
  geom_label_repel(aes(label=original_language),
                   segment.color="grey50") +
  scale_fill_discrete(name = "Línguas", labels = c("zh: Chinês", "de: Alemão",
                                                   "fr: Francês", "hi: Hindi",
                                                   "ja: Japonês", "ko: Coreano",
                                                   "ru: Russo", "sv: Sueco",
                                                   "tr: Turco"))

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




# Relação palavras-chave por ano -----------------------------------------------
keywords <- read.csv("generated_data/keywords.csv", stringsAsFactors = F)

movies_word <- merge(x=movies_root, y=keywords, by.x="id", by.y="movie_id")
movies_word <- select(movies_word, id, belongs_to_collection, title,
                      release_year, release_month, revenue, vote_count,
                      vote_average, keyword)

decade_keyword <- movies_word %>%
  select(release_year, keyword)
decade_keyword$decade <- as.integer(decade_keyword$release_year/10)*10

layout(matrix(c(1, 2), nr=1, byrow=T))
# década 20
word20 <- paste(
  filter(decade_keyword, decade==1920)$keyword,
  collapse = " "
)

word20 <- Corpus(VectorSource(word20))
matrix <- as.matrix(TermDocumentMatrix(word20)) 
word20 <- sort(rowSums(matrix),decreasing=TRUE) 
word20 <- data.frame(word = names(word20),freq=word20)

wordcloud(words = word20$word, freq = word20$freq,
          min.freq=1, max.words=50,
          random.order=F, rot.per=0,
          colors=brewer.pal(8, "Dark2"),
          scale=c(5,.9))

# década 30
word30 <- paste(
  filter(decade_keyword, decade==1930)$keyword,
  collapse = " "
)

word30 <- Corpus(VectorSource(word30))
matrix <- as.matrix(TermDocumentMatrix(word30)) 
word30 <- sort(rowSums(matrix),decreasing=TRUE) 
word30 <- data.frame(word = names(word30),freq=word30)

wordcloud(words = word30$word, freq = word30$freq,
          min.freq=1, max.words=50,
          random.order=F, rot.per=0,
          colors=brewer.pal(8, "Dark2"),
          scale=c(4,.9))

# década 40
word40 <- paste(
  filter(decade_keyword, decade==1940)$keyword,
  collapse = " "
)

word40 <- Corpus(VectorSource(word40))
matrix <- as.matrix(TermDocumentMatrix(word40)) 
word40 <- sort(rowSums(matrix),decreasing=TRUE) 
word40 <- data.frame(word = names(word40),freq=word40)

wordcloud(words = word40$word, freq = word40$freq,
          min.freq=1, max.words=50,
          random.order=F, rot.per=0,
          colors=brewer.pal(8, "Dark2"),
          scale=c(4,0.9))

# década 50
word50 <- paste(
  filter(decade_keyword, decade==1950)$keyword,
  collapse = " "
)

word50 <- Corpus(VectorSource(word50))
matrix <- as.matrix(TermDocumentMatrix(word50)) 
word50 <- sort(rowSums(matrix),decreasing=TRUE) 
word50 <- data.frame(word = names(word50),freq=word50)

wordcloud(words = word50$word, freq = word50$freq,
          min.freq=1, max.words=50,
          random.order=F, rot.per=0,
          colors=brewer.pal(8, "Dark2"),
          scale=c(3,0.4))

# década 60
word60 <- paste(
  filter(decade_keyword, decade==1960)$keyword,
  collapse = " "
)

word60 <- Corpus(VectorSource(word60))
matrix <- as.matrix(TermDocumentMatrix(word60)) 
word60 <- sort(rowSums(matrix),decreasing=TRUE) 
word60 <- data.frame(word = names(word60),freq=word60)

wordcloud(words = word60$word, freq = word60$freq,
          min.freq=1, max.words=50,
          random.order=F, rot.per=0,
          colors=brewer.pal(8, "Dark2"),
          scale=c(3,0.4))

# década 70
word70 <- paste(
      filter(decade_keyword, decade==1970)$keyword,
      collapse = " "
    )

word70 <- Corpus(VectorSource(word70))
matrix <- as.matrix(TermDocumentMatrix(word70)) 
word70 <- sort(rowSums(matrix),decreasing=TRUE) 
word70 <- data.frame(word = names(word70),freq=word70)

wordcloud(words = word70$word, freq = word70$freq,
          min.freq=1, max.words=100,
          random.order=F, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

# década 80
word80 <- paste(
  filter(decade_keyword, decade==1980)$keyword,
  collapse = " "
)

word80 <- Corpus(VectorSource(word80))
matrix <- as.matrix(TermDocumentMatrix(word80)) 
word80 <- sort(rowSums(matrix),decreasing=TRUE) 
word80 <- data.frame(word = names(word80),freq=word80)

wordcloud(words = word80$word, freq = word80$freq,
          min.freq=1, max.words=100,
          random.order=F, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

# década 90
word90 <- paste(
  filter(decade_keyword, decade==1990)$keyword,
  collapse = " "
)

word90 <- Corpus(VectorSource(word90))
matrix <- as.matrix(TermDocumentMatrix(word90)) 
word90 <- sort(rowSums(matrix),decreasing=TRUE) 
word90 <- data.frame(word = names(word90),freq=word90)

wordcloud(words = word90$word, freq = word90$freq,
          min.freq=1, max.words=100,
          random.order=F, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

# década 00
word00 <- paste(
  filter(decade_keyword, decade==2000)$keyword,
  collapse = " "
)

word00 <- Corpus(VectorSource(word00))
matrix <- as.matrix(TermDocumentMatrix(word00)) 
word00 <- sort(rowSums(matrix),decreasing=TRUE) 
word00 <- data.frame(word = names(word00),freq=word00)

wordcloud(words = word00$word, freq = word00$freq,
          min.freq=1, max.words=100,
          random.order=F, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

# década 10
word10 <- paste(
  filter(decade_keyword, decade==2010)$keyword,
  collapse = " "
)

word10 <- Corpus(VectorSource(word10))
matrix <- as.matrix(TermDocumentMatrix(word10)) 
word10 <- sort(rowSums(matrix),decreasing=TRUE) 
word10 <- data.frame(word = names(word10),freq=word10)

wordcloud(words = word10$word, freq = word10$freq,
          min.freq=1, max.words=100,
          random.order=F, rot.per=0,
          colors=brewer.pal(8, "Dark2"))

# Filmes mais bem sucedidos da era de ouro -------------------------------------
golden_age <- movies_root %>%
    select(title, release_year, revenue, vote_count, vote_average) %>%
    filter(release_year>=1920, release_year<1960, vote_count>50)

golden_age$decade <- as.integer(golden_age$release_year/10)*10
golden_age$revenue <- golden_age$revenue/1000000

golden_revenue_decade <- golden_age %>%
    group_by(decade) %>%
    mutate(top.revenue=max(revenue)) %>%
    ungroup() %>%
    filter(revenue==top.revenue)
golden_revenue_decade <- golden_revenue_decade[-4,]

ggplot(golden_revenue_decade,
       aes(x=decade,
           y=revenue,
           fill=title)) + 
  geom_bar(stat="identity", position = "dodge") +
  xlab("Genres") + ylab("Average rating") + labs(fill="Filmes") +
  scale_y_log10()

# Crescimento de Hollywood -----------------------------------------------------
grow_count <- movies_root %>%
    select(release_year, revenue, production_countries) %>%
    filter(release_year<1940, release_year > 1890, production_countries!="[]") %>%
    group_by(release_year,production_countries) %>%
    summarise(count=n(), revenue=sum(revenue))

grow_holly <- grow_count %>% 
    filter(production_countries=="United States of America") %>%
    select(release_year, count, production_countries)
grow_rest <- grow_count %>% 
    filter(production_countries!="United States of America") %>%
    group_by(release_year) %>%
    summarise(count=sum(count), production_countries="Rest of the World")

ggplot(data=grow_holly, mapping=aes(x=release_year,y=count)) +
    geom_line(color='red') +
    geom_line(data=grow_rest, mapping=aes(x=release_year, y=count), color='steelblue') +
    xlab("Ano") + ylab("Quantidade de filmes lançados") +
    scale_fill_manual(values=c("red", "steelblue"),
                      breaks=c("United States of America", "Rest of the World"),
                      labels=c("United States", "Rest of the World"))

# Maiores produtoras -----------------------------------------------------------
production_companies <- read.csv("generated_data/production_metadata.csv", stringsAsFactors = F)
movies_productions <- read.csv("generated_data/movies_productions.csv", stringsAsFactors = F)

movies_productions <- merge(movies_productions, production_companies, by.x="production_id", by.y="id")
movies_productions <- merge(movies_root, movies_productions, by.x="id",by.y="movie_id")

count_production <- movies_productions %>%
    group_by(name) %>%
    summarise(count=n()) %>%
    arrange(desc(count))

