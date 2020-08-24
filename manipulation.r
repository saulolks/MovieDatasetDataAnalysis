library(rjson)
movies_root = read.csv("original_data/movies_metadata.csv", stringsAsFactors = F)

movies_root[1:50,]
# Tratamento da coluna "genre" -------------------------------------------------
movies = vector()
genres = vector()

for(row in 1:nrow(movies_root)){
  movie_id = movies_root$id[row]
  data = fromJSON(movies_root$genres[row])
  
  movies <- c(movies, rep(movie_id, length(data)))
  for(item in data){
    genres <- c(genres, item$name)
  }
}

movie_genre <- data.frame("movie_id"=movies, "genre"=genres)

write.csv(
  movie_genre,
  file="movie_genre.csv",
  row.names = F
)

# Tratamento de linhas com erro ------------------------------------------------
indexes_to_remove = which(movies_root$adult != "False" & movies_root$adult != "True")
movies_root <- movies_root[-indexes_to_remove,]

# Tratamento da coluna "belongs_to_collection" ---------------------------------

for(row in 1:nrow(movies_root)){
  data = movies_root$belongs_to_collection[row]
  if(data != ""){
    collection = fromJSON(data)
    movies_root$belongs_to_collection[row] <- collection$name
  }else{
    movies_root$belongs_to_collection[row] <- NA
  }
}

write.csv(
  movies_root,
  file="Generated-Data/movies_metadata.csv",
  row.names = F
)

# Tratamento da coluna "production_companies" ----------------------------------
movies = vector()
production_ids = vector()
production_names = vector()

for(row in 1:nrow(movies_root)){
  movie_id = movies_root$id[row]
  if(movies_root$production_companies[row] != ""){
    data = fromJSON(movies_root$production_companies[row])
    
    movies <- c(movies, rep(movie_id, length(data)))
    for(item in data){
      production_ids <- c(production_ids, item$id)
      production_names <- c(production_names, item$name)
    }
    print(length(movies))
    print(length(production_names))
  }
}

production_metadata <- data.frame("id"=production_ids, "name"=production_names, stringsAsFactors = F)
indexes <- which(duplicated(production_metadata))
production_metadata <- production_metadata[-indexes,]

movies_productions <- data.frame("movie_id"=movies, "production_id"=production_ids, stringsAsFactors = F)

write.csv(
  production_metadata,
  file="Generated-Data/production_metadata.csv",
  row.names = F
)

write.csv(
  movies_productions,
  file="Generated-Data/movies_productions.csv",
  row.names = F
)

movies_root <- movies_root[,-4]
movies_root <- movies_root[,-12]


# Tratamento da coluna "release_date" ------------------------------------------
library(tidyr)
library(dplyr)

aux <- movies_root %>% separate(col="release_date", into=c("release_year", "release_month", "release_day"), sep="-")
write.csv(
  aux,
  file="Generated-Data/movies_metadata.csv",
  row.names = F
)

# Tratamento de keywords -------------------------------------------------------
keywords <- read.csv("original_data/keywords.csv", stringsAsFactors = F)

movies <- vector()
words <- vector()

for(row in 1:nrow(keywords)){
  movie_id = keywords$id[row]
  data = fromJSON(keywords$keywords[row])
  
  movies <- c(movies, rep(movie_id, length(data)))
  for(item in data){
    words <- c(words, item$name)
  }
}

key_data <- data.frame("movie_id"=movies, "keyword"=words)
write.csv(
  key_data,
  file="generated_data/keywords.csv",
  row.names = F
)

# Tratamento de production countries
movies_root = read.csv("generated_data/movies_metadata.csv", stringsAsFactors = F)
for(row in 1:nrow(movies_root)){
  data = movies_root$production_countries[row]
  if(data != ""){
    countries = fromJSON(data)

    for(country in countries){
      movies_root$production_countries[row] <- country$name
      break
    }
  }else{
    movies_root$belongs_to_collection[row] <- NA
  }
}

write.csv(
  movies_root,
  file="generated_data/movies_metadata.csv",
  row.names = F
)
