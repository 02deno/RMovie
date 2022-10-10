rm(list = ls())

#necessary packages
install.packages('pacman')
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr)
install.packages('ggpubr')

library(ggpubr)
library(readr)
library(dplyr)

#read the table
movies <- read.csv('/Users/pc/Desktop/tmdb_5000_movies.csv')

#information about the table
head(movies)
str(movies)
summary(movies)

# data cleaning
movies = movies %>% select(-homepage,-overview,-status,-id,-original_title,-tagline)

movies <- distinct(movies)
dim(movies)

original_col_names <- colnames(movies)
print(original_col_names)

movies <- movies[apply(movies!=0, 1, all),]

#json format
typeof(movies$genres)
head(movies$genres)

library(stringr)
a <-str_detect(movies$genres,"Action") & str_detect(movies$genres,"Adventure")
a
length(a[a== TRUE])

b <-str_detect(movies$genres,"Adventure")
length(b[b== TRUE])
which(b[b== TRUE])

c <- !str_detect(movies$genres,"Adventure")
c

z <- movies$genres == '[{"id": 28, "name": "Action"}]'
length(z[z== TRUE])

bn <- str_detect(movies$keywords,"culture clash")
bn
length(bn[bn== TRUE])



deneme <- str_detect(movies$genres,"Action") & str_detect(movies$keywords,"culture clash")
deneme
length(deneme[deneme== TRUE])


length(unique(movies$original_language))
diller = unique(movies$original_language)
sorted_diller = as.data.frame(table(movies$original_language)) %>% arrange(desc(Freq))
plot(sorted_diller)

garfield <- ggplot(sorted_diller, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
    ylab("Filmde kullanılan diller?") + 
  xlab("Diller")
  
garfield

sorted_garfield <- ggplot(sorted_diller, aes(x = reorder(Var1,-Freq), y = Freq)) +
  geom_bar(stat = "identity",fill = "#0073C2FF") +
  ylab("En çok kullanılan 3 dil hangisi?") + 
  xlab("Diller")

sorted_garfield

top_languages_by_total_movies <- sorted_diller %>%
  group_by(Var1) %>%
  summarize(total_movies = max(Freq)) %>%
  top_n(9,total_movies)
top_languages_by_total_movies


#datadan 0 içeren filmleri çıkarınca 33 film other
#dataya 0 içeren filmler dahilolunca 59 film other
new_row = data.frame(Var1 = "other",
                     total_movies = 33) 
all_languages_by_total_movies = rbind(top_languages_by_total_movies,new_row)
all_languages_by_total_movies

ggpie(
  top_languages_by_total_movies, x = "total_movies", label = "Var1",
  lab.pos = "in", lab.font = list(color = "white"), 
  fill = "Var1", color = "white",
  palette = "jco"
)



topten <- ggplot(top_languages_by_total_movies, aes(x = reorder(Var1,-total_movies), y = total_movies)) +
  geom_bar(stat = "identity",fill = "#0073C2FF") +
  ylab("En çok kullanılan 10 dil hangisi?") + 
  xlab("Diller")

topten



min_budget = min(movies$budget)
max_budget = max(movies$budget)
mean_budget = mean(movies$budget)


min_vote = min(movies$vote_average)
max_vote = max(movies$vote_average)
mean_vote = mean(movies$vote_average)



english_movies <- filter(movies, original_language == 'en')
english_movies
mean(english_movies$vote_average)
min(english_movies$vote_average)
max(english_movies$vote_average)
median(english_movies$vote_average)
var(english_movies$vote_average)
sd(english_movies$vote_average)
cor(english_movies$vote_average,english_movies$budget)
typeof(english_movies)

Zx <- (english_movies$vote_average-mean(english_movies$vote_average))/sd(english_movies$vote_average)
Zx

french_movies <- filter(movies, original_language == 'fr')
french_movies
mean(french_movies$vote_average)
cor(french_movies$vote_average,french_movies$budget)

spanish_movies <- filter(movies, original_language == 'es')
spanish_movies
mean(spanish_movies$vote_average)
cor(spanish_movies$vote_average,spanish_movies$budget)

german_movies <- filter(movies, original_language == 'de')
german_movies
mean(german_movies$vote_average)
cor(german_movies$vote_average,german_movies$budget)

cor(movies$vote_average,movies$budget)

ggplot(english_movies, aes(x=vote_average, y=budget)) + geom_point(shape=18) +
  labs(title="İngilice olan filmlerdeki göre oy ve bütçe arasındaki ilişki",x="Oy", y = "Bütçe") 

ggplot(french_movies, aes(x=vote_average, y=budget)) + geom_point(shape=18, color="blue")+
  labs(title="Fransızca olan filmlerdeki göre oy ve bütçe arasındaki ilişki",x="Oy", y = "Bütçe")

ggplot(spanish_movies, aes(x=vote_average, y=budget)) + geom_point(shape=18, color="red") +
  labs(title="İspanyolca olan filmlerdeki göre oy ve bütçe arasındaki ilişki",x="Oy", y = "Bütçe")

ggplot(german_movies, aes(x=vote_average, y=budget)) + geom_point(shape=18, color="green") +
  labs(title="Almanca olan filmlerdeki göre oy ve bütçe arasındaki ilişki",x="Oy", y = "Bütçe")

ggplot(movies, aes(x=vote_average, y=budget)) + geom_point(shape=18, color="pink") +
  labs(title="Bütün filmlerdeki göre oy ve bütçe arasındaki ilişki",x="Oy", y = "Bütçe")


four_movie_languages <- movies %>%
  filter(original_language %in% c("en","fr","es","de") )
four_movie_languages

ggplot(four_movie_languages, aes(x=vote_average, y=budget, shape=original_language,color=original_language,size=original_language)) +
  geom_point() +
  scale_size_manual(values=c(3,.5,3,3)) +
  scale_shape_manual(values=c(15,3,17,16)) +
  labs(title="4 farklı dilde dillere göre oy ve bütçe arasındaki ilişki",
       x="Oy", y = "Bütçe") 

three_movie_languages <- movies %>%
  filter(original_language %in% c("fr","es","de") )
three_movie_languages

ggplot(three_movie_languages, aes(x=vote_average, y=budget, shape=original_language,color=original_language)) +
  geom_point() +
  scale_shape_manual(values=c(15,17,16)) +
  labs(title="3 farklı dilde dillere göre oy ve bütçe arasındaki ilişki",
       x="Oy", y = "Bütçe")

min_vote = movies[which.min(movies$vote_average),]
max_vote = movies[which.max(movies$vote_average),]


#Rplot11
vote <-ggplot(data = movies,aes(x= vote_average))
vote + geom_histogram(binwidth = .1,color = "white",fill = "blue")+
  ggtitle("Histogram for Audience ratings v/s number of movies")+ 
  xlab("Vote")+
  ylab("Total number of Movies")+
  theme(axis.title.x = element_text(color="blue",size = 20),
        axis.title.y = element_text(color="blue",size = 20),
        plot.title = element_text(color="purple",size = 15))

#Rplot12
budget <-ggplot(data = movies,aes(x= budget))
budget + geom_histogram(binwidth = 10000000,color = "white",fill = "pink")+
  ggtitle("Histogram for Budget v/s number of movies")+ 
  xlab("Budget")+
  ylab("Total number of Movies")+
  theme(axis.title.x = element_text(color="pink",size = 20),
        axis.title.y = element_text(color="pink",size = 20),
        plot.title = element_text(color="purple",size = 15))

movies[movies$release_date] <- movies[movies$release_date]
typeof(movies$release_date)
movies$release_date <- substr(movies$release_date, 1, 4) 

#Rplot13
ggplot(movies, aes(x = vote_average, y = budget, colour = release_date)) +
  geom_point()

#Rplot14
date <- movies %>% count(release_date)
ggplot(data=date, aes(x=release_date, y=n, group=1)) +
  geom_line()+
  geom_point()


small_date <- date %>%
  group_by(release_date) %>%
  summarize(total_movies = max(n)) %>%
  top_n(40,total_movies)
small_date


#Rplot15
ggplot(data=small_date, aes(x=release_date, y=total_movies, group=1)) +
  geom_line()+
  geom_point()
