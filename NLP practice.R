library(textreadr)
answer <- read_document(file="/Users/bodmeryao/Desktop/DD/Text Analytics/answers.docx")

a <- 54 #how many observations to you have
  b <- 6 #how many variables do you have
  my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- answer[i*b+z-b]
  }#closing z loop
}#closing i loop
  
library(stringr)
library(tidytext)
library(dplyr)
library(wordcloud)
library(ggplot2)
data(stop_words)
  
for (i in 1:6) {
  mydf <- data_frame(line=1:a, text=my_df[,i])
  print(mydf)
  # Clean up the data for Q6, avoiding affects of TV issue
  if (i == 6) {
    mydf$text <- gsub('YouTube TV', 'Youtube', mydf$text)
    mydf$text <- gsub('sling TV', 'sling', mydf$text)
    mydf$text <- gsub('NBA TV', 'NBATV', mydf$text)
    mydf$text <- gsub('Apple TV', 'AppleTV', mydf$text)
    mydf$text <- gsub('game center', 'game_center', mydf$text)
    mydf$text <- gsub('television', 'TV', mydf$text)
    mydf$text <- gsub('streaming TV', 'streaming', mydf$text)
  }
  # Clean up the data for Q5
  if (i == 5) {
    mydf$text <- gsub('once', '1', mydf$text)
    mydf$text <- gsub('Once', '1', mydf$text)
    mydf$text <- gsub('every day', '365', mydf$text)
    mydf$text <- gsub('Every day', '365', mydf$text)
    mydf$text <- gsub('Everyday', '365', mydf$text)
    mydf$text <- gsub('Every week', '52', mydf$text)
    mydf$text <- gsub('weekly', '52', mydf$text)
    mydf$text <- gsub('every week', '52', mydf$text)
    mydf$text <- gsub('every weekend', '52', mydf$text)
    mydf$text <- gsub('twice', '2', mydf$text)
    mydf$text <- gsub('Twice', '2', mydf$text)
    mydf$text <- gsub('one', '1', mydf$text)
    mydf$text <- gsub('everyday', '365', mydf$text)
    mydf$text <- gsub('five', '5', mydf$text)
    for (j in 1:a) {
      if (grepl('week', mydf$text[j])) {
        if (grepl('1', mydf$text[j])) {
          mydf$text[j] <- "52"
        }
        if (grepl('2', mydf$text[j])) {
          mydf$text[j] <- '104'
        }
        if (grepl('3', mydf$text[j])) {
          mydf$text[j] <- '156'
        }
        if (grepl('4', mydf$text[j])) {
          mydf$text[j] <- '208'
        }
        if (grepl('5', mydf$text[j])) {
          mydf$text[j] <- '260'
        }
        if (grepl('7', mydf$text[j])) {
          mydf$text[j] <- '365'
        }
        } # close loop of converting frequency
      if (grepl('month', mydf$text[j])) {
        mydf$text[j] <- '12'
      }
      } # close loop of searching line by line
    } # close loop of i=5
  frequencies_tokens_nostop <- mydf %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    count(word, sort=TRUE)
  print(i)
  if (i == 4) {
    frequencies_tokens_nostop <- frequencies_tokens_nostop %>%
      filter(word != 'sport') %>%
      filter(word != 'favorite') %>%
      filter(word != 'india') %>%
      filter(word != 'sports') %>%
      filter(n > 1)
    sport <- ggplot(data = frequencies_tokens_nostop,aes(x = word, y = n, fill = word)) +
        geom_bar(stat = "identity") +
        scale_color_brewer(palette="Dark2") +
        geom_text(aes(label=n), vjust=1.6, color="black", size=3.5) +
        xlab('Sport')
    print(sport)
  }
  if (i == 5) {
    frequencies_tokens_nostop <- frequencies_tokens_nostop %>%
      filter(word != 'watch') %>%
      filter(word != 'game') %>%
      filter(word != 'soccer') %>%
      filter(n > 1)
    Fre <- ggplot(data = frequencies_tokens_nostop,aes(x = word, y = n, fill = word)) +
      geom_bar(stat = "identity") +
      scale_color_brewer(palette="Dark2") +
      geom_text(aes(label=n), vjust=1.6, color="black", size=3.5) +
      xlab('Frequency per year')
    print(Fre)
  }
  if (i == 6) {
    benchmark <- data.frame(word = c('tv','streaming','youtube','hulu','espn','radio','website','apple','bar','reddit'),n = 1:10)
    frequencies_tokens_nostop <- frequencies_tokens_nostop %>%
      inner_join(benchmark, frequencies_tokens_nostop,by = 'word')
    frequencies_tokens_nostop %>%
      with(wordcloud(word, n.x, max.words = 100, min.freq = 1))
  }
}

# Col between female and male about 'sports' and 'frequency'
# frequency
# Seperate tidy dataset based on genders
my_df$V1 <- gsub('Male', '1', my_df$V1)
my_df$V1 <- gsub('Female', '0', my_df$V1)
my_df$V1 <- gsub('female', '0', my_df$V1)

# clean-up the dataset
mydf <- data_frame(line=1:a, text=my_df[,5])
mydf$text <- gsub('once', '1', mydf$text)
mydf$text <- gsub('Once', '1', mydf$text)
mydf$text <- gsub('every day', '365', mydf$text)
mydf$text <- gsub('Every day', '365', mydf$text)
mydf$text <- gsub('Everyday', '365', mydf$text)
mydf$text <- gsub('Every week', '52', mydf$text)
mydf$text <- gsub('weekly', '52', mydf$text)
mydf$text <- gsub('every week', '52', mydf$text)
mydf$text <- gsub('every weekend', '52', mydf$text)
mydf$text <- gsub('twice', '2', mydf$text)
mydf$text <- gsub('Twice', '2', mydf$text)
mydf$text <- gsub('one', '1', mydf$text)
mydf$text <- gsub('everyday', '365', mydf$text)
mydf$text <- gsub('five', '5', mydf$text)
for (j in 1:a) {
  if (grepl('week', mydf$text[j])) {
    if (grepl('1', mydf$text[j])) {
      mydf$text[j] <- "52"
    }
    if (grepl('2', mydf$text[j])) {
      mydf$text[j] <- '104'
    }
    if (grepl('3', mydf$text[j])) {
      mydf$text[j] <- '156'
    }
    if (grepl('4', mydf$text[j])) {
      mydf$text[j] <- '208'
    }
    if (grepl('5', mydf$text[j])) {
      mydf$text[j] <- '260'
    }
    if (grepl('7', mydf$text[j])) {
      mydf$text[j] <- '365'
    }
  } # close loop of converting frequency
  if (grepl('month', mydf$text[j])) {
    mydf$text[j] <- '12'
  }
} # close loop of searching line by line

f <- 1
m <- 1
freq_m <- data.frame()
freq_f <- data.frame()
for (i in 1:a) {
  if (grepl('1', my_df$V1[i])) {
    freq_m[m,1] <- mydf$text[i]
    m <- m + 1
  }
  else {
    freq_f[f,1] <- mydf$text[i]
    f <- f + 1
  }
}
# tokenize both dataset
tidy_freq_f <- freq_f %>%
  unnest_tokens(word, V1) %>%
  anti_join(stop_words)
tidy_freq_m <- freq_m %>%
  unnest_tokens(word, V1) %>%
  anti_join(stop_words)

library(tidyr)
frequency <- bind_rows(mutate(tidy_freq_m, gender = 'Male'),
                       mutate(tidy_freq_f, gender = 'Female')) %>%
  count(gender, word) %>%
  group_by(gender) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  filter(word != 'game') %>%
  filter(word != 'soccer') %>%
  filter(word != 'watch') %>%
  filter(word != 'times') %>%
  spread(gender, proportion) %>%
  gather(gender, proportion,'Male')

library(scales)
print(ggplot(frequency, aes(x = proportion, y = `Female`, 
                      color = abs(`Female` - proportion))) + 
  geom_abline(color = "black", lty = 2) + 
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_x_log10(labels = percent_format()) + 
  scale_y_log10(labels = percent_format()) + 
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "black", high = "black") + 
  facet_wrap(~gender, ncol = 2) + 
  theme(legend.position="none")
)

# Sport
# Seperate tidy dataset based on genders
my_df$V1 <- gsub('Male', '1', my_df$V1)
my_df$V1 <- gsub('Female', '0', my_df$V1)
my_df$V1 <- gsub('female', '0', my_df$V1)

# clean-up the dataset
mydf <- data_frame(line=1:a, text=my_df[,4])
f <- 1
m <- 1
spt_m <- data.frame()
spt_f <- data.frame()
for (i in 1:a) {
  if (grepl('1', my_df$V1[i])) {
    spt_m[m,1] <- mydf$text[i]
    m <- m + 1
  }
  else {
    spt_f[f,1] <- mydf$text[i]
    f <- f + 1
  }
}
# tokenize both dataset
tidy_spt_f <- spt_f %>%
  unnest_tokens(word, V1) %>%
  anti_join(stop_words)
tidy_spt_m <- spt_m %>%
  unnest_tokens(word, V1) %>%
  anti_join(stop_words)

# clean up the data
tidy_spt_f <- tidy_spt_f %>%
  filter(word != 'sport') %>%
  filter(word != 'favorite') %>%
  filter(word != 'india') %>%
  filter(word != 'sports')
tidy_spt_m <- tidy_spt_m %>%
  filter(word != 'sport') %>%
  filter(word != 'favorite') %>%
  filter(word != 'india') %>%
  filter(word != 'sports')

library(tidyr)
frequency <- bind_rows(mutate(tidy_spt_m, gender = 'Male'),
                       mutate(tidy_spt_f, gender = 'Female')) %>%
  count(gender, word) %>%
  group_by(gender) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(gender, proportion) %>%
  gather(gender, proportion,'Male')

library(scales)
print(ggplot(frequency, aes(x = proportion, y = `Female`, 
                      color = abs(`Female` - proportion))) + 
  geom_abline(color = "black", lty = 2) + 
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_x_log10(labels = percent_format()) + 
  scale_y_log10(labels = percent_format()) + 
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "black", high = "black") + 
  facet_wrap(~gender, ncol = 2) + 
  theme(legend.position="none")
)

# Country and Sport
# Get ready the country dataset
mydf_country <- data_frame(line=1:a, country=my_df[,3])
mydf_country$country <- gsub('United States', 'usa', mydf_country$country)
mydf_country$country <- gsub('South Korea', 'SouthKorea', mydf_country$country)
mydf_country$country <- gsub('Puerto Rico', 'PuertoRico', mydf_country$country)
mydf_country$country <- gsub(' Dominican Republic', ' DominicanRepublic', mydf_country$country)
mydf_country$country <- gsub(' Hong Kong', ' HongKong', mydf_country$country)

tidy_country <- mydf_country %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, country) %>%
  filter(word != "i’m") %>%
  anti_join(stop_words)
# Get ready the sport dataset
mydf_spt <- data_frame(line=1:a, sport = my_df[,4])
mydf_spt$sport <- gsub('Ice hockey', 'hockey', mydf_spt$sport)
tidy_spt <- mydf_spt %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, sport) %>%
  anti_join(stop_words) %>%
  filter(word != 'sport') %>%
  filter(word != 'favorite') %>%
  filter(word != 'india') %>%
  filter(word != 'sports')

# join the two dataset
tidy_cs <- left_join(tidy_country,tidy_spt, by = "linenumber")
tidy_df_cs <- data.frame(country = tidy_cs$word.x, sport = tidy_cs$word.y) %>%
  count(country, sport) %>%
  group_by(country)
###################################################################
mydf <- data_frame(line=1:a, text=my_df[,5])
# Clean up the data for Q5
  mydf$text <- gsub('once', '1', mydf$text)
  mydf$text <- gsub('Once', '1', mydf$text)
  mydf$text <- gsub('every day', '365', mydf$text)
  mydf$text <- gsub('Every day', '365', mydf$text)
  mydf$text <- gsub('Everyday', '365', mydf$text)
  mydf$text <- gsub('Every week', '52', mydf$text)
  mydf$text <- gsub('weekly', '52', mydf$text)
  mydf$text <- gsub('every week', '52', mydf$text)
  mydf$text <- gsub('every weekend', '52', mydf$text)
  mydf$text <- gsub('twice', '2', mydf$text)
  mydf$text <- gsub('Twice', '2', mydf$text)
  mydf$text <- gsub('one', '1', mydf$text)
  mydf$text <- gsub('everyday', '365', mydf$text)
  mydf$text <- gsub('five', '5', mydf$text)
  for (j in 1:a) {
    if (grepl('week', mydf$text[j])) {
      if (grepl('1', mydf$text[j])) {
        mydf$text[j] <- "52"
      }
      if (grepl('2', mydf$text[j])) {
        mydf$text[j] <- '104'
      }
      if (grepl('3', mydf$text[j])) {
        mydf$text[j] <- '156'
      }
      if (grepl('4', mydf$text[j])) {
        mydf$text[j] <- '208'
      }
      if (grepl('5', mydf$text[j])) {
        mydf$text[j] <- '260'
      }
      if (grepl('7', mydf$text[j])) {
        mydf$text[j] <- '365'
      }
    } # close loop of converting frequency
    if (grepl('month', mydf$text[j])) {
      mydf$text[j] <- '12'
    }
  } # close loop of searching line by line
  tidy_freq <- mydf %>%
    mutate(linenumber = row_number()) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    filter(word != 'watch') %>%
    filter(word != 'nba') %>%
    filter(word != 'basketball') %>%
    filter(word != 'soccer') %>%
    filter(word != 'times') %>%
    filter(word != 'game') %>%
    filter(word != 'sports') %>%
    filter(word != 'days') %>%
    filter(word != 'weeks') %>%
    filter(word != 'time') %>%
    filter(word != 'seldom') %>%
    filter(word != 'friends') %>%
    filter(word != 'sport') %>%
    filter(word != 'live') %>%
    filter(word != 'week') %>%
    filter(word != 'rarely') %>%
    filter(word != "don’t") 

  tidy_cf <- left_join(tidy_country,tidy_freq, by = "linenumber")    
  tidy_cf_df <- data.frame(country = tidy_cf$word.x, frequency = tidy_cf$word.y) %>%
    count(country, frequency)%>%
    group_by(country) %>%
    filter(frequency!='NA')
  


    
  
  