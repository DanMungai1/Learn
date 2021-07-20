# Tidy Tuesday 40
# Beyonce & Taylor Swift Lyrics
library(tidyverse)
library(tidytext)
library(readr)
library(wordcloud2)
library(wesanderson)
library(webshot)
# webshot::install_phantomjs()
library("htmlwidgets")
TaylorSwift <- read_csv("data/TaylorSwift.csv")
View(TaylorSwift)
# count NA's
sapply(TaylorSwift, function(x) sum(is.na(x)))

# find each individual word used in line and count how many time it was in the lyrics
Taylor_tokens <- TaylorSwift %>% select(Lyrics) %>%
  unnest_tokens(word, Lyrics) %>% count(word, sort=TRUE)

# remove any stop words using the stopwords dataset
Taylor_clean <- Taylor_tokens %>%
  anti_join(get_stopwords())

# Create wordcloud plot
word_plot <- wordcloud2(Taylor_clean, shape='triangle', size=0.75, 
                        color=rep(wes_palettes$Rushmore1[3:5], nrow(Taylor_clean)/length(wes_palettes$Rushmore1[3:5])))

# save widget in html
saveWidget(word_plot,"Taylor_plot.html",selfcontained = F)

# and in png
webshot("Taylor_plot.html","Taylor_word_plot.png", delay =5)

#plot the counts of words
top10 <- Taylor_clean %>% top_n(10, n)
ggplot(top10, aes(n, fct_reorder(word, n), fill = word)) + 
  geom_col() + 
  geom_text(aes(x = n, label = n, hjust = 1.2)) +
  geom_text(aes(x=0, hjust = 0, y= fct_reorder(word, n), label = word))+
  labs(title = "Top 10 words of Beyonce's Lyrics",
       y = "word", x = "Frequency")+
  theme(plot.title = element_text(hjust = .5, family = "serif"),
        axis.ticks = element_blank(),
        legend.position = "none",
        axis.text.y  = element_blank())

