library(syuzhet)
library(vosonSML)
library(tidyverse)
# youtube authentication sets the api key
youtubeAuth <- Authenticate("youtube", apiKey = "AIzaSyDpnYYIWm1mgvKGjCC1mVdbRAfJV3mxU-8")
youtubeVideoIds <- GetYoutubeVideoIDs("https://www.youtube.com/watch?v=o9FLx4Zjwkk")
youtubeData <- youtubeAuth %>%
  Collect(videoIDs = youtubeVideoIds,
          maxComments = 100,
          verbose = FALSE)                                        
comments <- iconv(youtubeData$Comment, to = "utf-8")
sentiments <- get_nrc_sentiment(comments)
senti <- sentiments %>% pivot_longer(cols = 1:10, names_to = "Sentiments") %>% 
  mutate(Sentiments = as.factor(Sentiments)) %>% group_by(Sentiments) %>% 
  summarise(value = sum(value))
ggplot(senti, aes(value, fct_reorder(Sentiments, value), fill = Sentiments)) + geom_col()+
  labs(title = "Youtube Comments on President Uhuru Kenyatta's Last Covid19 Address",
       subtitle = "The comments were hugely positive, lower on anger and average level of anticipation",
       x = "Sentiment Scores",
       y= "Sentiment Category",
       caption = "https://www.youtube.com/watch?v=o9FLx4Zjwkk") +
  theme(plot.title = element_text(hjust = .5, family = "serif"),
      plot.subtitle = element_text(hjust = .5, family = "serif"),
      legend.position = "none")
