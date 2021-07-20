library(magrittr)
library(vosonSML)
library(tidytext)
library(tidyverse)
library(syuzhet)
# twitter authentication creates an access token as part of the auth object
twitterAuth <- Authenticate("twitter", 
                            appName = "DanMungai",
                            apiKey = "i0khRRJTY8tPLkQJGVgRZU6Nn",
                            apiSecret = "HeDvhN9M7AxX78CFRBMjCqJrByB4HrVeiFH29mm0RfgEmwHiCA",
                            accessToken = "532108678-rT1kXHGQjx7zG9xZjXL6HUOoSqa4qoYoED6DYAK9",
                            accessTokenSecret = "DN4JN2XlE2bA4K9cSqZZ6oyDdgCZ9FeYwMmqcj5Q9zmJh")

# save the object to file after authenticate
saveRDS(twitterAuth, file = "twitter_auth")
twitterAuth <- readRDS("twitter_auth")
# collect 100 recent tweets for the hashtag #auspol
twitterData <- twitterAuth %>%
  Collect(searchTerm = "#RutoByForce",
          searchType = "recent",
          numTweets = 100,
          includeRetweets = FALSE,
          retryOnRateLimit = TRUE,
          writeToFile = FALSE,
          verbose = TRUE)

activityNetwork <- twitterData %>% 
  Create("activity") %>%
  Graph(writeToFile = TRUE) %>%
  summary()

unnest_tokens <- twitterData %>% 
  unnest_tokens(word, text) %>% count(word, sort = TRUE)
comments <- iconv(twitterData$text, to = "utf-8") 
twitts <- get_nrc_sentiment(comments) %>% 
  pivot_longer(1:10, names_to = "tweets") %>% group_by(tweets) %>% 
  summarise(value = sum(value))

ggplot(twitts, aes(value, fct_reorder(tweets, value), fill = tweets)) +
  geom_col() +
  labs(title = "Mafeelings za wasee kupitia #RutoByForce tweets",
       subtitle = "Positive and Trust",
       x = "",
       y = "Sentiments",
       caption = "#RutoByForce") + theme_fivethirtyeight()+
  theme(plot.title = element_text(hjust = .5, family = "serif", color = "maroon"),
        plot.subtitle = element_text(hjust = .5, family = "serif", color = "red", size = 15),
        legend.position = "none")+
  ggsave("RutoByForce.png", dpi = 300)
  
  
