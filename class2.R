library(tidyverse)
library(readr)
df <- read_csv("Physio-chemical.csv", col_types = cols(Station = col_factor(levels = c("1", 
                                                                                       "2", "3", "4")), month = col_factor(levels = c("May", 
                                                                                                                                      "June", "July", "August", "September", 
                                                                                                                                      "October"))))
View(df)

#gathering, spreading, 
#pivotlonger, pivotwider

df1 <- pivot_longer(data= df, cols = 5:20, names_to = "parameters", values_to = "measurement")
view(df1)

df2 <- pivot_wider(data = df1, names_from = parameters, values_from = measurement)
view(df2)

#select
#df3 <- river %>% station %>% flocculant %>% filtering %>% chlorinate %>% distribute
df3 <- df %>% select(Station, month, Temp, DO)
view(df3)
# filter
df4 <- df3 %>% filter(month == "May")
view(df4)
#mutate
df5 <- df4 %>% mutate(ratio = DO/Temp)
view(df5)
#summarise
#select, filter, mutate, summarise, group-by, arrange, rename
df6 <- df5 %>% rename(proportion = ratio)
view(df6)

df7 <- df %>% group_by(month) %>% summarise(Temp = mean(Temp))
view(df7)

df8 <- df %>% select(Station, month, DO, Temp) %>% 
  filter(month == c("June", "September") ) %>% mutate(ratio = DO/Temp) %>% 
  rename(proportion = ratio) %>% group_by(month) %>% 
  summarise(proportion = sum(proportion))
view(df8)

#dplyr

df9 <- summarySE(df1, measurevar = "measurement", 
                 groupvars = c("month", "Station"))
view(df9)
