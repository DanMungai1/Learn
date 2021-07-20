library(tidyverse)
library(readr)
df <- read_csv("data/Physio-chemical.csv", 
               col_types = cols(Station = col_factor(levels = c("1", 
                                                                "2", "3", "4")), month = col_factor(levels = c("May", 
                                                                                                               "June", "July", "August", "September", 
                                                                                                               "October"))))
View(df)

#ANOVA, One way analysis of variance
#Ho: the mean temp is equal for all months
#HA: atleast one month has a different mean temp

df <- df %>% mutate(month = str_replace_all(month, "September", "Sept"),
                    month = str_replace_all(month, "August", "Aug"),
                    month = str_replace_all(month, "October", "Oct"))
boxplot(Temp~month, data = df)
Anov <- aov(Temp ~ month, data = df)
Anov
summary(Anov)
attributes(Anov)
Anov$coefficients
Anov$residuals
Anov$model

#Post hoc test
TukeyHSD(Anov)
plot(TukeyHSD(Anov), las = 1)

Anov2 <- aov(Temp ~ Station, data = df)
summary(Anov2)

TukeyHSD(Anov2)
plot(TukeyHSD(Anov2))
