##load packages
library(tidyverse)
library(readr)
#load the data
df <- read_csv(file.choose())
view(df)

df1 <- df %>% filter(num_rank <= 10)
attach(df1)

