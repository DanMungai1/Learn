#load packages
library(tidyverse)
library(readr)
library(patchwork)
#Load data
streams <- read_csv(file.choose())

df <- streams %>% filter(num_rank <= 10)
attach(df)
ggplot(df, aes(x= streams, y = fct_reorder(lab_text, streams))) +
  geom_col()

ggplot(df, aes(x= streams, y= fct_reorder(lab_text, streams))) +
  geom_col(width = 0.2, fill = "skyblue") +
geom_text(aes(label = lab_text),x = 0, hjust = 0, 
          position = position_nudge(y = 0.3)) +
  theme(axis.text = element_blank())

ggplot(df, aes(x= streams, y= fct_reorder(lab_text, streams))) +
  geom_col(width = 0.2, fill = "skyblue") +
  geom_text(aes(label = lab_text),x = 0, hjust = 0, 
            position = position_nudge(y = 0.3))+
  geom_text(aes(label = streams_text), hjust = 1.2,
            position = position_nudge(y = 0.3)) +
  theme(axis.text = element_blank())
ggplot(df, aes(x= streams, y = fct_reorder(lab_text, streams))) +
  geom_col(fill = 4) +
  geom_point(shape = 21, size = 20, fill= "white") +
  geom_text(aes(label = streams)) +
  geom_text(aes(label = lab_text),x = 0, hjust = 0) +
  theme(axis.text = element_blank())

ggplot(df, aes(x= streams, y = fct_reorder(lab_text, streams))) +
  geom_col(fill = 3) +
  geom_point(shape = 21, size = 10, fill= "white") +
  geom_text(aes(label = streams))+
  theme(axis.text = element_blank()) +
  coord_polar()



