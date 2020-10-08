df1 <- read.delim(file.choose())
view(df1)

df2 <- df1 %>% filter(Smoke == "yes")
attach(df2)

#HO: smokers are less than 13 
#HA: smokers are geater than 15

t.test(Age, mu = 16, alternative = "greater")

