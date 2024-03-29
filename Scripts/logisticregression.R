library(tidyverse)
library(tidytext)
library(readr)
wcgs <- read_csv("wcgsdata.csv") 
model <- glm(chd~wt2, binomial(link = logit), wcgs)
model
summary(model)
model1 <- glm(chd~weight, binomial(link = logit), wcgs)
summary(model1)
exp(0.43)
exp(0.9)
exp(0.28)
exp(model$coefficients)
model2 <- glm(chd~smoke, "binomial", wcgs)
summary(model2)
exp(model2$coefficients)
model3 <- glm(chd~smoke + age, "binomial", wcgs)
summary(model3)
exp(model3$coefficients)
tidy(model3)  
tidy(model)
AIC(model3)
AIC(model2)
anova(model2, model3)

model4 <- glm(chd~smoke + age + weight, "binomial", wcgs)
summary(model4)
exp(model4$coefficients)
anova(model3, model4, test = "Chisq")

model5 <- glm(chd~smoke + age + weight + smoke*age, "binomial", wcgs)
summary(model5)
exp(model5$coefficients)
anova(model4, model5, test = "Chisq")
tidy(anova(model4, model5, test = "Chisq"))
