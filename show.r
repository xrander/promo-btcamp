
# Import Data -------------------------------------------------------------
read_csv("data/popdata.csv")
read_csv("data/popdata2.csv", skip = 5)

poplar_data <- read.csv("data/popdata2.csv", skip = 5)
# Understanding Data ------------------------------------------------------
str(poplar_data)
summary(poplar_data)

head(poplar_data)
tail(poplar_data)
dim(poplar_data)

## Should we convert -----------------------------------------------------
table(poplar_data$block)
names(poplar_data)
table(poplar_data$clone)

table(poplar_data$fert)
table(poplar_data$cutw)
table(poplar_data$height)

## Data Types Explanation -------------------------------------------------

## Changing Data Types ----------------------------------------------------
poplar_data$block <- factor(poplar_data$block)
poplar_data$fert <- factor(poplar_data$fert)
poplar_data$clone <- factor(poplar_data$clone)

summary(poplar_data)
str(poplar_data)

# Descriptive Statistics --------------------------------------------------

## Measure of Central Tendency --------------------------------------------
table(poplar_data$block)
mean(poplar_data$cutw)
median(poplar_data$cutw)
mean(poplar_data$height)

summary(poplar_data)
## Measure of Dispersion --------------------------------------------------
var(poplar_data$cutw)
sd(poplar_data$cutw)
### Simple Visualization --------------------------------------------------
plot(table(poplar_data$block))
### Single variable -------------------------------------------------------
plot(poplar_data$cutw)
#### Cat variable ---------------------------------------------------------
plot(poplar_data$cutw ~ poplar_data$block)
plot(poplar_data$cutw ~ poplar_data$fert)
#### Num variable ---------------------------------------------------------
#### Num vs Num -----------------------------------------------------------
plot(poplar_data$cutw, poplar_data$height, type = "l")

# Packages ----------------------------------------------------------------
## Tidyverse --------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)

poplar_data <- poplar_data |> 
  mutate(
    fert = ifelse(fert == 1, "fertilized", "control")
  )
## ggplot -----------------------------------------------------------------
poplar_data |> 
  ggplot(aes(fert, height, fill = block)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  labs(
    x = "Type",
    y = "Height",
    title = "Height of Experiment Types"
  ) +
  theme_classic()


poplar_data |> 
  ggplot(aes(cutw, height, col = fert)) +
  geom_line() +
  facet_wrap(~block)

# Inferential Statistics with R -------------------------------------------

## Brief Intro on Hypothesis Testing --------------------------------------

## Calculating new variable -----------------------------------------------
## One Sample T-Test (compare with mean) ----------------------------------
t.test(poplar_data$cutw, mu = 4)
t.test(poplar_data$cutw, poplar_data$height)

## Two Sample T-Test ------------------------------------------------------

### Lesser ----------------------------------------------------------------
### Greater ---------------------------------------------------------------

## Anova ------------------------------------------------------------------
anova(aov(poplar_data$height ~ poplar_data$block))
anova(aov(poplar_data$height ~ poplar_data$clone))
anova(aov(poplar_data$height ~ poplar_data$fert))

# Communication our Data with Quarto---------------------------------------