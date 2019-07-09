# -------------------------------------------------------------------------
# GOAL: predict the wine variety using words in the description/review
# DESCRIPTION:  The model still won't be able to taste the wine, but 
# theoretically it could identify the wine based on a description that a
# sommelier could give. If anyone has any ideas on how to accomplish this,
# please post them!
# -------------------------------------------------------------------------

# library -----------------------------------------------------------------

library(tidyverse)


# data --------------------------------------------------------------------

data <- read_csv("data/winemag-data-130k-v2.csv")


# text analysis -----------------------------------------------------------

# take a sample of the text
sample <- data %>% 
  select(description) %>% 
  sample_n(size = 5)
sample
# HYPOTHESIS: 
# Look fot capital letters to detect unique names, like countries or regions

# detect how many review do I have by each region
data %>% 
  group_by(country) %>% 
  summarise(n = n()) %>% 
  ggplot() +
    geom_col(aes(x = reorder(country, desc(n)), y = n)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p1
plotly::ggplotly(p1)

data %>% 
  group_by(country) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round(
    (n/sum(n))*100,2)
    ) %>%
  arrange(desc(freq)) -> country_freq
country_freq
