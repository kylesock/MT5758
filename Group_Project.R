library(tidyverse)
library(countrycode) # continent group data
library(GGally)
fat <- read_csv('Fat_Supply_Quantity_Data.csv')%>% select(-`Unit (all except Population)`)%>% drop_na()
protein <- read_csv('Protein_Supply_Quantity_Data.csv') %>% select(-`Unit (all except Population)`)%>% drop_na()
kcal <- read_csv('Food_Supply_kcal_Data.csv')%>% select(-`Unit (all except Population)`) %>% drop_na()
kg <- read_csv('Food_Supply_Quantity_kg_Data.csv') %>% select(-`Unit (all except Population)`)%>% drop_na()

# Convert <2.5 in undernourished column to 0, and set as numeric column
kg <- kg %>% 
  mutate(Undernourished = 
           as.numeric(
             replace(
               Undernourished, Undernourished == "<2.5", "0"
               )
             )
         )
# correlation matrix without Country label column
kg_cor <- kg %>% select(-Country) %>% cor(.)
column_names <- colnames(kg)

kg_cont <- kg %>% 
  mutate(Continent = factor(countrycode(Country, 'country.name', 'continent'))) %>%
  select(c('Country', 'Continent', column_names[2:length(column_names)])) %>%
  pivot_longer(cols = colnames(kg_cont)[3:length(kg_cont)],
               names_to='variable',
               values_to='value')

kg_cont_long <- kg_cont %>%
  pivot_longer(cols = colnames(kg_cont)[3:length(kg_cont)],
               names_to='variable',
               values_to='value')

plots <- ggplot(kg_cont_long, aes(x=value, colour=Continent, fill=Continent)) 
plots + geom_density(alpha=0.5) + facet_wrap(~ variable, scales='free')
