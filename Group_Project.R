library(tidyverse)
library(countrycode) # continent group data
library(corrplot)

#read files in - focus is on kg dataset
fat <- read_csv('data/Fat_Supply_Quantity_Data.csv')%>% select(-`Unit (all except Population)`)%>% drop_na()
protein <- read_csv('data/Protein_Supply_Quantity_Data.csv') %>% select(-`Unit (all except Population)`)%>% drop_na()
kcal <- read_csv('data/Food_Supply_kcal_Data.csv')%>% select(-`Unit (all except Population)`) %>% drop_na()
kg <- read_csv('data/Food_Supply_Quantity_kg_Data.csv') %>% select(-`Unit (all except Population)`)%>% drop_na()

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

# Correlation plot illustrating as lot of correltions
corrplot(kg_cor, type='upper')

# vector of column names
column_names <- colnames(kg)

# add continent column, and factor
kg_cont <- kg %>% 
  mutate(Continent = factor(countrycode(Country, 'country.name', 'continent'))) %>%
  select(c('Country', 'Continent', column_names[2:length(column_names)])) %>%
  pivot_longer(cols = colnames(.)[3:length(.)],
               names_to='variable',
               values_to='value')

# plot densities if each variable to evaluate efficacy of clustering methods
plots <- ggplot(kg_cont, aes(x=value, colour=Continent, fill=Continent)) 
plots + geom_density(alpha=0.5) + facet_wrap(~ variable, scales='free')
