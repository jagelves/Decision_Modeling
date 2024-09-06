
rm(list=ls())
library(tidyverse)
library(fpp3)

avo<-read_csv("https://jagelves.github.io/Data/avocado2020-2023.csv")

avo %>% select(Geography,Date, Type, AveragePrice) %>% 
  filter(Geography=="Richmond/Norfolk") %>% 
  filter(Type=="Conventional") %>% 
  mutate(Date=mdy(Date)) %>% 
  mutate(Date=yearweek(Date)) %>% 
  select(Date,AveragePrice) %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  arrange(Date) %>% 
  as_tsibble(index=Date) %>% 
  filter_index("2023 W2"~.) -> avo_ts


avo_ts %>% model(MEAN=MEAN(AveragePrice),
                 NAIVE=NAIVE(AveragePrice),
                 TSLM=TSLM(AveragePrice~trend())) -> fit

fit %>% forecast(h=4) %>% 
  as_tibble() %>% 
  select(Date,.model,.mean) %>% 
  mutate(Margin=(1.5-.mean)/1.5)
