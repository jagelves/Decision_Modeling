library(tidyverse)
library(fpp3)
library(ggthemes)
rm(list=ls())
Load<-read_csv("http://jagelves.github.io/Data/AirlinesLoad.csv")

Load %>% mutate(Date=yearmonth(paste(Year, Month, sep = "-"))) %>% 
  filter(Airline=="Delta") %>% 
  select(Date,LF_total) %>% as_tsibble(index=Date) -> Load_ts

Load_ts %>% filter_index("2022 Jan"~"2023 Dec") -> train
Load_ts %>% filter_index("Jan 2024"~.) -> test

train %>% 
  model(TSLM=TSLM(LF_total~trend()+season()),
        ETS=ETS(LF_total)) -> fit

fit %>% forecast(test) %>% 
  accuracy(test)

Load_ts %>% 
  model(ETS=ETS(LF_total)) %>% 
  forecast(h=4)
