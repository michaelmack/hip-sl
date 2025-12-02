
library(tidyverse)
library(easystats)
library(stringr)

# load data
el <- 'late'
lesion <- 'intact'
runs <- '10'
filepath <- paste('output/outputacts/tstacts_',el,'_',lesion,'_runs_',runs,sep='')
files <- list.files(filepath,pattern='*.csv',full.names=T)
system2('cat',args=c(files[1],'>','tmp/tmp.csv'))
system2('awk',args=c('FNR!=1',files[2:length(files)],'>>','tmp/tmp.csv'))
df <- read_csv('tmp/tmp.csv',show_col_types=F)
df <- df %>% 
  mutate(type=case_when(
    str_detect(TrialName,'P')~'prototype',
    str_detect(TrialName,'R')~'rulefollower',
    str_detect(TrialName,'E')~'exception')) %>% 
  mutate(category=case_when(
    str_detect(TrialName,'A')~'A',
    str_detect(TrialName,'B')~'B')) %>% 
  mutate(acc=case_when(
    category=='A'~Ecout_8/(Ecout_8+Ecout_9),
    category=='B'~Ecout_9/(Ecout_8+Ecout_9)))

df$type <- factor(df$type,
                      levels=c('prototype','rulefollower','exception'))

ggplot(df,aes(x=Epoch,y=acc,colour=type))+
  stat_summary(fun.data=mean_se,geom='pointrange')+
  facet_wrap(~category)+
  coord_cartesian(ylim=c(0,1))+
  labs(title=paste('lesion:',lesion,'| curriculum:',el),
       y='Accuracy',
       x='Epoch')+
  theme_minimal()

