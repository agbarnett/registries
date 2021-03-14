# Missing over time

miss_over_time = mutate(for.model, 
                        year = as.numeric(format(submitted, '%Y'))) %>%
  filter(study_type == 'Interventional') %>%
  group_by(year, assignment) %>%
  tally() %>%
  group_by(year) %>%
  mutate(perc = prop.table(n)) %>%
  ungroup()

gplot = ggplot(data=miss_over_time, aes(x=year, y=perc, fill=assignment))+
  geom_bar(stat='identity')+
  theme_bw()
gplot
