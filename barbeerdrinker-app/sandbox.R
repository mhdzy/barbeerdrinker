bills <- read_csv("~/bills.csv")
tsns  <- read_csv("~/tsns.csv")

tmp <- bills %>% 
  group_by(bar, item, price) %>% 
  summarise()

per_dr <- tsns %>% 
          group_by(name) %>% 
          summarise(dr_total = sum(total)) %>% 
          arrange(desc(dr_total)) %>% 
          ungroup()

per_bar <- tsns %>% 
           group_by(name, bar) %>% 
           summarise(dr_total = sum(total)) %>% 
           arrange(desc(dr_total)) %>% 
           ungroup()



p <- per_dr %>%
  ggplot(aes(x = reorder(name, dr_total), y = dr_total)) +
  geom_bar(stat = "identity") +
  labs(title = "Top Spending Drinkers",
       caption = "top calculated by $ amt spent",
       x = "drinker name",
       y = "total $ spent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- per_dr %>% 
      #filter(dr_total < 500) %>% 
      ggplot(aes(x = dr_total)) +
      geom_density(alpha = 0.70, fill = "#9678b6")
