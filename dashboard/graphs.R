









icppt_entso_fr <- icppt_entso %>% filter(Country == "France")
icppt_eur_fr <- icppt_eur %>% filter(Country == "France")



library(ggh4x)

ggplot(NULL, aes(Date, Hydro)) +  
  geom_line(data = icppt_entso_fr, col = "red", size = 1) +
  geom_line(data = icppt_eur_fr, col = "blue", size = 1) +
  geom_point(data = icppt_eur_fr, col = "blue", size = 3) +
  geom_point(data = icppt_entso_fr, col = "red", size = 3)
  labs(title = "Evol over time")
