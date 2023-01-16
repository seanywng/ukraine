event_type <- acled %>% 
  #mutate(event_type = fct_relevel(event_type, 
  #                                "Violence against civilians",
  #                                "Explosions/Remote violence", 
  #                                "Strategic developments", 
  #                                "Battles", 
  #                                "Protests", 
  #                                "Riots")) %>% 
  mutate(month = floor_date(event_date, "month")) %>% 
  mutate(month = map(month, ~ seq.Date(as.Date(.), 
                                       as.Date("2022/12/31"), 
                                       by = "month"))) %>% 
  unnest(month) %>% 
  mutate(month = format_ISO8601(month, precision = "ymd")) %>%
  ggplot() + 
  geom_sf(data = pcode1_shape, size = 0.5, colour = "black", 
          alpha = 0) + 
  geom_point(aes(x = longitude, 
                 y = latitude, 
                 colour = factor(event_type), 
                 size = fatalities)) + 
  # Try and figure out why scale_colour_manual doesn't work 
  #scale_colour_manual(values = c("#D95F02", "#7570B3", "#1B9E77", "#E7298A", 
  #                               "#E6AB02", "#00AFBB", "#666666", "#A6761D", "#66A61E")) +
  theme_void() +
  guides(colour = guide_legend(override.aes = list(size = 5), order = 1)) + 
  theme(legend.text = element_text(size = 11), 
        legend.title = element_text(face = "bold"), 
        plot.caption = element_text(hjust = 0.2)) + 
  transition_manual(month) + 
  labs(title = "Types of conflict events as of { current_frame }", 
       caption = "Data source: Armed Conflict Location & Event Data Project; acleddata.com", 
       colour = "Event type")

# You might want to lower the duration 
animate(event_type, height = 1748, width = 2480, res = 150, duration = 22)

anim_save("./plots/event_type_2022.gif")

# It would be good if you could figure out the date intervals, 
# but as is, the gif is fine
acled %>% 
  mutate(interval = 
           case_when(
             event_date < "2022-02-01" ~ "January 30 2022", 
             event_date <= "2022-02-24" & event_date >= "2022-02-01" ~ "February 24 2022", 
             event_date <= "2022-03-24" & event_date > "2022-02-24" ~ "March 24 2022", 
             event_date <= "2022-04-24" & event_date > "2022-03-24" ~ "April 24 2022", 
             event_date <= "2022-05-24" & event_date > "2022-04-24" ~ "May 24 2022", 
             event_date <= "2022-06-24" & event_date > "2022-05-24" ~ "June 24 2022", 
             event_date <= "2022-07-24" & event_date > "2022-06-24" ~ "July 24 2022", 
             event_date <= "2022-08-24" & event_date > "2022-07-24" ~ "August 24 2022",
             event_date <= "2022-09-24" & event_date > "2022-08-24" ~ "September 24 2022",
             event_date <= "2022-10-24" & event_date > "2022-09-24" ~ "October 24 2022",
             event_date <= "2022-11-24" & event_date > "2022-10-24" ~ "November 24 2022",
             event_date <= "2022-12-24" & event_date > "2022-11-24" ~ "December 24 2022",
           )) %>% 
  sample_n(10) %>% 
  select(event_date, interval)