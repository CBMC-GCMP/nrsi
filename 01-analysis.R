library(tidyverse)


# Loading data ------------------------------------------------------------

fish <- read_csv("data/rocky_reef_fish_database.csv") 

nrsi <- fish |> 
  mutate(index_levels = case_when(
    str_detect(trophic_level_f, "2-2.5|4-4.5") ~ "ATL", 
    TRUE ~ "CTL"
  ), 
  protection = factor(protection, levels = c("Not Protected", "Lightly Protected", "Fishing Refuge", "Fully Protected"))) |> 
  group_by(year, id_reef, protection, latitude, longitude, depth2, index_levels, transect) |> 
  summarise(biomass = sum(biomass, na.rm = T)) |> 
  group_by(year, id_reef, index_levels, latitude, longitude, protection) |> 
  summarise(biomass = mean(biomass, na.rm = T)) |> 
  group_by(year, id_reef, latitude, longitude, protection) |> 
  mutate(rel_biomass = (biomass/sum(biomass, na.rm = T))*100) |> 
  select(-biomass) %>% 
  pivot_wider(names_from = index_levels, values_from = rel_biomass) |> 
  mutate(nrsi = (ATL-CTL)/(ATL+CTL)) 


# Density plot ------------------------------------------------------------

nrsi  |> 
  group_by(protection) |> 
  mutate(median = median(nrsi)) |> 
  ggplot(aes(x=nrsi, fill = protection))+
  geom_density(alpha = 0.3) +
  geom_vline(xintercept = 0) +
  geom_vline(aes(xintercept = median), linetype = "dashed", col = 'red') +
  xlim(c(-1,1)) +
  labs(y="Values density", x = "NRSI", fill = "") +
  scale_fill_manual(values = c("firebrick", "aquamarine", "lightgreen", "darkgreen")) +
  facet_grid(protection~.,) +
  theme_bw() +
  theme(legend.position = "", 
        strip.background = element_blank(),
        panel.spacing = unit(0, "lines"))
  
ggsave("figs/density_plot.png", dpi = 800, height = 5, width = 5)


# Time trends -------------------------------------------------------------


nrsi |>  
  filter(protection !=  "Fishing Refuge" | !year < 2006) |> # eliminating unclear site
  mutate(nrsiF = cut(nrsi, breaks=c(-1, -0.75, -0.25, 0, 0.25, 0.75, 1), include.lowest = TRUE)) |> 
  mutate(nrsiF = factor(nrsiF, levels = c('[-1,-0.75]', '(-0.75,-0.25]', '(-0.25,0]', '(0,0.25]', '(0.25,0.75]', '(0.75,1]'),
                       labels = c("Very Poor", "Poor",  "Fair", "Fair",  "High", "Very high"))) |> 
  ggplot(aes(x=year, y=nrsi, group=protection)) +
  geom_point(aes(col = nrsiF), alpha = .4) +
  geom_smooth(se = F, col = "black", method = "loess") +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_color_manual(values = c("firebrick", "orange", "gray", "green", "darkgreen"))+
  labs(x="Year", y="NRSI", col = "") + 
  facet_grid(protection~.,) +
  ylim(-1,1) +
  theme_bw() +
  theme(strip.background = element_blank(), 
        legend.position = "top")

ggsave("figs/time_trends.png", height = 8, width = 8, dpi = 800)




# Map of the study area ---------------------------------------------------

nrsi_sf <- sf::st_as_sf(nrsi, coords = c("longitude", "latitude"), crs = 4326)

nrsi_sf |> 
  group_by(id_reef, protection) |> 
  summarise(nrsi = median(nrsi)) |> 
  mutate(nrsiF = cut(nrsi, breaks=c(-1, -0.75, -0.25, 0, 0.25, 0.75, 1), include.lowest = TRUE)) |> 
  mutate(nrsiF = factor(nrsiF, levels = c('[-1,-0.75]', '(-0.75,-0.25]', '(-0.25,0]', '(0,0.25]', '(0.25,0.75]', '(0.75,1]'),
                        labels = c("Very Poor", "Poor",  "Fair", "Fair",  "High", "Very high"))) |> 
  ggplot() +
  geom_sf(data = dafishr::mx_shape) +
  geom_sf(aes(col = (nrsiF))) +
  coord_sf(xlim = c(min(nrsi$longitude)-1, max(nrsi$longitude)+1),
           ylim = c(min(nrsi$latitude)-0.1, max(nrsi$latitude))+0.1) +
  labs(col = "NRSI", subtitle = "All monitored sites in the program") +
  scale_color_manual(values = c("firebrick", "orange", "gray", "green", "darkgreen"))+
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )) +
  theme(legend.position = "right", 
        legend.key = element_blank(),
        panel.background = element_rect(fill = NA), 
        panel.grid = element_line(color = "gray90", linetype = 2), 
        panel.border = element_rect(fill = NA, color = "black")) 

nrsi_sf |> 
  ggplot() +
  geom_sf(data = dafishr::mx_shape) +
  geom_sf(aes(col = protection)) +
  scale_color_manual(values = c("firebrick", "aquamarine", "lightgreen", "darkgreen")) +
  coord_sf(xlim = c(min(nrsi$longitude)-1, max(nrsi$longitude)+1),
           ylim = c(min(nrsi$latitude)-0.1, max(nrsi$latitude))+0.1) +
  labs(col = "NRSI", subtitle = "All monitored sites in the program") +
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )) +
  theme(legend.position = "right", 
        legend.key = element_blank(),
        panel.background = element_rect(fill = NA), 
        panel.grid = element_line(color = "gray90", linetype = 2), 
        panel.border = element_rect(fill = NA, color = "black")) 

ggsave("figs/study_area.png", height = 6, width = 8, dpi = 800)




# Statistical comparisons -------------------------------------------------

m0 <- glm(nrsi~protection, data = nrsi)


report::report(m0)


# Reef NRSI trends analysis -----------------------------------------------

library(broom) # to change model results into data frame


## dataset to model
tomode <- nrsi |> 
  group_by(id_reef) |> 
  mutate(n_years = n_distinct(year)) |> # Getting the number of years
  filter(n_years > 3) 


models <- tomode |> 
  group_by(id_reef, protection) |> 
  do(tidy(lm(nrsi ~ year, data = .))) |> 
  filter(term == "year") |> 
  mutate(trend = ifelse(estimate > 0, "Increasing", "Decreasing"))

models |> 
  group_by(protection) |> 
  count(trend) |> 
  mutate(perc = round((n/sum(n)) *100))

models |> 
  group_by(protection) |> 
  mutate(mean_est = mean(estimate)) |> 
  ggplot(aes(x = estimate, y = reorder(protection, -mean_est))) +
  geom_line() +
  geom_point(aes(x=mean_est)) +
  labs(x="NRSI variation", y = "") +
  xlim(-0.2, 0.2) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  theme_bw() +
  theme(legend.position = "", 
        strip.background = element_blank(),
        panel.spacing = unit(0, "lines"))
