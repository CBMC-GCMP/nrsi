library(tidyverse)
library(sf)
library(patchwork)
library(moments)

# Loading data ------------------------------------------------------------
fish <- read_csv("data/rocky_reef_fish_database.csv")

nrsi <- fish |> 
  mutate(index_levels = case_when(
    str_detect(trophic_level_f, "2-2.5|4-4.5") ~ "ATL", 
    TRUE ~ "CTL"
  ), 
  protection = factor(protection, levels = c("Not Protected", "Lightly Protected", "Fish Refuge", "Fully Protected"))) |>
  mutate(degree = round(latitude)) |> 
  group_by(year, id_reef, protection,degree, latitude, longitude, depth2, index_levels, transect) |> 
  summarise(biomass = sum(biomass, na.rm = T)) |> 
  group_by(year, id_reef, index_levels, degree, latitude, longitude, protection) |> 
  summarise(biomass = mean(biomass, na.rm = T)) |> 
  group_by(year, id_reef, degree, latitude, longitude, protection) |> 
  mutate(rel_biomass = (biomass/sum(biomass, na.rm = T))*100) |> 
  select(-biomass) %>% 
  pivot_wider(names_from = index_levels, values_from = rel_biomass) |> 
  mutate(nrsi = (ATL-CTL)/(ATL+CTL)) 


summary_stats <- nrsi %>%
  group_by(protection) %>% 
  summarise(
    mean_nrsi = mean(nrsi, na.rm = TRUE),
    sd_nrsi = sd(nrsi, na.rm = TRUE),
    min_nrsi = min(nrsi, na.rm = TRUE),
    max_nrsi = max(nrsi, na.rm = TRUE),
    q1_nrsi = quantile(nrsi, 0.25, na.rm = TRUE),
    median_nrsi = median(nrsi, na.rm = TRUE),
    q3_nrsi = quantile(nrsi, 0.75, na.rm = TRUE), 
    skewness_nrsi = skewness(nrsi, na.rm = TRUE),
    kurtosis_nrsi = kurtosis(nrsi, na.rm = TRUE),
    ci_lower = t.test(nrsi, conf.level = 0.95)$conf.int[1],
    ci_upper = t.test(nrsi, conf.level = 0.95)$conf.int[2]
  )

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



# BoxPlots -------------------------------------------------------------

p1 <- nrsi %>%  
  filter(protection !=  "Fish Refuge" | !year < 2006) %>% # eliminating unclear site
  group_by(protection) %>% 
  mutate(nrsi_avg = median(nrsi, na.rm = T)) %>% 
  mutate(nrsiF = cut(nrsi, breaks=c(-1, -0.75, -0.25, 0, 0.25, 0.75, 1), include.lowest = TRUE))  %>% 
  mutate(nrsiF = factor(nrsiF, levels = c('[-1,-0.75]', '(-0.75,-0.25]', '(-0.25,0]', '(0,0.25]', '(0.25,0.75]', '(0.75,1]'),
                        labels = c("Very Low", "Low",  "Fair", "Fair",  "High", "Very high")))  %>% 
  ggplot(aes(x=nrsi, y=protection)) +
  geom_jitter(aes(col = nrsiF), height = .1, alpha = .4) +
  geom_boxplot(outlier.colour = NA, width=.1, fatten =.1, alpha = 0.1) +
  geom_point(aes(x=nrsi_avg), size =4) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_color_manual(values = c("firebrick", "orange", "gray", "green", "darkgreen"))+
  labs(x="NRSI", y="", col = "") + 
  xlim(-1,1) +
  theme_bw() +
  theme(strip.background = element_blank(), 
        legend.position = "")
p1


ref_biomass <- fish |> 
  mutate(protection = factor(protection, levels = c("Not Protected", "Lightly Protected", "Fish Refuge", "Fully Protected"))) |>
  group_by(year, id_reef, protection, depth2, transect) |> 
  summarise(biomass = sum(biomass, na.rm = T)) |> 
  group_by(year, id_reef, protection) |> 
  summarise(biomass = mean(biomass, na.rm = T)) %>% 
  group_by(protection) %>% 
  summarise(avg_biomass = log1p(median(biomass))) %>% 
  filter(protection == "Not Protected") %>% pull(avg_biomass)

p2 <- fish |> 
  mutate(protection = factor(protection, levels = c("Not Protected", "Lightly Protected", "Fish Refuge", "Fully Protected"))) |>
  group_by(year, id_reef, protection, depth2, transect) |> 
  summarise(biomass = sum(biomass, na.rm = T)) |> 
  group_by(year, id_reef, protection) |> 
  summarise(biomass = mean(biomass, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(avg_biomass = median(biomass)) %>% 
  ggplot(aes(x=log1p(biomass), y=protection)) +
  geom_jitter(height = .1, col = "gray60") +
  geom_boxplot(outlier.colour = NA, width=.1, alpha = 0.1) +
  geom_vline(xintercept = ref_biomass, linetype = 2) +
  labs(x="log(Standing Biomass + 1)", y="", col = "") + 
  theme_bw() +
  theme(strip.background = element_blank(), 
        axis.text.y = element_blank(),
        legend.position = "top")
p2


# Time trends -------------------------------------------------------------

p3 <- nrsi |>  
  filter(protection !=  "Fish Refuge" | !year < 2006) |> # eliminating unclear site
  mutate(nrsiF = cut(nrsi, breaks=c(-1, -0.75, -0.25, 0, 0.25, 0.75, 1), include.lowest = TRUE)) |> 
  mutate(nrsiF = factor(nrsiF, levels = c('[-1,-0.75]', '(-0.75,-0.25]', '(-0.25,0]', '(0,0.25]', '(0.25,0.75]', '(0.75,1]'),
                       labels = c("Very Low", "Low",  "Fair", "Fair",  "High", "Very high"))) |> 
  ggplot(aes(x=year, y=nrsi, group=protection)) +
  geom_point(aes(col = nrsiF), alpha = .4) +
  geom_smooth(se = F, col = "black", method = "loess") +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_color_manual(values = c("firebrick", "orange", "gray", "green", "darkgreen"))+
  scale_x_continuous(breaks = seq(1998, 2022, by=1)) +
  labs(x="Year", y="NRSI", col = "") + 
  facet_grid(protection~.,) +
  ylim(-1,1) +
  theme_bw() +
  theme(strip.background = element_blank(), 
        axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 6),
        legend.position = "top")
p3

p1 / p3 + plot_annotation(tag_levels = "A")

ggsave("figs/Figure_3.jpeg", height = 8, width = 6, dpi = 800)


nrsi_sf <- sf::st_as_sf(nrsi, coords = c("longitude", "latitude"), crs = 4326)

nrsi_sf |> 
  group_by(id_reef, protection) |> 
  summarise(nrsi = median(nrsi)) |> 
  mutate(nrsiF = cut(nrsi, breaks=c(-1, -0.75, -0.25, 0, 0.25, 0.75, 1), include.lowest = TRUE)) |> 
  mutate(nrsiF = factor(nrsiF, levels = c('[-1,-0.75]', '(-0.75,-0.25]', '(-0.25,0]', '(0,0.25]', '(0.25,0.75]', '(0.75,1]'),
                        labels = c("Very Low", "Low",  "Fair", "Fair",  "High", "Very high"))) |> 
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

p2 <- nrsi_sf |> 
  ggplot() +
  geom_sf(data = dafishr::mx_shape) +
  geom_sf(aes(col = protection)) +
  scale_color_manual(values = c("firebrick", "aquamarine", "lightgreen", "darkgreen")) +
  coord_sf(xlim = c(min(nrsi$longitude)-1, max(nrsi$longitude)+1),
           ylim = c(min(nrsi$latitude)-0.1, max(nrsi$latitude))+0.1) +
  labs(col = "", subtitle = "All monitored sites in the program") +
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
  scale_y_continuous(breaks = seq(15, 30, by=1)) +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 8),
        legend.key = element_blank(),
        panel.background = element_rect(fill = NA), 
        panel.grid = element_line(color = "gray90", linetype = 2), 
        panel.border = element_rect(fill = NA, color = "black")) 
p2
ggsave("figs/study_area.png", height = 6, width = 8, dpi = 800)





 # Statistical comparisons -------------------------------------------------

library(betareg)

# Assuming your data frame is named df and the index variable is called 'index_value'
# Transform your index values from [-1, 1] to (0, 1)
nrsi$transformed_index = (nrsi$nrsi + 1) / 2

# Fit a beta regression model
# Let's say you're modeling this transformed index as a function of some predictor 'predictor_var'
beta_model = betareg(transformed_index ~ protection, data = nrsi)

# Summary of the model
summary(beta_model)

# Assuming the coefficient for "Fully Protected"
coef_fully_protected <- 0.77440

# Convert the coefficient from log-odds to odds
odds_increase <- exp(coef_fully_protected)

# Calculate the percent increase from the odds
percent_increase <- (odds_increase - 1) * 100

# Print the percent increase
print(percent_increase)

nrsi %>% 
  group_by(protection) %>% 
  summarise(reef = n_distinct(id_reef))

# Reef NRSI trends analysis -----------------------------------------------

library(broom) # to change model results into data frame


## dataset to model
library(dplyr)
library(betareg)
library(broom)

# Filter reefs with more than 3 years of data
tomode <- nrsi |> 
  group_by(id_reef) |> 
  mutate(n_years = n_distinct(year)) |> # Getting the number of years
  filter(n_years > 3)

# Fit beta regression models for each reef and protection level, and extract slope of 'year'
models <- tomode |> 
  group_by(id_reef, protection) |> 
  mutate(nrsi_transformed = (nrsi + 1) / 2) %>% 
  do(tidy(betareg(nrsi_transformed ~ year, data = .))) |> 
  filter(term == "year") |> 
  mutate(trend = ifelse(estimate > 0, "Increasing", "Decreasing"))

# Calculate the percentage of reefs with increasing or decreasing trends by protection level
models_perc <- models |> 
  group_by(protection) |> 
  count(trend) |> 
  mutate(perc = round((n/sum(n)) * 100))

models_perc

models_perc %>% 
  mutate(perc = ifelse(trend == "Decreasing", perc * -1, perc)) %>% 
  ggplot(aes(fill = trend, x = perc, y = protection)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Trend in Index by Protection Level",
       y = "Protection Level",
       x = "NRSI change over time (%)",
       fill = "Trend") +
  xlim(-100, 100) +
  theme_bw() +
  theme(legend.position = "")

