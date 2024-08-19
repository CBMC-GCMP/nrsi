library(tidyverse)
library(sf)
library(patchwork)
library(moments)
library(ggridges)
library(dafishr)

resample_mean <- function(df, n = 9999) {
  replicate(n, {
    sampled <- df[sample(nrow(df), replace = TRUE), ]
    mean((sampled$nrsi), na.rm = TRUE)
  })
}


# Loading data ------------------------------------------------------------
fish <- read_csv("data/fish_database_submission.csv") 

fish %>% 
  mutate(name = paste0(region, "_", reef)) %>% 
  group_by(protection) %>% 
  summarise(n_distinct(reef))

nrsi <-  fish %>% 
  mutate(index_levels = case_when(
    str_detect(trophic_level_f, "4-4.5") ~ "UTL", 
    str_detect(trophic_level_f, "2-2.5") ~ "LTL", 
    TRUE ~ "CTL"
  ), 
  protection = factor(protection, levels = c("Not Protected", "Lightly Protected", "Fish Refuge", "Fully Protected"))) %>% 
  mutate(degree = round(latitude)) %>% 
  group_by(year, region, reef, protection, degree, latitude, longitude, depth2, index_levels, transect) %>% 
  summarise(biomass = sum(biomass, na.rm = TRUE)) %>% 
  group_by(year, region, reef, protection, index_levels, degree, latitude, longitude) %>% 
  summarise(biomass = mean(biomass, na.rm = TRUE)) %>% 
  group_by(year, region, reef, protection, degree, latitude, longitude) %>% 
  mutate(rel_biomass = (biomass / sum(biomass, na.rm = TRUE)) * 100) %>% 
  select(-biomass) %>%
  pivot_wider(names_from = index_levels, values_from = rel_biomass) %>%
  mutate(
    UTL = if_else(is.na(UTL), 0, UTL),  # Replace NA with 0 for UTL calculations
    LTL = if_else(is.na(LTL), 0, LTL),  # Replace NA with 0 for LTL calculations
    CTL = if_else(is.na(CTL), 0, CTL),  # Replace NA with 0 for CTL calculations
    nrsi = case_when(
      LTL > UTL + CTL ~ UTL / (UTL + CTL),  # Condition 1: Use only UTL if LTL is greater than UTL + CTL
      TRUE ~ (UTL + LTL - CTL) / (UTL + LTL + CTL)  # Condition 2: Compute as normal otherwise
    )
  ) 


reefs_monitored_long_term <- nrsi %>% 
  ungroup() %>% 
  select(year, reef) %>% 
  unique() %>% 
  group_by(reef) %>% 
  summarise(series = n_distinct(year)) %>% 
  filter(series > 19) %>% 
  pull(reef)

cp_sites <- fish %>% 
  filter(region == "Cabo Pulmo") %>% 
  filter(year < 2001) %>% 
  pull(reef) %>% unique()

fishing_refuges <- c("REFUGIO_MORENA",
                     "REFUGIO_SAN_MARCIAL",
                     "SAN_DIEGO_ABNEGADO",
                     "PUNTA_BOTELLA",
                     "PARDITO_REFUGIO",
                     "ISLOTE_AGUA_VERDE",
                     "HABANA")

### First is long term analysis with only sites consistently monitored
ltms <- nrsi %>% 
  filter(reef %in% c(reefs_monitored_long_term, cp_sites, fishing_refuges)) %>% 
  mutate(protection = as.character(protection)) %>% 
  mutate(protection = ifelse(is.na(protection), "Fishing Refuge", protection))


resampled_prod <- ltms %>%
  group_by(year, protection) %>%
  nest() %>%
  mutate(
    ResampledMeans = map(data, ~resample_mean(.x)),
    Median = map_dbl(ResampledMeans, median)
  ) %>%
  select(year, protection, ResampledMeans, Median)

toplot_ltms <- resampled_prod %>%
  unnest(ResampledMeans) 


p1 <- toplot_ltms %>% 
  mutate(protection = factor(protection, levels = rev(c("Not Protected", "Lightly Protected", "Fishing Refuge", "Fully Protected")))) %>% 
  group_by(year, protection) %>% 
  summarise(Median = mean(Median), min = min(ResampledMeans), max = max(ResampledMeans)) %>% 
  ggplot(aes(x=year, y=Median)) +
  geom_point() +
  geom_errorbar(aes(ymin = min, ymax = max), width=0.2) +
  geom_hline(yintercept = 0) +
  geom_smooth(se=F) +
  scale_x_continuous(breaks = seq(1998, 2023, by=1)) +
  ylim(-1, 1) +
  labs(x = "Year", y = "NRSI Index") +
  facet_grid(protection~.,)+
  theme_bw() +
  theme(strip.background = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust=.5),
        legend.position = "")


### Second is comparing protection levels from average of recent years (>2015)
prot_levels <- nrsi %>% 
  filter(year > 2008) %>% 
  mutate(protection = as.character(protection)) %>% 
  mutate(protection = ifelse(is.na(protection), "Fishing Refuge", protection))



resampled_prod_prot_levels <- prot_levels %>%
  group_by(region, protection) %>%
  nest() %>%
  mutate(
    ResampledMeans = map(data, ~resample_mean(.x)),
    Median = map_dbl(ResampledMeans, median)
  ) %>%
  select(region, protection, ResampledMeans, Median)

toplot_prot_levels <- resampled_prod_prot_levels %>%
  unnest(ResampledMeans) 


resampled_prod_prot_levels <- prot_levels %>%
  group_by(protection) %>%
  nest() %>%
  mutate(
    ResampledMeans = map(data, ~resample_mean(.x)),
    Median = map_dbl(ResampledMeans, median)
  ) %>%
  select( protection, ResampledMeans, Median)

toplot_prot_levels <- resampled_prod_prot_levels %>%
  unnest(ResampledMeans) 


p2 <- toplot_prot_levels %>% 
  mutate(protection = factor(protection, levels = (c("Not Protected", "Lightly Protected", "Fishing Refuge", "Fully Protected")))) %>% 
  ggplot(aes(y = protection, x = ResampledMeans, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "NRSI", y = "") +
  scale_fill_gradientn(colors = c("firebrick", "orange", "gray", "green", "darkgreen"),
                       values = scales::rescale(c(-1, -0.75, -0.5, 0.5, 0.75, 1)))+
  xlim(-0.5,0.5) +
  theme_bw() +
  theme(legend.position = "", 
        axis.text.x = element_text(angle = 90, vjust = .5))  
p2

### Third is showing results by reef on the map
map_levels <- nrsi %>% 
  filter(year > 2008) %>% 
  mutate(protection = as.character(protection)) %>% 
  mutate(protection = ifelse(is.na(protection), "Fishing Refuge", protection)) 


resampled_prod_map_levels <- map_levels %>%
  group_by(region, reef, latitude, longitude) %>%
  nest() %>%
  mutate(
    ResampledMeans = map(data, ~resample_mean(.x)),
    Median = map_dbl(ResampledMeans, median)
  ) %>%
  select(region, reef, latitude, longitude, ResampledMeans, Median)

toplot_map_levels <- resampled_prod_map_levels %>%
  unnest(ResampledMeans) 


p3 <- toplot_map_levels %>% 
  group_by(region) %>% 
  summarise(latitude = mean(latitude, na.rm=T), longitude = mean(longitude, na.rm=T), Median = mean(Median)) %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>% 
  ggplot() +
  geom_sf(data = dafishr::mx_shape, fill = "gray90") + 
  geom_sf(aes(col = Median), size = 3) + 
  scale_color_gradientn(colors = c("firebrick", "orange", "green", "darkgreen"),
                       values = scales::rescale(c(-1, -0.5, 0, 0.5, 1)),
                       limits = c(-1, 1)) +
  coord_sf(xlim = c(-115, -95), ylim = c(15, 30)) +
  labs(col = "NRSI Index") +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.title.position =  "top",
        legend.title = element_text(hjust = 0.5),
        legend.position = "bottom")
p3



library(patchwork)
 

p3  
ggsave("figs/figure_3_general_results.jpeg", height = 6, width = 6, dpi = 600)


p2
ggsave("figs/figure_3_comparing.jpeg", height = 4, width = 4, dpi = 600)


p1
ggsave("figs/figure_3_time.jpeg", height = 5, width = 5, dpi = 600)



