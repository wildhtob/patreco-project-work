# libraries ---------------------------------------------------------------

# install package with wildobar data:
# devtools::install_github("ComputationalMovementAnalysis/ComputationalMovementAnalysisData")


library(tidyverse) # tidy essentials (ggplot, purr, tidyr, readr, dplyr)
library(lubridate) # handling dates and time
library(tmap) # map visualization
library(leaflet) # interactive maps
library(terra) # handling spatial data
library(ComputationalMovementAnalysisData) # wild boar data
library(sf) # handling spatial data
library(janitor) # clean and consistent naming
library(zoo) # moving window function
library(forcats) # handling factor levels

# functions ---------------------------------------------------------------

# data import -------------------------------------------------------------

wildboar_raw <- wildschwein_BE
head(wildboar_raw)

wildboar_meta <- wildschwein_metadata
head(wildboar_meta)
?wildschwein_metadata

wildboar_overlap <- wildschwein_overlap_temp
head(wildboar_overlap)
?wildschwein_overlap_temp

# add geometry ------------------------------------------------------------

wildboar_sf <-st_as_sf(wildboar_raw, coords = c("E", "N"), crs = 2056)


# data exploration --------------------------------------------------------

# plotting data points ####

ggplot(wildboar_sf, aes(color = TierName)) +
  geom_sf(alpha = 0.4) +
  coord_sf(datum = 2056)

ggplot(wildboar_sf, aes(color = as.factor(TierID))) +
  geom_sf(alpha = 0.4) +
  coord_sf(datum = 2056) +
  scale_color_discrete(name = "AnimalID")

# sampling regime ####

limits <- c(0,200)
breaks = seq(0,200,50)
labels = paste(c(rep("",length(breaks)-1),">"), breaks)

wildboar_raw %>%
  mutate(TierName = fct_reorder(TierName, DatetimeUTC,min, .desc = TRUE)) %>%
  group_by(TierID, TierName, CollarID) %>%
  mutate(
    timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC, units = "mins")),
  ) %>%
  ggplot(aes(DatetimeUTC, TierName, colour = timelag)) +
  geom_line(lwd = 10) +
  scale_color_gradientn(name = "Sampling interval", colours = RColorBrewer::brewer.pal(11, "Spectral"), limits = limits, na.value = NA, oob = scales::squish, breaks = seq(0,200,50), labels = labels) +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(color = guide_colorbar(title.position = "top", title.hjust = .5, barwidth = unit(20, "lines"), barheight = unit(.5, "lines")))

# temporal overlap ####

?wildschwein_overlap_temp

sampling_periods <- wildboar_raw %>%
  group_by(TierID, TierName, CollarID) %>%
  summarise(
    min = min(DatetimeUTC),
    max = max(DatetimeUTC)
  )

wildboar_overlap <- wildboar_overlap %>%
  left_join(sampling_periods, by = c("TierID", "TierName", "CollarID"))

wildboar_overlap %>%
  mutate(TierCollar = paste(TierName, CollarID)) %>%
  ggplot(aes(xmin = min, xmax = max, y = TierCollar)) +
  geom_errorbarh() +
  facet_grid(Groups~., scales = "free_y", space = "free_y")

ggplot(wildboar_sf, aes(x = DatetimeUTC, y = TierName)) +
  geom_point() +
  scale_x_datetime(breaks = "1 month") +
  theme_grey()


# spatial overlap ####

# convex hull ####

wildboar_sf <- wildboar_sf %>%
  mutate(tiercollar = paste(TierID, TierName, CollarID)) 

mcp <- wildboar_sf %>%
  group_by(TierID, TierName, CollarID) %>%
  summarise() %>%
  st_convex_hull()

# plot convex hull ####

mcp %>%
  mutate(tiercollar = paste(TierID, TierName, CollarID)) %>%
  ggplot(aes(fill = factor(TierID))) + geom_sf(alpha = 0.1) +
  coord_sf(datum = 2056) +
  facet_wrap(~tiercollar) +
  theme(legend.position = "none")


tmap_mode("view") +
  tm_shape(mcp) +
  tm_fill("TierName", alpha = 0.5) +
  tm_borders(col = "red", lwd = 1) +
  tm_layout(legend.bg.color = "white")



# time lags ####

wildboar_lags <- wildboar_raw %>%
  group_by(TierID) %>% 
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC), units = "secs"))

# round lags on -2 digits 
wildboar_lags$timelag_rounded <- round(wildboar_lags$timelag, -2)

# build categories with rounded lags
wildboar_lag_cat <- wildboar_lags %>%
#  st_drop_geometry() %>%
  group_by(timelag_rounded) %>%
  summarise(count = n())


# viualise change in lag over time ####
wildboar_lags %>%
  filter(timelag < 30000 & timelag > 0) %>%
  ggplot(., aes(DatetimeUTC, timelag, col = TierName)) +
  geom_point() +
  geom_line(size = 0.1)

# calculating steplength ####

wildboar_lags$steplength <- wildboar_raw %>%
  {
    (.$E - lead(.$E))^2 + (.$N - lead(.$N))^2
  } %>%
  sqrt()

# calculating speed based on timelag (t) in secs and steplength (s) in meter ####

wildboar_lags$speed <- wildboar_raw %>% {
  .$steplength / .$timelag
}

# cross-scale movement analysis ####

seq3 <- seq(from = 1, to = nrow(wildboar_raw), by = 3)
seq6 <- seq(from = 1, to = nrow(wildboar_raw), by = 6)
seq9 <- seq(from = 1, to = nrow(wildboar_raw), by = 9)

wildboar_3 <- wildboar_lags %>% slice(seq3)
wildboar_6 <- wildboar_lags %>% slice(seq6)
wildboar_9 <- wildboar_lags %>% slice(seq9)

# sample code for different intervals (to be discussed)

wildboar_3$steplength <- wildboar_3 %>%
  {
    (.$E - lead(.$E))^2 + (.$N - lead(.$N))^2
  } %>%
  sqrt()

wildboar_3$speed <- wildboar_3 %>% {
  .$steplength / .$timelag
}


