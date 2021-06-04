
# libraries ---------------------------------------------------------------

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

# plot animals
# ggplot(wildboar_raw, aes(E, N, colour = TierName)) +
#  geom_point() +

wildboar_sf %>% 
  ggplot(aes(fill = TierName)) +
  geom_sf(alpha = 0.4) +
  coord_sf(datum = 2056) 

# sampling regime

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

# temporal overlap

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

# spatial overlap

# convex hull

wildboar_sf <- wildboar_sf %>%
  mutate(tiercollar = paste(TierID, TierName, CollarID)) 

mcp <- wildboar_sf %>%
  group_by(TierID, TierName, CollarID) %>%
  summarise() %>%
  st_convex_hull()

# plot convex hull

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

