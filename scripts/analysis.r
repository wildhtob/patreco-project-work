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
# argument remove = False keeps the coordinates E and N 
wildboar_sf <- st_as_sf(wildboar_raw, coords = c("E", "N"), crs = 2056, remove = FALSE)

# data exploration --------------------------------------------------------

# plotting data points ####
# 
# ggplot(wildboar_sf, aes(color = TierName)) +
#   geom_sf(alpha = 0.4) +
#   coord_sf(datum = 2056)
# 
# ggplot(wildboar_sf, aes(color = as.factor(TierID))) +
#   geom_sf(alpha = 0.4) +
#   coord_sf(datum = 2056) +
#   scale_color_discrete(name = "AnimalID")

# sampling regime ####
# 
# limits <- c(0,200)
# breaks = seq(0,200,50)
# labels = paste(c(rep("",length(breaks)-1),">"), breaks)
# 
# wildboar_raw %>%
#   mutate(TierName = fct_reorder(TierName, DatetimeUTC,min, .desc = TRUE)) %>%
#   group_by(TierID, TierName, CollarID) %>%
#   mutate(
#     timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC, units = "mins")),
#   ) %>%
#   ggplot(aes(DatetimeUTC, TierName, colour = timelag)) +
#   geom_line(lwd = 10) +
#   scale_color_gradientn(name = "Sampling interval", colours = RColorBrewer::brewer.pal(11, "Spectral"), limits = limits, na.value = NA, oob = scales::squish, breaks = seq(0,200,50), labels = labels) +
#   theme_minimal() +
#   theme(legend.position = "top") +
#   guides(color = guide_colorbar(title.position = "top", title.hjust = .5, barwidth = unit(20, "lines"), barheight = unit(.5, "lines")))

# temporal overlap ####

# ?wildschwein_overlap_temp
# 
# sampling_periods <- wildboar_raw %>%
#   group_by(TierID, TierName, CollarID) %>%
#   summarise(
#     min = min(DatetimeUTC),
#     max = max(DatetimeUTC)
#   )
# 
# wildboar_overlap <- wildboar_overlap %>%
#   left_join(sampling_periods, by = c("TierID", "TierName", "CollarID"))
# 
# wildboar_overlap %>%
#   mutate(TierCollar = paste(TierName, CollarID)) %>%
#   ggplot(aes(xmin = min, xmax = max, y = TierCollar)) +
#   geom_errorbarh() +
#   facet_grid(Groups~., scales = "free_y", space = "free_y")
# 
# ggplot(wildboar_sf, aes(x = DatetimeUTC, y = TierName)) +
#   geom_point() +
#   scale_x_datetime(breaks = "1 month") +
#   theme_grey()


# spatial overlap ####

# convex hull ####
# 
# wildboar_sf <- wildboar_sf %>%
#   mutate(tiercollar = paste(TierID, TierName, CollarID)) 
# 
# mcp <- wildboar_sf %>%
#   group_by(TierID, TierName, CollarID) %>%
#   summarise() %>%
#   st_convex_hull()

# plot convex hull ####
# 
# mcp %>%
#   mutate(tiercollar = paste(TierID, TierName, CollarID)) %>%
#   ggplot(aes(fill = factor(TierID))) + geom_sf(alpha = 0.1) +
#   coord_sf(datum = 2056) +
#   facet_wrap(~tiercollar) +
#   theme(legend.position = "none")
# 
# 
# tmap_mode("view") +
#   tm_shape(mcp) +
#   tm_fill("TierName", alpha = 0.5) +
#   tm_borders(col = "red", lwd = 1) +
#   tm_layout(legend.bg.color = "white")

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

wildboar_lags$speed <- wildboar_lags %>% {
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

wildboar_3 <- wildboar_3 %>% 
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC), units = "secs"))

wildboar_3$steplength <- wildboar_3 %>%
  {
    (.$E - lead(.$E))^2 + (.$N - lead(.$N))^2
  } %>%
  sqrt()

wildboar_3$speed <- wildboar_3 %>% {
  .$steplength / .$timelag
}


# Sample data -------------------------------------------------------------

# step0 : select caroline and two weeks ####

wildboar_sf <- wildboar_sf %>% 
  mutate(
    month = month(DatetimeUTC),
    year = year(DatetimeUTC)
         )


caro <- wildboar_sf %>% 
  filter(
    year == 2015,
    month == 5,
    TierName == "Caroline"
  )

# step 1: sequence data (resolution to be discussed) ####

seq3 <- seq(from = 1, to = nrow(caro), by = 3)


caro_3 <- caro %>% slice(seq3)

# step 3a: timelag for segmented data ####

# problem mit code: es werden minuten berechnet, obschon sekunden angegeben werden. evtl
# ein problem mit dem filter in zeile 191f?
caro_3 <- caro_3 %>% 
mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
       steplength = sqrt(((E-lead(E,1))^2+(N-lead(N,1))^2)),
       speed = steplength/timelag)

caro <- caro %>% 
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
         steplength = sqrt(((E-lead(E,1))^2+(N-lead(N,1))^2)),
         speed = steplength/timelag)

# stept 3b: steplength and speed for segmented data ####
# Gibt es einen Grund speed so zu berechnen? Alternativ habe ich es in Zeile 217 und 218 implementiert
# caro_3$steplength <- caro_3 %>%
#   {
#     (.$E - lead(.$E))^2 + (.$N - lead(.$N))^2
#   } %>%
#   sqrt()
# 
# caro_3$speed <- caro_3 %>% {
#   .$steplength / .$timelag
# }
# 
# 
# caro$steplength <- caro %>%
#   {
#     (.$E - lead(.$E))^2 + (.$N - lead(.$N))^2
#   } %>%
#   sqrt()
# 
# caro$speed <- caro %>% {
#   .$steplength / .$timelag
# }

# plot trajectories ####

caro_3 %>%
  ggplot(aes(E, N)) +
  geom_path(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  theme(panel.border = element_blank())

caro %>%
  ggplot(aes(E, N)) +
  geom_path(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  theme(panel.border = element_blank())


# convex hull ####

mcp_caro <- caro %>%
  group_by(TierID, TierName, CollarID) %>%
  summarise() %>%
  st_convex_hull()

# plot convex hull ####

tmap_mode("view") +
  tm_shape(mcp_caro) +
  tm_fill("TierName", alpha = 0.5) +
  tm_borders(col = "red", lwd = 1) +
  tm_layout(legend.bg.color = "white")
