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

# create a sample of caroline
caro <- wildboar_sf %>% 
  filter(
    year == 2015,
    month == 5,
    TierName == "Caroline"
  )

# create a sample of Frida
frida <- wildboar_sf %>% 
  filter(
    year == 2016,
    month == 5,
    TierName == "Frida"
  )

# create a sample of Ueli
ueli <- wildboar_sf %>% 
  filter(
    year == 2016,
    month == 5,
    TierName == "Ueli"
  )

# step 1: sequence data (resolution to be discussed) ####

caro_seq3 <- seq(from = 1, to = nrow(caro), by = 3)
caro_seq6 <- seq(from = 1, to = nrow(caro), by = 6)
frida_seq3 <- seq(from = 1, to = nrow(frida), by = 3)
frida_seq6 <- seq(from = 1, to = nrow(frida), by = 6)
ueli_seq3 <- seq(from = 1, to = nrow(ueli), by = 3)
caro_3 <- caro %>% slice(caro_seq3)
caro_6 <- caro %>% slice(caro_seq6)
frida_3 <- frida %>% slice(frida_seq3)
frida_6 <- frida %>% slice(frida_seq6)
ueli_3 <- ueli %>% slice(ueli_seq3)

# step 2: calculate speed, steplength and timelag for segmented data ####
calc_movement_param <- function(boar_dt) {
  boar_dt %>% 
    mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
           steplength = sqrt(((E-lead(E,1))^2+(N-lead(N,1))^2)),
           speed = steplength/timelag,
           nMinus3 = sqrt((lag(E, 3) - E)^2 + (lag(N, 3) - N)^2),
           nMinus2 = sqrt((lag(E, 2) - E)^2 + (lag(N, 2) - N)^2),
           nMinus1 = sqrt((lag(E, 1) - E)^2 + (lag(N, 1) - N)^2),
           nPlus1 = sqrt((E - lead(E, 1))^2 + (N - lead(N, 1))^2),
           nPlus2 = sqrt((E - lead(E, 2))^2 + (N - lead(N, 2))^2),
           nPlus3 = sqrt((E - lead(E, 3))^2 + (N - lead(N, 3))^2)
    ) %>% 
    rowwise() %>%
    mutate(
      stepmean = mean(c(nMinus3, nMinus2, nMinus1,nPlus1,nPlus2, nPlus3))
    ) %>%
    ungroup()
  boar_dt
}

caro <- calc_movement_param(caro)
caro_3 <- calc_movement_param(caro_3)

caro_3 <- caro_3 %>% 
mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
       steplength = sqrt(((E-lead(E,1))^2+(N-lead(N,1))^2)),
       speed = steplength/timelag,
       nMinus3 = sqrt((lag(E, 3) - E)^2 + (lag(N, 3) - N)^2),
       nMinus2 = sqrt((lag(E, 2) - E)^2 + (lag(N, 2) - N)^2),
       nMinus1 = sqrt((lag(E, 1) - E)^2 + (lag(N, 1) - N)^2),
       nPlus1 = sqrt((E - lead(E, 1))^2 + (N - lead(N, 1))^2),
       nPlus2 = sqrt((E - lead(E, 2))^2 + (N - lead(N, 2))^2),
       nPlus3 = sqrt((E - lead(E, 3))^2 + (N - lead(N, 3))^2)
) %>% 
  rowwise() %>%
  mutate(
    stepmean = mean(c(nMinus3, nMinus2, nMinus1,nPlus1,nPlus2, nPlus3))
  ) %>%
  ungroup()

caro_6 <- caro_6 %>% 
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
         steplength = sqrt(((E-lead(E,1))^2+(N-lead(N,1))^2)),
         speed = steplength/timelag,
         nMinus3 = sqrt((lag(E, 3) - E)^2 + (lag(N, 3) - N)^2),
         nMinus2 = sqrt((lag(E, 2) - E)^2 + (lag(N, 2) - N)^2),
         nMinus1 = sqrt((lag(E, 1) - E)^2 + (lag(N, 1) - N)^2),
         nPlus1 = sqrt((E - lead(E, 1))^2 + (N - lead(N, 1))^2),
         nPlus2 = sqrt((E - lead(E, 2))^2 + (N - lead(N, 2))^2),
         nPlus3 = sqrt((E - lead(E, 3))^2 + (N - lead(N, 3))^2)
  ) %>% 
  rowwise() %>%
  mutate(
    stepmean = mean(c(nMinus3, nMinus2, nMinus1,nPlus1,nPlus2, nPlus3))
  ) %>%
  ungroup()

frida <- frida %>% 
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
         steplength = sqrt(((E-lead(E,1))^2+(N-lead(N,1))^2)),
         speed = steplength/timelag,
         nMinus3 = sqrt((lag(E, 3) - E)^2 + (lag(N, 3) - N)^2),
         nMinus2 = sqrt((lag(E, 2) - E)^2 + (lag(N, 2) - N)^2),
         nMinus1 = sqrt((lag(E, 1) - E)^2 + (lag(N, 1) - N)^2),
         nPlus1 = sqrt((E - lead(E, 1))^2 + (N - lead(N, 1))^2),
         nPlus2 = sqrt((E - lead(E, 2))^2 + (N - lead(N, 2))^2),
         nPlus3 = sqrt((E - lead(E, 3))^2 + (N - lead(N, 3))^2)
  ) %>% 
  rowwise() %>%
  mutate(
    stepmean = mean(c(nMinus3, nMinus2, nMinus1,nPlus1,nPlus2, nPlus3))
  ) %>%
  ungroup()

frida_3 <- frida_3 %>% 
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
         steplength = sqrt(((E-lead(E,1))^2+(N-lead(N,1))^2)),
         speed = steplength/timelag,
         nMinus3 = sqrt((lag(E, 3) - E)^2 + (lag(N, 3) - N)^2),
         nMinus2 = sqrt((lag(E, 2) - E)^2 + (lag(N, 2) - N)^2),
         nMinus1 = sqrt((lag(E, 1) - E)^2 + (lag(N, 1) - N)^2),
         nPlus1 = sqrt((E - lead(E, 1))^2 + (N - lead(N, 1))^2),
         nPlus2 = sqrt((E - lead(E, 2))^2 + (N - lead(N, 2))^2),
         nPlus3 = sqrt((E - lead(E, 3))^2 + (N - lead(N, 3))^2)
  ) %>% 
  rowwise() %>%
  mutate(
    stepmean = mean(c(nMinus3, nMinus2, nMinus1,nPlus1,nPlus2, nPlus3))
  ) %>%
  ungroup()

frida_6 <- frida_6 %>% 
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
         steplength = sqrt(((E-lead(E,1))^2+(N-lead(N,1))^2)),
         speed = steplength/timelag,
         nMinus3 = sqrt((lag(E, 3) - E)^2 + (lag(N, 3) - N)^2),
         nMinus2 = sqrt((lag(E, 2) - E)^2 + (lag(N, 2) - N)^2),
         nMinus1 = sqrt((lag(E, 1) - E)^2 + (lag(N, 1) - N)^2),
         nPlus1 = sqrt((E - lead(E, 1))^2 + (N - lead(N, 1))^2),
         nPlus2 = sqrt((E - lead(E, 2))^2 + (N - lead(N, 2))^2),
         nPlus3 = sqrt((E - lead(E, 3))^2 + (N - lead(N, 3))^2)
  ) %>% 
  rowwise() %>%
  mutate(
    stepmean = mean(c(nMinus3, nMinus2, nMinus1,nPlus1,nPlus2, nPlus3))
  ) %>%
  ungroup()

ueli <- ueli %>% 
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
         steplength = sqrt(((E-lead(E,1))^2+(N-lead(N,1))^2)),
         speed = steplength/timelag)

ueli_3 <- ueli_3 %>% 
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
         steplength = sqrt(((E-lead(E,1))^2+(N-lead(N,1))^2)),
         speed = steplength/timelag)

# Gibt es einen Grund speed so zu berechnen? Alternativ habe ich es oben implementiert
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

# step 3: plot histogram and movement trajectories ####
# plot histogram of steplength

hist_steplength <- ggplot(frida_6, aes(x = stepmean))
hist_steplength + geom_histogram(binwidth = 10) + 
  scale_x_continuous(limits = c(0,800)) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) +
  theme_bw() +
  theme(panel.border = element_blank())

# plot trajectories ####

caro %>%
  ggplot(aes(E, N)) +
  geom_path(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  theme(panel.border = element_blank())

caro_3 %>%
  ggplot(aes(E, N)) +
  geom_path(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  theme(panel.border = element_blank())

frida %>%
  ggplot(aes(E, N)) +
  geom_path(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  theme(panel.border = element_blank())

frida_3 %>%
  ggplot(aes(E, N)) +
  geom_path(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  theme(panel.border = element_blank())

ueli %>%
  ggplot(aes(E, N)) +
  geom_path(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  theme(panel.border = element_blank())

ueli_3 %>%
  ggplot(aes(E, N)) +
  geom_path(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  theme(panel.border = element_blank())

# step 4: define threshold and assing movement status ####


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

