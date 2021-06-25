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
library(forcats) # handling factor levels
library(raster) # rasterizing vector data

# globals ----------------------------------------------------------------------

# threshold that triggers the segmentation of the movement trajectory

segment_trigger <- as.numeric(30)

# functions ---------------------------------------------------------------

calc_movement_param <- function(boar_dt) {
  boar_dt <- boar_dt %>%
    mutate(
      timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
      steplength = sqrt(((E - lead(E, 1))^2 + (N - lead(N, 1))^2)),
      speed = steplength / timelag,
      nMinus3 = sqrt((lag(E, 3) - E)^2 + (lag(N, 3) - N)^2),
      nMinus2 = sqrt((lag(E, 2) - E)^2 + (lag(N, 2) - N)^2),
      nMinus1 = sqrt((lag(E, 1) - E)^2 + (lag(N, 1) - N)^2),
      nPlus1 = sqrt((E - lead(E, 1))^2 + (N - lead(N, 1))^2),
      nPlus2 = sqrt((E - lead(E, 2))^2 + (N - lead(N, 2))^2),
      nPlus3 = sqrt((E - lead(E, 3))^2 + (N - lead(N, 3))^2)
    ) %>%
    rowwise() %>%
    mutate(
      stepmean = mean(c(nMinus3, nMinus2, nMinus1, nPlus1, nPlus2, nPlus3)),
    ) %>%
    ungroup() %>%
    mutate(
      mov_static = if_else(stepmean < segment_trigger, TRUE, FALSE),
      mov_status = if_else(stepmean < segment_trigger, "resting", "moving"),
      cma_static = stepmean < mean(stepmean, na.rm = TRUE)
    )
  boar_dt
}

rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
}

# data import -------------------------------------------------------------

wildboar_raw <- wildschwein_BE
head(wildboar_raw)

wildboar_meta <- wildschwein_metadata
head(wildboar_meta)
?wildschwein_metadata

wildboar_overlap <- wildschwein_overlap_temp
head(wildboar_overlap)
?wildschwein_overlap_temp

# import feldaufnahmen
feldaufnahmen <- read_sf("data/Feldaufnahmen_Fanel.gpkg")

# add geometry and time---------------------------------------------------------

# argument remove = False keeps the coordinates E and N
wildboar_sf <- st_as_sf(wildboar_raw, coords = c("E", "N"), crs = 2056, remove = FALSE)
# join feldaufnahmen with wildboar data
wildboar_sf <- st_join(x = wildboar_sf, y = feldaufnahmen)
# add time
wildboar_sf <- wildboar_sf %>%
  mutate(
    month = month(DatetimeUTC),
    year = year(DatetimeUTC)
  )

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
# sampling_regime <- wildboar_raw %>%
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

# replace wildboar_raw by wildboar_sf for spatial information
wildboar_lags <- wildboar_sf %>%
  group_by(TierID) %>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC), units = "secs"))

# round lags on -2 digits
wildboar_lags$timelag_rounded <- round(wildboar_lags$timelag, -2)

# build categories with rounded lags
wildboar_lag_cat <- wildboar_lags %>%
  st_drop_geometry() %>%
  group_by(timelag_rounded) %>%
  summarise(count = n())
wildboar_lag_cat

# compares count of timelag_rounded
hist_lag_cat <- ggplot(wildboar_lag_cat)
hist_lag_cat + geom_col(aes(x = timelag_rounded, y = count)) +
  coord_cartesian(ylim=c(0,200000), xlim=c(0,5000)) +
  theme_bw() +
  theme(panel.border = element_blank())

# viualise change in lag over time ####

# takes a while for plotting..
# wildboar_lags %>%
#   filter(timelag < 30000 & timelag > 0) %>%
#   ggplot(., aes(DatetimeUTC, timelag, col = TierName)) +
#   geom_point() +
#   geom_line(size = 0.1)

# DELETE: calculating steplength ####

# wildboar_lags$steplength <- wildboar_raw %>%
#   {
#     (.$E - lead(.$E))^2 + (.$N - lead(.$N))^2
#   } %>%
#   sqrt()

# calculating speed based on timelag (t) in secs and steplength (s) in meter ####

# wildboar_lags$speed <- wildboar_lags %>% {
#   .$steplength / .$timelag
# }


# sample code for different intervals (to be discussed)
# wildboar_3 <- wildboar_3 %>%
#   mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC), units = "secs"))
#
# wildboar_3$steplength <- wildboar_3 %>%
#   {
#     (.$E - lead(.$E))^2 + (.$N - lead(.$N))^2
#   } %>%
#   sqrt()
#
# wildboar_3$speed <- wildboar_3 %>% {
#   .$steplength / .$timelag
# }

# preprocessing ####

# assign convenience variables for step 6
wildboar_lags <- wildboar_lags %>%
  # add wallow and nest criterias according to literature
  # enable line below if you want to filter all NAs in Frucht
  # filter(!is.na(Frucht)) %>%
  mutate(
    wallow_month = if_else(month > 3 & month < 10, TRUE, FALSE),
    wallow_day = case_when(
      day == "Abenddaemmerung" ~ FALSE,
      day == "Morgendaemmerung" ~ TRUE,
      day == "Nacht" ~ FALSE,
      day == "1Nachtviertel" ~ FALSE,
      day == "2Nachtviertel" ~ FALSE,
      day == "3Nachtviertel" ~ FALSE,
      day == "4Nachtviertel" ~ FALSE,
      day == "Tag" ~ TRUE,
      TRUE ~ NA # Default case
    ),
    wallow_area = case_when(
      Frucht == "Feuchtgebiet" ~ TRUE, # Literature says so?
      Frucht == "Wald" ~ TRUE,
      Frucht == "Weizen" ~ FALSE,
      Frucht == "Gerste" ~ FALSE,
      Frucht == "Zwiebeln" ~ FALSE,
      Frucht == "Bohnen" ~ FALSE,
      Frucht == "Kartoffeln" ~ FALSE,
      Frucht == "Rueben" ~ FALSE,
      Frucht == "Chinaschilf" ~ FALSE,
      Frucht == "Mangold" ~ FALSE,
      Frucht == "Wiese" ~ FALSE,
      Frucht == "Kohlrabi" ~ FALSE,
      Frucht == "Weide" ~ FALSE,
      Frucht == "Lupinen" ~ FALSE,
      Frucht == "Flugplatz" ~ FALSE,
      Frucht == "Mais" ~ FALSE,
      Frucht == "Raps" ~ FALSE,
      Frucht == "Karotten" ~ FALSE,
      Frucht == "Acker" ~ FALSE,
      Frucht == "Sonnenblumen" ~ FALSE,
      Frucht == "Erbsen" ~ FALSE,
      Frucht == "Kohl" ~ FALSE,
      Frucht == "Hafer" ~ FALSE,
      Frucht == "Roggen" ~ FALSE,
      Frucht == "Salat" ~ FALSE,
      Frucht == "Rhabarber" ~ FALSE,
      Frucht == "Sellerie" ~ FALSE,
      Frucht == "Brache" ~ FALSE,
      Frucht == "Spargel" ~ FALSE,
      Frucht == "Obstplantage" ~ FALSE,
      Frucht == "Fenchel" ~ FALSE,
      Frucht == "Gemuese" ~ FALSE,
      Frucht == "Gewaechshaus" ~ FALSE,
      Frucht == "Zuchetti" ~ FALSE,
      Frucht == "Zucchetti" ~ FALSE,
      Frucht == "Flachs" ~ FALSE,
      Frucht == "Kuerbis" ~ FALSE,
      TRUE ~ NA # Default case
    ),
    nest_day = case_when(
      day == "Abenddaemmerung" ~ FALSE,
      day == "Morgendaemmerung" ~ FALSE,
      day == "Nacht" ~ FALSE,
      day == "1Nachtviertel" ~ FALSE,
      day == "2Nachtviertel" ~ FALSE,
      day == "3Nachtviertel" ~ FALSE,
      day == "4Nachtviertel" ~ FALSE,
      day == "Tag" ~ TRUE,
      TRUE ~ NA # Default case
    ),
    nest_summer_mon = if_else(month >= 5 & month <= 10, TRUE, FALSE),
    nest_winter_mon = if_else(month >= 5 & month <= 10, FALSE, TRUE),
    nest_summer_area = case_when(
      Frucht == "Feuchtgebiet" ~ FALSE,
      Frucht == "Wald" ~ TRUE,
      Frucht == "Weizen" ~ TRUE,
      Frucht == "Gerste" ~ TRUE,
      Frucht == "Zwiebeln" ~ FALSE,
      Frucht == "Bohnen" ~ FALSE,
      Frucht == "Kartoffeln" ~ FALSE,
      Frucht == "Rueben" ~ FALSE,
      Frucht == "Chinaschilf" ~ FALSE,
      Frucht == "Mangold" ~ FALSE,
      Frucht == "Wiese" ~ FALSE,
      Frucht == "Kohlrabi" ~ FALSE,
      Frucht == "Weide" ~ FALSE,
      Frucht == "Lupinen" ~ FALSE,
      Frucht == "Flugplatz" ~ FALSE,
      Frucht == "Mais" ~ TRUE,
      Frucht == "Raps" ~ TRUE,
      Frucht == "Karotten" ~ FALSE,
      Frucht == "Acker" ~ FALSE,
      Frucht == "Sonnenblumen" ~ FALSE,
      Frucht == "Erbsen" ~ FALSE,
      Frucht == "Kohl" ~ FALSE,
      Frucht == "Hafer" ~ TRUE,
      Frucht == "Roggen" ~ TRUE,
      Frucht == "Salat" ~ FALSE,
      Frucht == "Rhabarber" ~ FALSE,
      Frucht == "Sellerie" ~ FALSE,
      Frucht == "Brache" ~ FALSE,
      Frucht == "Spargel" ~ FALSE,
      Frucht == "Obstplantage" ~ FALSE,
      Frucht == "Fenchel" ~ FALSE,
      Frucht == "Gemuese" ~ FALSE,
      Frucht == "Gewaechshaus" ~ FALSE,
      Frucht == "Zuchetti" ~ FALSE,
      Frucht == "Zucchetti" ~ FALSE,
      Frucht == "Flachs" ~ FALSE,
      Frucht == "Kuerbis" ~ FALSE,
      TRUE ~ NA # Default case
    ),
    nest_winter_area = case_when(
      Frucht == "Feuchtgebiet" ~ TRUE,
      Frucht == "Wald" ~ TRUE,
      Frucht == "Weizen" ~ FALSE,
      Frucht == "Gerste" ~ FALSE,
      Frucht == "Zwiebeln" ~ FALSE,
      Frucht == "Bohnen" ~ FALSE,
      Frucht == "Kartoffeln" ~ FALSE,
      Frucht == "Rueben" ~ FALSE,
      Frucht == "Chinaschilf" ~ FALSE,
      Frucht == "Mangold" ~ FALSE,
      Frucht == "Wiese" ~ FALSE,
      Frucht == "Kohlrabi" ~ FALSE,
      Frucht == "Weide" ~ FALSE,
      Frucht == "Lupinen" ~ FALSE,
      Frucht == "Flugplatz" ~ FALSE,
      Frucht == "Mais" ~ FALSE,
      Frucht == "Raps" ~ FALSE,
      Frucht == "Karotten" ~ FALSE,
      Frucht == "Acker" ~ FALSE,
      Frucht == "Sonnenblumen" ~ FALSE,
      Frucht == "Erbsen" ~ FALSE,
      Frucht == "Kohl" ~ FALSE,
      Frucht == "Hafer" ~ FALSE,
      Frucht == "Roggen" ~ FALSE,
      Frucht == "Salat" ~ FALSE,
      Frucht == "Rhabarber" ~ FALSE,
      Frucht == "Sellerie" ~ FALSE,
      Frucht == "Brache" ~ FALSE,
      Frucht == "Spargel" ~ FALSE,
      Frucht == "Obstplantage" ~ FALSE,
      Frucht == "Fenchel" ~ FALSE,
      Frucht == "Gemuese" ~ FALSE,
      Frucht == "Gewaechshaus" ~ FALSE,
      Frucht == "Zuchetti" ~ FALSE,
      Frucht == "Zucchetti" ~ FALSE,
      Frucht == "Flachs" ~ FALSE,
      Frucht == "Kuerbis" ~ FALSE,
      TRUE ~ NA # Default case
    )
  )

# This checks a certain variable for NAs
# na_check <- wildboar_lags %>%
#   filter(is.na(wallow_area))

wildboar_lags <- calc_movement_param(wildboar_lags)
wildboar_lags <- wildboar_lags %>%
  # run segment-based analysis with rle_id. copied from cma_week 3
  # choose between: own threshold (mov_static) or from cma_week 3 (cma_static)
  mutate(segment_id = rle_id(mov_static)) %>%
  # select only rows with an certain timelag
  # if no filter is applied, sequencing creates misleading results
  # fraglich, ob der filter hier angewendet werden soll oder nach Sample data
  filter(timelag_rounded == 900)


summary(as.factor(wildboar_lags$mov_status))

mov_status <- data.frame(summary(as.factor(wildboar_lags$mov_status))) %>% 
  rename(count = 1) %>% 
  slice(-3) %>% 
  rownames_to_column(var = "status") %>% 
  mutate(prop = count/sum(count) * 100,
         prop = round(prop, 1)) %>% 
  arrange(desc(status)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5 * prop)

pie <- ggplot(mov_status, aes(x = "", y = prop, fill = status)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = prop), color = "white") +
  scale_fill_discrete(name = "Status", labels = c("Resting", "Moving")) +
  theme_void()

pie

# cross-scale movement analysis ####
seq3 <- seq(from = 1, to = nrow(wildboar_raw), by = 3)
seq6 <- seq(from = 1, to = nrow(wildboar_raw), by = 6)
seq9 <- seq(from = 1, to = nrow(wildboar_raw), by = 9)
# Mein Vorschlag: unsere Auswertung aller Daten basiert auf wildboar_lags
# wildboar_3 <- wildboar_lags %>% slice(seq3)
# wildboar_6 <- wildboar_lags %>% slice(seq6)
# wildboar_9 <- wildboar_lags %>% slice(seq9)

# Sample data -------------------------------------------------------------

# step0 : select three different wildboars for a duration of two weeks ####
# Kritisch: hier werden samples aus wildboar_sf kreiert. so gehen viele
# convenience variablen verloren. Zudem ist es fehleranfälliger. Überdenken.

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
# for sample data, for wildboar_lags its already done
caro_seq3 <- seq(from = 1, to = nrow(caro), by = 3)
caro_seq6 <- seq(from = 1, to = nrow(caro), by = 6)
frida_seq3 <- seq(from = 1, to = nrow(frida), by = 3)
frida_seq6 <- seq(from = 1, to = nrow(frida), by = 6)
ueli_seq3 <- seq(from = 1, to = nrow(ueli), by = 3)
ueli_seq6 <- seq(from = 1, to = nrow(ueli), by = 6)
caro_3 <- caro %>% slice(caro_seq3)
caro_6 <- caro %>% slice(caro_seq6)
frida_3 <- frida %>% slice(frida_seq3)
frida_6 <- frida %>% slice(frida_seq6)
ueli_3 <- ueli %>% slice(ueli_seq3)
ueli_6 <- ueli %>% slice(ueli_seq6)


# step 2: calculate speed, steplength and timelag for segmented da --------



# for sample data, for wildboar_lags its already done
caro <- calc_movement_param(caro)
caro_3 <- calc_movement_param(caro_3)
caro_6 <- calc_movement_param(caro_6)
frida <- calc_movement_param(frida)
frida_3 <- calc_movement_param(frida_3)
frida_6 <- calc_movement_param(frida_6)
ueli <- calc_movement_param(ueli)
ueli_3 <- calc_movement_param(ueli_3)
ueli_6 <- calc_movement_param(ueli_6)



# step 3: plot histogram and movement trajectories ------------------------


# plot histogram of steplmean
# mit einem stepmean von 6 Datenpunkten im Intervall von 15 Minuten wird
# meiner Meinung nach gut ersichtlich wann die Wildschweine pausieren.
# Hier einsetzen und vergleichen: caro_6, frida_6, ueli_6
# Mein Vorschlag fuer segment_trigger: 30 bis 50 Meter, GPS genauigkeit bedenken.
hist_steplength <- ggplot(frida_6, aes(x = stepmean))
hist_steplength + geom_histogram(binwidth = 5) +
  scale_x_continuous(limits = c(0, 800)) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) +
  theme_bw() +
  theme(panel.border = element_blank())


# step 4 to 6: assign movement status and apply criterias -----------------


# for all data

wildboar_lags <- wildboar_lags %>%
  group_by(segment_id) %>%
  mutate(segment_dur = n()) %>%
  filter(row_number(segment_id) == 1) %>% # keep only the first data point per segment
  ungroup() %>%
  mutate(wallow_dur = if_else(segment_dur <= 3, TRUE, FALSE)) %>%
  filter(mov_status == "resting") %>%
  mutate(
    wallow = if_else(wallow_month & wallow_day & wallow_area & wallow_dur,
      TRUE, FALSE
    ),
    nest = if_else((nest_summer_mon & nest_day & nest_summer_area & segment_dur > 3) |
      (nest_winter_mon & nest_day & nest_winter_area & segment_dur > 3),
    TRUE, FALSE
    ),
    conflict = if_else(nest & wallow, TRUE, FALSE),
    site_type = as.factor(case_when(
      conflict == TRUE ~ "both",
      nest == TRUE ~ "nest",
      wallow == TRUE ~ "wallow",
      !wallow & !nest == TRUE ~ "none",
      TRUE ~ "NA" # Default case
    ))
  )
  

# check the dataset (number of wallows, nests, NAs etc)

summary(wildboar_lags)

# create pie chart

site_type <- data.frame(summary(wildboar_lags$site_type)) %>% 
  rename(count = 1) %>% 
  rownames_to_column(var = "type") %>% 
  mutate(prop = count/sum(count) * 100,
         prop = round(prop, 1)) %>% 
  arrange(desc(type)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5 * prop)

pie <- ggplot(site_type, aes(x = "", y = prop, fill = type)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = prop), color = "white") +
  scale_fill_discrete(name = "Type", labels = c("NA", "Nest", "None", "Wallow")) +
  theme_void()

pie




# plot sample data -------------------------------------------------------


# generate new samples from wildboar_lags data

ueli_filter <- wildboar_lags %>%
  filter(
    year == 2016,
    # month == 5,
    site_type == "wallow" | site_type == "nest",
    TierName == "Ueli"
  ) %>% 
  mutate(site_type = droplevels(site_type))

frida_filter <- wildboar_lags %>%
  filter(
    year == 2016,
    # month == 5,
    site_type == "wallow" | site_type == "nest",
    TierName == "Frida"
  ) %>% 
  mutate(site_type = droplevels(site_type))

caro_filter <- wildboar_lags %>%
  filter(
    year == 2016,
    # month == 5,
    site_type == "wallow" | site_type == "nest",
    TierName == "Caroline"
  ) %>% 
  mutate(site_type = droplevels(site_type))

wallows_sample <- wildboar_lags %>%
  filter(
    year == 2016,
    # month == 5,
    site_type == "wallow", 
    TierName == "Caroline" | TierName == "Ueli" | TierName == "Frida"
  ) %>% 
  mutate(site_type = droplevels(site_type))

nests_sample <- wildboar_lags %>%
  filter(
    year == 2016,
    # month == 5,
    site_type == "nest", 
    TierName == "Caroline" | TierName == "Ueli" | TierName == "Frida"
  ) %>% 
  mutate(site_type = droplevels(site_type))



# frida_filter <- wildboar_lags %>%
#   filter(
#     year == 2016,
#     # month == 5,
#     TierName == "Frida",
#     site_type == "nest"
#   )
# 
# caro_filter <- wildboar_lags %>%
#   filter(
#     year == 2016,
#     # month == 5,
#     TierName == "Caroline",
#     # site_type == "nest"
#   )

# Plot site_type
# alter site_type to explore (nest, wallow, both, none and NA)
# alter data to explore different boars

# ggplot(data = ueli_filter, mapping = aes(E, N, colour = segment_id)) +
#   # geom_path() +
#   geom_point() +
#   coord_equal() +
#   labs(title = "Moving segements coloured by segment ID") +
#   theme_classic() +
#   # RStudio crashes if legend.position "bottom" is chosen
#   theme(legend.position = "none")

# Polt interactive map with sample animals ####

# one layer per animal

oranges <- tmaptools::get_brewer_pal("Oranges", n = 3, contrast = c(0.3, 0.9))
purples <- tmaptools::get_brewer_pal("Purples", n = 3, contrast = c(0.3, 0.9))

tm_animals <- 
  tmap_mode("view") +
  tm_shape(ueli_filter) +
  tm_symbols(col = "site_type", size = "segment_dur", alpha = 0.5, title.col = "Ueli", 
             palette = c("orange", "purple"), size.lim = c(1, 10)) +
  tm_shape(frida_filter) +
  tm_symbols(col = "site_type", size = "segment_dur", alpha = 0.5, title.col = "Frida", 
             palette = c("orange", "purple"), size.lim = c(1, 10)) +
    tm_shape(caro_filter) +
  tm_symbols(col = "site_type", size = "segment_dur", alpha = 0.5, title.col = "Caroline", 
             palette = c("orange", "purple"), size.lim = c(1, 10))

# one layer per field type (nests and wallos only)
tm_sample <- 
  tmap_mode("view") +
  tm_shape(nests_sample) +
  tm_symbols(col = "TierName", size = "segment_dur", alpha = 0.5, title.col = "Nests", 
             palette = oranges, size.lim = c(1, 10)) +
  tm_shape(wallows_sample) +
  tm_symbols(col = "TierName", size = "segment_dur", alpha = 0.5, title.col = "Wallows", 
             palette = purples, size.lim = c(1, 10))

tm_sample



# unused plots ------------------------------------------------------------


# caro %>%
#   ggplot(aes(E, N)) +
#   geom_path(alpha = 0.5) +
#   geom_point(alpha = 0.5) +
#   theme_bw() +
#   theme(panel.border = element_blank())
#
# caro_3 %>%
#   ggplot(aes(E, N)) +
#   geom_path(alpha = 0.5) +
#   geom_point(alpha = 0.5) +
#   theme_bw() +
#   theme(panel.border = element_blank())
#
# frida %>%
#   ggplot(aes(E, N)) +
#   geom_path(alpha = 0.5) +
#   geom_point(alpha = 0.5) +
#   theme_bw() +
#   theme(panel.border = element_blank())
#
# frida_3 %>%
#   ggplot(aes(E, N)) +
#   geom_path(alpha = 0.5) +
#   geom_point(alpha = 0.5) +
#   theme_bw() +
#   theme(panel.border = element_blank())
#
# ueli %>%
#   ggplot(aes(E, N)) +
#   geom_path(alpha = 0.5) +
#   geom_point(alpha = 0.5) +
#   theme_bw() +
#   theme(panel.border = element_blank())
#
# ueli_3 %>%
#   ggplot(aes(E, N)) +
#   geom_path(alpha = 0.5) +
#   geom_point(alpha = 0.5) +
#   theme_bw() +
#   theme(panel.border = element_blank())
#
# mcp_caro <- caro %>%
#   group_by(TierID, TierName, CollarID) %>%
#   summarise() %>%
#   st_convex_hull()
#
# plot convex hull
#
# tmap_mode("view") +
#   tm_shape(mcp_caro) +
#   tm_fill("TierName", alpha = 0.5) +
#   tm_borders(col = "red", lwd = 1) +
#   tm_layout(legend.bg.color = "white")



# mcp_caro <- caro_filter %>%
#   group_by(site_type, segment_id) %>%
#   summarise() %>%
#   st_convex_hull()
#
# tmap_mode("view") +
#   tm_shape(mcp_caro) +
#   tm_fill("segment_id", alpha = 0.5) +
#   tm_borders(col = "red", lwd = 1) +
#   tm_layout(legend.bg.color = "white")

# step 7 to 8: rasterize data ----------------------------------------------------------

# Erstellen eines raster-templates mit Aufloesung 30m (Analog zu steplenth threshold)
raster_100 <- raster(extent(wildboar_lags), resolution = 100, crs = 2056)
raster_30 <- raster(extent(wildboar_lags), resolution = 30, crs = 2056)

# resting filtern, jeweils ersten datenpunk pro segment id 

resting <- wildboar_lags %>% 
  group_by(segment_id) %>% 
  filter(row_number(segment_id) == 1)

# Wallows und Nests filtern fuer separate Layers, jeweils ersten Datenpunk pro segment id
wallows <- wildboar_lags %>%
  filter(wallow == "TRUE") %>% 
  group_by(segment_id) %>% 
  filter(row_number(segment_id) == 1)

nests <- wildboar_lags %>%
  filter(nest == "TRUE") %>% 
  group_by(segment_id) %>% 
  filter(row_number(segment_id) == 1)


# rasterize data

# Rastern der layer

resting_raster_100 <- raster::rasterize(resting, raster_100, field = 1, fun = "count")
wallows_raster_100 <- raster::rasterize(wallows, raster_100, field = 1, fun = "count")
nests_raster_100 <- raster::rasterize(nests, raster_100, field = 1, fun = "count")

resting_raster_30 <- raster::rasterize(resting, raster_30, field = 1, fun = "count")
wallows_raster_30 <- raster::rasterize(wallows, raster_30, field = 1, fun = "count")
nests_raster_30 <- raster::rasterize(nests, raster_30, field = 1, fun = "count")

# plots

greens <- tmaptools::get_brewer_pal("Greens", n = 6, contrast = c(0.3, 0.9))
oranges <- tmaptools::get_brewer_pal("Oranges", n = 5, contrast = c(0.3, 0.9))
purples <- tmaptools::get_brewer_pal("Purples", n = 5, contrast = c(0.3, 0.9))

# resolution: 100 m

tm_r_100 <- 
  tmap_mode("view") +
  tm_shape(resting_raster_100) +
  tm_raster(palette = greens, title = "Resting sites", alpha = 1)
  
tm_r_100

tm_nw_100 <-
  tmap_mode("view") +
  tm_shape(nests_raster_100) +
  tm_raster(palette = oranges, title = "Nests", alpha = 1) +
  tm_shape(wallows_raster_100) +
  tm_raster(palette = purples, title = "Wallows", alpha = 1) +
  tm_view(control.position = c("right", "top"))

tm_nw_100

# resolution: 30 m

tm_r_30 <- 
  tmap_mode("view") +
  tm_shape(resting_raster_30) +
  tm_raster(palette = greens, title = "Resting sites", alpha = 1)

tm_r_30

tm_nw_30 <-
  tmap_mode("view") +
  tm_shape(nests_raster_30) +
  tm_raster(palette = oranges, title = "Nests", alpha = 1) +
  tm_shape(wallows_raster_30) +
  tm_raster(palette = purples, title = "Wallows", alpha = 1) +
  tm_view(control.position = c("right", "top"))

tm_nw_30


# layer landnutzung: nicht sinnvoll, da unuebersichtlich

# tm +
# tm_shape(feldnutzung) +
# tm_polygon(col = "Frucht")

# leaflet plotten

# layer controls funktionieren nicht richtig

# tm %>%
#   tmap_leaflet() %>%
#   addLayersControl(
#     overlayGroups = c("Nests", "Wallows"),
#     options = layersControlOptions(collapsed = FALSE)
#   )
#
