library(tidyverse)
library(lubridate)
library(sf)
library(tigris)
library(leaflet)
pa_state <- "PA"
pa_counties <- c("Philadelphia", "Delaware", "Montgomery", "Bucks", "Chester")
tracts <- tracts(state = pa_state, county = pa_counties, year = 2023, cb = TRUE, class = "sf") |>
  st_transform(4326)
phila <- counties(state = pa_state, year = 2023, cb = TRUE, class = "sf") |>
  filter(NAME=="Philadelphia")|>
  st_transform(4326)

tracts<- tracts |>
  select(GEOID, NAME, geometry)
after_cuts_nonpeak <- read_csv("data/outputs/after_cuts_non_peak_final.csv")

after_cuts_nonpeak <- after_cuts_nonpeak |>
  select(from_id,average)%>%
  rename(after=average)

before_cuts_nonpeak<- read_csv("data/outputs/before_cuts_non_peak_final.csv")
before_cuts_nonpeak <- before_cuts_nonpeak |>
  select(from_id,average)%>%
  rename(before=average)
non_peak_comparison <- left_join(before_cuts_nonpeak, after_cuts_nonpeak, by = c("from_id" = "from_id"))

non_peak_comparison <- non_peak_comparison |>
  filter(!is.na(before) & !is.na(after)) |>
  mutate(change = before - after,
         percent_change = (change / before) * 100)

after_cuts_peak <- read_csv("data/outputs/after_cuts_peak_final.csv")
after_cuts_peak <- after_cuts_peak |>
  select(from_id,average)%>%
  rename(after=average)
before_cuts_peak<- read_csv("data/outputs/before_cuts_peak_final.csv")
before_cuts_peak <- before_cuts_peak |>
  select(from_id,average)%>%
  rename(before=average)
peak_comparison <- left_join(before_cuts_peak, after_cuts_peak, by = c("from_id" = "from_id"))
peak_comparison <- peak_comparison |>
  filter(!is.na(before) & !is.na(after)) |>
  mutate(change = before - after,
         percent_change = (change / before) * 100)

summary(peak_comparison$change > 0)
summary(non_peak_comparison$change > 0)

# joined to census tract shapefile
peak_comparison <- peak_comparison %>%
  mutate(GEOID = as.character(from_id))
peak_sf <- tracts %>%
  left_join(peak_comparison, by = "GEOID")
peak_sf <- peak_sf %>%
  filter(!is.na(change))

non_peak_comparison<- non_peak_comparison%>%
  mutate(GEOID = as.character(from_id))
non_peak_sf <- tracts %>%
  left_join(non_peak_comparison, by = "GEOID")
non_peak_sf <- non_peak_sf %>%
  filter(!is.na(change))

## plotting with leaflet
library(RColorBrewer)


make_div_pal_dark_mid <- function(x, n = 256) {
  # symmetric domain around 0
  lim <- max(abs(x), na.rm = TRUE)

  # Custom diverging colors:
  #   dark red  → light red → mid gray → light blue → dark blue
  cols <- c("#264653", "#2a9d8f", "#edf2fb", "#f3722c", "#f94144")

  colorNumeric(
    palette = colorRampPalette(cols)(n),
    domain  = c(-lim, lim)
  )
}

pal_peak <- make_div_pal_dark_mid(peak_sf$change)
pal_peak_percent <- make_div_pal_dark_mid(peak_sf$percent_change)

pal_nonpeak <- make_div_pal_dark_mid(non_peak_sf$change)
pal_nonpeak_percent <- make_div_pal_dark_mid(non_peak_sf$percent_change)


# pal_peak <- colorNumeric(palette = "RdYlGn", domain = peak_sf$change, reverse = TRUE)
# pal_peak_percent <- colorNumeric(palette = "RdYlGn", domain = peak_sf$percent_change, reverse = TRUE)
# pal_nonpeak <- colorNumeric(palette = "RdYlGn", domain = non_peak_sf$change, reverse = TRUE)
# pal_nonpeak_percent <- colorNumeric(palette = "RdYlGn", domain = non_peak_sf$percent_change, reverse = TRUE)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = peak_sf,
              fillColor = ~pal_peak(change),
              fillOpacity = 0.7,
              color = "white",
              weight = 1,
              popup = ~paste0("Tract: ", NAME, "<br>",
                              "Change in Travel Time (Peak): ", round(change,2), " minutes<br>",
                              "Percent Change (Peak): ", round(percent_change,2), "%")) %>%
  addPolygons(data = phila,
              fill = FALSE,
              color = "black",
              weight = 2) %>%
  addLegend(pal = pal_peak, values = peak_sf$change,
            title = "Change in Travel Time (Peak)",
            position = "bottomright")

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = peak_sf,
              fillColor = ~pal_peak_percent(percent_change),
              fillOpacity = 0.7,
              color = "white",
              weight = 1,
              popup = ~paste0("Tract: ", NAME, "<br>",
                              "Change in Travel Time (Peak): ", round(change,2), " minutes<br>",
                              "Percent Change (Peak): ", round(percent_change,2), "%")) %>%
  addPolygons(data = phila,
              fill = FALSE,
              color = "black",
              weight = 2) %>%
  addLegend(pal = pal_peak_percent, values = peak_sf$percent_change,
            title = "Percent Change (Peak)",
            position = "bottomright")

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = non_peak_sf,
              fillColor = ~pal_nonpeak(change),
              fillOpacity = 0.7,
              color = "white",
              weight = 1,
              popup = ~paste0("Tract: ", NAME, "<br>",
                              "Change in Travel Time (Non-Peak): ", round(change,2), " minutes<br>",
                              "Percent Change (Non-Peak): ", round(percent_change,2),"%")) %>%
  addPolygons(data = phila,
              fill = FALSE,
              color = "black",
              weight = 2) %>%
  addLegend(pal = pal_nonpeak, values = non_peak_sf$change,
            title = "Change in Travel Time (Non-Peak)",
            position = "bottomright")

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = non_peak_sf,
              fillColor = ~pal_nonpeak_percent(percent_change),
              fillOpacity = 0.7,
              color = "white",
              weight = 1,
              popup = ~paste0("Tract: ", NAME, "<br>",
                              "Change in Travel Time (Non-Peak): ", round(change,2), " minutes<br>",
                              "Percent Change (Non-Peak): ", round(percent_change,2),"%")) %>%
  addPolygons(data = phila,
              fill = FALSE,
              color = "black",
              weight = 2) %>%
  addLegend(pal = pal_nonpeak_percent, values = non_peak_sf$percent_change,
            title = "Percent Change (Non-Peak)",
            position = "bottomright")
