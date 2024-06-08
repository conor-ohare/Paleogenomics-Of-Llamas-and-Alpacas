library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggforce)

# ----------------------
## PREPROCESSING OF DATA
# ----------------------

# Longitude and latitude coordinates of modern samples
data <- c(
  "SRR11097120", NA, NA,
  "SRR11905246", -38.83459227046051, -72.44352278856611, # del sur
  "SRR11905247", -18.195946351034323, -69.55926631159195, # putre
  "SRR11905248", -18.195946351034323, -69.55926631159195, # putre
  "SRR11905249", -53.29474038325634, -70.37951231137336, # porvenir
  "SRR11905250", -47.117394788346616, -72.48646531285894, # valle chacabuco
  "SRR11905251", -36.399758974055004, -69.3826444887965, # la payunia
  "SRR11905252", -32.3956474169312, -70.4168149664248, # rio rocin
  "SRR11905253", -38.83459227046051, -72.44352278856611, # del sur
  "SRR11905254", -38.83459227046051, -72.44352278856611, # del sur
  "SRR11905255", -38.83459227046051, -72.44352278856611, # del sur
  "SRR11905256", -38.83459227046051, -72.44352278856611, # del sur
  "SRR11905257", -38.83459227046051, -72.44352278856611, # del sur
  "SRR11905258", -38.83459227046051, -72.44352278856611, # del sur
  "SRR11905259", -38.83459227046051, -72.44352278856611, # del sur
  "SRR11905260", -30.34735688409382, -61.90893521037703, # san guillermo
  "SRR11905261", -30.60411730472761, -71.19624428536378, # ovalle
  "SRR11905262", -27.027614502920734, -69.05201015636545, # nevado tres cruces
  "SRR11905263", -22.10078780718874, -65.86748123474422, # cienaguillas
  "SRR11905264", -24.913797038859332, -68.82043033163896, # llullaillaca
  "SRR11905265", -18.91554430027371, -69.1047476062493, # surire
  "SRR11905266", -18.231335419225324, -69.31406683370442, # lauca
  "SRR11905267", -13.604071730560111, -75.35277925942599, # huaytara
  "SRR11905268", -38.83459227046051, -72.44352278856611, # del sur
  "SRR11905269", -38.83459227046051, -72.44352278856611, # del sur
  "SRR11905270", -38.83459227046051, -72.44352278856611, # del sur
  "SRR11905271", -38.83459227046051, -72.44352278856611, # del sur
  "SRR11905272", -25.00145252395033, -70.46239977399807, # paposo
  "SRR11905273", -18.195946351034323, -69.55926631159195, # putre
  "SRR13340600", -10.677106907334101, -76.26287367875705,
  "SRR13340601", -10.677106907334101, -76.26287367875705,
  "SRR13340602", -10.677106907334101, -76.26287367875705,
  "SRR13340603", -15.839751573646295, -70.02205453493842,
  "SRR13340604", -15.839751573646295, -70.02205453493842,
  "SRR13340605", -15.839751573646295, -70.02205453493842,
  "SRR15927273", -13.5316747892664, -71.97015313151456,
  "SRR15966550", -13.5316747892664, -71.97015313151456,
  "SRR16008793", -13.5316747892664, -71.97015313151456,
  "SRR16060509", -13.5316747892664, -71.97015313151456,
  "SRR16077698", -14.475879409768956, -70.63651417612596, # nunoa
  "SRR16097856", -14.475879409768956, -70.63651417612596 # nunoa
)


long <- as.numeric(data[seq(2, length(data), 3)])
lati <- as.numeric(data[seq(3, length(data), 3)])

modern <- read_csv("modern.csv") %>% mutate(lat = long, lon = lati)


# Add jitter to the latitude and longitude coordinates
jitter_amount <- 0.4  # Adjust the amount of jittering
jittered_data <- modern %>%
  mutate(
    lon_jittered = lon + runif(n(), -jitter_amount, jitter_amount),
    lat_jittered = lat + runif(n(), -jitter_amount, jitter_amount)
  )

# Get South America map data from Natural Earth dataset
world_map <- ne_countries(scale = "medium", returnclass = "sf")
south_america <- world_map[world_map$subregion == "South America", ]


# Create the map plot with jittered points
map_plot <- ggplot() +
  geom_sf(data = south_america, fill = "lightgray", color = "white") +
  geom_point(data = jittered_data, aes(x = lon_jittered, y = lat_jittered, color = Species), size = 2, alpha = 0.6) +
  xlab("Longitude") + 
  ylab("Latitude") + 
  coord_sf(xlim = c(-85, -35))  + # Set the desired latitude and longitude limits
  theme_minimal() + 
  theme(legend.text = element_text(size = 15),  # Adjust the legend text size
        legend.title = element_text(size = 20),
        legend.spacing = unit(1, "cm"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))


# Display the combined plot
plot(map_plot)

ggsave("map-plot.png", plot=map_plot)












