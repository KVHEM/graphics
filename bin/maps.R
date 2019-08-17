#Not finished 

source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

#library(rgdal)
library(tmap)
library(maps)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
EU_LON <- c(-25, 40)
EU_LAT <- c(33, 71)
my_proj <- "+proj=robin"

#see more about projections https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf
#my_proj <- "+proj=longlat"
#my_proj <- "+proj=laea"

#https://learn.r-journalism.com/en/mapping/static_maps/static-maps/
#https://www.computerworld.com/article/3038270/create-maps-in-r-in-10-fairly-easy-steps.html
#https://www.earthdatascience.org/courses/earth-analytics/spatial-data-r/make-maps-with-ggplot-in-R/
#http://zevross.com/blog/2018/10/02/creating-beautiful-demographic-maps-in-r-with-the-tidycensus-and-tmap-packages/#initial-visualization-of-the-health-insurance-data-using-ggplot
#https://geocompr.robinlovelace.net/spatial-operations.html
#https://geocompr.robinlovelace.net/spatial-class.html#intro-sf
#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
#https://bookdown.org/rdpeng/RProgDA/mapping.html

# Blank ggplot themes for maps
theme_map <- list(theme(panel.background = element_blank(),
                               plot.background = element_rect(fill = "white"),
                               panel.border = element_blank(),
                               plot.title = element_text(size = 22),
                               legend.key = element_rect(fill = "white")))


theme_map_minimal <- list(theme(panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_blank(),
                        plot.background = element_rect(fill = "white"),
                        panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size = 22),
                        legend.key = element_rect(fill = "white")))

theme_map_pop <- theme(
  text = element_text(color = '#444444'),
  panel.background = element_rect(fill = '#CCCCCC'), 
  plot.background = element_rect(fill = '#CCCCCC'), 
  legend.background = element_rect(fill = '#CCCCCC'), 
  panel.grid = element_blank(),
  plot.title = element_text(size = 18, 
                            face = 'bold'),
  plot.subtitle = element_text(size = 12),
  legend.key = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank())

##################################
# data.table map
##################################

# Create map data
map_dt <- data.table(map_data('world')) #turns data from the maps package in to a data table suitable for plotting with ggplot2

# Inspect map data
glimpse(map_dt)
names(map_dt)

# Rename 'region' to 'country'
map_dt <- rename(map_dt, country = region) 
europe_dt <- map_dt[long > EU_LON[1] & 
                      long < EU_LON[2] & 
                      lat > EU_LAT[1] &
                      long < EU_LAT[2]]
no_eu_countries <- length(unique(europe_dt$country))

ggplot(europe_dt, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = 'grey60') +
  theme_map_minimal 

# See https://www.r-bloggers.com/how-to-map-public-debt-data-with-ggplot2/

###############################################
#Simple feature maps 
###############################################

# See https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

theme_set(theme_bw())

world <- ne_countries(scale = "medium", 
                      returnclass = "sf")

europe <- ne_countries(scale = "medium", 
                      continent = "Europe",
                      returnclass = "sf")

europe <- europe %>% filter(name != "Russia")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = my_proj) +
  theme_map +
  geom_path(data = floyd_track, aes(x = -longitude, 
                                    y = latitude,
                                    group = NA),
            color = "red")
  
ggplot(data = europe) +
  geom_sf(aes(fill = name)) +
  scale_fill_manual(values = colorRampPalette(palettes_bright$colset_sun_sky)(no_eu_countries)) +
  guides(fill = FALSE) + 
  coord_sf(xlim = EU_LON, ylim = EU_LAT, expand = FALSE) 

# Other aesthecic approach
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot(data = world) + 
  geom_sf(fill= 'antiquewhite') + 
  geom_text(data = world_points,
            aes(x=X, y=Y, label = name), 
            color = 'darkblue', 
            fontface = 'bold', 
            check_overlap = FALSE) + 
  annotate(geom = 'text', x = -90, y = 26, 
           label = 'Gulf of Mexico', 
           fontface = 'italic', 
           color = 'grey22', 
           size = 6) + 
  coord_sf(xlim = c(-102.15, -74.12), 
           ylim = c(7.65, 33.97), 
           expand = FALSE) + 
  xlab('') + 
  ylab('') + 
  ggtitle('Map of the Gulf of Mexico and the Caribbean Sea') + 
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = 'dashed', 
                                        size = 0.5), 
        panel.background = element_rect(fill = 'aliceblue'))

#package tmap
tm_shape(world) +
  tm_borders() 

tm_shape(world) +
  tm_fill() +
  tm_borders() 

###############################################
#Using shapefiles
###############################################
map_path = "../geodata/"

# Read shapefiles
wmap <- readOGR(dsn = paste0(map_path, "ne_110m_land"))
wmap_df <- fortify(wmap)
bbox <- readOGR(paste0(map_path,"ne_110m_graticules_all"), layer = "ne_110m_wgs84_bounding_box") 
countries <- readOGR(paste0(map_path,"ne_110m_admin_0_countries"), layer = "ne_110m_admin_0_countries") 
grat <- readOGR(paste0(map_path,"ne_110m_graticules_all"), layer = "ne_110m_graticules_15") 
grat <- readOGR(paste0(map_path,"ne_110m_graticules_all"), layer = "ne_110m_graticules_30") 

# Add projection
wmap_proj <- spTransform(wmap, CRS(my_proj))
countries_proj <- spTransform(countries, CRS(my_proj)) 
bbox_proj <- spTransform(bbox, CRS(my_proj))
grat_proj <- spTransform(grat, CRS(my_proj))

# Make plots
ggplot(bbox_proj, aes(long, lat, group = group)) + 
  geom_polygon(fill = "grey70") +
  geom_polygon(data = countries_proj, aes(long,lat, group = group, fill = hole)) + 
  geom_path(data = grat_proj, aes(long, lat, group = group, fill = NULL), 
            linetype = "dashed", color="grey50") +
  scale_size_continuous(range = c(1, 10)) + 
  geom_polygon(fill = "transparent", col = 'black') +
  coord_equal(ratio = 1) + 
  theme_map +
  scale_fill_manual(values=c("black", "black"), guide = "none")

ggplot(bbox_proj, aes(long, lat, group = group)) + 
  geom_polygon(data = wmap_proj, aes(long,lat, group = group, fill = hole), col = 'grey10') + 
  geom_path(data = grat_proj, aes(long, lat, group = group, fill = NULL), color="grey80") +
  geom_polygon(fill = "transparent", col = 'black') +
  scale_size_continuous(range = c(1, 10)) + 
  coord_equal(ratio = 1) + 
  theme_map +
  scale_fill_manual(values=c("white", "white"), guide = "none") 


