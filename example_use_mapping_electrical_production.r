#This R script demonstrates creating and then mapping raster bricks based on geo-encoded data.
library(magick)
library(readxl)
library(dplyr)
library(cartography)
library(sf)
library(sp)
library(ggplot2)
library(igraph)
#Get data to be mapped by state. Save to your working directory.
#Data source for this example: https://www.eia.gov/electricity/data/state/generation_monthly.xlsx
df<-readxl::read_xlsx("generation_monthly.xlsx", skip=4, sheet = "2020_Preliminary")
names(df) <-c("year", "month","state", "producer", "source_type","megawatts")

#Find top source by state
df<-df[df$producer=="Total Electric Power Industry",] 
df<-df[!df$source_type=="Total",]
top_source_by_state<-df %>% 
  select(state,source_type,megawatts) %>%
  group_by(state,source_type) %>% 
  summarize(annual_megawatts =sum(megawatts)) %>%
  arrange(state, desc(annual_megawatts)) %>%
  slice_max(order_by = annual_megawatts)

#associate image filename with factor 
vals<-as.numeric(as.factor(top_source_by_state$source_type))
top_source_by_state$img_file<-as.character(factor(vals, labels = c("coal.png", "hydro.png", "ng.png" ,"nuclear.png","petroleum.png","wind.png")))

#create two tempfiles. (Used to store and then extract the zipped SHP file.)
temp <- tempfile()
temp2 <- tempfile()
#Download the open data ArcGIS zip folder containing the parcel data as a .shp file.  
download.file("https://opendata.arcgis.com/datasets/1b02c87f62d24508970dc1a6df80c98e_0.zip",temp)
#Unzip the contents of 'temp' object and save unzipped content in 'temp2'
unzip(zipfile = temp, exdir = temp2)
#Finds the filepath of the shapefile (.shp) file in the temp2 unzip folder.
#the $ at the end of ".shp$" ensures you are not also finding files such as .shp.xml 
your_SHP_file<-list.files(temp2, pattern = ".shp$",full.names=TRUE)
#Read the shapefile.
states_map2<- sf::st_read(your_SHP_file)
# tells object new f that it is in EPSG 2285, without changing any value
states_map2 <- st_set_crs(states_map2, 4326) 
# projects new f from existing CRS (e.g. 4326) to EPSG:2285
# coordinate values are changed
states_map2  <- st_transform(states_map2, 4326)
states_map2<-states_map2[!states_map2$State_Code %in% c("AK", "HI", "DC"),]

#Merge stae map with state data
states_map2<-inner_join(states_map2, top_source_by_state,by= c("State_Code" = "state"))

#Find which states border & also have same factor value (e.g. top electrical source) in order to merge those shapes
#First create a T/F matrix of which states share a border (e.g. interect)
which_join <-sf::st_intersects(states_map2, states_map2, sparse = F) 
#Then check which of these combinations have the same factor value (e.g. top electrical source)
for (i in 1:nrow(states_map2)){
  which_join[i,]<- which_join[i,] + (states_map2$source_type[i]==states_map2$source_type)
}
#The result is a matrix with values 0,1 or 2. 2 = both true.
#Create a datafame of all bordering states with the same category
all_state_groups<- data.frame(which(which_join == 2, arr.ind = TRUE)) %>% rename("id"=row,"group"=col)

#Group combinations of bordering states with th same factor value using a network graph (e.g. igraph)
states_grouped<-data.frame(components(igraph::graph_from_data_frame(all_state_groups))$membership)
library(tibble)
states_grouped<-tibble::rownames_to_column(states_grouped, "state") 
names(states_grouped)[2]<-"groups"
states_grouped$state<-as.numeric(states_grouped$state)
states_grouped<-states_grouped %>% arrange(state)
states_map2$state_groups <- states_grouped$groups

#Group shapefile geographies (eg. boardering states with same factor value)
states_map<- states_map2%>% 
  group_by(state_groups, .drop=FALSE) %>% 
  summarize()

#Since file labels are lost in the summary, they need to be rejoined to the summarized geometries
img_to_join<- states_map2 %>% 
  select(img_file, state_groups) %>% 
  sf::st_drop_geometry(.) %>% 
  unique(.)
#join image filenames to summarized geometries.
states_map<- merge(states_map,img_to_join)

states_map <- st_set_crs(states_map, 4326) 
states_map <- st_transform(states_map, 4326)

########################


#Map it. Create a cartography map as a magick object
fig <- magick::image_graph(width = 2560, height = 1280, res = 200)
cartography::ghostLayer(states_map, bg="lightblue")
plot(states_map, col="grey90", add=TRUE)
#Create raster bricks that are the same shape as the joined geographies. Add to map.
for (i in 1:nrow(states_map)) {
  a = getPngLayer(states_map[i,],states_map$img_file[i],mask=TRUE, margin = 1)
  pngLayer(a, add = TRUE)
}
dev.off()

#Annotate the map with a key and text using magick.
c<-image_scale(image_read("coal.png"),  "150")
h<-image_scale(image_read("hydro.png"),  "150")
ng<-image_scale(image_read("ng.png"),  "150")
n<-image_scale(image_read("nuclear.png"),  "150")
w<-image_scale(image_read("wind.png"),  "150")
out <- image_composite(fig, c, offset = "+2050+600") %>%
  image_composite(., h, offset = "+2050+750") %>%
  image_composite(., ng, offset = "+2050+900") %>%
  image_composite(., n, offset = "+200+750")%>%
  image_composite(., w, offset = "+200+900") %>%
  image_annotate(., "Coal",location = "+2200+650" , size = 17,color = "#000000", weight = 500, kerning = -1) %>%
  image_annotate(., "Hydroelectric",location = "+2200+790" , size = 17,color = "#000000", weight = 500, kerning = -1) %>%
  image_annotate(., "Natural Gas",location = "+2200+950" , size = 17,color = "#000000", weight = 500, kerning = -1) %>%
  image_annotate(., "Nuclear",location = "+350+790" , size = 17,color = "#000000", weight = 500, kerning = -1) %>%
  image_annotate(., "Wind",location = "+350+950" , size = 17,color = "#000000", weight = 500, kerning = -1) %>%
  image_annotate(., "Top electrical sources in 2020 by state.(Neighboring states with same top source are grouped.)",location = "+150+50" , size = 18,color = "#000000", weight = 700, kerning = -1) %>%
  image_annotate(., "Source: https://www.eia.gov/electricity/data/state/generation_monthly.xlsx ",location = "+150+110" , size = 12,color = "#000000", weight = 500, kerning = -1)
print(out)
#Save your result. This script could be reused/reworked to map other geographies such as zipcodes, counties, etc.