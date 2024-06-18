# libraries
librarian::shelf(
  here, leaflet, tbep-tech/tbeptools, tidyverse, patchwork, shiny, 
  sf, scales, htmltools, bslib
)

prj <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

# # local file path
# xlsx <- here('data', 'rawdat.xls')
# 
# # import data
# epcdata <- read_importwq(xlsx, download_latest = T)
# save(epcdata, file = here::here('data', 'epcdata.RData'))
load(here::here('data', 'epcdata.RData'))

maxyr <- 2023

# minor theme tweaks
pthm <- theme(
  axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
  legend.text = element_text(size = 12), 
  axis.title.y = element_text(size = 12)
)

# locations
locs <- epcdata %>% 
  select(epchc_station, Longitude, Latitude) %>% 
  unique