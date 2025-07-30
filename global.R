shinyOptions(cache = cachem::cache_disk("./app_cache/cache/"))

# libraries
librarian::shelf(
  here, leaflet, tbep-tech/tbeptools, tidyverse, patchwork, shiny, 
  sf, scales, htmltools, bslib, RColorBrewer
)

source(here('R/funcs.R'))

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

# add otb subsegs to targets
trgotb <- targets %>% 
  filter(bay_segment == 'OTB') %>% 
  select(-bay_segment, -name)
trgadd <- tibble(
  bay_segment = c('NW', 'NE', 'CW', 'CE', 'SW', 'SE') ,
  name = c('Old Tampa Bay Northwest', 'Old Tampa Bay Northeast', 'Old Tampa Bay Central West', 
           'Old Tampa Bay Central East', 'Old Tampa Bay Southwest', 'Old Tampa Bay Southeast')
) %>% 
  bind_cols(trgotb)
targets <- targets %>% 
  bind_rows(trgadd)
