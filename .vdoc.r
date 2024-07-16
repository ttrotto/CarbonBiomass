#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
library(r3PG)
library(terra)
library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)
library(purrr)
library(lubridate)

# time frame
period <- '2011-2040'
from = '2024-01'
to = '2100-01'

# elevation
dem <- rast('data/DEM.tif')
names(dem) <- 'altitude'
dem <- terra::as.data.frame(dem, na.rm = F)

# latitude
lat <- rast('data/Lat.tif')
names(lat) <- 'latitude'
lat <- terra::as.data.frame(lat, na.rm = F)

# ASW
asw_max <- rast('data/ASW_Max.tif')
names(asw_max) <- 'asw_max'
asw_max <- terra::as.data.frame(asw_max, na.rm = F)
asw_min <- 0
asw_i <- asw_max
names(asw_i) <- 'asw_i'

# prepare sites
# the model wants all variable to be explicit
# if we don't have values, just report a constant
site <- data.frame(latitude = lat,
                   altitude = dem,
                   soil_class = 2,
                   asw_i = asw_i,
                   asw_min = asw_min,
                   asw_max = asw_max,
                   from = from,
                   to = to)
#
#
#
#
#
#
#
#
#
spp <- c('PSEU.MEN', 'THUJ.PLI', 'TSUG.HET')

# # date planted
# age <- rast('data/age.tif')
# names(age) <- 'age'
# age <- terra::as.data.frame(age, na.rm = F)
# # convert to yyyy-mm format
# dates <- years(as.integer(age$age) + 1)
# planted <- as.Date("2024-01-01") - dates
# planted <- format(planted, "%Y-%m")

# soil fertility
ftl <- rast('data/FTL.tif')
names(ftl) <- 'fertility'
ftl <- terra::as.data.frame(ftl, na.rm = F)

# prepare species
species <- data.frame(spp1 = spp[1],
                      spp2 = spp[2],
                      spp3 = spp[3],
                      planted = "2024-07",
                      fertility = ftl,
                      stems_n = 5000,
                      biom_stem = 6,
                      biom_root = 3,
                      biom_foliage = 1)
# reshape dataset so all data is repeated for each species
# important for downstream estimation tasks
species <- melt(species,
                id.vars=names(species)[4:ncol(species)],
                value.name='species') %>%
           select(-variable) %>%
           relocate(species)
#
#
#
#
#
# let's these steps for the other rasters
# i'll implement a function to make the code
# less lengthy, but any other approach would work
needed <- c('Tmax', 'Tmin', 'NFFD', 'PPT')
build <- function(need) {
  f <- list.files('./data/ssp2',
                pattern = paste0(need, '.*', period),
                full.names = T)
  # open each file
  dfs <- lapply(seq_along(f), function(i) {
    r <- rast(f[i])
    # name <- paste0('month', i)
    names(r) <- i
    r <- terra::as.data.frame(r, na.rm = F)
  })
  # merge into single dataframe and conver to long format
  out <- dfs %>% 
    bind_cols() %>% 
    melt() %>%
    rename(!!sym(need) := value) %>%
    select(-variable)
}
climate <- lapply(needed, function(need) {
  build(need)
})

# put them in a single dataframe that can be used later on
climate <- bind_cols(climate)

# rename to match r3PG expectations
names(climate) <- c('tmp_max', 'tmp_min', 'frost_days', 'prcp')
#
#
#
#
#
#
#
# species file
param <- read_csv('data/Final_Parameters.csv', show_col_types = F) %>%
  select(all_of(spp))  # latin names
#
#
#
#
#
#
#
#
#
site <- prepare_site(site)
species <- prepare_species(species)
climate <- prepare_climate(climate, from, to)
param <- prepare_parameters(param)
#
#
#
#
#
# basic usage for 1 single pixel
out_3pg <- run_3PG(site = site[1,],
                   species = species[1,],
                   climate = climate[1,],
                   parameters = parameters[1,],
                   check_input = T,
                   df_out = T)
#
#
#
