library(dplyr)
library(sf)
library(stringr)

# set the working directory
setwd("C:/Users/...") 

# load the necessary files
# california tracts
cal_tracts <- st_read("C:/Users/../tl_2025_06_tract.shp")
View(cal_tracts)

# add rent table making sure GEOIDs have leading '0'
rent <- read.csv(
  "C:/Users//rent_data_2010_2023.csv",
  colClasses = c(GEOID = "character")
)

# make sure there is the leading 0 and 11 characters
rent$GEOID <- str_pad(rent$GEOID, width = 11, side = "left", pad = "0") 
View(rent)

# see that the rents are numeric
rent$rent_2010 <- as.numeric(rent$rent_2010)
rent$rent_2023 <- as.numeric(rent$rent_2023)

# checking rent data
head(rent[, c("GEOID", "rent_2010", "rent_2023")])
summary(rent$rent_2010)
summary(rent$rent_2023)

############################################################################
# cross walking - here the 2010 census tracts will be projected to the 2020 census tracts 
# this will allow for continuity between rent values

# crosswalk dataset
crosswalk <- read.csv("C:/Users/../2010_2020_crosswalk.csv")
View(crosswalk)

names(crosswalk)

# format to remove leading G
crosswalk$GEOID10_clean <- str_pad(crosswalk$tr2010ge, width = 11, side = "left", pad = "0") 
crosswalk$GEOID20_clean <- str_pad(crosswalk$tr2020ge, width = 11, side = "left", pad = "0")
crosswalk$housing_weight <- crosswalk$wt_hu
View(crosswalk)

# create subset if crosswalk data with only what is needed
# the weight being used is for housing units ("wt_hu")
cross_sub <- crosswalk %>%
  select(GEOID10_clean, GEOID20_clean, housing_weight)

# simplify names
names(cross_sub) <- c('geoid10', 'geoid20', 'weight')
View(cross_sub)

# 2010 rent table is represented by 2010 GEOI2D
rent_2010 <- rent %>%
  select(GEOID, rent_2010) %>%
  rename(geoid10 = GEOID)

# join rent_2010 onto crosswalk
rent_long <- cross_sub %>%
  left_join(rent_2010, by = 'geoid10')

# weighted 2010 rent
rent_long$rent_2010_weighted <- rent_long$rent_2010 * rent_long$weight

# sum up to 2020 tracts
rent_2010_2020geo <- rent_long %>%
  group_by(geoid20) %>%
  summarise(
    rent_2010_2010d_2020geo = sum(rent_2010_weighted, na.rm = TRUE),
    .groups = "drop"
  )

############################################################################
# now we'll adjust 2010 rent for inflation using the Consumer Price Index

# 2010 cpi: 218.056
# 2020 cpi: 305.628

CPI_multiplier <- (305.628 / 218.056)

rent_2010_2020geo$rent_2010_2023d_2020geo <- 
  (rent_2010_2020geo$rent_2010_2010d_2020geo * CPI_multiplier)

############################################################################
# attach 2023 rent 

rent_2023_table <- rent %>%
  select(GEOID, rent_2023) %>%
  rename(geoid20 = GEOID)

# combine crosswalked 2010 + 2023
rent_compare <- rent_2010_2020geo %>%
  left_join(rent_2023_table, by = "geoid20")

# calculate absolute and percent change
rent_compare$rent_change_abs <- 
  (rent_compare$rent_2023 - rent_compare$rent_2010_2023d_2020geo)

rent_compare$rent_change_pct <- 
  (100 * rent_compare$rent_change_abs / rent_compare$rent_2010_2023d_2020geo)

# make sure the geoid columns line up and view 
rent_compare$geoid20 <- as.character(rent_compare$geoid20)
View(rent_compare)


############################################################################
# joining and exporting to geopackage and shapefile
joined <- cal_tracts %>%
  left_join(rent_compare, by = c("GEOID" = "geoid20"))
 
View(joined)

# save as geopackage (or shapefile; this is what the internet recommended)
st_write(joined, "C:/Users/../CA_Rent_Xwalk_Joined_Adjusted.gpkg")
st_write(joined, "C:/Users/../CA_Rent_Xwalk_Adjusted.shp") # might as well do shapefile

