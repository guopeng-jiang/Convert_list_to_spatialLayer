########### Query parcels based on name search ############

name_match = function(keyword, target_field){
  
  count_matches = numeric(length(strsplit(target_field, ", ")[[1]]))
  
  for (i in seq(strsplit(target_field, ", ")[[1]])) {
    count_matches[i] = sum(strsplit(keyword, " ")[[1]] %in% strsplit(strsplit(target_field, ", ")[[1]][i], " ")[[1]])
  }
  
  max(count_matches) >= 2
  
}

# Initial testing (success)
# keyword = c("a c", "b c") # one problem with the sequence between first, middle and surname
# target_field = c("a b c, b g h, b a", "a b", "a c")
# random = lapply(keyword, function(keyword){which(mapply(name_match, keyword, strsplit(target_field, ", ") ) == TRUE)} )

# testing using real-world data 
library(sp)
library(rgdal)
library(tidyverse)
library(tm)
AllProperty = readOGR(dsn = "N:/Gorden_Jiang/Tukituki_LUC", layer = "ALLproperties")

keyword = readxl::read_excel("N:/FEMP - 2021 - M+G/Correspondence/June 2021 letter - 275 addresses/Follow up letter addresses - 30.6.21.xlsx", 
                             sheet = "All properties") %>% select(`Owners Name`) %>% pull()
target_field = removeWords(AllProperty$owners, c(LETTERS, " & ", " + ", "Limited", "Station", "Trust", "Trustee", "Family", "Farm"))
# problem with company names, initials, symbols

random = lapply(keyword, function(keyword){which(mapply(name_match, keyword, strsplit(target_field, ", ") ) == TRUE)} )
random1 = unlist(random)

target_parcels = AllProperty[random1,]
target_parcels = target_parcels[target_parcels$Hectares >= 10, ]

# target_parcels_dislv = raster::aggregate(target_parcels, by = "owners", fun = sum, dissolve = TRUE)
# target_parcels_dislv$extent_area = raster::area(target_parcels_dislv)/10000
