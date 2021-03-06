
# 00_Greg Setup ###

library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
library(GGally)

theme_set(theme_cowplot())

# DF <- readRDS("fnbasedata_full.Rda")

zz2012 <- readRDS("Data/fnbasedata_2012.Rda")
zz2013 <- readRDS("Data/fnbasedata_2013.Rda")
zz2014 <- readRDS("Data/fnbasedata_2014.Rda")

zz2012 %>% bind_rows(zz2013, zz2014) -> Tits

Tits %<>% rename_all(CamelConvert)

# Neighbour stuff ####

Tits %>% Grelect(matches("^N[1-9]")) %>% 
  Grelect(contains("bs")) %>% 
  rowSums(na.rm = T) ->
  # rowMeans(na.rm = T) -> 
  Tits$AllNeighbours

Tits %>% Grelect(matches("^N[1-9]")) %>% 
  Grelect(contains("fbs")) %>% 
  rowSums(na.rm = T) ->
  # rowMeans(na.rm = T) -> 
  Tits$FemaleNeighbours

Tits %>% Grelect(matches("^N[1-9]")) %>% 
  Grelect(contains("mbs")) %>% 
  rowSums(na.rm = T) ->
  # rowMeans(na.rm = T) -> 
  Tits$MaleNeighbours

Tits %>% Grelect(matches("^N[1-9]")) %>% 
  Grelect(contains("bs")) %>% 
  # rowSums(na.rm = T) -> 
  rowMeans(na.rm = T) ->
  Tits$AllNeighbours.Mean

Tits %>% Grelect(matches("^N[1-9]")) %>% 
  Grelect(contains("fbs")) %>% 
  # rowSums(na.rm = T) -> 
  rowMeans(na.rm = T) ->
  Tits$FemaleNeighbours.Mean

Tits %>% Grelect(matches("^N[1-9]")) %>% 
  Grelect(contains("mbs")) %>% 
  # rowSums(na.rm = T) -> 
  rowMeans(na.rm = T) ->
  Tits$MaleNeighbours.Mean

Tits %>% Grelect(matches("^N[1-9]")) %>% 
  Grelect(contains("bs")) %>% mutate_all(AsBinary) %>% 
  rowSums(na.rm = T) ->
  # rowMeans(na.rm = T) -> 
  Tits$NAllNeighbours

Tits %>% Grelect(matches("^N[1-9]")) %>% 
  Grelect(contains("fbs")) %>% mutate_all(AsBinary) %>% 
  rowSums(na.rm = T) ->
  # rowMeans(na.rm = T) -> 
  Tits$NFemaleNeighbours

Tits %>% Grelect(matches("^N[1-9]")) %>% 
  Grelect(contains("mbs")) %>% mutate_all(AsBinary) %>% 
  rowSums(na.rm = T) ->
  # rowMeans(na.rm = T) -> 
  Tits$NMaleNeighbours


# Density ####

library(adehabitatHR)

Tits %>% 
  group_by(Focal.ring) %>% 
  summarise_at(c("X", "Y"), ~mean(.x, na.rm = T)) ->
  LifetimeCentroids

LifetimeCentroids %<>% na.omit

SPDF <- SpatialPointsDataFrame(data = LifetimeCentroids[,c("X", "Y")], 
                               coords = LifetimeCentroids[,c("X", "Y")])

LifetimeKUDL <- kernelUD(SPDF, same4all = TRUE, grid = 500)

LifetimeKUDL %>% raster::raster() %>% raster::extract(Tits[,c("X", "Y")]) ->
  
  Tits$LifetimeDensity

Tits %>% arrange(Year.w) %>% pull(Year.w) %>% unique %>% as.character -> FocalYear.ws

FocalYear.ws %<>% c(min(as.numeric(FocalYear.ws))-1, .)

Tits %>% 
  filter(Year.w %in% FocalYear.ws) %>% 
  group_by(Focal.ring, Year.w) %>% 
  summarise_at(c("X", "Y"), 
               ~mean(.x, na.rm = T)) %>% 
  rename(XCentroidAnnual = X, YCentroidAnnual = Y) -> 
  
  AnnualCentroids

AnnualCentroids %<>% filter(Year.w %in% FocalYear.ws) %>% na.omit

SPDF <- SpatialPointsDataFrame(data = AnnualCentroids[,c("XCentroidAnnual", "YCentroidAnnual", "Year.w")], 
                               coords = AnnualCentroids[,c("XCentroidAnnual", "YCentroidAnnual")])

SPDF <- SPDF[,"Year.w"]

KUDL <- kernelUD(SPDF, same4all=TRUE, grid=500)

2:length(FocalYear.ws) %>% lapply(function(a){
  
  print(FocalYear.ws[a])
  
  DF <- Tits %>% filter(Year.w == FocalYear.ws[a])
  
  KUDL2 <- KUDL[[FocalYear.ws[a]]]
  
  KUDL2 %>% raster::raster() %>% raster::extract(DF[,c("X", "Y")]) ->
    
    DF$AnnualDensity
  
  return(DF)
  
}) -> DensityList

DensityList %>% bind_rows -> Tits

#adding binary success column 
Tits$Binary.succ <- Tits$Num.fledglings
Tits$Binary.succ <- with(Tits, ifelse(Binary.succ == 0, "0", 
                                       Tits$Binary.succ)) 
Tits$Binary.succ <- with(Tits, ifelse(Binary.succ > 0, "1", 
                                      Tits$Binary.succ)) 

Tits$Binary.succ <- as.numeric(Tits$Binary.succ)

Tits %>% saveRDS("Data/CleanData.rds")
