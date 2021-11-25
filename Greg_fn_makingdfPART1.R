
# Greg editing Samin's workflow ####
library(sf); library(tidyverse); library(magrittr); library(readxl); library(sp)

#load in data
#raw.breeding.data <- read.csv("Data/BREEDINGDATA.csv") # This needs to be in the repo
#nestbox.data <- read.csv("Data/Nestboxes.csv")
#wood.outline <- sf::st_read("Data/perimeter poly with clearings_region.shp")
#wood.outline <- wood.outline[1,] #keep first polygon

#temp
setwd("~/Documents/2/Familiar_neighbors/DATA")
raw.breeding.data <- read.csv("BREEDINGDATA.csv") # This needs to be in the repo
raw.breeding.data$Father <- toupper(raw.breeding.data$Father)
raw.breeding.data$Mother <- toupper(raw.breeding.data$Mother)
nestbox.data <- read.csv("Nestboxes.csv")
wood.outline <- sf::st_read("perimeter poly with clearings_region.shp")
wood.outline <- wood.outline[1,] #keep first polygon

#add box locations to breeding data
box.locations <- nestbox.data %>% dplyr::select(Box, x, y)
rm(nestbox.data)

breeding.data <-
  raw.breeding.data %>% 
  filter(year > 1964) %>% 
  mutate_at("Pnum", as.character) %>% 
  mutate(temp = str_replace_all(Pnum, "^.{0,4}", "")) %>% 
  mutate(attempt = substr(temp, 1, 1))

#only keep first attempt at breeding
breeding.data %<>% 
  filter(attempt == 1) %>% 
  mutate(Box = str_replace_all(temp, "^.{1,1}", "")) %>% 
  left_join(box.locations, by = "Box")

#get a base with great tits only to work with 
xdata <- breeding.data %>% filter(Species == "g")

#remove records where a parent is not identified 
xdata$nest.id.type <- "NA" #make new column 

xdata <-
  xdata %>% 
  mutate_at(c("Mother", "Father"), as.character) %>% 
  mutate(mID = Mother != "") %>% 
  mutate(fID = Father != "")

xdata <-
  xdata %>% 
  mutate_at(c("mID", "fID"), as.numeric) %>% 
  mutate(nest.id.type = mID + fID + 1) %>% 
  mutate(nest.id.type = c("neither", "one", "both")[nest.id.type])

#remove neithers and ones 
xdata %<>% filter(nest.id.type == "both")


#### pair familiarity #### 
pairs <- xdata[,c("Father","Mother", "year")] #get a list of all the pairs for each year
pairs <- unique(pairs) #remove duplicates (when the same pair breeds more than once in the same year)

pairs$yearplusone <- pairs$year + 1 #we want to match the previous year's information, so adding 1
pairs$ring_ring_year <-(with(pairs, paste(Father, Mother, yearplusone, sep="_"))) #making an identifier column

pairs$Pairfp <- TRUE #a column indicating that these pairs were familiar in the previous year
pairs <- pairs[,c(5,6)] #keep relevant columns

xdata$ring_ring_year <-(with(xdata, paste(Father, Mother, year, sep="_"))) #make identifier column in xdata 

xdata %<>% 
  mutate(Pairfp = ring_ring_year %in% pairs$ring_ring_year)

xdata$ring_ring_year <- NULL

##### end pair familiarity ####

#remove manipulated nests 
xdata %<>% filter(Num.eggs.manipulated == "0")

#make a row for the mom and the dad of each nest 
x <- data.table::setDT(xdata)[, .(parent = c("mother","father")),
                              .(year, Box, x, y, Section,
                                Species, April.lay.date, Laying.rate, Incubation.started,
                                April.hatch.date, Incubation.duration, Total.egg.weight, Num.eggs.weighed,
                                Clutch.size, Num.chicks, Num.dead.chicks, Num.fledglings, Mean.chick.weight,
                                Father, Mother, Pairfp)]

x <- 
  x %>% mutate(box.year.parentid = paste(Box, year, parent, sep = "_"))

xdata2 <- x 

rm(x)

#get the focal ring 
 xdata2$focal.ring <- NA
 
 xdata2$focal.ring <- with(xdata2, ifelse(parent == "mother", xdata2$Mother, 
                                        xdata2$focal.ring)) #label one ID
 
 xdata2$focal.ring <- with(xdata2, ifelse(parent == "father", xdata2$Father, 
                                        xdata2$focal.ring)) #label one ID

#add sex
xdata2$focal.sex <- NA

xdata2$focal.sex <- with(xdata2, ifelse(parent == "mother", "F", 
                                      xdata2$focal.sex)) 
xdata2$focal.sex <- with(xdata2, ifelse(parent == "father", "M", 
                                      xdata2$focal.sex)) 

#add binary success fitness variable 
xdata2 %<>% mutate(Binary.succ = as.numeric(Num.fledglings > 0))

base.fn.data <- xdata2


rm(xdata2)

###get a version of the breeding data for finding neighbors 
zdata <- breeding.data
#get a base with great tits only to work with 
zdata %<>% filter(Species == "g")

breeding.data.neighbors <- zdata

rm(zdata)

####get neighbors#### 

bbox_polygon <- function(x) {
  bb <- sf::st_bbox(x)
  
  p <- matrix(
    c(bb["xmin"], bb["ymin"], 
      bb["xmin"], bb["ymax"],
      bb["xmax"], bb["ymax"], 
      bb["xmax"], bb["ymin"], 
      bb["xmin"], bb["ymin"]),
    ncol = 2, byrow = T
  )
  
  sf::st_polygon(list(p))
}

FocalYears <- breeding.data.neighbors$year %>% unique %>% sort
FocalYears <- FocalYears[FocalYears >= 1964]
FocalYear <- FocalYears[1]

NeighbourReferenceList <- list()
TerritoriesListList <- list()

# Beginning of loop ####

for(FocalYear in FocalYears){
  
  print(FocalYear)
  
  breeding.data.1964 <- breeding.data.neighbors %>% filter(year == FocalYear)
  
  breeding.data.1964 %<>% filter(!is.na(x))
  
  #converting it into a spatial object
  breeding.data.1964 <- sf::st_as_sf(breeding.data.1964, 
                                     coords = c("x","y"), 
                                     remove = F, 
                                     crs = 27700)
  
  #calculating a bounding box for the function that calculates the territory polygons
  box <- 
    breeding.data.1964 %>% 
    bbox_polygon %>% 
    st_sfc()
  
  territories <- breeding.data.1964 %>% st_union %>% st_voronoi(box) %>% 
    st_cast# %>% st_intersection(st_union(wood.outline))
  
  #joining the territory polygons back up with the individuals that bred in them
  breeding.ids.1964 <- 
    breeding.data.1964 %>% dplyr::select(Box, Father, Mother)
  
  territories <- sf::st_sf(geom = territories)
  territories <- sf::st_join(territories, breeding.ids.1964)
  
  #now we want to figure out who was in the neighboring territories for each box
  territories.list <- st_intersection(territories, territories)
  
  #this includes the box itself when making the comparisons so we'll remove those
  territories.list <- subset(territories.list, Box.1 != Box)
  
  #removing the geometry column as we don't need that anymore
  st_geometry(territories.list) <- NULL
  
  #fill in empty spaces with NA
  territories.list %<>% 
    mutate_at(c("Father", "Mother"), 
              ~.x %>% as.character) %>% 
    mutate_at(c("Father", "Mother"), 
              ~ifelse(.x == "", NA, .x))
                                                  
  territories.list %<>% 
    mutate_at(c("Father.1", "Mother.1"), 
              ~.x %>% as.character) %>% 
    mutate_at(c("Father.1", "Mother.1"), 
              ~ifelse(.x == "", NA, .x))

  
  #now just getting the dataframe into a nice order with informative column names
  territories.list %<>% dplyr::select(Box.1, 
                                      Father.1, Mother.1, 
                                      Box, 
                                      Father, Mother)
  

  # check that these columns are the right way round?
  territories.list %<>% 
    rename(Focal.box = Box.1, 
           Focal.male = Father.1, 
           Focal.female = Mother.1,
           Box.N = Box, 
           Neighboring.male = Father, Neighboring.female = Mother
           
    )
  
  neighbors.1964 <- territories.list
  
  #change NA ids to UNKNOWN 
  neighbors.1964 %<>% mutate_at(c("Focal.male", "Focal.female", 
                                  "Neighboring.male", "Neighboring.female"),
                                ~ifelse(is.na(.x), "UNKNOWN", .x))

  
  Focals <- c(neighbors.1964$Focal.male, neighbors.1964$Focal.female) %>% unique %>% sort
  Neighbours <- c(neighbors.1964$Neighboring.male, neighbors.1964$Neighboring.female) %>% unique %>% sort
  
  N1964 <- 
    neighbors.1964 %>% mutate(N1964.b = paste(Focal.male, Neighboring.male, sep="_"),
                              N1964.c = paste(Focal.male, Neighboring.female, sep="_"),
                              N1964.d = paste(Focal.female, Neighboring.female, sep="_"),
                              N1964.e = paste(Focal.female, Neighboring.male, sep="_")) %>% 
    dplyr::select(contains("N1964"))
  
  N1964 %<>% gather(value = "ring_ring") %>% dplyr::select(ring_ring)
  
  N1964$neighbors <- TRUE
  N1964$Year.s <- FocalYear + 1
  
  N1964 %<>% filter(!str_detect(ring_ring, "UNKNOWN"))
  
  NeighbourReferenceList[[which(FocalYears == FocalYear)]] <- N1964
  TerritoriesListList[[which(FocalYears == FocalYear)]] <- territories.list
  
}

# End of loop ####

# New Loop ###

NEIOutputList <- list()

FocalYears <- breeding.data.neighbors$year %>% unique %>% sort
FocalYears <- FocalYears[FocalYears >= 1965]
FocalYear <- FocalYears[1]

for(FocalYear in FocalYears){ 
  
  print(FocalYear)
  
  ydata <- breeding.data.neighbors %>% filter(year == FocalYear)
  
  ydata %<>% 
    mutate_at(c("Father", "Mother"), 
              ~.x %>% as.character) %>% 
    mutate_at(c("Father", "Mother"), 
              ~ifelse(.x == "", NA, .x))
  
  nei1965 <- TerritoriesListList[[which(FocalYears == FocalYear)]][,c(1,4)]
  
  nei1965 %<>% 
    group_by(Focal.box) %>% 
    mutate(id = seq_along(Focal.box))
  
  nei1965 <- tidyr::pivot_wider(nei1965, 
                                names_from = "id", 
                                values_from = "Box.N")
  

  nei1965 %<>% 
    rename_all(~paste0("N.", .x)) %>% 
    rename(Focal.box = 1)

  
  par1965 <- 
    ydata %>% 
    dplyr::select(c("Box", "Mother", "Father")) %>% 
    as.data.frame()
  
  
  par1965 <- distinct(par1965, Box, .keep_all = TRUE)
  

  NeighbourNumbers <- 
    nei1965 %>% 
    ungroup %>% # Get rid of focal box
    dplyr::select(starts_with("N.")) %>% 
    names
  
  nei1965 %<>% as.data.frame()
  
  for(NName in NeighbourNumbers){
    
    nei1965$JoinName <- nei1965[,NName]
    
    nei1965 <-
      nei1965 %>% 
      left_join(par1965 %>% 
                  rename_at(c("Mother", "Father"), 
                            ~paste0(NName, ".", .x)), 
                by = c("JoinName" = "Box"))
    
  }
  
  #and identifying column 
  nei1965$year <- FocalYear

  nei1965a <- 
    nei1965 %>% mutate(box.year.parentid = paste(Focal.box, year, "mother", sep = "_"))
  
  nei1965b <- 
    nei1965 %>% mutate(box.year.parentid = paste(Focal.box, year, "father", sep = "_"))
  
  nei1965 <- rbind(nei1965a, nei1965b)
  rm(nei1965a, nei1965b)

  
  NEIOutputList[[which(FocalYears == FocalYear)]] <- nei1965
  
}

# combine all info from list ####
newdata <- NEIOutputList %>% bind_rows(.id = "Year")

###### 

## add number of neighbors 
temp <- as.data.frame((is.na(newdata[,c("N.1","N.2","N.3","N.4","N.5","N.6","N.7","N.8","N.9","N.10", "N.11", "N.12", "N.13")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num <- 13 - (temp$sumna)
newdata$N.num <- temp$N.num

#add number of identified female neighbors 
temp <- as.data.frame((!is.na(newdata[,c("N.1.Mother","N.2.Mother","N.3.Mother","N.4.Mother","N.5.Mother","N.6.Mother","N.7.Mother","N.8.Mother","N.9.Mother","N.10.Mother", "N.11.Mother", "N.12.Mother", "N.13.Mother")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumIDf <- rowSums(temp)
newdata$N.num.female.ID <- temp$sumIDf

#add number of identified male neighbors 
temp <- as.data.frame((!is.na(newdata[,c("N.1.Father","N.2.Father","N.3.Father","N.4.Father","N.5.Father","N.6.Father","N.7.Father","N.8.Father","N.9.Father","N.10.Father", "N.11.Father", "N.12.Father", "N.13.Father")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumIDm <- rowSums(temp)
newdata$N.num.male.ID <- temp$sumIDm

#add it to the base
newdata$year <- NULL #base already has the year
base.fn.data.temp <- merge(base.fn.data, newdata, by="box.year.parentid", all.x=TRUE)

#remove boxes without locations/that don't exist anymore
base.fn.data.temp <- base.fn.data.temp[which(!is.na(base.fn.data.temp$x)),]

#only keep after 1973 
#base.fn.data.temp <- base.fn.data.temp[which(base.fn.data.temp$year > "1973"),]


#### HEREE ##### 
#add familiarity based on previous years  ####

DF <- base.fn.data.temp

#okay so N_reference was the single list that had all of the neighbors in it for all of the years
#so it corresponds to the "NeighbourReferenceList" but i guess we need to pull out the list from each year?

N_reference <- (do.call(rbind, NeighbourReferenceList))
names(N_reference)[3] <- "year"

#each neighbor at a time

#N1 
DF$ring_ring <-(with(DF, paste(focal.ring, N.1.Mother, sep="_")))
names(N_reference)[2] <- "N1.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN1.mother <- DF.temp[,c(3,73)] #box.year.parentid and the column we just made (N1.MOTHERfp)
DF$ring_ring <-(with(DF, paste(focal.ring, N.1.Father, sep="_")))
names(N_reference)[2] <- "N1.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN1.father<- DF.temp[,c(3,73)] #box.year.parentid and the column we just made (N1.FATHERfp)

CN1.mother <- unique(CN1.mother)
CN1.father <- unique(CN1.father)


#N2 
DF$ring_ring <-(with(DF, paste(focal.ring, N.2.Mother, sep="_")))
names(N_reference)[2] <- "N2.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN2.mother <- DF.temp[,c(3,73)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.2.Father, sep="_")))
names(N_reference)[2] <- "N2.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN2.father<- DF.temp[,c(3,73)]

CN2.mother <- unique(CN2.mother)
CN2.father <- unique(CN2.father)

#N3
DF$ring_ring <-(with(DF, paste(focal.ring, N.3.Mother, sep="_")))
names(N_reference)[2] <- "N3.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN3.mother <- DF.temp[,c(3,73)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.3.Father, sep="_")))
names(N_reference)[2] <- "N3.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN3.father<- DF.temp[,c(3,73)]

CN3.mother <- unique(CN3.mother)
CN3.father <- unique(CN3.father)

#N4 
DF$ring_ring <-(with(DF, paste(focal.ring, N.4.Mother, sep="_")))
names(N_reference)[2] <- "N4.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN4.mother <- DF.temp[,c(3,73)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.4.Father, sep="_")))
names(N_reference)[2] <- "N4.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN4.father<- DF.temp[,c(3,73)]

CN4.mother <- unique(CN4.mother)
CN4.father <- unique(CN4.father)

#N5 
DF$ring_ring <-(with(DF, paste(focal.ring, N.5.Mother, sep="_")))
names(N_reference)[2] <- "N5.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN5.mother <- DF.temp[,c(3,73)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.5.Father, sep="_")))
names(N_reference)[2] <- "N5.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN5.father<- DF.temp[,c(3,73)]

CN5.mother <- unique(CN5.mother)
CN5.father <- unique(CN5.father)

#N6 
DF$ring_ring <-(with(DF, paste(focal.ring, N.6.Mother, sep="_")))
names(N_reference)[2] <- "N6.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN6.mother <- DF.temp[,c(3,73)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.6.Father, sep="_")))
names(N_reference)[2] <- "N6.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN6.father<- DF.temp[,c(3,73)]

CN6.mother <- unique(CN6.mother)
CN6.father <- unique(CN6.father)

#N7 
DF$ring_ring <-(with(DF, paste(focal.ring, N.7.Mother, sep="_")))
names(N_reference)[2] <- "N7.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN7.mother <- DF.temp[,c(3,73)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.7.Father, sep="_")))
names(N_reference)[2] <- "N7.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN7.father<- DF.temp[,c(3,73)]

CN7.mother <- unique(CN7.mother)
CN7.father <- unique(CN7.father)

#N8 
DF$ring_ring <-(with(DF, paste(focal.ring, N.8.Mother, sep="_")))
names(N_reference)[2] <- "N8.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN8.mother <- DF.temp[,c(3,73)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.8.Father, sep="_")))
names(N_reference)[2] <- "N8.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN8.father<- DF.temp[,c(3,73)]

CN8.mother <- unique(CN8.mother)
CN8.father <- unique(CN8.father)

#N9 
DF$ring_ring <-(with(DF, paste(focal.ring, N.9.Mother, sep="_")))
names(N_reference)[2] <- "N9.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN9.mother <- DF.temp[,c(3,73)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.9.Father, sep="_")))
names(N_reference)[2] <- "N9.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN9.father<- DF.temp[,c(3,73)]

CN9.mother <- unique(CN9.mother)
CN9.father <- unique(CN9.father)

#N10 
DF$ring_ring <-(with(DF, paste(focal.ring, N.10.Mother, sep="_")))
names(N_reference)[2] <- "N10.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN10.mother <- DF.temp[,c(3,73)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.10.Father, sep="_")))
names(N_reference)[2] <- "N10.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN10.father<- DF.temp[,c(3,73)]

CN10.mother <- unique(CN10.mother)
CN10.father <- unique(CN10.father)

#N11 
DF$ring_ring <-(with(DF, paste(focal.ring, N.11.Mother, sep="_")))
names(N_reference)[2] <- "N11.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN11.mother <- DF.temp[,c(3,73)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.11.Father, sep="_")))
names(N_reference)[2] <- "N11.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN11.father<- DF.temp[,c(3,73)]

CN11.mother <- unique(CN11.mother)
CN11.father <- unique(CN11.father)

#N12 
DF$ring_ring <-(with(DF, paste(focal.ring, N.12.Mother, sep="_")))
names(N_reference)[2] <- "N12.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN12.mother <- DF.temp[,c(3,73)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.12.Father, sep="_")))
names(N_reference)[2] <- "N12.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN12.father<- DF.temp[,c(3,73)]

CN12.mother <- unique(CN12.mother)
CN12.father <- unique(CN12.father)

#N13 
DF$ring_ring <-(with(DF, paste(focal.ring, N.13.Mother, sep="_")))
names(N_reference)[2] <- "N13.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN13.mother <- DF.temp[,c(3,73)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.13.Father, sep="_")))
names(N_reference)[2] <- "N13.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN13.father<- DF.temp[,c(3,73)]

CN13.mother <- unique(CN13.mother)
CN13.father <- unique(CN13.father)

#ok let's try to put them together?

CN1 <- merge(CN1.mother, CN1.father, by="box.year.parentid")
CN2 <- merge(CN2.mother, CN2.father, by="box.year.parentid")
CN3 <- merge(CN3.mother, CN3.father, by="box.year.parentid")
CN4 <- merge(CN4.mother, CN4.father, by="box.year.parentid")
CN5 <- merge(CN5.mother, CN5.father, by="box.year.parentid")
CN6 <- merge(CN6.mother, CN6.father, by="box.year.parentid")
CN7 <- merge(CN7.mother, CN7.father, by="box.year.parentid")
CN8 <- merge(CN8.mother, CN8.father, by="box.year.parentid")
CN9 <- merge(CN9.mother, CN9.father, by="box.year.parentid")
CN10 <- merge(CN10.mother, CN10.father, by="box.year.parentid")
CN11 <- merge(CN11.mother, CN11.father, by="box.year.parentid")
CN12 <- merge(CN12.mother, CN12.father, by="box.year.parentid")
CN13 <- merge(CN13.mother, CN13.father, by="box.year.parentid")

x <- merge(CN1, CN2, by="box.year.parentid")
x <- merge(x, CN3, by="box.year.parentid")
x <- merge(x, CN4, by="box.year.parentid")
x <- merge(x, CN5, by="box.year.parentid")
x <- merge(x, CN6, by="box.year.parentid")
x <- merge(x, CN7, by="box.year.parentid")
x <- merge(x, CN8, by="box.year.parentid")
x <- merge(x, CN9, by="box.year.parentid")
x <- merge(x, CN10, by="box.year.parentid")
x <- merge(x, CN11, by="box.year.parentid")
x <- merge(x, CN12, by="box.year.parentid")
x <- merge(x, CN13, by="box.year.parentid")

DF$ring_ring <- NULL
DF.temp <- merge(DF, x, by="box.year.parentid", all.x=TRUE)


###add familiarity info ####
#familiarity to mothers 
temp <- as.data.frame((is.na(DF.temp[,c("N1.MOTHERfp","N2.MOTHERfp", "N3.MOTHERfp","N4.MOTHERfp",  "N5.MOTHERfp",  "N6.MOTHERfp",  "N7.MOTHERfp","N8.MOTHERfp", "N9.MOTHERfp" ,"N10.MOTHERfp","N11.MOTHERfp", "N12.MOTHERfp", "N13.MOTHERfp")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num.familiar <- 13 - (temp$sumna)
hist(temp$N.num.familiar)
DF.temp$N.num.FEMALEind.familiar <- temp$N.num.familiar
DF.temp$N.prop.FEMALEind.familiar <- DF.temp$N.num.FEMALEind.familiar / DF.temp$N.num.female.ID

#familiarity to father 
temp <- as.data.frame((is.na(DF.temp[,c("N1.FATHERfp","N2.FATHERfp", "N3.FATHERfp","N4.FATHERfp",  "N5.FATHERfp",  "N6.FATHERfp",  "N7.FATHERfp","N8.FATHERfp", "N9.FATHERfp" ,"N10.FATHERfp", "N11.FATHERfp", "N12.FATHERfp", "N13.FATHERfp")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num.familiar <- 13 - (temp$sumna)
hist(temp$N.num.familiar)
DF.temp$N.num.MALEind.familiar <- temp$N.num.familiar
DF.temp$N.prop.MALEind.familiar <- DF.temp$N.num.MALEind.familiar / DF.temp$N.num.male.ID

#label number of familiar neighbors (individuals )
DF.temp$N.num.ind.familiar <- DF.temp$N.num.MALEind.familiar + DF.temp$N.num.FEMALEind.familiar
DF.temp$N.prop.ind.familiar <- DF.temp$N.num.ind.familiar / (DF.temp$N.num.female.ID + DF.temp$N.num.male.ID)

#change NA to false

DF.temp %<>% 
  mutate_at(c("N1.MOTHERfp", "N2.MOTHERfp", "N3.MOTHERfp", "N4.MOTHERfp",
              "N5.MOTHERfp", "N6.MOTHERfp", "N7.MOTHERfp", "N8.MOTHERfp",
              "N9.MOTHERfp", "N10.MOTHERfp","N11.MOTHERfp", "N12.MOTHERfp",
              "N13.MOTHERfp"), 
            ~ifelse(is.na(.x), "FALSE", .x))

DF.temp %<>% 
  mutate_at(c("N1.FATHERfp", "N2.FATHERfp", "N3.FATHERfp", "N4.FATHERfp",
              "N5.FATHERfp", "N6.FATHERfp", "N7.FATHERfp", "N8.FATHERfp",
              "N9.FATHERfp", "N10.FATHERfp","N11.FATHERfp", "N12.FATHERfp",
              "N13.FATHERfp"), 
            ~ifelse(is.na(.x), "FALSE", .x))

DF.temp %<>% 
  mutate_at(c("N1.FATHERfp", "N2.FATHERfp", "N3.FATHERfp", "N4.FATHERfp",
              "N5.FATHERfp", "N6.FATHERfp", "N7.FATHERfp", "N8.FATHERfp",
              "N9.FATHERfp", "N10.FATHERfp","N11.FATHERfp", "N12.FATHERfp",
              "N13.FATHERfp"), 
            ~ifelse(is.na(.x), "FALSE", .x))

DF.temp %<>% 
  mutate_at(c("N.prop.FEMALEind.familiar", "N.prop.MALEind.familiar", "N.prop.ind.familiar"), 
          ~ifelse(.x == "NaN", 0, .x))

fn.data <- DF.temp

####add the ages ####
agedata <- read.csv("GRETI_Age_Data.csv")
agedata <- agedata[,c(2,3,4,10)]
agedata$Season <- gsub("^.{0,5}", "", agedata$Season)  

agedata$Age <- agedata$Estimate_Age
agedata$Age <- with(agedata, ifelse(Age > 1, "adult", agedata$Age)) 
agedata$Age <- with(agedata, ifelse(Age < 2, "juvenile", agedata$Age)) 

agedata <- agedata[,c(1,2,4,5)]
names(agedata)[1] <- "focal.ring"
names(agedata)[2] <- "year"
names(agedata)[3] <- "age_num"
names(agedata)[4] <- "age_cat"
agedata <- data.frame(agedata,"ring.year"=paste(agedata$focal.ring, agedata$year,sep="_")) 
agedata <- agedata[!duplicated(agedata[,"ring.year"]),]
agedata$ring.year <- NULL

fn.data$focal.ring <- toupper(fn.data$focal.ring)

fn.data.temp <- merge(fn.data, agedata, by=c("focal.ring", "year"), all.x=TRUE)

#remove the juveniles and NA 
fn.data <- fn.data.temp[which(fn.data.temp$age_cat=="adult"),]


###adding oak health info#### 
HQ <- read_excel("Habitatqualitydata.xls")
names(HQ)[1] <- "Box"
HQ$Box <- toupper(HQ$Box)
names(HQ)[2] <- "Largeoaks"

fn.data <- merge(fn.data, HQ, by=c("Box"), all.x=TRUE)

#uppercase column names 
colnames(fn.data) <- stringr::str_to_title(colnames(fn.data))


####add distance moved from previous year for each individual ####

#make a base data frame to pull info from 
x <- breeding.data[which(breeding.data$year > 1972),] #filter year
x %<>% filter(attempt == 1) #only first attempt
x <- x %>% filter(Species == "g") #greti only

#keep only relevant columns
x <- data.table::setDT(x)[, .(parent = c("mother","father")), 
                          .(year, Box, x, y,Father, Mother)]
x <- as.data.frame(x)

parents <- x

#get the focal ring 
parents$focal.ring <- NA
parents$focal.ring <- with(x, ifelse(parent == "mother", parents$Mother, parents$focal.ring)) 
parents$focal.ring <- with(x, ifelse(parent == "father", parents$Father, parents$focal.ring)) 
x <- parents

#remove rows without a focal 
x %<>% 
  mutate_at(c("focal.ring"), 
            ~.x %>% as.character) %>% 
  mutate_at(c("focal.ring"), 
            ~ifelse(.x == "", NA, .x))

x <- as.data.frame(x)

x <- x[!is.na(x$focal.ring), ]

x <- x %>% mutate(year.focal = paste(year, focal.ring, sep = "_"))

#make another dataframe that has info about the previous year 
y <- x[,c(1,2,3,4,8)]
#rename to have informative columns 
names(y)[2] <- "box.prev"
names(y)[3] <- "x.prev"
names(y)[4] <- "y.prev"
y$year <- y$year + 1

y <- y %>% mutate(year.focal = paste(year, focal.ring, sep = "_"))

y$year <- NULL 
y$focal.ring <- NULL
z <- merge(x, y, by="year.focal", all.x=TRUE)

z <- z[!duplicated(z[ , "year.focal"]), ] 
#z <- z[which(z$year > 1973),]

z <- z[which(!is.na(z$x)),]
z <- z[which(!is.na(z$x.prev)),]

z$x <- as.numeric(z$x)
z$y <- as.numeric(z$y)
z$x.prev <- as.numeric(z$x.prev)
z$y.prev <- as.numeric(z$y.prev)

#get points of current box 
box.xy <- z[,c(4,5)]
box.points <- SpatialPoints(box.xy) #turn to point object

#and of previous box 
box.xy.prev <- z[,c(11,12)]
box.points.prev <- SpatialPoints(box.xy.prev) #turn to point object

#get distances 
library(raster)
distance <- pointDistance(box.points, box.points.prev, lonlat=FALSE)
z$distance <- distance #add it back to the df 

z <- z %>% mutate(Box.year.parentid = paste(Box, year, parent, sep = "_"))

dist.info <- z[,c(13,14)]

#remove the extra year column in fn.data 
names(fn.data)[27] <- "null"
fn.data$null<- NULL

fn.data <- merge(fn.data, dist.info, by="Box.year.parentid", all.x=TRUE) #add the info to the base 

#uppercase column names 
colnames(fn.data) <- stringr::str_to_title(colnames(fn.data))



#datachecks/cleanup
fn <- fn.data
plot(table(fn$Year)) 
table(fn$Box.year.parentid)[table(fn$Box.year.parentid)>1]
fn.dups<-fn[fn$Box.year.parentid%in%fn$Box.year.parentid[duplicated(fn$Box.year.parentid)],]
fn.dups[1:4,] 
unique(fn.dups)[,1:8]
fn2<-unique(fn)
fn2$ring.yr<-paste(fn2$Focal.ring,fn2$Year)
table(fn2$ring.yr)[table(fn2$ring.yr)>1] 
fn2.dups<-fn2[fn2$ring.yr%in%fn2$ring.yr[duplicated(fn2$ring.yr)],]
fn2.dups[1:4,]
min.dates<-tapply(fn2$April.lay.date,fn2$ring.yr,function(a)min(a))
fn3<-fn2[paste(fn2$ring.yr,fn2$April.lay.date) %in% paste(names(min.dates),min.dates),]
fn3$box.yr<-paste(fn3$Box,fn3$Year)
table(fn3$box.yr)[table(fn3$box.yr)>2] #after removing the above individual-within-year duplicates, we don't appear to have any issues with box-within-year  duplicates either
table(fn3$box.yr[fn3$Parent%in%"mother"])[table(fn3$box.yr[fn3$Parent%in%"mother"])>1] #true for just mothers
table(fn3$box.yr[fn3$Parent%in%"father"])[table(fn3$box.yr[fn3$Parent%in%"father"])>1] #and true for just fathers
fath.t<-fn3[fn3$Parent=="father",c("box.yr","Father","Mother")]
moth.t<-fn3[fn3$Parent=="mother",c("box.yr","Father","Mother")]
mean(unique(fn3$box.yr) %in% fath.t$box.yr  & unique(fn3$box.yr) %in% moth.t$box.yr) #only minority of the box-year have both the mother and father known?
both.parents<-unique(fn3$box.yr[fn3$box.yr %in% fath.t$box.yr  & fn3$box.yr %in% moth.t$box.yr])
fath.b<-fath.t[match(both.parents,fath.t$box.yr),]
moth.b<-moth.t[match(both.parents,moth.t$box.yr),]
mean(fath.b$Father ==moth.b$Father)
mean(fath.b$Mother ==moth.b$Mother)

#DONE

#only keep after 1973 
fn4 <- fn3[which(fn3$year > "1973"),]

saveRDS(fn3, "fn.data.full.ALLYEARS.Rds")
saveRDS(fn4, "fn.data.full.Rds")

fn.data.clean <- fn3[,c("Box","Focal.ring","Year", "Box.year.parentid", "X", "Y", "April.lay.date",
                       "Clutch.size", "Num.fledglings", "Mean.chick.weight", "Pairfp", "Focal.sex", "Binary.succ", 
                       "N.num", "N.num.female.id", "N.num.male.id", "N.num.ind.familiar", "N.num.femaleind.familiar", 
                       "N.prop.femaleind.familiar", "N.num.maleind.familiar", "N.prop.maleind.familiar", "N.prop.ind.familiar", 
                       "Age_num", "Age_cat", "Largeoaks", "Distance")]

saveRDS(fn.data.clean, "fn.data.clean.Rds")

table(fn3$Parent, fn3$Focal.sex)
