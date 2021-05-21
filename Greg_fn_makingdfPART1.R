
# Greg editing Samin's workflow ####
setwd("~/Documents/2/Familiar_neighbors/DATA") # Don't use this, use a project

library(sf); library(tidyverse); library(magrittr)

#load in data
raw.breeding.data <- read.csv("Data/BREEDINGDATA.csv") # This needs to be in the repo
nestbox.data <- read.csv("Data/Nestboxes.csv")
wood.outline <- sf::st_read("Data/perimeter poly with clearings_region.shp")
wood.outline <- wood.outline[1,] #keep first polygon


#add box locations to breeding data
# box.locations <- nestbox.data[,c(2,3,4)]

box.locations <- nestbox.data %>% dplyr::select(Box, x, y)

# breeding.data <- raw.breeding.data[which(raw.breeding.data$year > 1964),]
# breeding.data$Pnum <- as.character(breeding.data$Pnum)
# breeding.data$temp <- gsub("^.{0,4}", "", breeding.data$Pnum)
# breeding.data$attempt <- substr(breeding.data$"temp",1,1) #label attempt number

breeding.data <-
  raw.breeding.data %>% 
  filter(year > 1964) %>% 
  mutate_at("Pnum", as.character) %>% 
  mutate(temp = str_replace_all(Pnum, "^.{0,4}", "")) %>% 
  mutate(attempt = substr(temp, 1, 1))

# breeding.data <- breeding.data[which(breeding.data$attempt==1),] #remove the ones that are second attempts
# breeding.data$Box <- gsub("^.{1,1}", "", breeding.data$temp) #get box number in right format
# breeding.data <- dplyr::left_join(breeding.data, box.locations, by="Box") #add box locations

breeding.data %<>% 
  filter(attempt == 1) %>% 
  mutate(Box = str_replace_all(temp, "^.{1,1}", "")) %>% 
  left_join(box.locations, by = "Box")

#get a base with great tits only to work with 
# xdata <- breeding.data[which(breeding.data$Species=="g"),]

xdata <- breeding.data %>% filter(Species == "g")

#remove records where a parent is not identified 
xdata$nest.id.type <- "NA" #make new column 

#fill in empty spaces with NA
# xdata$Mother <- as.character(xdata$Mother)
# xdata$mID <- with(xdata, ifelse(Mother=="", NA, 
#                                 xdata$Mother)) #label one ID
# xdata$mID <- !is.na(xdata$mID)
# 
# xdata$Father <- as.character(xdata$Father)
# xdata$fID <- with(xdata, ifelse(Father=="", NA, 
#                                 xdata$Father)) #label one ID
# xdata$fID <- !is.na(xdata$fID)

xdata <-
  xdata %>% 
  mutate_at(c("Mother", "Father"), as.character) %>% 
  mutate(mID = Mother != "") %>% 
  mutate(fID = Father != "")

# summary(as.factor(xdata$mID))

table(xdata$mID)

# xdata$nest.id.type <- "neither"
# 
# xdata$nest.id.type <- with(xdata, ifelse(mID == TRUE & fID == FALSE, "one", 
#                                          xdata$nest.id.type)) #label one ID
# 
# xdata$nest.id.type <- with(xdata, ifelse(mID == FALSE & fID == TRUE, "one", 
#                                          xdata$nest.id.type)) #label one ID
# 
# xdata$nest.id.type <- with(xdata, ifelse(mID == TRUE & fID == TRUE, "both", 
#                                          xdata$nest.id.type)) #label both ID

# You could do it this way, with the TRUE and FALSE versions:

xdata <- 
  xdata %>% 
  mutate(nest.id.type = case_when(mID == FALSE & fID == FALSE ~ "neither",
                                  mID == FALSE & fID == TRUE ~ "one",
                                  mID == TRUE & fID == FALSE ~ "one",
                                  TRUE ~ "both"
                                  
  ))

# But I would do this:

xdata <-
  xdata %>% 
  mutate_at(c("mID", "fID"), as.numeric) %>% 
  mutate(nest.id.type = mID + fID + 1) %>% 
  mutate(nest.id.type = c("neither", "one", "both")[nest.id.type])

# xdata$fID <- NULL
# xdata$mID <- NULL

#remove neithers and ones 
# xdata <- xdata[which(xdata$nest.id.type == "both"),] 

xdata %<>% filter(nest.id.type == "both")

#make a row for the mom and the dad of each nest 

# You COULD do what you had: 
x <- data.table::setDT(xdata)[, .(parent = c("mother","father")),
                              .(year, Box, x, y, Section,
                                Species, April.lay.date, Laying.rate, Incubation.started,
                                April.hatch.date, Incubation.duration, Total.egg.weight, Num.eggs.weighed,
                                Clutch.size, Num.chicks, Num.dead.chicks, Num.fledglings, Mean.chick.weight,
                                Father, Mother)]

# x <- data.frame(x, "box.year.parentid" = paste(x$Box, x$year, x$parent,sep="_")) 

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

#xdata2 %>% 
#  mutate(focal.ring = ifelse(parent == "father",
#                             Father, 
#                             ifelse(parent == "mother", 
#                                    Mother, 
#                                    NA))) # This is called a "nested/cascading ifelse statement"

# You could also do this: 

#xdata %<>% as.data.frame()
#
#Longxdata <- 
#  xdata %>% pivot_longer(cols = c(Father, Mother), 
#                         names_to = "parent",
#                         values_to = "Focal.ring")
#
#xdata <- Longxdata

#add sex

# xdata$focal.sex <- NA
# 
# xdata$focal.sex <- with(xdata, ifelse(parent == "mother", "F", 
#                                       xdata$focal.sex)) 
# xdata$focal.sex <- with(xdata, ifelse(parent == "father", "M", 
#                                       xdata$focal.sex)) 

xdata2 %<>% mutate(focal.sex = substr(parent, 1, 1))

#add binary success fitness variable 
# xdata$Binary.succ <- xdata$Num.fledglings
# xdata$Binary.succ <- with(xdata, ifelse(Binary.succ == 0, "0", 
#                                         xdata$Binary.succ)) 
# xdata$Binary.succ <- with(xdata, ifelse(Binary.succ > 0, "1", 
#                                         xdata$Binary.succ)) 

# xdata$Binary.succ <- as.numeric(xdata$Binary.succ)

xdata2 %<>% mutate(Binary.succ = as.numeric(Num.fledglings > 0))

base.fn.data <- xdata2

###get a version of the breeding data for finding neighbors 
zdata <- raw.breeding.data

#make breeding data to use to get neighbors 
# zdata$Pnum <- as.character(zdata$Pnum)
# zdata$temp <- gsub("^.{0,4}", "", zdata$Pnum)  
# zdata$attempt <- substr(zdata$"temp",1,1) #label attempt number
# zdata <- zdata[which(zdata$attempt==1),] #remove the ones that are second attempts
# zdata$Box <- gsub("^.{1,1}", "", zdata$temp) #get box number in right format 
# zdata <- dplyr::left_join(zdata, box.locations, by="Box") #add box locations 

zdata <-
  raw.breeding.data %>% 
  mutate_at("Pnum", as.character) %>% 
  mutate(temp = str_replace_all(Pnum, "^.{0,4}", "")) %>% 
  mutate(attempt = substr(temp, 1, 1)) %>% 
  filter(attempt == 1) %>% 
  mutate(Box = str_replace_all(temp, "^.{1,1}", "")) %>% 
  left_join(box.locations, by = "Box")

#get a base with great tits only to work with 
# zdata <- zdata[which(zdata$Species=="g"),]

zdata %<>% filter(Species == "g")

breeding.data.neighbors <- zdata

xdata <- breeding.data.neighbors %>% filter(Species == "g")

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

FocalYears <- xdata$year %>% unique %>% sort

FocalYears <- FocalYears[FocalYears >= 1964]

FocalYear <- FocalYears[1]

NeighbourReferenceList <- list()
TerritoriesListList <- list()

FocalYear <- FocalYears[1]

# Beginning of loop ####

for(FocalYear in FocalYears){
  
  print(FocalYear)
  
  #1964
  # breeding.data.1964 <- xdata[which(xdata$year == 1964),] 
  
  breeding.data.1964 <- xdata %>% filter(year == FocalYear)
  
  # breeding.data.1964 <- breeding.data.1964[!is.na(breeding.data.1964$x), ] #can get this info somehow? for now removing the 10 without coords
  
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
  
  # territories <- sf::st_voronoi(sf::st_union(breeding.data.1964), box)
  
  # territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))
  
  territories <- breeding.data.1964 %>% st_union %>% st_voronoi(box) %>% 
    st_cast# %>% st_intersection(st_union(wood.outline))
  
  # plot(territories)
  
  #joining the territory polygons back up with the individuals that bred in them
  # data.frame(colnames(breeding.data.1964))
  # breeding.ids.1964 <- breeding.data.1964[,c(57,37,38)]
  
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
  
  #we'll remove cases where the identities of neighbors were unknown 
  #(presumably because they weren't caught or it failed before they were)
  
  #fill in empty spaces with NA
  # territories.list$Mother <- as.character(territories.list$Mother)
  # territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
  #                                                          territories.list$Mother)) #label one ID
  # 
  # territories.list$Father <- as.character(territories.list$Father)
  # territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
  #                                                          territories.list$Father)) #label one ID
  
  territories.list %<>% 
    mutate_at(c("Father", "Mother"), 
              ~.x %>% as.character) %>% 
    mutate_at(c("Father", "Mother"), 
              ~ifelse(.x == "", NA, .x))
  
  # territories.list$Mother.1 <- as.character(territories.list$Mother.1)
  # territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
  #                                                            territories.list$Mother.1)) #label one ID
  # 
  # territories.list$Father.1 <- as.character(territories.list$Father.1)
  # territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
  #                                                            territories.list$Father.1)) #label one ID
  
  territories.list %<>% 
    mutate_at(c("Father.1", "Mother.1"), 
              ~.x %>% as.character) %>% 
    mutate_at(c("Father.1", "Mother.1"), 
              ~ifelse(.x == "", NA, .x))
  
  # territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))
  
  territories.list %<>% filter(!is.na(Father), !is.na(Mother))
  
  #we also want to do the same if the focal nest box had no ID information
  # territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))
  
  territories.list %<>% filter(!is.na(Father.1), !is.na(Mother.1))
  
  #now just getting the dataframe into a nice order with informative column names
  
  # territories.list <- territories.list[,c(4,5,6,1,2,3)]
  
  territories.list %<>% dplyr::select(Box.1, 
                                      Father.1, Mother.1, 
                                      Box, 
                                      Father, Mother)
  
  # colnames(territories.list) <- 
  #   c("Focal.box", "Focal.male", "Focal.female", 
  #     "Box.N", "Neighboring.male", "Neighboring.female")
  
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
  # neighbors.1964$Focal.male <- with(neighbors.1964, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
  # neighbors.1964$Focal.female <- with(neighbors.1964, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
  # neighbors.1964$Neighboring.male <- with(neighbors.1964, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
  # neighbors.1964$Neighboring.female <- with(neighbors.1964, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 
  
  neighbors.1964 %<>% mutate_at(c("Focal.male", "Focal.female", 
                                  "Neighboring.male", "Neighboring.female"),
                                ~ifelse(is.na(.x), "UNKNOWN", .x))
  
  #remove unknowns so we just have a full list 
  
  # N1964.b <- as.data.frame(with(neighbors.1964, paste(Focal.male, Neighboring.male, sep="_")))
  # names(N1964.b)[1] <- "ring_ring"
  # N1964.c <- as.data.frame(with(neighbors.1964, paste(Focal.male, Neighboring.female, sep="_")))
  # names(N1964.c)[1] <- "ring_ring"
  # N1964.d <- as.data.frame(with(neighbors.1964, paste(Focal.female, Neighboring.female, sep="_")))
  # names(N1964.d)[1] <- "ring_ring"
  # N1964.e <- as.data.frame(with(neighbors.1964, paste(Focal.female, Neighboring.male, sep="_")))
  # names(N1964.e)[1] <- "ring_ring"
  # 
  # N1964 <- rbind(N1964.b, N1964.c, N1964.d, N1964.e)
  # rm(N1964.b, N1964.c, N1964.d, N1964.e)
  
  Focals <- c(neighbors.1964$Focal.male, neighbors.1964$Focal.female) %>% unique %>% sort
  Neighbours <- c(neighbors.1964$Neighboring.male, neighbors.1964$Neighboring.female) %>% unique %>% sort
  
  N1964 <- 
    neighbors.1964 %>% mutate(N1964.b = paste(Focal.male, Neighboring.male, sep="_"),
                              N1964.c = paste(Focal.male, Neighboring.female, sep="_"),
                              N1964.d = paste(Focal.female, Neighboring.female, sep="_"),
                              N1964.e = paste(Focal.female, Neighboring.male, sep="_")) %>% 
    dplyr::select(contains("N1964"))
  
  N1964 %<>% gather(value = "ring_ring") %>% select(ring_ring)
  
  N1964$neighbors <- TRUE
  # N1964$Year.s <- 1965
  N1964$Year.s <- FocalYear + 1
  
  # N1964 <- N1964[!grepl("UNKNOWN", N1964$ring_ring),]
  
  N1964 %<>% filter(!str_detect(ring_ring, "UNKNOWN"))
  
  # N_reference <- N1964
  
  NeighbourReferenceList[[which(FocalYears == FocalYear)]] <- N1964
  TerritoriesListList[[which(FocalYears == FocalYear)]] <- territories.list
  
}

# End of loop ####

# New Loop ###

NEIOutputList <- list()

#so i think the problem here is that the loop needs to start at 1965 rather than 1964??
#so i reset these? just wanted to leave a note incase i mess things up haha
FocalYears <- xdata$year %>% unique %>% sort

FocalYears <- FocalYears[FocalYears >= 1965]

FocalYear <- FocalYears[1]

for(FocalYear in FocalYears){ 
  
  print(FocalYear)
  
  # ydata <- base.fn.data[which(base.fn.data$year==1965),]
  ydata <- base.fn.data %>% filter(year == FocalYear)
  
  # nei1965 <- territories.list[,c(1,4)]
  nei1965 <- TerritoriesListList[[which(FocalYears == FocalYear)]][,c(1,4)]
  
  # nei1965 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1965
  nei1965 %<>% 
    group_by(Focal.box) %>% 
    mutate(id = seq_along(Focal.box))
  
  nei1965 <- tidyr::pivot_wider(nei1965, 
                                names_from = "id", 
                                values_from = "Box.N")
  
  #add "N" to column names 
  # colnames(nei1965) <- paste0('N', colnames(nei1965))
  # names(nei1965)[1] <- "Focal.box"
  
  nei1965 %<>% 
    rename_all(~paste0("N.", .x)) %>% 
    rename(Focal.box = 1)
  
  ##so now we need to add the parent information for each neighbor
  #get a list of all parents in 1965
  # as.data.frame(colnames(ydata))
  # par1965 <- ydata[,c(2,19,20)]
  # par1965 <- distinct(par1965, Box, .keep_all = TRUE)
  
  # For future reference you can do this 
  
  ydata[,c("Box", "Mother", "Father")]
  
  # Or this
  
  par1965 <- 
    ydata %>% 
    dplyr::select(c("Box", "Mother", "Father")) %>% 
    as.data.frame()
  
  # Both of these options will prevent what's happened here (I think)
  # Which is that it isn't selecting what I'm expecting
  
  par1965 <- distinct(par1965, Box, .keep_all = TRUE)
  
  # ####add neighbor ids
  # ####1965
  # #N1
  # names(par1965)[names(par1965) == "Box"] <- "N.1"
  # names(par1965)[names(par1965) == "Mother"] <- "N1.mother"
  # names(par1965)[names(par1965) == "Father"] <- "N1.father"
  # nei1965 <- merge(nei1965, par1965, by="N.1", all.x=TRUE)
  # #N2
  # names(par1965)[names(par1965) == "N.1"] <- "N.2"
  # names(par1965)[names(par1965) == "N1.mother"] <- "N2.mother"
  # names(par1965)[names(par1965) == "N1.father"] <- "N2.father"
  # nei1965 <- merge(nei1965, par1965, by="N.2", all.x=TRUE)
  # #N3
  # names(par1965)[names(par1965) == "N.2"] <- "N.3"
  # names(par1965)[names(par1965) == "N2.mother"] <- "N3.mother"
  # names(par1965)[names(par1965) == "N2.father"] <- "N3.father"
  # nei1965 <- merge(nei1965, par1965, by="N.3", all.x=TRUE)
  # #N4
  # names(par1965)[names(par1965) == "N.3"] <- "N.4"
  # names(par1965)[names(par1965) == "N3.mother"] <- "N4.mother"
  # names(par1965)[names(par1965) == "N3.father"] <- "N4.father"
  # nei1965 <- merge(nei1965, par1965, by="N.4", all.x=TRUE)
  # #N5
  # names(par1965)[names(par1965) == "N.4"] <- "N.5"
  # names(par1965)[names(par1965) == "N4.mother"] <- "N5.mother"
  # names(par1965)[names(par1965) == "N4.father"] <- "N5.father"
  # nei1965 <- merge(nei1965, par1965, by="N.5", all.x=TRUE)
  # #N6
  # names(par1965)[names(par1965) == "N.5"] <- "N.6"
  # names(par1965)[names(par1965) == "N5.mother"] <- "N6.mother"
  # names(par1965)[names(par1965) == "N5.father"] <- "N6.father"
  # nei1965 <- merge(nei1965, par1965, by="N.6", all.x=TRUE)
  # #N7
  # names(par1965)[names(par1965) == "N.6"] <- "N.7"
  # names(par1965)[names(par1965) == "N6.mother"] <- "N7.mother"
  # names(par1965)[names(par1965) == "N6.father"] <- "N7.father"
  # nei1965 <- merge(nei1965, par1965, by="N.7", all.x=TRUE)
  # #N8
  # names(par1965)[names(par1965) == "N.7"] <- "N.8"
  # names(par1965)[names(par1965) == "N7.mother"] <- "N8.mother"
  # names(par1965)[names(par1965) == "N7.father"] <- "N8.father"
  # nei1965 <- merge(nei1965, par1965, by="N.8", all.x=TRUE)
  # #N9
  # names(par1965)[names(par1965) == "N.8"] <- "N.9"
  # names(par1965)[names(par1965) == "N8.mother"] <- "N9.mother"
  # names(par1965)[names(par1965) == "N8.father"] <- "N9.father"
  # nei1965 <- merge(nei1965, par1965, by="N.9", all.x=TRUE)
  # #N10
  # names(par1965)[names(par1965) == "N.9"] <- "N.10"
  # names(par1965)[names(par1965) == "N9.mother"] <- "N10.mother"
  # names(par1965)[names(par1965) == "N9.father"] <- "N10.father"
  # nei1965 <- merge(nei1965, par1965, by="N.10", all.x=TRUE)

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
  
  # nei1965a  <- data.frame(nei1965, "box.year.parentid" = 
  #                           paste(nei1965$Focal.box, nei1965$year, "mother",sep="_")) 
  # 
  # nei1965b  <- data.frame(nei1965,"box.year.parentid" = 
  #                           paste(nei1965$Focal.box, nei1965$year, "father",sep="_"))
  
  nei1965a <- 
    nei1965 %>% mutate(box.year.parentid = paste(Focal.box, year, "mother", sep = "_"))
  
  nei1965b <- 
    nei1965 %>% mutate(box.year.parentid = paste(Focal.box, year, "father", sep = "_"))
  
  nei1965 <- rbind(nei1965a, nei1965b)
  rm(nei1965a, nei1965b)
  
  #ok so another problem here is that i manually looked at the max number of neighbors there were for each year
  #and then added columns with NA (like i'm doing below) so that they would all have ten... 
  #could you help me with this? 
  
  #### From Greg: you don't need to do that with bind_rows(); it adds NA's in automatically, 
  # and also aligns them automatically
  # Tidyverse is honestly so much better because of things like this
  
  # nei1965$N9 <- NA 
  # nei1965$N10 <- NA
  # 
  # nei1965$N9.mother <- NA
  # nei1965$N9.father <- NA
  # nei1965$N10.mother <- NA
  # nei1965$N10.father <- NA
  
  # nei1965 <- nei1965[,order(colnames(nei1965))]
  
  # nei_output <- nei1965
  
  NEIOutputList[[which(FocalYears == FocalYear)]] <- nei1965
  
}

# Combine everything in this list using this ####

newdata <- NEIOutputList %>% bind_rows#(.id = "Year")


###### 

## add number of neighbors 

temp <- as.data.frame((is.na(newdata[,c("N.1","N.2","N.3","N.4","N.5","N.6","N.7","N.8","N.9","N.10")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num <- 10 - (temp$sumna)
newdata$N.num <- temp$N.num


#add it to the base
newdata$year <- NULL #base already has the year
base.fn.data.temp <- merge(base.fn.data, newdata, by="box.year.parentid", all.x=TRUE)

#remove boxes without locations/that don't exist anymore
base.fn.data.temp <- base.fn.data.temp[which(!is.na(base.fn.data.temp$x)),]


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
CN1.mother <- DF.temp[,c(3,60)] #box.year.parentid and the column we just made (N1.MOTHERfp)
DF$ring_ring <-(with(DF, paste(focal.ring, N.1.Father, sep="_")))
names(N_reference)[2] <- "N1.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN1.father<- DF.temp[,c(3,60)] #box.year.parentid and the column we just made (N1.FATHERfp)


#N2 
DF$ring_ring <-(with(DF, paste(focal.ring, N.2.Mother, sep="_")))
names(N_reference)[2] <- "N2.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN2.mother <- DF.temp[,c(3,60)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.2.Father, sep="_")))
names(N_reference)[2] <- "N2.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN2.father<- DF.temp[,c(3,60)]

#N3
DF$ring_ring <-(with(DF, paste(focal.ring, N.3.Mother, sep="_")))
names(N_reference)[2] <- "N3.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN3.mother <- DF.temp[,c(3,60)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.3.Father, sep="_")))
names(N_reference)[2] <- "N3.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN3.father<- DF.temp[,c(3,60)]

#N4 
DF$ring_ring <-(with(DF, paste(focal.ring, N.4.Mother, sep="_")))
names(N_reference)[2] <- "N4.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN4.mother <- DF.temp[,c(3,60)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.4.Father, sep="_")))
names(N_reference)[2] <- "N4.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN4.father<- DF.temp[,c(3,60)]

#N5 
DF$ring_ring <-(with(DF, paste(focal.ring, N.5.Mother, sep="_")))
names(N_reference)[2] <- "N5.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN5.mother <- DF.temp[,c(3,60)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.5.Father, sep="_")))
names(N_reference)[2] <- "N5.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN5.father<- DF.temp[,c(3,60)]

#N6 
DF$ring_ring <-(with(DF, paste(focal.ring, N.6.Mother, sep="_")))
names(N_reference)[2] <- "N6.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN6.mother <- DF.temp[,c(3,60)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.6.Father, sep="_")))
names(N_reference)[2] <- "N6.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN6.father<- DF.temp[,c(3,60)]

#N7 
DF$ring_ring <-(with(DF, paste(focal.ring, N.7.Mother, sep="_")))
names(N_reference)[2] <- "N7.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN7.mother <- DF.temp[,c(3,60)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.7.Father, sep="_")))
names(N_reference)[2] <- "N7.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN7.father<- DF.temp[,c(3,60)]

#N8 
DF$ring_ring <-(with(DF, paste(focal.ring, N.8.Mother, sep="_")))
names(N_reference)[2] <- "N8.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN8.mother <- DF.temp[,c(3,60)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.8.Father, sep="_")))
names(N_reference)[2] <- "N8.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN8.father<- DF.temp[,c(3,60)]

#N9 
DF$ring_ring <-(with(DF, paste(focal.ring, N.9.Mother, sep="_")))
names(N_reference)[2] <- "N9.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN9.mother <- DF.temp[,c(3,60)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.9.Father, sep="_")))
names(N_reference)[2] <- "N9.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN9.father<- DF.temp[,c(3,60)]

#N10 
DF$ring_ring <-(with(DF, paste(focal.ring, N.10.Mother, sep="_")))
names(N_reference)[2] <- "N10.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN10.mother <- DF.temp[,c(3,60)]
DF$ring_ring <-(with(DF, paste(focal.ring, N.10.Father, sep="_")))
names(N_reference)[2] <- "N10.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN10.father<- DF.temp[,c(3,60)]


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

x <- merge(CN1, CN2, by="box.year.parentid")
x <- merge(x, CN3, by="box.year.parentid")
x <- merge(x, CN4, by="box.year.parentid")
x <- merge(x, CN5, by="box.year.parentid")
x <- merge(x, CN6, by="box.year.parentid")
x <- merge(x, CN7, by="box.year.parentid")
x <- merge(x, CN8, by="box.year.parentid")
x <- merge(x, CN9, by="box.year.parentid")
x <- merge(x, CN10, by="box.year.parentid")

DF$ring_ring <- NULL
DF.temp <- merge(DF, x, by="box.year.parentid", all.x=TRUE)


test1 <- as.data.frame(order(DF$box.year.parentid))
test2 <- as.data.frame(order(DF.temp$box.year.parentid))
summary(arsenal::comparedf(test1, test2))
DF.temp <- DF.temp[c(1:20586),]
summary(arsenal::comparedf(DF, DF.temp))

#label number of familiar neighbors (individuals )
temp <- as.data.frame((is.na(DF.temp[,c("N1.MOTHERfp","N1.FATHERfp","N2.MOTHERfp", "N2.FATHERfp", "N3.MOTHERfp","N3.FATHERfp","N4.MOTHERfp", "N4.FATHERfp", "N5.MOTHERfp", "N5.FATHERfp", "N6.MOTHERfp", "N6.FATHERfp", "N7.MOTHERfp","N7.FATHERfp","N8.MOTHERfp", "N8.FATHERfp", "N9.MOTHERfp" ,"N9.FATHERfp","N10.MOTHERfp", "N10.FATHERfp")])))
library(dplyr)
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num.familiar <- 20 - temp$sumna
hist(temp$N.num.familiar)
DF.temp$N.num.ind.familiar <- temp$N.num.familiar

#familiarity to mothers 
temp <- as.data.frame((is.na(DF.temp[,c("N1.MOTHERfp","N2.MOTHERfp", "N3.MOTHERfp","N4.MOTHERfp",  "N5.MOTHERfp",  "N6.MOTHERfp",  "N7.MOTHERfp","N8.MOTHERfp", "N9.MOTHERfp" ,"N10.MOTHERfp")])))
library(dplyr)
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num.familiar <- 10 - (temp$sumna)
hist(temp$N.num.familiar)
DF.temp$N.num.FEMALEind.familiar <- temp$N.num.familiar

#familiarity to father 
temp <- as.data.frame((is.na(DF.temp[,c("N1.FATHERfp","N2.FATHERfp", "N3.FATHERfp","N4.FATHERfp",  "N5.FATHERfp",  "N6.FATHERfp",  "N7.FATHERfp","N8.FATHERfp", "N9.FATHERfp" ,"N10.FATHERfp")])))
library(dplyr)
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num.familiar <- 10 - (temp$sumna)
hist(temp$N.num.familiar)
DF.temp$N.num.MALEind.familiar <- temp$N.num.familiar


#change NA to false
DF.temp$N1.MOTHERfp <- with(DF.temp, ifelse(is.na(N1.MOTHERfp), FALSE, N1.MOTHERfp))
DF.temp$N2.MOTHERfp <- with(DF.temp, ifelse(is.na(N2.MOTHERfp), FALSE, N2.MOTHERfp)) 
DF.temp$N3.MOTHERfp <- with(DF.temp, ifelse(is.na(N3.MOTHERfp), FALSE, N3.MOTHERfp)) 
DF.temp$N4.MOTHERfp <- with(DF.temp, ifelse(is.na(N4.MOTHERfp), FALSE, N4.MOTHERfp)) 
DF.temp$N5.MOTHERfp <- with(DF.temp, ifelse(is.na(N5.MOTHERfp), FALSE, N5.MOTHERfp)) 
DF.temp$N6.MOTHERfp <- with(DF.temp, ifelse(is.na(N6.MOTHERfp), FALSE, N6.MOTHERfp)) 
DF.temp$N7.MOTHERfp <- with(DF.temp, ifelse(is.na(N7.MOTHERfp), FALSE, N7.MOTHERfp)) 
DF.temp$N8.MOTHERfp <- with(DF.temp, ifelse(is.na(N8.MOTHERfp), FALSE, N8.MOTHERfp)) 
DF.temp$N9.MOTHERfp <- with(DF.temp, ifelse(is.na(N9.MOTHERfp), FALSE, N9.MOTHERfp)) 
DF.temp$N10.MOTHERfp <- with(DF.temp, ifelse(is.na(N10.MOTHERfp), FALSE, N10.MOTHERfp)) 

DF.temp$N1.FATHERfp <- with(DF.temp, ifelse(is.na(N1.FATHERfp), FALSE, N1.FATHERfp))
DF.temp$N2.FATHERfp <- with(DF.temp, ifelse(is.na(N2.FATHERfp), FALSE, N2.FATHERfp)) 
DF.temp$N3.FATHERfp <- with(DF.temp, ifelse(is.na(N3.FATHERfp), FALSE, N3.FATHERfp)) 
DF.temp$N4.FATHERfp <- with(DF.temp, ifelse(is.na(N4.FATHERfp), FALSE, N4.FATHERfp)) 
DF.temp$N5.FATHERfp <- with(DF.temp, ifelse(is.na(N5.FATHERfp), FALSE, N5.FATHERfp)) 
DF.temp$N6.FATHERfp <- with(DF.temp, ifelse(is.na(N6.FATHERfp), FALSE, N6.FATHERfp)) 
DF.temp$N7.FATHERfp <- with(DF.temp, ifelse(is.na(N7.FATHERfp), FALSE, N7.FATHERfp)) 
DF.temp$N8.FATHERfp <- with(DF.temp, ifelse(is.na(N8.FATHERfp), FALSE, N8.FATHERfp)) 
DF.temp$N9.FATHERfp <- with(DF.temp, ifelse(is.na(N9.FATHERfp), FALSE, N9.FATHERfp)) 
DF.temp$N10.FATHERfp <- with(DF.temp, ifelse(is.na(N10.FATHERfp), FALSE, N10.FATHERfp)) 


fn.data <- DF.temp

saveRDS(fn.data, "fn.data.noage.Rds")
rm(x,xdata,xdata2,ydata,zdata,test1,test2,temp)
##everything until here should be fine?? 

#setwd("~/Documents/2/Familiar_neighbors/DATA")
#fn.data <- readRDS("fn.data.noage.Rds")


####get info for if pairs were together in previous year ####

pairs <- fn.data[,c("Father","Mother", "year")]
pairs <- unique(pairs)

pairs$Father <- toupper(pairs$Father)
pairs$Mother <- toupper(pairs$Mother)
pairs$yearplusone <- pairs$year + 1
pairs$ring_ring <-(with(pairs, paste(Father, Mother, sep="_")))

pairs <- pairs[,c(4,5)]
names(pairs)[1] <- "year"
pairs$Pairfp <- TRUE

fn.data$ring_ring <-(with(fn.data, paste(Father, Mother, sep="_")))


####add the ages also ####
agedata <- read.csv("GRETI_Age_Data.csv")
agedata <- agedata[,c(2,3,4,10)]
agedata$Season <- gsub("^.{0,5}", "", agedata$Season)  

agedata$Age <- agedata$Estimate_Age
agedata$Age <- with(agedata, ifelse(Age > 1, "adult", agedata$Age)) 
agedata$Age <- with(agedata, ifelse(Age < 2, "juvenile", agedata$Age)) 

agedata <- agedata[,c(1,2,5)]
names(agedata)[1] <- "focal.ring"
names(agedata)[2] <- "year"
agedata <- data.frame(agedata,"ring.year"=paste(agedata$focal.ring, agedata$year,sep="_")) 
agedata <- agedata[!duplicated(agedata[,"ring.year"]),]
agedata$ring.year <- NULL

fn.data$focal.ring <- toupper(fn.data$focal.ring)

fn.data.temp <- merge(fn.data, agedata, by=c("focal.ring", "year"), all.x=TRUE)

#remove the juveniles and NA 
summary(as.factor(fn.data.temp$Age))
table(fn.data.temp$Age, fn.data.temp$year)
fn.data <- fn.data.temp[which(fn.data.temp$Age=="adult"),]


#add pair info
temp <- merge(fn.data, pairs, by=c("ring_ring", "year"), all.x=TRUE)

temp <- as.data.frame(temp[,c(1,2,84)])

#change NA to false
temp$Pairfp <- as.logical(with(temp, ifelse(is.na(Pairfp), "FALSE", Pairfp))) 
summary(as.factor(temp$Pairfp))


###trying it another way? #### 
pairs2 <- fn.data[,c("Father","Mother", "year")]
pairs2$Father <- toupper(pairs2$Father)
pairs2$Mother <- toupper(pairs2$Mother)
pairs2$ring_ring <-(with(pairs2, paste(Father, Mother, sep="_")))

pairs2 <- pairs2[,c(3,4)]
temp <- merge(pairs2, pairs, by=c("ring_ring", "year"), all.x=TRUE)
temp$Pairfp <- as.logical(with(temp, ifelse(is.na(Pairfp), "FALSE", Pairfp))) 

temp <- merge(fn.data, temp, by=c("ring_ring", "year"), all.x=TRUE)
table(temp$Pairfp, temp$Mean.chick.weight)
#nope##### 



fn.data <- temp

#uppercase column names 
colnames(fn.data) <- stringr::str_to_title(colnames(fn.data))

saveRDS(fn.data, "fn.data.Rds")


