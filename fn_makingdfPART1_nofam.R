
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

#remove manipulated nests 
xdata %<>% filter(Num.eggs.manipulated == "0")

#make a row for the mom and the dad of each nest 
x <- data.table::setDT(xdata)[, .(parent = c("mother","father")),
                              .(year, Box, x, y, Section,
                                Species, April.lay.date, Laying.rate, Incubation.started,
                                April.hatch.date, Incubation.duration, Total.egg.weight, Num.eggs.weighed,
                                Clutch.size, Num.chicks, Num.dead.chicks, Num.fledglings, Mean.chick.weight,
                                Father, Mother)]

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

base.fn.data$focal.ring <- toupper(base.fn.data$focal.ring)

fn.data.temp <- merge(base.fn.data, agedata, by=c("focal.ring", "year"), all.x=TRUE)

###adding oak health info#### 
HQ <- read_excel("Habitatqualitydata.xls")
names(HQ)[1] <- "Box"
HQ$Box <- toupper(HQ$Box)
names(HQ)[2] <- "Largeoaks"

fn.data <- merge(fn.data.temp, HQ, by=c("Box"), all.x=TRUE)

#uppercase column names 
colnames(fn.data) <- stringr::str_to_title(colnames(fn.data))


####add distance moved from previous year for each individual ####

#make a base data frame to pull info from 
x <- breeding.data
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


saveRDS(fn3, "data.nofn.Rds")


