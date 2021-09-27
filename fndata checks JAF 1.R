

rm(list=ls())
fn<-readRDS("fn.data.Rds")

head(fn)


#YEARS
plot(table(fn$Year)) #I think we should perhaps only use years from 1973 onwards perhaps? Before that point all the years have less than 35 datapoints which feels fairly low, and will be biased to certain areas of the woods I think


#COMPLETE DUPLICATIONS
table(fn$Box.year.parentid)[table(fn$Box.year.parentid)>1] #some Box.year.parentid combinations have 4 entries? others have 2

#normally we just choose the first laydate in each box (and discard the second box attempts) and also only use individuals first breeding attempt across boxes too --- but these duplications look identical?
fn.dups<-fn[fn$Box.year.parentid%in%fn$Box.year.parentid[duplicated(fn$Box.year.parentid)],]
nrow(fn.dups)
fn.dups[1:4,] #look like duplicates in terms of laydates etc

unique(fn.dups)[,1:8] #each only has one row now? 

#so

fn2<-unique(fn) #this should fix it



#PARTIAL INDIV DUPLICATE WITHIN YEARS
fn2$ring.yr<-paste(fn2$Focal.ring,fn2$Year)
table(fn2$ring.yr)[table(fn2$ring.yr)>1] #even after removing the above duplicates, we see that we have this issue of duplicates of individuals within years

fn2.dups<-fn2[fn2$ring.yr%in%fn2$ring.yr[duplicated(fn2$ring.yr)],]
fn2.dups[1:4,] #looks like its the same individual breeding in different boxes - I think we just take their first choices (given thats what we are using for the other individuals) --- feels strange to include second choices

#so
min.dates<-tapply(fn2$April.lay.date,fn2$ring.yr,function(a)min(a))
fn3<-fn2[paste(fn2$ring.yr,fn2$April.lay.date) %in% paste(names(min.dates),min.dates),]



#PARTIAL BOX DUPLICATE WITHIN YEARS

fn3$box.yr<-paste(fn3$Box,fn3$Year)
table(fn3$box.yr)[table(fn3$box.yr)>2] #after removing the above individual-within-year duplicates, we don't appear to have any issues with box-within-year  duplicates either

table(fn3$box.yr[fn3$Parent%in%"mother"])[table(fn3$box.yr[fn3$Parent%in%"mother"])>1] #true for just mothers
table(fn3$box.yr[fn3$Parent%in%"father"])[table(fn3$box.yr[fn3$Parent%in%"father"])>1] #and true for just fathers
#no fix needed



#NON-MATCHING PAIRS
fath.t<-fn3[fn3$Parent=="father",c("box.yr","Father","Mother")]
moth.t<-fn3[fn3$Parent=="mother",c("box.yr","Father","Mother")]

mean(unique(fn3$box.yr) %in% fath.t$box.yr  & unique(fn3$box.yr) %in% moth.t$box.yr) #only minority of the box-year have both the mother and father known?

both.parents<-unique(fn3$box.yr[fn3$box.yr %in% fath.t$box.yr  & fn3$box.yr %in% moth.t$box.yr])


fath.b<-fath.t[match(both.parents,fath.t$box.yr),]

moth.b<-moth.t[match(both.parents,moth.t$box.yr),]


mean(fath.b$Father ==moth.b$Father)
mean(fath.b$Mother ==moth.b$Mother)
#the father and mother datasets are identical/symmetrical when considering only boxes with both know - no fix needed

saveRDS(fn3, "fn.data.Rds")


