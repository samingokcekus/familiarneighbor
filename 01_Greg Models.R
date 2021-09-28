
# 0_Greg Script ####

library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
library(GGally); library(patchwork); library(dplyr)

theme_set(theme_cowplot())

DF <- readRDS("Data/fn.data.rds")

DF %<>% mutate(Pair = paste0(Mother, "_", Father))

DF %<>% mutate(BoxYear = paste0(Box, "_", Year))

Resps <- c(#"April.hatch.date",
  "April.lay.date",
  "Binary.succ",
  "Clutch.size",
  # "Incubation.duration",
  # "Incubation.started",
  # "Laying.rate",
  "Mean.chick.weight",
  # "Total.egg.weight",
  # "Num.chicks",
  # "Num.dead.chicks",
  # "Num.eggs.weighed",
  "Num.fledglings") %>% 
  sort

Families <- c("gaussian", "binomial", 
              rep("gaussian", 3))

names(Families) <- Resps

Covar <- c("Year", "Focal.sex") %>% setdiff("Focal.sex")

SocialCovar <- c("N.num",
                 "N.num.maleind.familiar",
                 "N.num.femaleind.familiar",
                 "Pairfp"
)

ClashList <- list(SocialCovar[1:3])
# ClashList <- list()

IMList <- 
  IMList2 <- 
  list()

# Overall ####

r <- 1

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  TestDF <- DF %>% 
    dplyr::select(all_of(Covar), 
                  all_of(SocialCovar),
                  Focal.ring,
                  Focal.sex,
                  BoxYear,
                  Resps[r], X, Y) %>% 
    mutate(fYear = Year) %>% 
    na.omit
  
  TestDF %>% nrow %>% print
  
  if(Resps[r] == "April.lay.date"){
    
    TestDF %<>% 
      filter(April.lay.date < 55)
    
  }
  
  IM1 <- INLAModelAdd(Data = TestDF %>% filter(Focal.sex == "f"),
                      Response = Resps[r],
                      Explanatory = Covar,
                      Add = SocialCovar, # %>% c(DensityCovar),
                      AllModels = T,
                      Base = T,
                      # Rounds = 1,
                      Clashes = ClashList,
                      Family = Families[Resps[r]],
                      Random = c("Focal.ring", 
                                 # "BoxYear", 
                                 "fYear"), 
                      RandomModel = rep("iid", 3),
                      AddSpatial = T,
                      # Groups = T,
                      Beep = F,
                      GroupVar = "fYear")
  
  IM2 <- INLAModelAdd(Data = TestDF %>% filter(Focal.sex == "m"),
                      Response = Resps[r],
                      Explanatory = Covar,
                      Add = SocialCovar, # %>% c(DensityCovar),
                      AllModels = T,
                      Base = T,
                      # Rounds = 1,
                      Clashes = ClashList,
                      Family = Families[Resps[r]],
                      Random = c("Focal.ring", 
                                 # "BoxYear", 
                                 "fYear"), 
                      RandomModel = rep("iid", 3),
                      AddSpatial = T,
                      # Groups = T,
                      Beep = F,
                      GroupVar = "fYear")
  
  IMList[[Resps[r]]]$Female <- IM1
  IMList[[Resps[r]]]$Male <- IM2
  
}

