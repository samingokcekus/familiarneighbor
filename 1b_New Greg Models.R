
# 0_Greg Script ####

library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
library(GGally); library(patchwork); library(dplyr)

theme_set(theme_cowplot())

DF <- readRDS("Data/fn.data.rds")

DF %<>% mutate(Pair = paste0(Mother, "_", Father))

DF %<>% mutate(BoxYear = paste0(Box, "_", Year))

Resps <- c("April.lay.date",
           "Binary.succ",
           "Clutch.size",
           "Mean.chick.weight",
           "Num.fledglings") %>% 
  sort

Families <- c("gaussian", "binomial", 
              rep("gaussian", 3))

names(Families) <- Resps

Covar <- "Year"

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
  
  if(Resps[r] == "April.lay.date"){
    
    TestDF %<>% 
      filter(April.lay.date < 55)
    
  }
  
  print("Male!")

  IMMale <- INLAModelAdd(Data = TestDF %>% filter(Focal.sex == "m"),
                         Response = Resps[r],
                         Explanatory = Covar,
                         Add = SocialCovar, # %>% c(DensityCovar),
                         # AllModels = T,
                         Base = T,
                         # Rounds = 1,
                         Clashes = ClashList,
                         Family = Families[Resps[r]],
                         Random = c("Focal.ring", "fYear"), RandomModel = rep("iid", 2),
                         AddSpatial = T,
                         # Groups = T,
                         GroupVar = "fYear")

  print("Female!")

  IMFemale <- INLAModelAdd(Data = TestDF %>% filter(Focal.sex == "f"),
                           Response = Resps[r],
                           Explanatory = Covar,
                           Add = SocialCovar, # %>% c(DensityCovar),
                           # AllModels = T,
                           Base = T,
                           # Rounds = 1,
                           Clashes = ClashList,
                           Family = Families[Resps[r]],
                           Random = c("Focal.ring", "fYear"), RandomModel = rep("iid", 2),
                           AddSpatial = T,
                           # Groups = T,
                           GroupVar = "fYear")
  
  IMSubsampled <- INLAModelAdd(Data = TestDF %>% 
                                 group_by(BoxYear) %>% 
                                 RandomSlice %>% 
                                 mutate(N = 1:n()) %>% 
                                 filter(N == 1),
                               Response = Resps[r],
                               Explanatory = Covar,
                               Add = SocialCovar, # %>% c(DensityCovar),
                               # AllModels = T,
                               Base = T,
                               # Rounds = 1,
                               Clashes = ClashList,
                               Family = Families[Resps[r]],
                               Random = c("Focal.ring", "fYear"), RandomModel = rep("iid", 2),
                               AddSpatial = T,
                               # Groups = T,
                               GroupVar = "fYear")
  
  IMList[[Resps[r]]] <- list(Male = IMMale, 
                             Female = IMFemale,
                             Subsampled = IMSubsampled)
  
}

IMList %>% map(c("Subsampled", "FinalModel")) %>% 
  Efxplot(ModelNames = names(IMList), 
          Intercept = F)

# Trialling just adding the interactions ####

ClashList2 <- list(paste0("Focal.sex:", SocialCovar))

IMList3 <- list()

r <- 1

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  TestDF <- DF %>% 
    dplyr::select(all_of(Covar), 
                  all_of(SocialCovar),
                  Focal.ring,
                  Focal.sex,
                  Resps[r], X, Y) %>% 
    mutate(fYear = Year) %>% 
    na.omit
  
  if(Resps[r] == "April.lay.date"){
    
    TestDF %<>% 
      filter(April.lay.date < 55)
    
  }
  
  IM3 <- INLAModelAdd(Data = TestDF,
                      Response = Resps[r],
                      Explanatory = Covar,
                      Add = paste0("Focal.sex:", SocialCovar),
                      AllModels = T,
                      Base = T,
                      # Rounds = 1,
                      Family = Families[Resps[r]],
                      Clashes = ClashList2,
                      Random = c("Focal.ring", "fYear"), RandomModel = rep("iid", 2),
                      # AddSpatial = T,
                      # Groups = T,
                      GroupVar = "fYear")
  
  IMList3[[Resps[r]]] <- IM3
  
}

IMList3 %>% 
  map("FinalModel") %>% 
  Efxplot(ModelNames = Resps, 
          Intercept = F,
          PointOutline = T) +
  scale_colour_brewer(palette = "Spectral")

DF %>% 
  SinaGraph("Focal.sex", Resps[3],
            "Pairfp", ColourGroups = T)
