
# 0_Greg Script ####

library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
library(GGally); library(patchwork); library(dplyr); library(fs)

theme_set(theme_cowplot())

dir_create("Figures")

# DF <- readRDS("Data/fn.data.rds")
DF <- readRDS("Data/fn.data.full.rds")

DF <- DF[!duplicated(colnames(DF))]

DF %<>% mutate(Pair = paste0(Male, "_", Female))

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

Covar <- c("Year", 
           "Largeoaks",
           "Age_num",
           "Focal.sex") %>% setdiff("Focal.sex")

SocialCovar <- c("N.num",
                 "N.num.maleind.familiar",
                 "N.num.femaleind.familiar",
                 
                 "N.prop.maleind.familiar",
                 "N.prop.femaleind.familiar",
                 
                 # "PrevDist",
                 
                 "Pairfp"
)

ClashList <- list(SocialCovar[1:5])
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
  
  print("Female!")
  
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
  
  print("Male!")
  
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

# IMList %>% saveRDS("IMList.rds")

IMList %>% map("Female") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps) +
  
  ggtitle("Female") +
  
  IMList %>% map("Male") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps) +
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("Figures/BaseModelOutput.jpeg", units = "mm", height = 180, width = 250)

IMList %>% map("Female") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps) +
  
  ggtitle("Female") +
  
  IMList %>% map("Male") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps) +
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("Figures/SPDEModelOutput.jpeg", units = "mm", height = 180, width = 250)

IMList %>% map("Female") %>% map("FinalModel") %>% map("dDIC")

IMList %>% map("Male") %>% map("FinalModel") %>% map("dDIC")


