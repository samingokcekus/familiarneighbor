
# 0_Greg Script ####

library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
library(GGally); library(patchwork); library(dplyr)

theme_set(theme_cowplot())

# DF <- readRDS("Data/CleanData.rds")

DF <- readRDS("Data/fn.data.rds")

# Response variables:
#   •	Lay date [April.lay.date]
# •	Binary success [Binary.succ]
# •	Clutch size [Clutch.size]
# •	Mean chick weight [Mean.chick.weight]
# •	Number of fledglings [Num.fledglings]

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
  "Num.fledglings")

Covar <- "Year"

SocialCovar <- c("N.num",
                 "N.num.maleind.familiar",
                 "N.num.femaleind.familiar",
                 "Pairfp"
)

# DF %>% dplyr::select(SocialCovar) %>% GGally::ggpairs()

IMList <- list()
IMList2 <- list()

Resps %<>% sort

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
  
  # IM2 <- INLAModelAdd(Data = TestDF, 
  #                     Response = Resps[r], 
  #                     Explanatory = Covar, 
  #                     Add = SocialCovar, # %>% c(DensityCovar),
  #                     AllModels = T,
  #                     Base = T,
  #                     # Rounds = 1,
  #                     # Clashes = ClashList,
  #                     Random = c("Focal.ring", "fYear"), RandomModel = rep("iid", 2),
  #                     AddSpatial = T,
  #                     # Groups = T, 
  #                     GroupVar = "fYear")
  # 
  # IMList[[Resps[r]]] <- IM2
  
  IM2b <- INLAModelAdd(Data = TestDF, 
                       Response = Resps[r], 
                       Explanatory = Covar %>% c(IMList[[Resps[r]]]$Kept), 
                       Add = paste0("Focal.sex:", IMList[[Resps[r]]]$Kept),
                       AllModels = T,
                       Base = T,
                       # Rounds = 1,
                       # Clashes = ClashList,
                       Random = c("Focal.ring", "fYear"), RandomModel = rep("iid", 2),
                       AddSpatial = T,
                       # Groups = T, 
                       GroupVar = "fYear")
  
  IMList2[[Resps[r]]] <- IM2b
  
}

IMList %>% map("FinalModel") %>% 
  Efxplot(ModelNames = Resps, PointOutline = T) +
  scale_colour_brewer(palette = "Spectral") +
  IMList %>% map(c("Spatial", "Model")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T) +
  scale_colour_brewer(palette = "Spectral") +
  plot_layout(guides = "collect")

IMList %>% 
  map(~list(.x$FinalModel, .x$Spatial$Model) %>% INLADICFig) %>% 
  ArrangeCowplot()

IMList %>% names %>% 
  map(~ggField(IMList[[.x]]$Spatial$Model, IMList[[.x]]$Spatial$Mesh) + 
        labs(fill = .x) +
        scale_fill_discrete_sequential(palette = "Mint")) %>% 
  ArrangeCowplot() + 
  ggsave("Fields.jpeg", units = "mm", width = 400, height = 300)
