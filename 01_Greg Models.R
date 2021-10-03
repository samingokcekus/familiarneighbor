
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
           "Pairfp",
           "Focal.sex") %>% setdiff("Focal.sex")

SocialCovar <- c(#"N.num",
  # "N.num.ind.familiar",
  # "N.num.maleind.familiar",
  # "N.num.femaleind.familiar"
  
  "N.prop.maleind.familiar",
  "N.prop.femaleind.familiar",
  "N.prop.ind.familiar"
  
  # "PrevDist",
  
  # "Pairfp"
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

IMList %>% saveRDS("IMListNumber.rds")

IMList %>% map("Female") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps) +
  
  ggtitle("Female") +
  
  IMList %>% map("Male") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps) +
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("Figures/BaseModelOutputNumber.jpeg", units = "mm", height = 180, width = 250)

IMList %>% map("Female") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps) +
  
  ggtitle("Female") +
  
  IMList %>% map("Male") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps) +
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("Figures/SPDEModelOutputNumber.jpeg", units = "mm", height = 180, width = 250)

IMList %>% map(c("Female", "AllModels", 1)) %>% map(~map(.x, "dDIC"))

(IMList %>% map(c("Female", "AllModels")) %>% map(2) %>%
  map(~Efxplot(.x, VarOrder = SocialCovar))) %>% 
  append(

(IMList %>% map(c("Male", "AllModels")) %>% map(2) %>%
  map(~Efxplot(.x, VarOrder = SocialCovar)))) %>% 
  ArrangeCowplot() +
  plot_layout(guides = "collect", 
              ncol = 5)

ggsave("Figures/AllOutputsProportion.jpeg", units = "mm", height = 180, width = 350)

IMList %>% map("Male") %>% map("FinalModel") %>% map("dDIC")

IMList %>% 
  map(function(a){
    
    a %>% map(function(b){
      
      b$FinalModel$summary.fixed %>% as.data.frame() %>% 
        rownames_to_column() %>% 
        rename(Variable = rowname)
      
    }) %>% bind_rows(.id = "Sex") %>% 
      select(Sex, 
             Variable,
             Estimate = mean,
             Lower = `0.025quant`,
             Upper = `0.975quant`) %>% 
      mutate_at(2:4+1, ~round(.x, 3)) %>% 
      mutate(Significant = as.numeric(Lower*Upper > 0))
    
  }) %>% 
  saveRDS("ModelOutputs.rds")
