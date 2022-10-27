#### samin modifying greg model script

library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
library(GGally); library(patchwork); library(dplyr); library(fs)

theme_set(theme_cowplot())

#dir_create("allYear22")


#all models with model selection on number of neighbors #### 
DF <- readRDS("Data/fn.data.full.ALLYEARSto22.Rds")
DF <- DF[!duplicated(colnames(DF))]

DF %<>% mutate(Pair = paste0(Father, "_", Mother))

DF %<>% mutate(BoxYear = paste0(Box, "_", Year))


#getmovedornot column 

DF$samebox <- ifelse(DF$Distance != 0 | is.na(DF$Distance), "moved", "samebox")

Resps <- c(#"April.hatch.date",
  "April.lay.date",
  "Binary.succ",
  "Clutch.size",
  "Mean.chick.weight",
  "Num.fledglings") %>% 
  sort

Families <- c("gaussian", "binomial", 
              rep("gaussian", 3))

names(Families) <- Resps

Covar <- c("Year", 
           "Largeoaks",
           "Age_num",
           "samebox",
           "Focal.sex") %>% setdiff("Focal.sex")

SocialCovar <- c(#"N.num",
  "N.num.ind.familiar",
  "N.num.maleind.familiar",
  "N.num.femaleind.familiar",
  "Pairfp"
)

ClashList <- list(SocialCovar[1:3])
# ClashList <- list()

IMList <- 
  IMList2 <- 
  list()

# run model ####

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
  
  IM1 <- INLAModelAdd(Data = TestDF %>% filter(Focal.sex == "F"),
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
  
  IM2 <- INLAModelAdd(Data = TestDF %>% filter(Focal.sex == "M"),
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

#IMList %>% saveRDS("IMListNumber.rds")
IMListNum <- IMList


IMListNum %>% map("Female") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F, Size = 3, 
          ModelNames = Resps)  +   scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) 

IMListNum %>% map("Female") %>% map(c("FinalModel")) %>% 
  Efxplot(Intercept = F, Size = 3, 
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Stayed in box",
                           "Number of familiar neighbors","Pair familiarity (true)",
                           "Number of male familiar neighbors"))) +
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Female") +
  
  IMListNum %>% map("Male") %>% map(c("FinalModel")) %>% 
  Efxplot(Intercept = F, Size = 3, 
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Stayed in box",
                           "Pair familiarity (true)"))) +
  scale_color_brewer(palette= "Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")


ggsave("allYear22/BaseModelOutputNumberDistance.jpeg", units = "mm", height = 200, width = 400)







IMListNum %>% map("Female") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F, Size = 3, 
          ModelNames = Resps)  +   scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) 

IMListNum %>% map("Female") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F, Size = 3, 
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Stayed in box",
                           "Number of familiar neighbors","Pair familiarity (true)",
                           "Number of male familiar neighbors"))) +
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Female") +
  
  IMListNum %>% map("Male") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F, Size = 3, 
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Stayed in box",
                           "Pair familiarity (true)"))) +
  scale_color_brewer(palette= "Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")


ggsave("allYear22/SPDEModelOutputNumberDistance.jpeg", units = "mm", height = 200, width = 400)


IMListNum %>% 
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
  saveRDS("allYear22/NumberModelOutputsDistance.rds")


IMListNum %>% 
  map(function(a){
    
    a %>% map(function(b){
      
      b$Spatial$Model$summary.fixed %>% as.data.frame() %>% 
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
  saveRDS("allYear22/NumberModelOutputsDistanceSPDE.rds")
