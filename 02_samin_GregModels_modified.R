#### samin modifying greg model script

library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
library(GGally); library(patchwork); library(dplyr); library(fs)

theme_set(theme_cowplot())

dir_create("allYear")


#all models with model selection on number of neighbors #### 
DF <- readRDS("Data/fn.data.full.ALLYEARS.rds")
DF <- DF[!duplicated(colnames(DF))]

DF %<>% mutate(Pair = paste0(Father, "_", Mother))

DF %<>% mutate(BoxYear = paste0(Box, "_", Year))

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

IMList %>% map("Female") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Pair familiarity (true)", 
                       "Number of familiar neighbors", "Number of male familiar neighbors"))) +
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Female") +
  
  IMList %>% map("Male") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Pair familiarity (true)", 
                            "Number of female familiar neighbors"))) +
  scale_color_brewer(palette= "Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("allYear/BaseModelOutputNumber.jpeg", units = "mm", height = 180, width = 250)

IMList %>% map("Female") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Pair familiarity (true)", 
                           "Number of familiar neighbors", "Number of male familiar neighbors"))) +
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Female") +
  
  IMList %>% map("Male") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Pair familiarity (true)", 
                           "Number of female familiar neighbors"))) +
  scale_color_brewer(palette= "Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("allYear/SPDEModelOutputNumber.jpeg", units = "mm", height = 180, width = 250)


#get model outputs####

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
  saveRDS("allYear/NumberModelOutputs.rds")


IMList %>% 
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
  saveRDS("allYear/NumberModelOutputsSPDE.rds")






###Forcing proportion in based on model selection on number####

###female lay date ####
DF <- readRDS("Data/fn.data.full.ALLYEARS.rds")

DF <- DF[!duplicated(colnames(DF))]

DF %<>% mutate(Pair = paste0(Father, "_", Mother))

DF %<>% mutate(BoxYear = paste0(Box, "_", Year))

Resps <- "April.lay.date"

Families <- "gaussian"

names(Families) <- Resps

Covar <- c("Year", 
           "Largeoaks",
           "Age_num",
           "Focal.sex") %>% setdiff("Focal.sex")

SocialCovar <- c(
  "N.prop.ind.familiar",
  "Pairfp")

ClashList <- list()

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
  
  IM1 <- INLAModelAdd(Data = TestDF %>% filter(Focal.sex == "F"),
                      Response = Resps[r],
                      Explanatory = c(Covar, SocialCovar),
                      #Add = SocialCovar, # %>% c(DensityCovar),
                      AllModels = T,
                      Base = T,
                      Family = Families[Resps[r]],
                      Random = c("Focal.ring", 
                                 "fYear"), 
                      RandomModel = rep("iid", 3),
                      AddSpatial = T,
                      # Groups = T,
                      Beep = F,
                      GroupVar = "fYear")
  
  IMList[[Resps[r]]]$Female <- IM1
  
}

female.lay.prop <- IMList


female.lay.prop %>% map("Female") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps,
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Proportion familiar neighbors", 
                           "Pair familiarity (true)"))) +
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Female") 


female.lay.prop %>% map("Female") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps,
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Proportion familiar neighbors", 
                           "Pair familiarity (true)"))) +
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Female") 




###female clutch size ####
DF <- readRDS("Data/fn.data.full.ALLYEARS.rds")

DF <- DF[!duplicated(colnames(DF))]

DF %<>% mutate(Pair = paste0(Father, "_", Mother))

DF %<>% mutate(BoxYear = paste0(Box, "_", Year))

Resps <- "Clutch.size"

Families <- "gaussian"

names(Families) <- Resps

Covar <- c("Year", 
           "Largeoaks",
           "Age_num",
           "Focal.sex") %>% setdiff("Focal.sex")

SocialCovar <- c(
  "N.prop.maleind.familiar",
  "Pairfp")

ClashList <- list()

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
  
  print("Female!")
  
  IM1 <- INLAModelAdd(Data = TestDF %>% filter(Focal.sex == "F"),
                      Response = Resps[r],
                      Explanatory = c(Covar, SocialCovar),
                      #Add = SocialCovar, # %>% c(DensityCovar),
                      AllModels = T,
                      Base = T,
                      Family = Families[Resps[r]],
                      Random = c("Focal.ring", 
                                 "fYear"), 
                      RandomModel = rep("iid", 3),
                      AddSpatial = T,
                      # Groups = T,
                      Beep = F,
                      GroupVar = "fYear")
  
  IMList[[Resps[r]]]$Female <- IM1
  
}

female.clutch.prop <- IMList


female.clutch.prop %>% map("Female") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps,
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Proportion male familiar neighbors", 
                           "Pair familiarity (true)"))) +
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Female") 


female.clutch.prop %>% map("Female") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps,
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Proportion familiar neighbors", 
                           "Pair familiarity (true)"))) +
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Female") 


summary.female.prop <- Efxplot(list(female.clutch.prop[["Clutch.size"]][["Female"]][["FinalModel"]],
                                    female.lay.prop[["April.lay.date"]][["Female"]][["FinalModel"]]), 
                               Intercept = F,
                               ModelNames = c("Clutch size", "Lay date"),
                               VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Proportion male familiar neighbors", 
                                                "Pair familiarity (true)", "Proportion familiar neighbors"))) +
                            
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) + 
  ggtitle("Female (proportion)")

summary.female.prop.spatial <- Efxplot(list(female.clutch.prop[["Clutch.size"]][["Female"]][["Spatial"]][["Model"]],
                                            female.lay.prop[["April.lay.date"]][["Female"]][["Spatial"]][["Model"]]), 
                                       Intercept = F,
                                       ModelNames = c("Clutch size", "Lay date"),
                                       VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Proportion male familiar neighbors", 
                                                        "Pair familiarity (true)", "Proportion familiar neighbors"))) +
  
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) + 
  ggtitle("Female (proportion) + SPDE") 





###male clutch size ####
DF <- readRDS("Data/fn.data.full.ALLYEARS.rds")

DF <- DF[!duplicated(colnames(DF))]

DF %<>% mutate(Pair = paste0(Father, "_", Mother))

DF %<>% mutate(BoxYear = paste0(Box, "_", Year))

Resps <- "Clutch.size"

Families <- "gaussian"

names(Families) <- Resps

Covar <- c("Year", 
           "Largeoaks",
           "Age_num",
           "Focal.sex") %>% setdiff("Focal.sex")

SocialCovar <- c(
  "N.prop.femaleind.familiar",
  "Pairfp")

ClashList <- list()

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
  
  print("Male!")
  
  IM1 <- INLAModelAdd(Data = TestDF %>% filter(Focal.sex == "M"),
                      Response = Resps[r],
                      Explanatory = c(Covar, SocialCovar),
                      #Add = SocialCovar, # %>% c(DensityCovar),
                      AllModels = T,
                      Base = T,
                      Family = Families[Resps[r]],
                      Random = c("Focal.ring", 
                                 "fYear"), 
                      RandomModel = rep("iid", 3),
                      AddSpatial = T,
                      # Groups = T,
                      Beep = F,
                      GroupVar = "fYear")
  
  IMList[[Resps[r]]]$Male <- IM1
  
}

male.clutch.prop <- IMList


male.clutch <- male.clutch.prop %>% map("Male") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps,
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Proportion female familiar neighbors", 
                           "Pair familiarity (true)"))) +
  
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Male (proportion)") 


male.clutch.spatial <- male.clutch.prop %>% map("Male") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps,
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Proportion female familiar neighbors", 
                           "Pair familiarity (true)"))) +
  
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Male (proportion) + SPDE") 

summary.prop <- plot_grid(summary.female.prop, male.clutch)
summary.prop.spatial <- plot_grid(summary.female.prop.spatial, male.clutch.spatial)


ggsave("allYear/BaseModelForceProp.jpeg", plot = summary.prop, units = "mm", height = 180, width = 250)
ggsave("allYear/SPDEModelForceProp.jpeg", plot = summary.prop.spatial, units = "mm", height = 180, width = 250)




female.clutch.prop %>% 
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
  saveRDS("allYear/female.clutch.prop.rds")

female.clutch.prop %>% 
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
  saveRDS("allYear/female.clutch.propSPDE.rds")


female.lay.prop %>% 
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
  saveRDS("allYear/female.lay.prop.rds")

female.lay.prop %>% 
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
  saveRDS("allYear/female.lay.propSPDE.rds")



male.clutch.prop %>% 
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
  saveRDS("allYear/male.clutch.prop.rds")

male.clutch.prop %>% 
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
  saveRDS("allYear/male.clutch.propSPDE.rds")


####supplementary only inds with at least 5 neighbors identified####

DFsupp <- readRDS("Data/fn.data.full.ALLYEARS.rds")

DFsupp$N.num.id <- DFsupp$N.num.female.id + DFsupp$N.num.male.id

DFsupp <- DFsupp[which(DFsupp$N.num.id > 4 ),]

DFsupp <- DFsupp[!duplicated(colnames(DFsupp))]

DFsupp %<>% mutate(Pair = paste0(Father, "_", Mother))

DFsupp %<>% mutate(BoxYear = paste0(Box, "_", Year))

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

# Overall ####

r <- 1

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  TestDFsupp <- DFsupp %>% 
    dplyr::select(all_of(Covar), 
                  all_of(SocialCovar),
                  Focal.ring,
                  Focal.sex,
                  BoxYear,
                  Resps[r], X, Y) %>% 
    mutate(fYear = Year) %>% 
    na.omit
  
  TestDFsupp %>% nrow %>% print
  
  if(Resps[r] == "April.lay.date"){
    
    TestDFsupp %<>% 
      filter(April.lay.date < 55)
    
  }
  
  print("Female!")
  
  IM1 <- INLAModelAdd(Data = TestDFsupp %>% filter(Focal.sex == "F"),
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
  
  IM2 <- INLAModelAdd(Data = TestDFsupp %>% filter(Focal.sex == "M"),
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

IMList %>% map("Female") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Pair familiarity (true)", 
                           "Number of familiar neighbors", "Number of male familiar neighbors"))) +
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Female") +
  
  IMList %>% map("Male") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps%>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Pair familiarity (true)", 
                           "Number of female familiar neighbors"))) +
  scale_color_brewer(palette= "Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("allYear/BaseModelOutputNumber.suppid.jpeg", units = "mm", height = 180, width = 250)

IMList %>% map("Female") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps) +
  
  ggtitle("Female") +
  
  IMList %>% map("Male") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps%>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Pair familiarity (true)", 
                           "Number of female familiar neighbors"))) +
  scale_color_brewer(palette= "Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("allYear/SPDEModelOutputNumber.suppid.jpeg", units = "mm", height = 180, width = 250)

####supplementary only inds with nearest neighbor identified####
DFsupp <- readRDS("Data/fn.data.full.ALLYEARS.rds")

DFsupp <- DFsupp[which(!is.na(DFsupp$N.1.Mother) & !is.na(DFsupp$N.1.Father)),]

DFsupp <- DFsupp[!duplicated(colnames(DFsupp))]

DFsupp %<>% mutate(Pair = paste0(Father, "_", Mother))

DFsupp %<>% mutate(BoxYear = paste0(Box, "_", Year))

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

# Overall ####

r <- 1

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  TestDFsupp <- DFsupp %>% 
    dplyr::select(all_of(Covar), 
                  all_of(SocialCovar),
                  Focal.ring,
                  Focal.sex,
                  BoxYear,
                  Resps[r], X, Y) %>% 
    mutate(fYear = Year) %>% 
    na.omit
  
  TestDFsupp %>% nrow %>% print
  
  if(Resps[r] == "April.lay.date"){
    
    TestDFsupp %<>% 
      filter(April.lay.date < 55)
    
  }
  
  print("Female!")
  
  IM1 <- INLAModelAdd(Data = TestDFsupp %>% filter(Focal.sex == "F"),
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
  
  IM2 <- INLAModelAdd(Data = TestDFsupp %>% filter(Focal.sex == "M"),
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

IMList %>% map("Female") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Pair familiarity (true)", 
                           "Number of familiar neighbors", "Number of male familiar neighbors"))) +
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Female") +
  
  IMList %>% map("Male") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps%>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Pair familiarity (true)", 
                           "Number of female familiar neighbors"))) +
  scale_color_brewer(palette= "Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("allYear/BaseModelOutputNumber.suppid.jpeg", units = "mm", height = 180, width = 250)

IMList %>% map("Female") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps%>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Pair familiarity (true)", 
                           "Number of familiar neighbors", "Number of male familiar neighbors"))) +
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Female") +
  
  IMList %>% map("Male") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings")),
          VarNames = rev(c("Intercept", "Year", "Habitat quality", "Age", "Pair familiarity (true)", 
                           "Number of female familiar neighbors"))) +
  scale_color_brewer(palette= "Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("allYear/SPDEModelOutputNumber.suppid.jpeg", units = "mm", height = 180, width = 250)



#### age without familiar neighbor info #### 

age.data <- readRDS("Data/data.nofn.rds")

age.data %<>% mutate(Pair = paste0(Father, "_", Mother))
age.data %<>% mutate(BoxYear = paste0(Box, "_", Year))

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
           "Focal.sex") %>% setdiff("Focal.sex")


ClashList <- list(SocialCovar[1:3])
# ClashList <- list()

IMList <- 
  IMList2 <- 
  list()

# Overall ####

r <- 1

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  TestDF <- age.data %>% 
    dplyr::select(all_of(Covar), 
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


IMList %>% map("Female") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))) +
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Female") +
  
  IMList %>% map("Male") %>% map("FinalModel") %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))) +
  scale_color_brewer(palette= "Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("allYear/BaseModelOutputAge.jpeg", units = "mm", height = 180, width = 250)

IMList %>% map("Female") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))) +
  scale_color_brewer(palette="Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Female") +
  
  IMList %>% map("Male") %>% map(c("Spatial", "Model")) %>% 
  Efxplot(Intercept = F,
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))) +
  scale_color_brewer(palette= "Dark2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")

ggsave("allYear/SPDEModelOutputAge.jpeg", units = "mm", height = 180, width = 250)


