## library path
.libPaths(new =  "/home/wanyu368/R/foss-2023a/4.3"  )
#for parallel processing

library(tidymodels)
library(tidyverse)
library("sva")
library(future)
options(future.globals.maxSize = Inf)
plan(multisession, workers = 15)


datafolder <- "/Volumes/sci-psy-narun/Nesi/Yue/stacking_gfactor_modelling_7.0/"

source(paste0(datafolder,"r_functions_7.0.R"))

resp_data_set <- "gfactor_cfa_7.0.RData"

# dataset <-"X2backvs0back_ROI_.RDS"
# batch_idx <- 1
# dataset <-"antiLargeRewVsSmallRew_ROI_.RDS"
# dataset <-"incorrectgovsincorrectstop_ROI_.RDS"
# dataset <-"Normalised_T1_.RDS"
# dataset <-"Normalised_T2_.RDS"
# dataset <-"DTI_FA_AT_FS_.RDS"


dataset <- "rsmri_gordon_aseg_data.RDS"


for(batch_idx in c(1,2,6,7,8,9,10,13,15)){
  
  
  data_input <- readRDS(file = paste0(datafolder, "data/", dataset))
  resp_input <- readRDS(file = paste0(datafolder, "data/", resp_data_set))
  gen_dyn_site_age <- readRDS(file = paste0(datafolder, "data/", "gen_dyn_site_age.RDS")) %>% select(-ab_g_dyn__visit_age)
  
  
  site_vec <- names(resp_input)
  
  site_input <- site_vec[batch_idx]
  
  ### comput and process gfactor
  
  gfactor_list <- resp_input[[site_input]]
  
  event_vec <- unique(data_input$session_id)
  
  data_event_site <- left_join(data_input,gen_dyn_site_age, by =  c("participant_id","session_id")  )
  data_event_list <-  map(event_vec,~filter(data_event_site,session_id ==.))
  
  
  
  data_split <- split_func(data_input = data_event_site, site = site_input)
  
  
  subj_info <-  c("participant_id","session_id","ab_g_dyn__design_site")   
  
  features <- data_event_site %>% select(-all_of(subj_info))%>% colnames()
  
  ### processing features
  
  features_data_list <- data_processing_cross_sites_seperate(split_input = data_split)
  
  train_features <-filter(features_data_list, fold =="train")
  
  test_features <-filter(features_data_list, fold =="test")
  
  gfactor_train <- gfactor_list[["train_list"]] %>% do.call(rbind,.)
  
  gfactor_test <- gfactor_list[["test_list"]] %>% do.call(rbind,.)
  
  
  train_data <- left_join(gfactor_train,train_features, by = c("participant_id","session_id","ab_g_dyn__design_site"))%>% drop_na()
  
  test_data <- left_join(gfactor_test,test_features, by = c("participant_id","session_id","ab_g_dyn__design_site"))%>% drop_na()
  
  
  train_select <-select(train_data,"gfactor",all_of(features))
  
  test_select <- select(test_data,"gfactor",all_of(features))
  
  ### enent model fitting
  
  recipe_scale_seperate <-recipe_prep(train_input=train_select) 
  
  enet_fit_scale_seperate <-  enet_tuning(recipe_input = recipe_scale_seperate) 
  
  enet_fit_wf_scale_seperate <- enet_fit_scale_seperate[["enet_wf_final"]]
  
  
  enet_pred_scale_seperate <- model_final_fit(recipe_input = recipe_scale_seperate, 
                                              wf_input = enet_fit_wf_scale_seperate,
                                              test_data = test_select)
  
  
  enet_pred_scale_seperate_train <- model_final_fit(recipe_input = recipe_scale_seperate, 
                                                    wf_input = enet_fit_wf_scale_seperate,
                                                    test_data = train_select)
  
  
  output_list <- list(enet_fit = enet_fit_scale_seperate,
                      test_pred = enet_pred_scale_seperate,
                      train_pred = enet_pred_scale_seperate_train,
                      train_data=train_data,
                      test_data=test_data)
  
  data_name <- gsub(dataset, pattern = "\\.RDS", replacement = "" )
  
  saveRDS(output_list, paste0(datafolder, "enet_results/" ,data_name, ".",batch_idx,".RDS" ))
  
}





dataset <- "smri_T2_mean_total_data.RDS"


for(batch_idx in c(4,8,11)){
  
  
  data_input <- readRDS(file = paste0(datafolder, "data/", dataset))
  resp_input <- readRDS(file = paste0(datafolder, "data/", resp_data_set))
  gen_dyn_site_age <- readRDS(file = paste0(datafolder, "data/", "gen_dyn_site_age.RDS")) %>% select(-ab_g_dyn__visit_age)
  
  
  site_vec <- names(resp_input)
  
  site_input <- site_vec[batch_idx]
  
  ### comput and process gfactor
  
  gfactor_list <- resp_input[[site_input]]
  
  event_vec <- unique(data_input$session_id)
  
  data_event_site <- left_join(data_input,gen_dyn_site_age, by =  c("participant_id","session_id")  )
  data_event_list <-  map(event_vec,~filter(data_event_site,session_id ==.))
  
  
  
  data_split <- split_func(data_input = data_event_site, site = site_input)
  
  
  subj_info <-  c("participant_id","session_id","ab_g_dyn__design_site")   
  
  features <- data_event_site %>% select(-all_of(subj_info))%>% colnames()
  
  ### processing features
  
  features_data_list <- data_processing_cross_sites_seperate(split_input = data_split)
  
  train_features <-filter(features_data_list, fold =="train")
  
  test_features <-filter(features_data_list, fold =="test")
  
  gfactor_train <- gfactor_list[["train_list"]] %>% do.call(rbind,.)
  
  gfactor_test <- gfactor_list[["test_list"]] %>% do.call(rbind,.)
  
  
  train_data <- left_join(gfactor_train,train_features, by = c("participant_id","session_id","ab_g_dyn__design_site"))%>% drop_na()
  
  test_data <- left_join(gfactor_test,test_features, by = c("participant_id","session_id","ab_g_dyn__design_site"))%>% drop_na()
  
  
  train_select <-select(train_data,"gfactor",all_of(features))
  
  test_select <- select(test_data,"gfactor",all_of(features))
  
  ### enent model fitting
  
  recipe_scale_seperate <-recipe_prep(train_input=train_select) 
  
  enet_fit_scale_seperate <-  enet_tuning(recipe_input = recipe_scale_seperate) 
  
  enet_fit_wf_scale_seperate <- enet_fit_scale_seperate[["enet_wf_final"]]
  
  
  enet_pred_scale_seperate <- model_final_fit(recipe_input = recipe_scale_seperate, 
                                              wf_input = enet_fit_wf_scale_seperate,
                                              test_data = test_select)
  
  
  enet_pred_scale_seperate_train <- model_final_fit(recipe_input = recipe_scale_seperate, 
                                                    wf_input = enet_fit_wf_scale_seperate,
                                                    test_data = train_select)
  
  
  output_list <- list(enet_fit = enet_fit_scale_seperate,
                      test_pred = enet_pred_scale_seperate,
                      train_pred = enet_pred_scale_seperate_train,
                      train_data=train_data,
                      test_data=test_data)
  
  data_name <- gsub(dataset, pattern = "\\.RDS", replacement = "" )
  
  saveRDS(output_list, paste0(datafolder, "enet_results/" ,data_name, ".",batch_idx,".RDS" ))
  
}


