## library path
.libPaths(new =  "/home/wanyu368/R/foss-2023a/4.3"  )
#for parallel processing

library(tidymodels)
library(tidyverse)
library(future)
options(future.globals.maxSize = Inf)
plan(future.callr::callr, workers = strtoi(Sys.getenv('SLURM_CPUS_PER_TASK')),gc = TRUE)
#plan(future.callr::callr, workers = 15,gc = TRUE)
#assign 8gb ram to each core


datafolder <- "/nesi/nobackup/uoo03493/Yue/stacking_gfactor_modelling_7.0/"
#datafolder <- "/Volumes/sci-psy-narun/Nesi/Yue/stacking_gfactor_modelling_7.0/"

source(paste0(datafolder,"r_functions_7.0.R"))

args <- commandArgs(trailingOnly = TRUE)
dataset <- args[1]
# dataset <- "random_forest_list.RDS"

batch_idx <- strtoi(Sys.getenv('SLURM_ARRAY_TASK_ID'))
#batch_idx <- 9

#print(batch_idx)

# number of threads to use for parallel xgboost
# passed into set_engine()
#threads <- strtoi(Sys.getenv("SLURM_CPU_PER_TASK"))

print("Setup complete!")
message("Setup complete!")

list_input <- readRDS(file = paste0(datafolder, "random_forest_data/", dataset))

train_list <- list_input[["train_list"]]
test_list <- list_input[["test_list"]]

modality_names=c("rsmri_gordon_aseg_data","rsmri_subnet","rsmri_within_avg_data")%>% 
  str_remove_all("data$")


train_input <- train_list[[batch_idx]]%>% drop_na() %>% 
  select(all_of(c("participant_id","session_id","ab_g_dyn__design_site","gfactor")),contains(modality_names))%>%
  mutate(Total_features = rowSums(pick(contains(modality_names)), 
                                  na.rm = TRUE)) %>% 
  filter(Total_features != 0) %>% 
  select(-Total_features)  

test_input <- test_list[[batch_idx]]%>% drop_na()%>% 
  select(all_of(c("participant_id","session_id","ab_g_dyn__design_site","gfactor")),contains(modality_names))%>%
  mutate(Total_features = rowSums(pick(contains(modality_names)), 
                                  na.rm = TRUE)) %>% 
  filter(Total_features != 0) %>% 
  select(-Total_features)  

subj_info <-  c("participant_id","session_id","ab_g_dyn__design_site")   

subj_info_resp <-  c(subj_info,"gfactor")

features <- train_input %>% select(-all_of(subj_info_resp))%>% colnames()

train_select <-select(train_input,-all_of(subj_info))

test_select <- select(test_input,-all_of(subj_info))

recipe_scale_seperate <-recipe_prep(train_input=train_select) 



set.seed(123) 
train_input <- recipe_scale_seperate[["preped_recipe"]]  %>% bake(new_data=NULL)
tuning_cv_folds <- train_input  %>%
  vfold_cv(v = 10)

## mtry is the number of predictors to sample at each split
## min_n (the number of observations needed to keep splitting nodes)
model_tune <-rand_forest(mtry = tune(),
                         trees = 500,
                         min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(recipe_scale_seperate[["norm_recipe"]]) %>%
  add_model(model_tune)


## automate generate grid for hyperparameters
grid_vec <- seq(1:40)
get_grid <- function(){
  model_grid <- 
    model_tune %>% 
    extract_parameter_set_dials(tune_wf)%>% 
    ## update the values of the parameters
    update(min_n =min_n(range = c(2,2000)))%>%
    update(mtry =mtry(range = c(1, 6)))%>%
    grid_latin_hypercube(size = 30)
  return(model_grid)
}
grid_list <- grid_vec %>% map(.,~get_grid())


tune_ctrl <- control_grid(save_pred = TRUE, verbose = TRUE
                          ,parallel_over ="everything"# "resamples"
)
#library(doFuture)
#registerDoFuture()
#plan(multisession(workers = 30))
start <- Sys.time()

#library(doParallel)
#registerDoParallel(strtoi(Sys.getenv('SLURM_CPUS_PER_TASK')))    

#doParallel::registerDoParallel(cores = 20)

tune_res <- map(grid_list,~tune_grid(
  tune_wf,
  resamples = tuning_cv_folds,
  metrics = metric_set(rmse),
  grid = .,
  control= tune_ctrl
))
end <- Sys.time()

##check how much of the grid it went through
#tune_res %>% 
#  collect_metrics()%>%
#    print()



best_tune_list <-map(tune_res, ~select_best(.,metric = "rmse")) %>% do.call(rbind,.)

best_tuned_param_list <- map(tune_res,~show_best(.,metric="rmse"))


final_grid <- best_tune_list %>% dplyr::select(-.config)

tune_res_final <- tune_grid(
  tune_wf,
  resamples = tuning_cv_folds,
  metrics = metric_set(rmse),
  grid = final_grid,
  control= tune_ctrl
)


best_tune <- select_best(tune_res_final,  metric = "rmse")

best_tuned_param <- show_best(tune_res_final, metric="rmse")

rf_final_wf <- tune_wf %>% finalize_workflow(best_tune)


random_forest_fit_scale_seperate <-  list(rf_wf_final = rf_final_wf, 
                                          best_rf_model = best_tune,
                                          best_rf_param = best_tuned_param)


random_forest_fit_wf_scale_seperate <- random_forest_fit_scale_seperate[["rf_wf_final"]]


rf_pred_scale_seperate <- model_final_fit(recipe_input = recipe_scale_seperate, 
                                          wf_input = random_forest_fit_wf_scale_seperate,
                                          test_data = test_select)


rf_pred_scale_seperate_train <- model_final_fit(recipe_input = recipe_scale_seperate, 
                                                wf_input = random_forest_fit_wf_scale_seperate,
                                                test_data = train_select)

output_list <- list(random_forest_fit = random_forest_fit_scale_seperate,
                    test_pred = rf_pred_scale_seperate,
                    train_pred = rf_pred_scale_seperate_train,
                    train_data = train_input,
                    test_data = test_input)

data_name <- gsub(dataset, pattern = "\\.RDS", replacement = "" )

saveRDS(output_list, paste0(datafolder, "random_forest_results/" ,"random_forest_rsmri", ".",batch_idx,".RDS" ))

