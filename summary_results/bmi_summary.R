#------------------------------------------------------------
# Purpose: compile GBD2021 global results for high BMI
#------------------------------------------------------------

rm(list = ls())
source('../subnational_mapping.R')
source("../get_model_results.R")
source("../get_age_metadata.R")
source("../get_location_metadata.R")
source("../get_population.R")
source("../get_crosswalk_version.R")
source("../make_aggregates.R")
source("../get_outputs.R")
source("../get_draws.R")
source("../get_cause_metadata.R")

library(data.table)
library(tidyverse)
prevalence <- T
results_fp <- paste0("FILEPATH")

# results directories
ow_not_ob <- "../prev_ow_not_ob_custom/"
ow_and_ob <- "../prev_ow_custom/"
obesity <- "../prev_ob_custom/"
models <- data.table(ow_not_ob, ow_and_ob, obesity)

# use release_id 9 for all the machinary results
locs <- get_location_metadata(release_id = 9, location_set_id = 35)
ages <- get_age_metadata(release_id = 9)
locs_of_interest <- locs[level==3, .(location_id, ihme_loc_id, location_name, super_region_id, super_region_name, region_id, region_name)]

# pull population estimates
pops <- get_population(release_id = 9, age_group_id = c(6:20, 30:32, 235), sex_id = c(1,2,3),
                       year_id = c(1990:2021), location_id = locs_of_interest$location_id)

if(dir.exists(results_fp) == FALSE){
  dir.create(results_fp)
} 

if(prevalence){
  for(m in names(models)){
    print(m)
    results_dir <- models[[m]]
    
    prev_draws <- lapply(paste0(results_dir, locs_of_interest$location_id, ".csv"), fread) %>%
      rbindlist(use.names = TRUE)
    
    prev_draws <- prev_draws[year_id %in% c(1990:2021)]
    
    # reshape the draws to wide
    prev_draws[,draw:=paste0("draw_", draw)]
    prev_draws <- dcast.data.table(prev_draws[,.(location_id, year_id, age_group_id, sex_id, draw, value)], 
                                        location_id + year_id + age_group_id + sex_id ~ draw, value.var = "value")
    
    # create estimates for children (2-24) and adults (25+) separately
    for(age_group in c("children")){
      if(age_group == "children"){
        age_grps <- c(6:9)
        age_name <- "5_24"
      } else {
        age_grps <- c(10:20, 30:32, 235)
        age_name <- "25_125"
      }
      print(paste0("age group: ",age_group))
      print(age_grps)
      prev_draws_wide <- prev_draws[age_group_id %in% age_grps]
      
      # Defining names for new columns
      vars <- paste0("draw_", seq(0, 999, 1)) 
      vars_AS <- paste0("draw_AS_", seq(0, 999, 1)) 
      count_cols <- paste0("draw_count_", seq(0, 999, 1)) 
      vars_AA <- paste0("draw_AA_", seq(0, 999, 1))
      p_change_2021 <- paste0("draw_2021_", seq(0, 999, 1))
      p_change_1990 <- paste0("draw_1990_", seq(0, 999, 1))
      p_num_chg_2021 <- paste0("draw_chg_2021_", seq(0, 999, 1))
      p_num_chg_1990 <- paste0("draw_1990_", seq(0, 999, 1))
      
      # Merging population and age weights onto prevalence draws
      current <- merge(prev_draws_wide, ages[,.(age_group_id, age_group_name,age_group_weight_value)], by = "age_group_id", all.x = T)
      current <- merge(current[year_id >= 1990], pops[, .(age_group_id, location_id, year_id, sex_id, population)], by = c("location_id", "year_id", "age_group_id", "sex_id"), all.x = T)
      
      if(nrow(current[is.na(age_group_id)]) > 0) stop("Missing age groups")
      if(nrow(current[is.na(population)]) > 0) stop("Missing populations")
      
      print("Dataset prepared")
      
      # Creating both sexes sample
      both_sexes <- copy(current)
      
      # Creating global sample
      total <- copy(current)
      
      # Creating super region sample
      total_super_region <- copy(current)
      
      # Creating region sample
      total_region <- copy(current)
      
      # Creating global and regional sample for both sexes
      total_both_sexes <- copy(current)
      total_region_both_sexes <- copy(current)
      total_super_region_both_sexes <- copy(current)
      
      # Getting age-standardized estimates by sex-year-location
      current[, (vars_AS) := lapply(.SD, function(x) sum(x*age_group_weight_value/sum(age_group_weight_value))), .SDcols = vars, by = c("location_id", "year_id", "sex_id")]
      
      # Getting age-standardized estimates by year-location
      both_sexes[, both_sex_population := sum(population), by = c("location_id", "year_id", "age_group_id")]
      both_sexes[, (count_cols) := lapply(.SD, function(x) x*population), .SDcols = vars, by = c("location_id", "year_id", "sex_id", "age_group_id")]
      both_sexes[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("location_id", "year_id", "age_group_id")]
      
      both_sexes[, (vars) := lapply(.SD, function(x) x/both_sex_population), .SDcols = count_cols, by = c("location_id", "year_id", "age_group_id")]
      both_sexes$population <- NULL
      
      print(nrow(both_sexes))
      both_sexes <- both_sexes[sex_id == 1]
      both_sexes$sex_id <- NULL
      print(nrow(both_sexes))
      
      both_sexes[, (vars_AS) := lapply(.SD, function(x) sum(x*age_group_weight_value/sum(age_group_weight_value))), .SDcols = vars, by = c("location_id", "year_id")]
      
      # Getting age-standardized estimates by sex-year for global sample
      total[, full_pop := sum(population), by = c("sex_id", "age_group_id", "year_id")]
      total[, (count_cols) := lapply(.SD, function(x) x*population), .SDcols = vars, by = c("location_id", "year_id", "sex_id", "age_group_id")]
      total[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("sex_id", "age_group_id", "year_id")]
      
      total[, (vars) := lapply(.SD, function(x) x/full_pop), .SDcols = count_cols, by = c("sex_id", "age_group_id", "year_id")]
      total <- total[location_id == 66]
      total$location_id <- NULL
      total$population <- NULL
      
      total[, (vars_AS) := lapply(.SD, function(x) sum(x*age_group_weight_value/sum(age_group_weight_value))), .SDcols = vars, by = c("sex_id", "year_id")]
      
      # Getting age-standardized estimates by year for super region sample
      total_super_region <- merge(total_super_region, locs_of_interest[,.(location_id, super_region_id)], by = "location_id", all.x = T)
      total_super_region[, full_pop := sum(population), by = c("sex_id", "age_group_id", "year_id", "super_region_id")]
      total_super_region[, (count_cols) := lapply(.SD, function(x) x*population), .SDcols = vars, by = c("location_id", "year_id", "sex_id", "age_group_id")]
      total_super_region[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("sex_id", "age_group_id", "year_id", "super_region_id")]
      
      total_super_region[, (vars) := lapply(.SD, function(x) x/full_pop), .SDcols = count_cols, by = c("sex_id", "age_group_id", "year_id", "super_region_id")]
      total_super_region <- total_super_region[location_id %in% c(6,33,71,121,160,161,168)] # keep 7 super regions
      
      total_super_region$location_id <- NULL
      total_super_region$population <- NULL
      
      total_super_region[, (vars_AS) := lapply(.SD, function(x) sum(x*age_group_weight_value/sum(age_group_weight_value))), .SDcols = vars, by = c("sex_id", "year_id","super_region_id")]
      
      # Getting age-standardized estimates by year for region sample
      total_region <- merge(total_region, locs_of_interest[,.(location_id, region_id)], by = "location_id", all.x = T)
      total_region[, full_pop := sum(population), by = c("sex_id", "age_group_id", "year_id", "region_id")]
      total_region[, (count_cols) := lapply(.SD, function(x) x*population), .SDcols = vars, by = c("location_id", "year_id", "sex_id", "age_group_id")]
      total_region[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("sex_id", "age_group_id", "year_id", "region_id")]
      
      total_region[, (vars) := lapply(.SD, function(x) x/full_pop), .SDcols = count_cols, by = c("sex_id", "age_group_id", "year_id", "region_id")]
      reg_ids <- total_region[,region_id] %>% unique
      reg_locs <- NULL
      for(i in reg_ids){
        l <- locs_of_interest[region_id == i, location_id][1]
        reg_locs <- c(reg_locs, l)
      }
      
      total_region <- total_region[location_id %in% reg_locs] # keep 21 regions
      
      total_region$location_id <- NULL
      total_region$population <- NULL
      
      total_region[, (vars_AS) := lapply(.SD, function(x) sum(x*age_group_weight_value/sum(age_group_weight_value))), .SDcols = vars, by = c("sex_id", "year_id","region_id")]
      
      
      # Getting age-standardized estimates by year for both sexes for global
      total_both_sexes[, full_pop := sum(population), by = c("age_group_id", "year_id")]
      total_both_sexes[, (count_cols) := lapply(.SD, function(x) x*population), .SDcols = vars, by = c("location_id", "year_id", "age_group_id")]
      total_both_sexes[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("age_group_id", "year_id")]
      
      total_both_sexes[, (vars) := lapply(.SD, function(x) x/full_pop), .SDcols = count_cols, by = c("age_group_id", "year_id")]
      total_both_sexes <- total_both_sexes[location_id == 66 & sex_id==1]
      total_both_sexes$location_id <- NULL
      total_both_sexes$population <- NULL
      total_both_sexes$sex_id <- 3
      
      total_both_sexes[, (vars_AS) := lapply(.SD, function(x) sum(x*age_group_weight_value/sum(age_group_weight_value))), .SDcols = vars, by = c("year_id")]
      
      # Getting age-standardized estimates by year for both sexes for super region
      total_super_region_both_sexes <- merge(total_super_region_both_sexes, locs_of_interest[,.(location_id, super_region_id)], by = "location_id", all.x = T)
      total_super_region_both_sexes[, full_pop := sum(population), by = c("age_group_id", "year_id", "super_region_id")]
      total_super_region_both_sexes[, (count_cols) := lapply(.SD, function(x) x*population), .SDcols = vars, by = c("location_id", "year_id", "age_group_id", "super_region_id")]
      total_super_region_both_sexes[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("age_group_id", "year_id", "super_region_id")]
      
      total_super_region_both_sexes[, (vars) := lapply(.SD, function(x) x/full_pop), .SDcols = count_cols, by = c("age_group_id", "year_id", "super_region_id")]
      total_super_region_both_sexes <- total_super_region_both_sexes[location_id %in% c(6,33,71,121,160,161,168) & sex_id==1]
      total_super_region_both_sexes$location_id <- NULL
      total_super_region_both_sexes$population <- NULL
      total_super_region_both_sexes$sex_id <- 3
      
      total_super_region_both_sexes[, (vars_AS) := lapply(.SD, function(x) sum(x*age_group_weight_value/sum(age_group_weight_value))), .SDcols = vars, by = c("year_id","super_region_id")]
      
      # Getting age-standardized estimates by year for both sexes for region
      total_region_both_sexes <- merge(total_region_both_sexes, locs_of_interest[,.(location_id, region_id)], by = "location_id", all.x = T)
      total_region_both_sexes[, full_pop := sum(population), by = c("age_group_id", "year_id", "region_id")]
      total_region_both_sexes[, (count_cols) := lapply(.SD, function(x) x*population), .SDcols = vars, by = c("location_id", "year_id", "age_group_id", "region_id")]
      total_region_both_sexes[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("age_group_id", "year_id", "region_id")]
      
      total_region_both_sexes[, (vars) := lapply(.SD, function(x) x/full_pop), .SDcols = count_cols, by = c("age_group_id", "year_id", "region_id")]
      total_region_both_sexes <- total_region_both_sexes[location_id %in% reg_locs & sex_id==1]
      total_region_both_sexes$location_id <- NULL
      total_region_both_sexes$population <- NULL
      total_region_both_sexes$sex_id <- 3
      
      total_region_both_sexes[, (vars_AS) := lapply(.SD, function(x) sum(x*age_group_weight_value/sum(age_group_weight_value))), .SDcols = vars, by = c("year_id","region_id")]
      
      
      # Getting the number of ow/ob population by sex-year-location
      current[, (count_cols) := lapply(.SD, function(x) x*population), .SDcols = vars, by = c("location_id", "year_id", "sex_id", "age_group_id")]
      current[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("location_id", "year_id", "sex_id")]
      
      # Getting the number of ow/ob population by year-location
      both_sexes[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("location_id", "year_id")]
      
      # Getting the number of ow/ob population by year-sex
      total[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("sex_id", "year_id")]
      
      # Getting the number of ow/ob population by year-sex and super region
      total_super_region[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("sex_id", "year_id", "super_region_id")]
      
      # Getting the number of ow/ob population by year-sex and region
      total_region[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("sex_id", "year_id", "region_id")]
      
      # Getting the number of ow/ob population by year for both sexes
      total_both_sexes[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("year_id")]
      
      # Getting the number of ow/ob population by year for both sexes and by super region
      total_super_region_both_sexes[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("year_id","super_region_id")]
      
      # Getting the number of ow/ob population by year for both sexes and by region
      total_region_both_sexes[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("year_id","region_id")]
      
      # Getting all-ages prevalence by sex-year-location
      current[, all_pop := sum(population), by = c("location_id", "year_id", "sex_id")]
      current[, (vars_AA) := lapply(.SD, function(x) x/all_pop), .SDcols = count_cols, by = c("location_id", "sex_id", "year_id")]
      
      # Getting all-ages prevalence by year-location
      both_sexes[, all_pop := sum(both_sex_population), by = c("location_id", "year_id")]
      both_sexes[, (vars_AA) := lapply(.SD, function(x) x/all_pop), .SDcols = count_cols, by = c("location_id", "year_id")]
      
      # Getting all-ages prevalence by year-sex
      total[, all_pop := sum(full_pop), by = c("sex_id", "year_id")]
      total[, (vars_AA) := lapply(.SD, function(x) x/all_pop), .SDcols = count_cols, by = c("sex_id", "year_id")]
      
      # Getting all-ages prevalence by year, sex and super region
      total_super_region[, all_pop := sum(full_pop), by =c("sex_id", "year_id", "super_region_id")]
      total_super_region[, (vars_AA) := lapply(.SD, function(x) x/all_pop), .SDcols = count_cols, by = c("sex_id", "year_id", "super_region_id")]
      
      # Getting all-ages prevalence by year, sex and region
      total_region[, all_pop := sum(full_pop), by =c("sex_id", "year_id", "region_id")]
      total_region[, (vars_AA) := lapply(.SD, function(x) x/all_pop), .SDcols = count_cols, by = c("sex_id", "year_id", "region_id")]
      
      # Getting all-ages prevalence by year for both sexes
      total_both_sexes[, all_pop := sum(full_pop), by = c("year_id")]
      total_both_sexes[, (vars_AA) := lapply(.SD, function(x) x/all_pop), .SDcols = count_cols, by = c("year_id")]
      
      # Getting all-ages prevalence by year for both sexes and by super region
      total_super_region_both_sexes[, all_pop := sum(full_pop), by = c("year_id","super_region_id")]
      total_super_region_both_sexes[, (vars_AA) := lapply(.SD, function(x) x/all_pop), .SDcols = count_cols, by = c("year_id","super_region_id")]
      
      # Getting all-ages prevalence by year for both sexes and by region
      total_region_both_sexes[, all_pop := sum(full_pop), by = c("year_id","region_id")]
      total_region_both_sexes[, (vars_AA) := lapply(.SD, function(x) x/all_pop), .SDcols = count_cols, by = c("year_id","region_id")]
      
      #---------------prev estimates----------------
      print("Getting prevalence estimates!")
      
      # For age-specific:
      current[, mean_prev := rowMeans(.SD), .SD = vars]
      current[, lower_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars]
      current[, upper_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars]
      
      both_sexes[, mean_prev := rowMeans(.SD), .SD = vars]
      both_sexes[, lower_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars]
      both_sexes[, upper_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars]
      
      total[, mean_prev := rowMeans(.SD), .SD = vars]
      total[, lower_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars]
      total[, upper_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars]
      
      total_super_region[, mean_prev := rowMeans(.SD), .SD = vars]
      total_super_region[, lower_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars]
      total_super_region[, upper_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars]
      
      total_region[, mean_prev := rowMeans(.SD), .SD = vars]
      total_region[, lower_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars]
      total_region[, upper_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars]
      
      total_both_sexes[, mean_prev := rowMeans(.SD), .SD = vars]
      total_both_sexes[, lower_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars]
      total_both_sexes[, upper_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars]
      
      total_super_region_both_sexes[, mean_prev := rowMeans(.SD), .SD = vars]
      total_super_region_both_sexes[, lower_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars]
      total_super_region_both_sexes[, upper_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars]
      
      total_region_both_sexes[, mean_prev := rowMeans(.SD), .SD = vars]
      total_region_both_sexes[, lower_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars]
      total_region_both_sexes[, upper_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars]
      
      # For age-standardized:
      current[, mean_AS_prev := rowMeans(.SD), .SD = vars_AS]
      current[, lower_AS_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AS]
      current[, upper_AS_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AS]
      
      both_sexes[, mean_AS_prev := rowMeans(.SD), .SD = vars_AS]
      both_sexes[, lower_AS_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AS]
      both_sexes[, upper_AS_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AS]
      
      total[, mean_AS_prev := rowMeans(.SD), .SD = vars_AS]
      total[, lower_AS_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AS]
      total[, upper_AS_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AS]
      
      total_super_region[, mean_AS_prev := rowMeans(.SD), .SD = vars_AS]
      total_super_region[, lower_AS_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AS]
      total_super_region[, upper_AS_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AS]
      
      total_region[, mean_AS_prev := rowMeans(.SD), .SD = vars_AS]
      total_region[, lower_AS_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AS]
      total_region[, upper_AS_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AS]
      
      total_both_sexes[, mean_AS_prev := rowMeans(.SD), .SD = vars_AS]
      total_both_sexes[, lower_AS_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AS]
      total_both_sexes[, upper_AS_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AS]
      
      total_super_region_both_sexes[, mean_AS_prev := rowMeans(.SD), .SD = vars_AS]
      total_super_region_both_sexes[, lower_AS_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AS]
      total_super_region_both_sexes[, upper_AS_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AS]
      
      total_region_both_sexes[, mean_AS_prev := rowMeans(.SD), .SD = vars_AS]
      total_region_both_sexes[, lower_AS_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AS]
      total_region_both_sexes[, upper_AS_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AS]
      
      # For number of ow/ob population: 
      current[, mean_count := rowMeans(.SD), .SD = count_cols]
      current[, lower_count := apply(.SD, 1, quantile, c(.025)), .SDcols = count_cols]
      current[, upper_count := apply(.SD, 1, quantile, c(.975)), .SDcols = count_cols]
      
      both_sexes[, mean_count := rowMeans(.SD), .SD = count_cols]
      both_sexes[, lower_count := apply(.SD, 1, quantile, c(.025)), .SDcols = count_cols]
      both_sexes[, upper_count := apply(.SD, 1, quantile, c(.975)), .SDcols = count_cols]
      
      total[, mean_count := rowMeans(.SD), .SD = count_cols]
      total[, lower_count := apply(.SD, 1, quantile, c(.025)), .SDcols = count_cols]
      total[, upper_count := apply(.SD, 1, quantile, c(.975)), .SDcols = count_cols]
      
      total_super_region[, mean_count := rowMeans(.SD), .SD = count_cols]
      total_super_region[, lower_count := apply(.SD, 1, quantile, c(.025)), .SDcols = count_cols]
      total_super_region[, upper_count := apply(.SD, 1, quantile, c(.975)), .SDcols = count_cols]
      
      total_region[, mean_count := rowMeans(.SD), .SD = count_cols]
      total_region[, lower_count := apply(.SD, 1, quantile, c(.025)), .SDcols = count_cols]
      total_region[, upper_count := apply(.SD, 1, quantile, c(.975)), .SDcols = count_cols]
      
      total_both_sexes[, mean_count := rowMeans(.SD), .SD = count_cols]
      total_both_sexes[, lower_count := apply(.SD, 1, quantile, c(.025)), .SDcols = count_cols]
      total_both_sexes[, upper_count := apply(.SD, 1, quantile, c(.975)), .SDcols = count_cols]
      
      total_super_region_both_sexes[, mean_count := rowMeans(.SD), .SD = count_cols]
      total_super_region_both_sexes[, lower_count := apply(.SD, 1, quantile, c(.025)), .SDcols = count_cols]
      total_super_region_both_sexes[, upper_count := apply(.SD, 1, quantile, c(.975)), .SDcols = count_cols]
      
      total_region_both_sexes[, mean_count := rowMeans(.SD), .SD = count_cols]
      total_region_both_sexes[, lower_count := apply(.SD, 1, quantile, c(.025)), .SDcols = count_cols]
      total_region_both_sexes[, upper_count := apply(.SD, 1, quantile, c(.975)), .SDcols = count_cols]
      
      # For all-ages prevalence: 
      current[, mean_AA_prev := rowMeans(.SD), .SD = vars_AA]
      current[, lower_AA_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AA]
      current[, upper_AA_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AA]
      
      both_sexes[, mean_AA_prev := rowMeans(.SD), .SD = vars_AA]
      both_sexes[, lower_AA_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AA]
      both_sexes[, upper_AA_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AA]
      
      total[, mean_AA_prev := rowMeans(.SD), .SD = vars_AA]
      total[, lower_AA_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AA]
      total[, upper_AA_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AA]
      
      total_super_region[, mean_AA_prev := rowMeans(.SD), .SD = vars_AA]
      total_super_region[, lower_AA_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AA]
      total_super_region[, upper_AA_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AA]
      
      total_region[, mean_AA_prev := rowMeans(.SD), .SD = vars_AA]
      total_region[, lower_AA_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AA]
      total_region[, upper_AA_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AA]
      
      total_both_sexes[, mean_AA_prev := rowMeans(.SD), .SD = vars_AA]
      total_both_sexes[, lower_AA_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AA]
      total_both_sexes[, upper_AA_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AA]
      
      total_super_region_both_sexes[, mean_AA_prev := rowMeans(.SD), .SD = vars_AA]
      total_super_region_both_sexes[, lower_AA_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AA]
      total_super_region_both_sexes[, upper_AA_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AA]
      
      total_region_both_sexes[, mean_AA_prev := rowMeans(.SD), .SD = vars_AA]
      total_region_both_sexes[, lower_AA_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AA]
      total_region_both_sexes[, upper_AA_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AA]
      
      #---------percentage change of prevalence---------
      print("Onto percentage change of prevalence!")
      
      # Percent change of AS prevalence between 2021 and 1990 by sex-location-age
      current_2021 <- copy(current[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars_AS]
      current_1990 <- copy(current[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars_AS]
      
      current_p <- merge(current_2021, current_1990, by = c("location_id", "age_group_id", "sex_id"))
      
      temp <- (current_p[, ..p_change_2021]-current_p[, ..p_change_1990])/current_p[, ..p_change_1990]
      current_p[, (p_change_2021) := temp]
      
      # Percent change of AS prevalence between 2021 and 1990 by location-age
      both_sexes_2021 <- copy(both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars_AS]
      both_sexes_1990 <- copy(both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars_AS]
      
      both_sexes_p <- merge(both_sexes_2021, both_sexes_1990, by = c("location_id", "age_group_id"))
      
      temp <- (both_sexes_p[, ..p_change_2021]-both_sexes_p[, ..p_change_1990])/both_sexes_p[, ..p_change_1990]
      both_sexes_p[, (p_change_2021) := temp]
      
      # Percent change of AS prevalence between 2021 and 1990 by sex-age
      total_2021 <- copy(total[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars_AS]
      total_1990 <- copy(total[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars_AS]
      
      total_p <- merge(total_2021, total_1990, by = c("age_group_id", "sex_id"))
      
      temp <- (total_p[, ..p_change_2021]-total_p[, ..p_change_1990])/total_p[, ..p_change_1990]
      total_p[, (p_change_2021) := temp]
      
      # Percent change of AS prevalence between 2021 and 1990 by sex-age and by the super-region
      total_super_region_2021 <- copy(total_super_region[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars_AS]
      total_super_region_1990 <- copy(total_super_region[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars_AS]
      
      total_super_region_p <- merge(total_super_region_2021, total_super_region_1990, by = c("age_group_id", "sex_id", "super_region_id"))
      
      temp <- (total_super_region_p[, ..p_change_2021]-total_super_region_p[, ..p_change_1990])/total_super_region_p[, ..p_change_1990]
      total_super_region_p[, (p_change_2021) := temp]
      
      # Percent change of AS prevalence between 2021 and 1990 by sex-age and by the region
      total_region_2021 <- copy(total_region[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars_AS]
      total_region_1990 <- copy(total_region[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars_AS]
      
      total_region_p <- merge(total_region_2021, total_region_1990, by = c("age_group_id", "sex_id", "region_id"))
      
      temp <- (total_region_p[, ..p_change_2021]-total_region_p[, ..p_change_1990])/total_region_p[, ..p_change_1990]
      total_region_p[, (p_change_2021) := temp]
      
      # Percent change of AS prevalence between 2021 and 1990 for both sexes for the globe
      total_both_sexes_2021 <- copy(total_both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars_AS]
      total_both_sexes_1990 <- copy(total_both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars_AS]
      
      total_both_sexes_p <- merge(total_both_sexes_2021, total_both_sexes_1990, by = c("age_group_id", "sex_id"))
      
      temp <- (total_both_sexes_p[, ..p_change_2021]-total_both_sexes_p[, ..p_change_1990])/total_both_sexes_p[, ..p_change_1990]
      total_both_sexes_p[, (p_change_2021) := temp]
      
      # Percent change of AS prevalence between 2021 and 1990 for both sexes for the super-region
      total_super_region_both_sexes_2021 <- copy(total_super_region_both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars_AS]
      total_super_region_both_sexes_1990 <- copy(total_super_region_both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars_AS]
      
      total_super_region_both_sexes_p <- merge(total_super_region_both_sexes_2021, total_super_region_both_sexes_1990, by = c("age_group_id", "sex_id", "super_region_id"))
      
      temp <- (total_super_region_both_sexes_p[, ..p_change_2021]-total_super_region_both_sexes_p[, ..p_change_1990])/total_super_region_both_sexes_p[, ..p_change_1990]
      total_super_region_both_sexes_p[, (p_change_2021) := temp]
      
      # Percent change of AS prevalence between 2021 and 1990 for both sexes for the region
      total_region_both_sexes_2021 <- copy(total_region_both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars_AS]
      total_region_both_sexes_1990 <- copy(total_region_both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars_AS]
      
      total_region_both_sexes_p <- merge(total_region_both_sexes_2021, total_region_both_sexes_1990, by = c("age_group_id", "sex_id", "region_id"))
      
      temp <- (total_region_both_sexes_p[, ..p_change_2021]-total_region_both_sexes_p[, ..p_change_1990])/total_region_both_sexes_p[, ..p_change_1990]
      total_region_both_sexes_p[, (p_change_2021) := temp]
      
      # Percentage change of age-specific prevalence between 2021 and 1990 by age-sex-location
      current_by_age_2021 <- copy(current[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars]
      current_by_age_1990 <- copy(current[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars]
      
      current_by_age_p <- merge(current_by_age_2021, current_by_age_1990, by = c("location_id", "age_group_id", "sex_id"))
      
      temp <- (current_by_age_p[, ..p_change_2021]-current_by_age_p[, ..p_change_1990])/current_by_age_p[, ..p_change_1990]
      current_by_age_p[, (p_change_2021) := temp]
      
      # Percentage change of age-specific prevalence between 2021 and 1990 by age-location
      both_by_age_2021 <- copy(both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars]
      both_by_age_1990 <- copy(both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars]
      
      both_by_age_p <- merge(both_by_age_2021, both_by_age_1990, by = c("location_id", "age_group_id"))
      
      temp <- (both_by_age_p[, ..p_change_2021]-both_by_age_p[, ..p_change_1990])/both_by_age_p[, ..p_change_1990]
      both_by_age_p[, (p_change_2021) := temp]
      
      # Percentage change of age-specific prevalence between 2021 and 1990 by age-sex for the globe
      total_by_age_2021 <- copy(total[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars]
      total_by_age_1990 <- copy(total[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars]
      
      total_by_age_p <- merge(total_by_age_2021, total_by_age_1990, by = c("age_group_id", "sex_id"))
      
      temp <- (total_by_age_p[, ..p_change_2021]-total_by_age_p[, ..p_change_1990])/total_by_age_p[, ..p_change_1990]
      total_by_age_p[, (p_change_2021) := temp]
      
      # Percentage change of age-specific prevalence between 2021 and 1990 by age-sex for the super region
      total_sr_by_age_2021 <- copy(total_super_region[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars]
      total_sr_by_age_1990 <- copy(total_super_region[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars]
      
      total_sr_by_age_p <- merge(total_sr_by_age_2021, total_sr_by_age_1990, by = c("age_group_id", "sex_id", "super_region_id"))
      
      temp <- (total_sr_by_age_p[, ..p_change_2021]-total_sr_by_age_p[, ..p_change_1990])/total_sr_by_age_p[, ..p_change_1990]
      total_sr_by_age_p[, (p_change_2021) := temp]
      
      # Percentage change of age-specific prevalence between 2021 and 1990 by age-sex for the region
      total_rg_by_age_2021 <- copy(total_region[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars]
      total_rg_by_age_1990 <- copy(total_region[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars]
      
      total_rg_by_age_p <- merge(total_rg_by_age_2021, total_rg_by_age_1990, by = c("age_group_id", "sex_id", "region_id"))
      
      temp <- (total_rg_by_age_p[, ..p_change_2021]-total_rg_by_age_p[, ..p_change_1990])/total_rg_by_age_p[, ..p_change_1990]
      total_rg_by_age_p[, (p_change_2021) := temp]
      
      # Percentage change of age-specific prevalence between 2021 and 1990 by age only
      total_both_sexes_by_age_2021 <- copy(total_both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars]
      total_both_sexes_by_age_1990 <- copy(total_both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars]
      
      total_both_sexes_by_age_p <- merge(total_both_sexes_by_age_2021, total_both_sexes_by_age_1990, by = c("age_group_id", "sex_id"))
      
      temp <- (total_both_sexes_by_age_p[, ..p_change_2021]-total_both_sexes_by_age_p[, ..p_change_1990])/total_both_sexes_by_age_p[, ..p_change_1990]
      total_both_sexes_by_age_p[, (p_change_2021) := temp]
      
      # Percentage change of age-specific prevalence between 2021 and 1990 by age and by super region
      total_sr_both_sexes_by_age_2021 <- copy(total_super_region_both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars]
      total_sr_both_sexes_by_age_1990 <- copy(total_super_region_both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars]
      
      total_sr_both_sexes_by_age_p <- merge(total_sr_both_sexes_by_age_2021, total_sr_both_sexes_by_age_1990, by = c("age_group_id", "sex_id", "super_region_id"))
      
      temp <- (total_sr_both_sexes_by_age_p[, ..p_change_2021]-total_sr_both_sexes_by_age_p[, ..p_change_1990])/total_sr_both_sexes_by_age_p[, ..p_change_1990]
      total_sr_both_sexes_by_age_p[, (p_change_2021) := temp]
      
      # Percentage change of age-specific prevalence between 2021 and 1990 by age and by region
      total_rg_both_sexes_by_age_2021 <- copy(total_region_both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars]
      total_rg_both_sexes_by_age_1990 <- copy(total_region_both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars]
      
      total_rg_both_sexes_by_age_p <- merge(total_rg_both_sexes_by_age_2021, total_rg_both_sexes_by_age_1990, by = c("age_group_id", "sex_id", "region_id"))
      
      temp <- (total_rg_both_sexes_by_age_p[, ..p_change_2021]-total_rg_both_sexes_by_age_p[, ..p_change_1990])/total_rg_both_sexes_by_age_p[, ..p_change_1990]
      total_rg_both_sexes_by_age_p[, (p_change_2021) := temp]
      
      print("Summarizing percentage change!")
      
      # For percentage change
      current_p[, mean_AS_change := rowMeans(.SD), .SD = p_change_2021]
      current_p[, lower_AS_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      current_p[, upper_AS_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      both_sexes_p[, mean_AS_change := rowMeans(.SD), .SD = p_change_2021]
      both_sexes_p[, lower_AS_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      both_sexes_p[, upper_AS_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_p[, mean_AS_change := rowMeans(.SD), .SD = p_change_2021]
      total_p[, lower_AS_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_p[, upper_AS_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_super_region_p[, mean_AS_change := rowMeans(.SD), .SD = p_change_2021]
      total_super_region_p[, lower_AS_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_super_region_p[, upper_AS_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_region_p[, mean_AS_change := rowMeans(.SD), .SD = p_change_2021]
      total_region_p[, lower_AS_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_region_p[, upper_AS_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_both_sexes_p[, mean_AS_change := rowMeans(.SD), .SD = p_change_2021]
      total_both_sexes_p[, lower_AS_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_both_sexes_p[, upper_AS_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_super_region_both_sexes_p[, mean_AS_change := rowMeans(.SD), .SD = p_change_2021]
      total_super_region_both_sexes_p[, lower_AS_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_super_region_both_sexes_p[, upper_AS_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_region_both_sexes_p[, mean_AS_change := rowMeans(.SD), .SD = p_change_2021]
      total_region_both_sexes_p[, lower_AS_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_region_both_sexes_p[, upper_AS_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      current_by_age_p[, mean_change := rowMeans(.SD), .SD = p_change_2021]
      current_by_age_p[, lower_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      current_by_age_p[, upper_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      both_by_age_p[, mean_change := rowMeans(.SD), .SD = p_change_2021]
      both_by_age_p[, lower_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      both_by_age_p[, upper_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_by_age_p[, mean_change := rowMeans(.SD), .SD = p_change_2021]
      total_by_age_p[, lower_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_by_age_p[, upper_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_sr_by_age_p[, mean_change := rowMeans(.SD), .SD = p_change_2021]
      total_sr_by_age_p[, lower_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_sr_by_age_p[, upper_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_rg_by_age_p[, mean_change := rowMeans(.SD), .SD = p_change_2021]
      total_rg_by_age_p[, lower_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_rg_by_age_p[, upper_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_both_sexes_by_age_p[, mean_change := rowMeans(.SD), .SD = p_change_2021]
      total_both_sexes_by_age_p[, lower_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_both_sexes_by_age_p[, upper_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_sr_both_sexes_by_age_p[, mean_change := rowMeans(.SD), .SD = p_change_2021]
      total_sr_both_sexes_by_age_p[, lower_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_sr_both_sexes_by_age_p[, upper_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_rg_both_sexes_by_age_p[, mean_change := rowMeans(.SD), .SD = p_change_2021]
      total_rg_both_sexes_by_age_p[, lower_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_rg_both_sexes_by_age_p[, upper_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      #--------absolute change in number of ow/ob population---------
      print("Onto absolute change in number of ow/ob population")
      
      # absolute change between 2021 and 1990 by sex-location
      current_2021 <- copy(current[year_id == 2021]) %>% .[, (p_num_chg_2021) := .SD, .SDcols = count_cols]
      current_1990 <- copy(current[year_id == 1990]) %>% .[, (p_num_chg_1990) := .SD, .SDcols = count_cols]
      
      current_count <- merge(current_2021, current_1990, by = c("location_id", "age_group_id", "sex_id"))
      
      temp <- (current_count[, ..p_num_chg_2021]-current_count[, ..p_num_chg_1990])
      current_count[, (p_num_chg_2021) := temp]
      
      # Percent change between 2021 and 1990 by location
      both_sexes_2021 <- copy(both_sexes[year_id == 2021]) %>% .[, (p_num_chg_2021) := .SD, .SDcols = count_cols]
      both_sexes_1990 <- copy(both_sexes[year_id == 1990]) %>% .[, (p_num_chg_1990) := .SD, .SDcols = count_cols]
      
      both_sexes_count <- merge(both_sexes_2021, both_sexes_1990, by = c("location_id", "age_group_id"))
      
      temp <- (both_sexes_count[, ..p_num_chg_2021]-both_sexes_count[, ..p_num_chg_1990])
      both_sexes_count[, (p_num_chg_2021) := temp]
      
      # Percent change between 2021 and 1990 by sex
      total_2021 <- copy(total[year_id == 2021]) %>% .[, (p_num_chg_2021) := .SD, .SDcols = count_cols]
      total_1990 <- copy(total[year_id == 1990]) %>% .[, (p_num_chg_1990) := .SD, .SDcols = count_cols]
      
      total_count <- merge(total_2021, total_1990, by = c("age_group_id", "sex_id"))
      
      temp <- (total_count[, ..p_num_chg_2021]-total_count[, ..p_num_chg_1990])
      total_count[, (p_num_chg_2021) := temp]
      
      # Percent change between 2021 and 1990 by sex and super region
      total_sr_2021 <- copy(total_super_region[year_id == 2021]) %>% .[, (p_num_chg_2021) := .SD, .SDcols = count_cols]
      total_sr_1990 <- copy(total_super_region[year_id == 1990]) %>% .[, (p_num_chg_1990) := .SD, .SDcols = count_cols]
      
      total_sr_count <- merge(total_sr_2021, total_sr_1990, by = c("age_group_id", "sex_id", "super_region_id"))
      
      temp <- (total_sr_count[, ..p_num_chg_2021]-total_sr_count[, ..p_num_chg_1990])
      total_sr_count[, (p_num_chg_2021) := temp]
      
      # Percent change between 2021 and 1990 by sex and region
      total_rg_2021 <- copy(total_region[year_id == 2021]) %>% .[, (p_num_chg_2021) := .SD, .SDcols = count_cols]
      total_rg_1990 <- copy(total_region[year_id == 1990]) %>% .[, (p_num_chg_1990) := .SD, .SDcols = count_cols]
      
      total_rg_count <- merge(total_rg_2021, total_rg_1990, by = c("age_group_id", "sex_id", "region_id"))
      
      temp <- (total_rg_count[, ..p_num_chg_2021]-total_rg_count[, ..p_num_chg_1990])
      total_rg_count[, (p_num_chg_2021) := temp]
      
      # Percent change between 2021 and 1990 for both sexes
      total_both_sexes_2021 <- copy(total_both_sexes[year_id == 2021]) %>% .[, (p_num_chg_2021) := .SD, .SDcols = count_cols]
      total_both_sexes_1990 <- copy(total_both_sexes[year_id == 1990]) %>% .[, (p_num_chg_1990) := .SD, .SDcols = count_cols]
      
      total_both_sexes_count <- merge(total_both_sexes_2021, total_both_sexes_1990, by = c("age_group_id", "sex_id"))
      
      temp <- (total_both_sexes_count[, ..p_num_chg_2021]-total_both_sexes_count[, ..p_num_chg_1990])
      total_both_sexes_count[, (p_num_chg_2021) := temp]
      
      # Percent change between 2021 and 1990 for both sexes by super region
      total_sr_both_sexes_2021 <- copy(total_super_region_both_sexes[year_id == 2021]) %>% .[, (p_num_chg_2021) := .SD, .SDcols = count_cols]
      total_sr_both_sexes_1990 <- copy(total_super_region_both_sexes[year_id == 1990]) %>% .[, (p_num_chg_1990) := .SD, .SDcols = count_cols]
      
      total_sr_both_sexes_count <- merge(total_sr_both_sexes_2021, total_sr_both_sexes_1990, by = c("age_group_id", "sex_id", "super_region_id"))
      
      temp <- (total_sr_both_sexes_count[, ..p_num_chg_2021]-total_sr_both_sexes_count[, ..p_num_chg_1990])
      total_sr_both_sexes_count[, (p_num_chg_2021) := temp]
      
      # Percent change between 2021 and 1990 for both sexes by region
      total_rg_both_sexes_2021 <- copy(total_region_both_sexes[year_id == 2021]) %>% .[, (p_num_chg_2021) := .SD, .SDcols = count_cols]
      total_rg_both_sexes_1990 <- copy(total_region_both_sexes[year_id == 1990]) %>% .[, (p_num_chg_1990) := .SD, .SDcols = count_cols]
      
      total_rg_both_sexes_count <- merge(total_rg_both_sexes_2021, total_rg_both_sexes_1990, by = c("age_group_id", "sex_id", "region_id"))
      
      temp <- (total_rg_both_sexes_count[, ..p_num_chg_2021]-total_rg_both_sexes_count[, ..p_num_chg_1990])
      total_rg_both_sexes_count[, (p_num_chg_2021) := temp]
      
      print("Summarizing number change by population!")
      
      # For percentage change
      current_count[, mean_abs_count_change := rowMeans(.SD), .SD = p_num_chg_2021]
      current_count[, lower_abs_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_num_chg_2021]
      current_count[, upper_abs_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_num_chg_2021]
      
      both_sexes_count[, mean_abs_count_change := rowMeans(.SD), .SD = p_num_chg_2021]
      both_sexes_count[, lower_abs_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_num_chg_2021]
      both_sexes_count[, upper_abs_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_num_chg_2021]
      
      total_count[, mean_abs_count_change := rowMeans(.SD), .SD = p_num_chg_2021]
      total_count[, lower_abs_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_num_chg_2021]
      total_count[, upper_abs_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_num_chg_2021]
      
      total_sr_count[, mean_abs_count_change := rowMeans(.SD), .SD = p_num_chg_2021]
      total_sr_count[, lower_abs_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_num_chg_2021]
      total_sr_count[, upper_abs_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_num_chg_2021]
      
      total_rg_count[, mean_abs_count_change := rowMeans(.SD), .SD = p_num_chg_2021]
      total_rg_count[, lower_abs_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_num_chg_2021]
      total_rg_count[, upper_abs_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_num_chg_2021]
      
      total_both_sexes_count[, mean_abs_count_change := rowMeans(.SD), .SD = p_num_chg_2021]
      total_both_sexes_count[, lower_abs_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_num_chg_2021]
      total_both_sexes_count[, upper_abs_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_num_chg_2021]
      
      total_sr_both_sexes_count[, mean_abs_count_change := rowMeans(.SD), .SD = p_num_chg_2021]
      total_sr_both_sexes_count[, lower_abs_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_num_chg_2021]
      total_sr_both_sexes_count[, upper_abs_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_num_chg_2021]
      
      total_rg_both_sexes_count[, mean_abs_count_change := rowMeans(.SD), .SD = p_num_chg_2021]
      total_rg_both_sexes_count[, lower_abs_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_num_chg_2021]
      total_rg_both_sexes_count[, upper_abs_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_num_chg_2021]

      #------- percentage change in number of smokers--------
      print("Onto percentage change in number of ow/ob population")
      
      # Percent change between 2021 and 1990 by sex-location
      current_2021 <- copy(current[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = count_cols]
      current_1990 <- copy(current[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = count_cols]
      
      current_p_count <- merge(current_2021, current_1990, by = c("location_id", "age_group_id", "sex_id"))
      
      temp <- (current_p_count[, ..p_change_2021]-current_p_count[, ..p_change_1990])/current_p_count[, ..p_change_1990]
      current_p_count[, (p_change_2021) := temp]
      
      # Percent change between 2021 and 1990 by location
      both_sexes_2021 <- copy(both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = count_cols]
      both_sexes_1990 <- copy(both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = count_cols]
      
      both_sexes_p_count <- merge(both_sexes_2021, both_sexes_1990, by = c("location_id", "age_group_id"))
      
      temp <- (both_sexes_p_count[, ..p_change_2021]-both_sexes_p_count[, ..p_change_1990])/both_sexes_p_count[, ..p_change_1990]
      both_sexes_p_count[, (p_change_2021) := temp]
      
      # Percent change between 2021 and 1990 by sex
      total_2021 <- copy(total[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = count_cols]
      total_1990 <- copy(total[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = count_cols]
      
      total_p_count <- merge(total_2021, total_1990, by = c("age_group_id", "sex_id"))
      
      temp <- (total_p_count[, ..p_change_2021]-total_p_count[, ..p_change_1990])/total_p_count[, ..p_change_1990]
      total_p_count[, (p_change_2021) := temp]
      
      # Percent change between 2021 and 1990 by sex and super region
      total_sr_2021 <- copy(total_super_region[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = count_cols]
      total_sr_1990 <- copy(total_super_region[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = count_cols]
      
      total_sr_p_count <- merge(total_sr_2021, total_sr_1990, by = c("age_group_id", "sex_id", "super_region_id"))
      
      temp <- (total_sr_p_count[, ..p_change_2021]-total_sr_p_count[, ..p_change_1990])/total_sr_p_count[, ..p_change_1990]
      total_sr_p_count[, (p_change_2021) := temp]
      
      # Percent change between 2021 and 1990 by sex and region
      total_rg_2021 <- copy(total_region[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = count_cols]
      total_rg_1990 <- copy(total_region[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = count_cols]
      
      total_rg_p_count <- merge(total_rg_2021, total_rg_1990, by = c("age_group_id", "sex_id", "region_id"))
      
      temp <- (total_rg_p_count[, ..p_change_2021]-total_rg_p_count[, ..p_change_1990])/total_rg_p_count[, ..p_change_1990]
      total_rg_p_count[, (p_change_2021) := temp]
      
      # Percent change between 2021 and 1990 for both sexes
      total_both_sexes_2021 <- copy(total_both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = count_cols]
      total_both_sexes_1990 <- copy(total_both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = count_cols]
      
      total_both_sexes_p_count <- merge(total_both_sexes_2021, total_both_sexes_1990, by = c("age_group_id", "sex_id"))
      
      temp <- (total_both_sexes_p_count[, ..p_change_2021]-total_both_sexes_p_count[, ..p_change_1990])/total_both_sexes_p_count[, ..p_change_1990]
      total_both_sexes_p_count[, (p_change_2021) := temp]
      
      # Percent change between 2021 and 1990 for both sexes by super region
      total_sr_both_sexes_2021 <- copy(total_super_region_both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = count_cols]
      total_sr_both_sexes_1990 <- copy(total_super_region_both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = count_cols]
      
      total_sr_both_sexes_p_count <- merge(total_sr_both_sexes_2021, total_sr_both_sexes_1990, by = c("age_group_id", "sex_id", "super_region_id"))
      
      temp <- (total_sr_both_sexes_p_count[, ..p_change_2021]-total_sr_both_sexes_p_count[, ..p_change_1990])/total_sr_both_sexes_p_count[, ..p_change_1990]
      total_sr_both_sexes_p_count[, (p_change_2021) := temp]
      
      # Percent change between 2021 and 1990 for both sexes by region
      total_rg_both_sexes_2021 <- copy(total_region_both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = count_cols]
      total_rg_both_sexes_1990 <- copy(total_region_both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = count_cols]
      
      total_rg_both_sexes_p_count <- merge(total_rg_both_sexes_2021, total_rg_both_sexes_1990, by = c("age_group_id", "sex_id", "region_id"))
      
      temp <- (total_rg_both_sexes_p_count[, ..p_change_2021]-total_rg_both_sexes_p_count[, ..p_change_1990])/total_rg_both_sexes_p_count[, ..p_change_1990]
      total_rg_both_sexes_p_count[, (p_change_2021) := temp]
      
      print("Summarizing percentage change by population!")
      
      # For percentage change
      current_p_count[, mean_count_change := rowMeans(.SD), .SD = p_change_2021]
      current_p_count[, lower_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      current_p_count[, upper_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      both_sexes_p_count[, mean_count_change := rowMeans(.SD), .SD = p_change_2021]
      both_sexes_p_count[, lower_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      both_sexes_p_count[, upper_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_p_count[, mean_count_change := rowMeans(.SD), .SD = p_change_2021]
      total_p_count[, lower_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_p_count[, upper_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_sr_p_count[, mean_count_change := rowMeans(.SD), .SD = p_change_2021]
      total_sr_p_count[, lower_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_sr_p_count[, upper_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_rg_p_count[, mean_count_change := rowMeans(.SD), .SD = p_change_2021]
      total_rg_p_count[, lower_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_rg_p_count[, upper_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_both_sexes_p_count[, mean_count_change := rowMeans(.SD), .SD = p_change_2021]
      total_both_sexes_p_count[, lower_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_both_sexes_p_count[, upper_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_sr_both_sexes_p_count[, mean_count_change := rowMeans(.SD), .SD = p_change_2021]
      total_sr_both_sexes_p_count[, lower_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_sr_both_sexes_p_count[, upper_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      total_rg_both_sexes_p_count[, mean_count_change := rowMeans(.SD), .SD = p_change_2021]
      total_rg_both_sexes_p_count[, lower_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      total_rg_both_sexes_p_count[, upper_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      #-------final stretch----------
      print("In the final stretch!")
      
      # Get summary values for AS and AA prevalence
      both_summary <- unique(both_sexes[, .(year_id, location_id, 
                                            mean_count, lower_count, upper_count, 
                                            mean_AS_prev, lower_AS_prev, upper_AS_prev, 
                                            mean_AA_prev, lower_AA_prev, upper_AA_prev)])
      
      summary <- unique(current[, .(year_id, location_id, sex_id,
                                    mean_count, lower_count, upper_count, 
                                    mean_AS_prev, lower_AS_prev, upper_AS_prev, 
                                    mean_AA_prev, lower_AA_prev, upper_AA_prev)])
      
      total_summary <- unique(total[, .(year_id, sex_id,
                                        mean_count, lower_count, upper_count, 
                                        mean_AS_prev, lower_AS_prev, upper_AS_prev, 
                                        mean_AA_prev, lower_AA_prev, upper_AA_prev)])
      
      total_summary_sr <- unique(total_super_region[, .(year_id, super_region_id, sex_id,
                                        mean_count, lower_count, upper_count, 
                                        mean_AS_prev, lower_AS_prev, upper_AS_prev, 
                                        mean_AA_prev, lower_AA_prev, upper_AA_prev)])
      # merge location_id for super region
      total_summary_sr <- merge(total_summary_sr, locs[level==1,.(location_id, super_region_id)], by="super_region_id", all.x = T)
      
      total_summary_rg <- unique(total_region[, .(year_id, region_id, sex_id,
                                                  mean_count, lower_count, upper_count, 
                                                  mean_AS_prev, lower_AS_prev, upper_AS_prev, 
                                                  mean_AA_prev, lower_AA_prev, upper_AA_prev)])
      # merge location_id for region
      total_summary_rg <- merge(total_summary_rg, locs[level==2,.(location_id, region_id)], by="region_id", all.x = T)
      
      total_both_sexes_summary <- unique(total_both_sexes[, .(year_id, sex_id,
                                                              mean_count, lower_count, upper_count, 
                                                              mean_AS_prev, lower_AS_prev, upper_AS_prev, 
                                                              mean_AA_prev, lower_AA_prev, upper_AA_prev)])
      
      total_both_sexes_summary_sr <- unique(total_super_region_both_sexes[, .(year_id, super_region_id, sex_id,
                                                                             mean_count, lower_count, upper_count, 
                                                                             mean_AS_prev, lower_AS_prev, upper_AS_prev, 
                                                                             mean_AA_prev, lower_AA_prev, upper_AA_prev)])
      # merge location_id for super region (super_region_id and location_id are the same)
      total_both_sexes_summary_sr[, location_id := super_region_id]
      
      total_both_sexes_summary_rg <- unique(total_region_both_sexes[, .(year_id, region_id, sex_id,
                                                                        mean_count, lower_count, upper_count, 
                                                                        mean_AS_prev, lower_AS_prev, upper_AS_prev, 
                                                                        mean_AA_prev, lower_AA_prev, upper_AA_prev)])
      # merge location_id for region (region_id and location_id are the same)
      total_both_sexes_summary_rg[, location_id := region_id]

      # Get summary values for age-specific prevalence
      both_by_age <- unique(both_sexes[, .(year_id, location_id, age_group_id, 
                                           mean_prev, lower_prev, upper_prev)])
      
      by_age <- unique(current[, .(year_id, location_id, age_group_id, sex_id,
                                   mean_prev, lower_prev, upper_prev)])
      
      total_by_age <- unique(total[, .(year_id, age_group_id, sex_id,
                                       mean_prev, lower_prev, upper_prev)])
      
      total_by_age_sr <- unique(total_super_region[, .(year_id, age_group_id, sex_id, super_region_id,
                                                       mean_prev, lower_prev, upper_prev)])
      total_by_age_sr[, location_id := super_region_id]
      
      total_by_age_rg <- unique(total_region[, .(year_id, age_group_id, sex_id, region_id,
                                                 mean_prev, lower_prev, upper_prev)])
      total_by_age_rg[, location_id := region_id]
      
      total_both_sexes_by_age <- unique(total_both_sexes[, .(year_id, age_group_id, sex_id,
                                                             mean_prev, lower_prev, upper_prev)])
      
      total_both_sexes_by_age_sr <- unique(total_super_region_both_sexes[, .(year_id, age_group_id, sex_id, super_region_id,
                                                                             mean_prev, lower_prev, upper_prev)])
      total_both_sexes_by_age_sr[, location_id := super_region_id]
      
      total_both_sexes_by_age_rg <- unique(total_region_both_sexes[, .(year_id, age_group_id, sex_id, region_id,
                                                                       mean_prev, lower_prev, upper_prev)])
      total_both_sexes_by_age_rg[, location_id := region_id]
      
      # Get summary values of pct change of AS prevalence
      # for both sex
      p_both_sexes_total_summary <- unique(total_both_sexes_p[, .(sex_id, mean_AS_change, lower_AS_change, upper_AS_change)])
      p_both_sexes_total_summary_sr <- unique(total_super_region_both_sexes_p[,.(sex_id, super_region_id, mean_AS_change, lower_AS_change, upper_AS_change)])
      p_both_sexes_total_summary_sr[, location_id := super_region_id]
      p_both_sexes_total_summary_rg <- unique(total_region_both_sexes_p[,.(sex_id, region_id, mean_AS_change, lower_AS_change, upper_AS_change)])
      p_both_sexes_total_summary_rg[, location_id := region_id]
      
      # by sex
      p_total_summary <- unique(total_p[, .(sex_id, mean_AS_change, lower_AS_change, upper_AS_change)])
      p_total_summary_sr <- unique(total_super_region_p[, .(sex_id, super_region_id, mean_AS_change, lower_AS_change, upper_AS_change)])
      p_total_summary_sr[, location_id := super_region_id]
      p_total_summary_rg <- unique(total_region_p[, .(sex_id, region_id, mean_AS_change, lower_AS_change, upper_AS_change)])
      p_total_summary_rg[, location_id := region_id]
      
      # by sex and location
      p_summary <- unique(current_p[, .(sex_id, location_id, mean_AS_change, lower_AS_change, upper_AS_change)])
      p_both_summary <- unique(both_sexes_p[, .(location_id, mean_AS_change, lower_AS_change, upper_AS_change)])
      
      # Get summary values of pct change of age-specific prevalence
      # for both sex
      p_both_sexes_total_by_age <- unique(total_both_sexes_by_age_p[, .(sex_id, age_group_id, mean_change, lower_change, upper_change)])
      p_both_sexes_total_by_age_sr <- unique(total_sr_both_sexes_by_age_p[, .(sex_id, super_region_id, age_group_id, mean_change, lower_change, upper_change)])
      p_both_sexes_total_by_age_sr[, location_id := super_region_id]
      p_both_sexes_total_by_age_rg <- unique(total_rg_both_sexes_by_age_p[, .(sex_id, region_id, age_group_id, mean_change, lower_change, upper_change)])
      p_both_sexes_total_by_age_rg[, location_id := region_id]
      
      # by sex (and by sr and rg)
      p_total_by_age <- unique(total_by_age_p[, .(sex_id, age_group_id, mean_change, lower_change, upper_change)])
      p_total_by_age_sr <- unique(total_sr_by_age_p[, .(sex_id, super_region_id, age_group_id, mean_change, lower_change, upper_change)])
      p_total_by_age_sr[, location_id := super_region_id]
      p_total_by_age_rg <- unique(total_rg_by_age_p[, .(sex_id, region_id, age_group_id, mean_change, lower_change, upper_change)])
      p_total_by_age_rg[, location_id := region_id]
      
      # by sex and by location
      p_by_age <- unique(current_by_age_p[, .(sex_id, age_group_id, location_id, mean_change, lower_change, upper_change)])
      
      # for both sexes by location
      p_both_by_age <- unique(both_by_age_p[, .(location_id, age_group_id, mean_change, lower_change, upper_change)])
      
      # Get summary values of total count abs change
      # for both sex for global, super region and region
      both_sexes_total_count <- unique(total_both_sexes_count[, .(sex_id, mean_abs_count_change, lower_abs_count_change, upper_abs_count_change)])
      both_sexes_total_count_sr <- unique(total_sr_both_sexes_count[, .(sex_id, super_region_id, mean_abs_count_change, lower_abs_count_change, upper_abs_count_change)])
      both_sexes_total_count_sr[, location_id := super_region_id]
      both_sexes_total_count_rg <- unique(total_rg_both_sexes_count[, .(sex_id, region_id, mean_abs_count_change, lower_abs_count_change, upper_abs_count_change)])
      both_sexes_total_count_rg[, location_id := region_id]
      
      # by sex for global, super region and region
      total_count <- unique(total_count[, .(sex_id, mean_abs_count_change, lower_abs_count_change, upper_abs_count_change)])
      total_count_sr <- unique(total_sr_count[, .(sex_id, super_region_id, mean_abs_count_change, lower_abs_count_change, upper_abs_count_change)])
      total_count_sr[, location_id := super_region_id]
      total_count_rg <- unique(total_rg_count[, .(sex_id, region_id, mean_abs_count_change, lower_abs_count_change, upper_abs_count_change)])
      total_count_rg[, location_id := region_id]
      
      # by location by sex and for both sexes
      count <- unique(current_count[, .(sex_id, location_id, mean_abs_count_change, lower_abs_count_change, upper_abs_count_change)])
      both_count <- unique(both_sexes_count[, .(location_id, mean_abs_count_change, lower_abs_count_change, upper_abs_count_change)])
      
      # Get summary values of total count percent change
      # for both sexes for global, super region and region
      p_both_sexes_total_count <- unique(total_both_sexes_p_count[, .(sex_id, mean_count_change, lower_count_change, upper_count_change)])
      p_both_sexes_total_count_sr <- unique(total_sr_both_sexes_p_count[, .(sex_id, super_region_id, mean_count_change, lower_count_change, upper_count_change)])
      p_both_sexes_total_count_sr[, location_id := super_region_id]
      p_both_sexes_total_count_rg <- unique(total_rg_both_sexes_p_count[, .(sex_id, region_id, mean_count_change, lower_count_change, upper_count_change)])
      p_both_sexes_total_count_rg[, location_id := region_id]
      
      # by sex for global super region and region
      p_total_count <- unique(total_p_count[, .(sex_id, mean_count_change, lower_count_change, upper_count_change)])
      p_total_count_sr <- unique(total_sr_p_count[, .(sex_id, super_region_id, mean_count_change, lower_count_change, upper_count_change)])
      p_total_count_sr[, location_id := super_region_id]
      p_total_count_rg <- unique(total_rg_p_count[, .(sex_id, region_id, mean_count_change, lower_count_change, upper_count_change)])
      p_total_count_rg[, location_id := region_id]
      
      # by location
      p_count <- unique(current_p_count[, .(sex_id, location_id, mean_count_change, lower_count_change, upper_count_change)])
      p_both_count <- unique(both_sexes_p_count[, .(location_id, mean_count_change, lower_count_change, upper_count_change)])
      
      # Combine all of the datasets
      both_summary[, sex_id := 3]
      both_by_age[, sex_id := 3]
      p_both_summary[, sex_id := 3]
      p_both_by_age[, sex_id := 3]
      both_count[, sex_id := 3]
      p_both_count[, sex_id := 3]
      
      total_both_sexes_summary[, sex_id := 3]; total_both_sexes_summary_sr[, sex_id := 3]; total_both_sexes_summary_rg[, sex_id := 3]
      total_both_sexes_by_age[, sex_id := 3]; total_both_sexes_by_age_sr[, sex_id := 3]; total_both_sexes_by_age_rg[, sex_id := 3]
      
      p_both_sexes_total_summary[, sex_id := 3]; p_both_sexes_total_summary_sr[, sex_id := 3]; p_both_sexes_total_summary_rg[, sex_id := 3]
      p_both_sexes_total_by_age[, sex_id := 3]; p_both_sexes_total_by_age_sr[, sex_id := 3]; p_both_sexes_total_by_age_rg[, sex_id := 3]
      both_sexes_total_count[, sex_id := 3]; both_sexes_total_count_sr[, sex_id := 3]; both_sexes_total_count_rg[, sex_id := 3]
      p_both_sexes_total_count[, sex_id := 3]; p_both_sexes_total_count_sr[, sex_id := 3]; p_both_sexes_total_count_rg[, sex_id := 3]
      
      # global location_id == 1
      total_summary[, location_id := 1]
      total_by_age[, location_id := 1]
      p_total_summary[, location_id := 1]
      p_total_by_age[, location_id := 1]
      total_count[, location_id := 1]
      p_total_count[, location_id := 1]
      
      total_both_sexes_summary[, location_id := 1]
      total_both_sexes_by_age[, location_id := 1]
      p_both_sexes_total_summary[, location_id := 1]
      p_both_sexes_total_by_age[, location_id := 1]
      both_sexes_total_count[, location_id := 1]
      p_both_sexes_total_count[, location_id := 1]
      
      summary <- rbindlist(list(both_summary, summary, 
                                total_summary, total_summary_sr, total_summary_rg, 
                                total_both_sexes_summary, total_both_sexes_summary_sr, total_both_sexes_summary_rg), use.names = T, fill = T)
      
      by_age <- rbindlist(list(both_by_age, by_age, 
                               total_by_age, total_by_age_sr, total_by_age_rg, 
                               total_both_sexes_by_age, total_both_sexes_by_age_sr, total_both_sexes_by_age_rg), use.names = T, fill = T)
      
      p_summary <- rbindlist(list(p_both_sexes_total_summary, p_both_sexes_total_summary_sr, p_both_sexes_total_summary_rg, 
                                  p_total_summary, p_total_summary_sr, p_total_summary_rg, 
                                  p_summary, p_both_summary), use.names = T, fill = T)
      
      p_by_age <- rbindlist(list(p_both_sexes_total_by_age, p_both_sexes_total_by_age_sr, p_both_sexes_total_by_age_rg, 
                                 p_total_by_age, p_total_by_age_sr, p_total_by_age_rg,
                                 p_by_age, p_both_by_age), use.names = T, fill = T)
      
      count <- rbindlist(list(both_sexes_total_count, both_sexes_total_count_sr, both_sexes_total_count_rg, 
                              total_count, total_count_sr, total_count_rg, 
                              count, both_count), use.names = T, fill = T)
      
      p_count <- rbindlist(list(p_both_sexes_total_count, p_both_sexes_total_count_sr, p_both_sexes_total_count_rg,
                                p_total_count, p_total_count_sr, p_total_count_rg,
                                p_count, p_both_count), use.names = T, fill = T)
      
      # merge the results
      summary <- merge(summary[,-c("super_region_id", "region_id")], p_summary[,-c("super_region_id", "region_id")], by = c("sex_id", "location_id"), all = T)
      summary <- merge(summary, count[,-c("super_region_id", "region_id")], by = c("sex_id", "location_id"), all = T)
      summary <- merge(summary, p_count[,-c("super_region_id", "region_id")], by = c("sex_id", "location_id"), all = T)
      by_age <- merge(by_age[,-c("super_region_id", "region_id")], p_by_age[,-c("super_region_id", "region_id")], by = c("sex_id", "age_group_id", "location_id"), all = T)
      
      # Merge with locations and names
      summary <- merge(summary, locs[,.(location_id, location_name, super_region_id, super_region_name, region_id, region_name, level)], by = "location_id", all.x = T)
      by_age <- merge(by_age, locs[,.(location_id, location_name, super_region_id, super_region_name, region_id, region_name, level)], by = "location_id", all.x = T)
      by_age <- merge(by_age, ages[, .(age_group_id, age_group_name)], by = "age_group_id", all.x = T)
      
      # Clean up columns
      summary[, Sex := ifelse(sex_id == 1, "Male", 
                              ifelse(sex_id == 2, "Female", "Both"))]
      by_age[, Sex := ifelse(sex_id == 1, "Male", 
                             ifelse(sex_id == 2, "Female", "Both"))]
      
      
      summary[, location_level := ifelse(level == 0, "Global", 
                              ifelse(level == 1, "Super Region", 
                                     ifelse(level == 2, "Region", "Country")))]
      by_age[, location_level := ifelse(level == 0, "Global", 
                                        ifelse(level == 1, "Super Region", 
                                               ifelse(level == 2, "Region", "Country")))]
      
      # save the results
      print("save the results!")
      fwrite(summary, paste0(results_fp,"gbd_", age_name, "_", m, "_summary.csv"), row.names = F)
      fwrite(by_age, paste0(results_fp, "gbd_", age_name, "_", m, "_by_age.csv"), row.names = F)
    }
  }
  print("prevalence results done!")
}
