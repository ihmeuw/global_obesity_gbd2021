#------------------------------------------------------------
# Purpose: compile GBD2021 global forecast results for high BMI
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
save_data <- T
prevalence <- T
results_fp <- paste0("FILEPATH")

# results directories
fhs_dir <- "FILEPATH"

# use release_id 9 for all the machinary results
locs <- get_location_metadata(release_id = 9, location_set_id = 35)
ages <- get_age_metadata(release_id = 9)
locs_of_interest <- locs[level <= 3, .(location_id, ihme_loc_id, location_name, level)]


# pull the results and format them
for(age_grp in c("adolescent","adult")){
  for(model in c("obesity","overweight","overweight_less_obesity")){
    print(paste0(age_grp, ", ", model))
    
    # pull the results
    prev_draws <- lapply(paste0(fhs_dir, model,"_",age_grp,"_", locs_of_interest$location_id, ".csv"), fread) %>%
      rbindlist(use.names = TRUE) %>% .[,V1:=NULL]
    
    # pull the new age groups results for adolescents
    if(age_grp=="adolescent"){
     
      # change age_group_id for aggregate groups
      prev_draws[age_group_name=="All Ages (15 to 24)", age_group_id:=221524]
      prev_draws[age_group_name=="Age-standardized (15 to 24)", age_group_id:=271525]
      prev_draws[age_group_name=="All Ages (5 to 14)", age_group_id:=220514]
      prev_draws[age_group_name=="Age-standardized (5 to 14)", age_group_id:=270514]
      prev_draws[age_group_name=="All Ages (5 to 24)", age_group_id:=220524]
      prev_draws[age_group_name=="Age-standardized (5 to 24)", age_group_id:=270524]
      
    } else {
      prev_draws[age_group_id==157, age_group_name := "All Ages (25 plus)"]
    }
    
    # get data from 2021 and 2050 and calculate the change metrics
    prev_2021 <- prev_draws[year_id==2021,]
    prev_2050 <- prev_draws[year_id==2050,]
    
    setnames(prev_2021, "value", "prev_2021"); setnames(prev_2050, "value", "prev_2050")
    
    # merge
    prev_change <- merge(prev_2021, prev_2050, 
                         by=c("measure", "draw", "age_group_id", "age_group_name", "sex_id", "location_id"))
    
    # calculate pct change
    prev_change[, pct_change := (prev_2050 - prev_2021) / prev_2021]
    
    # calculate absolute change
    prev_change[, abs_change := prev_2050 - prev_2021]
    
    # calculate mean, upper and lower values for the change metrics
    vars <- c("pct_change", "abs_change")
    prev_change[, (paste0(vars,"_mean")):= lapply(.SD, mean), .SDcols = vars, by = c("measure","location_id", "sex_id", "age_group_id", "age_group_name")]
    prev_change[, (paste0(vars,"_upper")):= lapply(.SD, quantile, probs = 0.975, na.rm=T), .SDcols = vars, by = c("measure","location_id", "sex_id", "age_group_id", "age_group_name")]
    prev_change[, (paste0(vars,"_lower")):= lapply(.SD, quantile, probs = 0.025, na.rm=T), .SDcols = vars, by = c("measure","location_id", "sex_id", "age_group_id", "age_group_name")]
    
    # keep unique rows for change metrics
    prev_change <- prev_change[,.(measure, location_id, sex_id, age_group_id, age_group_name, 
                   pct_change_mean, pct_change_upper, pct_change_lower,
                   abs_change_mean, abs_change_upper, abs_change_lower)] %>% unique
    
    # collapse the count and prev draws for age-specific results
    prev_draws[, value_mean:= lapply(.SD, mean), .SDcols = "value", by = c("measure", "year_id", "location_id", "sex_id", "age_group_id", "age_group_name")]
    prev_draws[, value_lower:= lapply(.SD, quantile, probs = 0.025, na.rm=T), .SDcols = "value", by = c("measure", "year_id", "location_id", "sex_id", "age_group_id", "age_group_name")]
    prev_draws[, value_upper:= lapply(.SD, quantile, probs = 0.975, na.rm=T), .SDcols = "value", by = c("measure","year_id", "location_id", "sex_id", "age_group_id", "age_group_name")]
    
    
    # keep unique rows for age-specific results
    prev_draws <- prev_draws[,.(measure, year_id, location_id, sex_id, age_group_id, age_group_name, 
                   value_mean, value_lower, value_upper)] %>% unique
    
    # merge prev and change metrics
    prev_summary <- merge(prev_draws, prev_change, by=c("measure", "location_id", "sex_id","age_group_id", "age_group_name"))
    
    # add sex and location names column
    prev_summary[, Sex := ifelse(sex_id == 1, "Male", 
                                  ifelse(sex_id == 2, "Female", "Both"))]
    prev_summary <- merge(prev_summary, locs_of_interest, by="location_id", all.x = T)
    prev_summary[, location_level := ifelse(level == 3, "Country", 
                                            ifelse(level==2, "Region",
                                                   ifelse(level==1, "Super Region", "Global")))]
    
    # separate the results by all age and age-specific results. 
    prev_summary[, .(age_group_id, age_group_name)] %>% unique
    prev_age_spec <- prev_summary[!grepl("All Ages|Age-standardized", age_group_name),]
    
    # change varnames of the age-specific results
    setnames(prev_age_spec, c("value_mean", "value_lower", "value_upper"), c("mean_prev", "lower_prev", "upper_prev"))
    setnames(prev_age_spec, c("pct_change_mean", "pct_change_lower", "pct_change_upper"), c("mean_change", "lower_change", "upper_change"))
    setnames(prev_age_spec, c("abs_change_mean", "abs_change_lower", "abs_change_upper"), c("mean_pct_point_change", "lower_pct_point_change", "upper_pct_point_change"))
    
    # all age and age-standardized results
    prev_all_ages <- prev_summary[grepl("All Ages|Age-standardized", age_group_name)]
    valvars <- c("value_mean", "value_lower", "value_upper", "pct_change_mean", "pct_change_lower", "pct_change_upper", "abs_change_mean", "abs_change_lower", "abs_change_upper")
    prev_all_ages <- dcast.data.table(prev_all_ages, location_id + ihme_loc_id + location_name + level + location_level + 
                                        year_id + sex_id + Sex + age_group_id + age_group_name ~ measure, value.var = valvars)
    
    prev_all_ages_summary <- prev_all_ages[grepl("All Ages", age_group_name)]
    prev_age_standard_summary <- prev_all_ages[grepl("Age-standardized", age_group_name)]

    # change var names
    setnames(prev_all_ages_summary, c("value_mean_prevalence", "value_lower_prevalence", "value_upper_prevalence"), c("mean_AA_prev", "lower_AA_prev", "upper_AA_prev"))
    setnames(prev_all_ages_summary, c("value_mean_number", "value_lower_number", "value_upper_number"), c("mean_count", "lower_count", "upper_count"))
    setnames(prev_all_ages_summary, c("pct_change_mean_number", "pct_change_lower_number", "pct_change_upper_number"), c("mean_count_change", "lower_count_change", "upper_count_change"))
    setnames(prev_all_ages_summary, c("abs_change_mean_number", "abs_change_lower_number", "abs_change_upper_number"), c("mean_abs_count_change", "lower_abs_count_change", "upper_abs_count_change"))
    
    setnames(prev_age_standard_summary, c("value_mean_prevalence", "value_lower_prevalence", "value_upper_prevalence"), c("mean_AS_prev", "lower_AS_prev", "upper_AS_prev"))
    setnames(prev_age_standard_summary, c("pct_change_mean_prevalence", "pct_change_lower_prevalence", "pct_change_upper_prevalence"), c("mean_AS_change", "lower_AS_change", "upper_AS_change"))
    setnames(prev_age_standard_summary, c("abs_change_mean_prevalence", "abs_change_lower_prevalence", "abs_change_upper_prevalence"), c("mean_AS_pct_point_change", "lower_AS_pct_point_change", "upper_AS_pct_point_change"))
    
    # keep only relevant columns
    meta_vars <- c("location_id", "location_name", "location_level", "year_id", "sex_id", "Sex", "age_group_id", "age_group_name")
    keep_vars_aa <- c(meta_vars, "mean_AA_prev", "lower_AA_prev", "upper_AA_prev", "mean_count", "lower_count", "upper_count", "mean_count_change", "lower_count_change", "upper_count_change", "mean_abs_count_change", "lower_abs_count_change", "upper_abs_count_change")
    keep_vars_as <- c(meta_vars, "mean_AS_prev", "lower_AS_prev", "upper_AS_prev", "mean_AS_change", "lower_AS_change", "upper_AS_change", "mean_AS_pct_point_change", "lower_AS_pct_point_change", "upper_AS_pct_point_change")
    
    # keep only necessary vars
    prev_all_ages_summary <- prev_all_ages_summary[, keep_vars_aa, with=F]
    prev_age_standard_summary <- prev_age_standard_summary[, keep_vars_as, with=F]
    
    # merge all age and age-standardized results
    if(age_grp=="adolescent"){
      prev_aa_as_summary_15_24 <- merge(prev_all_ages_summary[age_group_name=="All Ages (15 to 24)"], prev_age_standard_summary[age_group_name=="Age-standardized (15 to 24)"], 
                                        by=meta_vars[!meta_vars %in% c("age_group_id", "age_group_name")])
      
      prev_aa_as_summary_5_24 <- merge(prev_all_ages_summary[age_group_name=="All Ages (5 to 24)"], prev_age_standard_summary[age_group_name=="Age-standardized (5 to 24)"], 
                                        by=meta_vars[!meta_vars %in% c("age_group_id", "age_group_name")])
      
      prev_aa_as_summary_5_14 <- merge(prev_all_ages_summary[age_group_name=="All Ages (5 to 14)"], prev_age_standard_summary[age_group_name=="Age-standardized (5 to 14)"], 
                                        by=meta_vars[!meta_vars %in% c("age_group_id", "age_group_name")])
      
    } else {
      prev_aa_as_summary <- merge(prev_all_ages_summary, prev_age_standard_summary, by=meta_vars[!meta_vars %in% c("age_group_id", "age_group_name")])
    }
    
    
    
    # save age-specific numbers and prev separately
    if(save_data){
      if(age_grp=="adolescent"){
        age_grp <- "children"
        fwrite(prev_age_spec[measure=="prevalence", -c("measure")], paste0(results_fp, "final_results_global/", "fhs_","5_24_", model,  "_by_age.csv"), row.names = F)
        fwrite(prev_aa_as_summary_15_24, paste0(results_fp,"final_results_global/", "fhs_","15_24_", model,  "_summary.csv"), row.names = F)
        fwrite(prev_aa_as_summary_5_24, paste0(results_fp,"final_results_global/", "fhs_","5_24_", model,  "_summary.csv"), row.names = F)
        fwrite(prev_aa_as_summary_5_14, paste0(results_fp,"final_results_global/", "fhs_", "5_14_", model,  "_summary.csv"), row.names = F)
        
        age_grp <- "adolescent" # change the name back for the loop
      } else {
        fwrite(prev_age_spec[measure=="prevalence", -c("measure")], paste0(results_fp,"final_results_global/", "fhs_","25_125_", model,  "_by_age.csv"), row.names = F)
        fwrite(prev_aa_as_summary, paste0(results_fp,"final_results_global/", "fhs_","25_125_", model,  "_summary.csv"), row.names = F)
      }
    }
  }
}

