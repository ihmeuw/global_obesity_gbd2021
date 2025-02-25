
rm(list = ls())
library(data.table)
library(tidyverse)

#------------------------------------------------------------- 
# pull the GBD and FHS results together by children and adults
#-------------------------------------------------------------

results_dir <- "FILEPATH"

locs <- get_location_metadata(release_id = 9, location_set_id = 35)
locs_of_interest <- locs[level <= 3, .(location_id, ihme_loc_id, location_name, 
                                       super_region_id, super_region_name, region_id, region_name, level)]

ages <- get_age_metadata(release_id = 9)

# pull the age specific results together
for(m in c("obesity","ow_and_ob","ow_not_ob")){
  
  if(m == "obesity"){
    fhs_m <- "obesity"
  } else if(m == "ow_and_ob"){
    fhs_m <- "overweight"
  } else if(m == "ow_not_ob"){
    fhs_m <- "overweight_less_obesity"
  }
  
  for(age_grp in c("5_24","25_125")){
    # read in the GBD results
    gbd <- fread(paste0(results_dir,"gbd_",age_grp,"_",m,"_by_age.csv")) %>% .[, version := "gbd"] %>% .[,metric := fhs_m]
    
    # read in the FHS results, only keep 2022 and later
    fhs <- fread(paste0(results_dir,"fhs_",age_grp,"_",fhs_m,"_by_age.csv")) %>% .[, version := "fhs"] %>% .[year_id>=2022] %>% .[,metric := fhs_m]
    
    # merge region and super region name 
    fhs <- merge(fhs, 
                 locs_of_interest[,.(location_id, super_region_id, super_region_name, region_id, region_name)], 
                 by.x = "location_id", by.y = "location_id", all.x = T)
    
    # change the percent change value by dividing 100.
    fhs[, mean_change := mean_change/100] ; fhs[, lower_change := lower_change/100] ; fhs[, upper_change := upper_change/100]

    # pull the results together
    final <- rbindlist(list(gbd, fhs[, names(gbd), with = F]), use.names = T)
    
    # sort the results by location, year, sex, and age
    setcolorder(final, c(2,4,3,1,5:ncol(final)))
    final <- final[order(location_id, year_id, sex_id, age_group_id), ]
    
    # calculate annualized rate of change (ARC)
    final[year_id <= 2021, mean_arc := mean_change/(2021-1990)]
    final[year_id > 2021, mean_arc := mean_change/(2050-2021)]
    
    final[year_id <= 2021, lower_arc := lower_change/(2021-1990)]
    final[year_id > 2021, lower_arc := lower_change/(2050-2021)]
    
    final[year_id <= 2021, upper_arc := upper_change/(2021-1990)]
    final[year_id > 2021, upper_arc := upper_change/(2050-2021)]
    
    # save the final combined results
    fwrite(final, paste0(results_dir,"combined_",age_grp,"_",m,"_by_age_arc.csv"), row.names = F)
  }
}

# pull the summary results together
for(m in c("obesity","ow_and_ob","ow_not_ob")){
  
  if(m == "obesity"){
    fhs_m <- "obesity"
  } else if(m == "ow_and_ob"){
    fhs_m <- "overweight"
  } else if(m == "ow_not_ob"){
    fhs_m <- "overweight_less_obesity"
  }
  
  all_data <- NULL
  for(age_grp in c("5_14","15_24","5_24","25_125")){
    # read in the GBD results
    gbd <- fread(paste0(results_dir,"gbd_",age_grp,"_",m,"_summary.csv")) %>% .[, version := "gbd"] %>% .[,metric := fhs_m] %>% .[, age_group := age_grp]
    gbd[,level := NULL]
    
    # read in the FHS results, only keep 2022 and later
    fhs <- fread(paste0(results_dir,"fhs_",age_grp,"_",fhs_m,"_summary.csv")) %>% .[, version := "fhs"] %>% 
      .[year_id>=2022] %>% .[,metric := fhs_m] %>% .[, age_group := age_grp] %>%
      .[, c("age_group_name.y","age_group_name.x", "age_group_id.x", "age_group_id.y") := NULL]
    
    # merge super region and region name
    fhs <- merge(fhs, 
                 locs_of_interest[,.(location_id, super_region_id, super_region_name, region_id, region_name)], 
                 by = "location_id", all.x = T)
    names(gbd)[!names(gbd) %in% names(fhs)]
    names(fhs)[!names(fhs) %in% names(gbd)]
    
    # change the percent change value by dividing 100.
    chvar <- names(fhs)[grepl("change", names(fhs)) & !grepl("abs", names(fhs)) & !grepl("pct", names(fhs))]
    fhs[, (chvar) := lapply(.SD, function(x) x/100), .SDcols = chvar]

    # pull the results together
    final <- rbindlist(list(gbd, fhs[,names(gbd), with = F]), use.names = T)
    
    # sort the results by location
    setcolorder(final, c(1,3,2,ncol(final),4:(ncol(final)-1)))
    final <- final[order(location_id, year_id, sex_id, age_group),]
    
    # combind results together
    all_data <- rbindlist(list(all_data, final), use.names = T)
  }
  
  # calculate annualized rate of change (ARC)
  all_data[year_id <= 2021, mean_arc := mean_AS_change/(2021-1990)]
  all_data[year_id > 2021, mean_arc := mean_AS_change/(2050-2021)]
  
  all_data[year_id <= 2021, lower_arc := lower_AS_change/(2021-1990)]
  all_data[year_id > 2021, lower_arc := lower_AS_change/(2050-2021)]
  
  all_data[year_id <= 2021, upper_arc := upper_AS_change/(2021-1990)]
  all_data[year_id > 2021, upper_arc := upper_AS_change/(2050-2021)]
  
  # save the final combined results
  fwrite(all_data, paste0(results_dir,"combined","_",m,"_summary_arc.csv"), row.names = F)
}
