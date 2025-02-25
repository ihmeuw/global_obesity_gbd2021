#--------------------------------------------------
# purpose: create figures for the BMI global papers
#--------------------------------------------------
#-------------------------------------------------------------------------------
# Figures/tables to use in the BMI global papers
# Figure 1. Map of AS prevalence for adults and children from 1990 to 2021
# Figure 2. Map of change of AS prevalence between 1990 and 2021 and between 2021 and 2050
# Figure 3. Line plots with the annual rate of change over time
#-------------------------------------------------------------------------------

## SET-UP ##
rm(list = ls())
library(data.table)
library(ggplot2)
library(tidyverse)
library(rhdf5)
library(scales)
library(Hmisc)
library(knitr)
library(ggridges)
library(RColorBrewer)
library(openxlsx)
library(ggrepel)
library(grid)
library(treemapify)
options(max.print=2000)
# load sources
source("../gbd2023_map_islands.R")
source("../gbd2023_map.R")
source("../graphing_labels_tables.R")
source("../utils.R")
invisible(sapply(list.files("../r/", full.names = T), source))

# main dir
main_dir <- "FILEPATH"

# Round specific variables
gbd_id <- 7
release <- 9
decomp <- "iterative"

# Getting location metadata
locs <- get_location_metadata(35, release_id = release)
locs_3 <- locs[level==3, location_id]
locs_of_interest <- locs[level==3, .(location_id, ihme_loc_id, location_name, super_region_id, super_region_name, region_id, region_name)]

# Getting age metadata
ages <- get_age_metadata(release_id = release)
ages[,defined := paste0(age_group_years_start,"-",age_group_years_end)]

#---------------------------------------------------------------------------------
# Figure 1. Map of AS prevalence for adults and children from 1990 to 2021
#---------------------------------------------------------------------------------
## Adult paper: figure 1 and figure S1
## Child paper: figure 2 and 4

age_groups <- c("5_14", "15_24", "25_125") 
metrics <- c("obesity", "ow_and_ob", "ow_not_ob") 

for (age_grp in age_groups){    
  for(m in metrics){
    # load data, keep countries only
    prev <- fread("FILEPATH") %>% .[location_level=="Country"] %>% .[age_group==age_grp]
    
    # plot age-standardized prev
    prev[, mapvar := mean_AS_prev * 100]
    
    years <- c(1990,2000,2010,2021,2050) # change this line to only plot 2021 results
    sex <- c(1,2,3)
    
    if(m=="ow_and_ob") {
      plot_title <- "prevalence overweight and obesity"
      plot_y <- "Prevalence overweight and obesity (%)"
      if(age_grp %in% c("25_125")) {
        limits <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
        labels <- c("< 10", "10 to 20", "20 to 30", "30 to 40", "40 to 50", "50 to 60", "60 to 70", "70 to 80", "80 to 90", "> 90")
      } else if(age_grp %in% c("5_24", "5_14", "15_24")) {
        limits <- c(0,5, 10, 20, 30, 40, 50, 60, 100) 
        labels <- c("< 5", "5 to 10", "10 to 20", "20 to 30", "30 to 40", "40 to 50", "50 to 60", "> 60") 
      }
    } else if(m=="obesity"){
      plot_title <- "Prevalence obesity"
      plot_y <- "Prevalence obesity (%)"
      if(age_grp %in% c("25_125")) {
        limits <- c(0,2,5,10,15,25,35,45,60,100)
        labels <- c("< 2", "2 to 5", "5 to 10", "10 to 15", "15 to 25", "25 to 35", "35 to 45", "45 to 60", "> 60")
      } else if(age_grp %in% c("5_24", "5_14", "15_24")) {
        limits <- c(0, 1, 2,5,10,15,25,35,100)
        labels <- c("< 1","1 to 2", "2 to 5", "5 to 10", "10 to 15", "15 to 25", "25 to 35", "> 35")
      }
    } else if(m=="ow_not_ob"){
      plot_title <- "Prevalence overweight"
      plot_y <- "Prevalence overweight (%)"
      if(age_grp %in% c("25_125")) {
        limits <- c(0, 2,5,10,15,25,35,45,50,100)
        labels <- c("< 2", "2 to 5", "5 to 10", "10 to 15", "15 to 25", "25 to 35", "35 to 45", "45 to 50", "> 50")
      } else if(age_grp %in% c("5_24", "5_14", "15_24")) {
        limits <- c(0, 1, 2,5,10,15,25,35,100)
        labels <- c("< 1","1 to 2", "2 to 5", "5 to 10", "10 to 15", "15 to 25", "25 to 35", "> 35")
      }
    }
    print(paste0(plot_title, ", age_group: ", age_grp))

    pdf(paste0("FILEPATH"),width=20,height=10)
    for(y in years){ 
      for(s in sex){
        sex_name <- ifelse(s==1, "Males",
                           ifelse(s==2,"Females", "Both sexes"))
        p <- gbd_map(prev[sex_id == s & year_id == y],
                     limits = limits,
                     sub_nat = "none",
                     legend = TRUE,
                     inset = TRUE,
                     col="YlOrRd",
                     # col.reverse = TRUE,
                     labels = labels,
                     na.color = "grey",
                     title = paste0("Age-standardized ",plot_title," among ", age_grp,", ", sex_name, ", " , y),
                     legend.title = plot_y,
                     legend.cex = 1.8,
                     legend.columns = 2)
        print(p)
        
        gg1 <- gbd_map_islands(prev[sex_id == s & year_id == y], 
                               limits = limits,
                               sub_nat = "none", 
                               legend = TRUE, 
                               inset = TRUE,
                               island=TRUE,
                               col="YlOrRd",
                               # col.reverse = TRUE,
                               isl_cols = 1,
                               labels = labels,
                               na.color = "grey",
                               title = paste0("Age-standardized ",plot_title," among ", age_grp,", ", sex_name, ", " , y), 
                               legend.title = plot_y, 
                               legend.cex = 1.9,
                               legend.columns = 2,
                               outdir = "FILEPATH",
                               fname=paste0("F1_",m,"_AS_map_",age_grp,"_", sex_name,"_",y,".pdf"))
        print(gg1)

      }
    }
    dev.off()
    
    
  }
}

#-------------------------------------------------------------------------------------------------
# Figure 2. Map of change of AS prevalence between 1990 and 2021 and between 2021 and 2050
#-------------------------------------------------------------------------------------------------
## Adult paper: figure S4
## Child paper: figure 2 and 4

age_groups <- c("5_14", "15_24", "25_125") 
metrics <- c("obesity", "ow_and_ob", "ow_not_ob") 

for (age_grp in age_groups){    
  for(m in metrics){
    # load data, keep countries only
    prev <- fread("FILEPATH") %>% .[location_level=="Country"] %>% .[age_group==age_grp]
    
    # plot age-standardized prev
    prev[, mapvar := mean_AS_change * 100]
    
    # change the mapvar to 0.05 if the increase is not significant
    prev[mean_AS_change > 0 & lower_AS_change < 0, mapvar := 0.05]
    
    # change the mapvar to -0.05 if the decrease is not significant
    prev[mean_AS_change < 0 & upper_AS_change > 0, mapvar := -0.05]
    
    years <- c(2021,2050)
    sex <- c(1,2,3)
    
    print(paste0(m, ", age_group: ", age_grp))

    pdf(paste0("FILEPATH/", "F2_",m,"_AS_pct_change_map_",age_grp,".pdf"),width=20,height=10)

    for(y in years){
      prev[year_id==y, mapvar] %>% summary %>% print

      if(m=="ow_and_ob") {
        ifelse(y==2021, plot_title <- "Percent change of age-standardized prevalence of overweight and obesity between 1990 and 2021",
               plot_title <- "Percent change of age-standardized prevalence of overweight and obesity between 2021 and 2050")
        plot_y <- "Percent change of overweight and obesity (%)"
        if(age_grp %in% c("25_125")){
          if(y==2021){
            limits <- c(0, 0.1, 15, 30, 45, 60, 80, 100, 150, 200, 1500)
            labels <- c("insignificant increase" ,"0 to 15", "15 to 30", "30 to 45", "45 to 60", "60 to 80", "80 to 100", "100 to 150", "150 to 200", ">200")
          } else if(y==2050){
            limits <- c(0, 0.1, 15, 30, 45, 60, 80, 100, 150, 1500)
            labels <- c("insignificant increase" ,"0 to 15", "15 to 30", "30 to 45", "45 to 60", "60 to 80", "80 to 100", "100 to 150", ">150")
          }
        } else if(age_grp %in% c("5_24", "5_14", "15_24")){
          if(y==2021){
            limits <- c(0, 0.1, 30, 60, 90, 120, 150, 180, 210, 240, 1500)
            labels <- c("insignificant increase" ,"0 to 30", "30 to 60", "60 to 90", "90 to 120", "120 to 150", "150 to 180", "180 to 210", "210 to 240", ">240")
          } else if(y==2050){
            limits <- c(0, 0.1, 30, 60, 90, 120, 150, 1500)
            labels <- c("insignificant increase" ,"0 to 30", "30 to 60", "60 to 90", "90 to 120", "120 to 150", ">150")
          }
        }
      } else if(m=="obesity"){
        ifelse(y==2021, plot_title <- "Percent change of age-standardized prevalence of obesity between 1990 and 2021",
               plot_title <- "Percent change of age-standardized prevalence of obesity between 2021 and 2050")
        plot_y <- "Percent change of obesity (%)"
        if(age_grp %in% c("25_125")){
          if(y==2021){
            limits <- c(0, 30, 50, 100, 150, 200, 250, 300, 350, 400, 500, 1500)
            labels <- c("0 to 30","30 to 50", "50 to 100", "100 to 150", "150 to 200", "200 to 250", "250 to 300", "300 to 350", "350 to 400", "400 to 500", ">500")
          } else if(y==2050){
            limits <- c(0, 0.1, 50, 100, 150, 200, 250, 300, 1500)
            labels <- c("insignificant increase" ,"0 to 50", "50 to 100", "100 to 150", "150 to 200", "200 to 250", "250 to 300", ">300")
          }
        } else if(age_grp %in% c("5_24", "5_14", "15_24")){
          if(y==2021){
            limits <- c(0, 0.1, 50, 100, 150, 200, 250, 300, 350, 400, 500, 1500)
            labels <- c("insignificant increase", "0 to 50", "50 to 100", "100 to 150", "150 to 200", "200 to 250", "250 to 300", "300 to 350", "350 to 400", "400 to 500", ">500")
          } else if(y==2050){
            limits <- c(0, 0.1, 50, 100, 150, 200, 250, 300, 1500)
            labels <- c("insignificant increase" ,"0 to 50", "50 to 100", "100 to 150", "150 to 200", "200 to 250", "250 to 300", ">300")
          }
        }
      } else if(m=="ow_not_ob"){
        ifelse(y==2021, plot_title <- "Percent change of age-standardized prevalence of overweight between 1990 and 2021",
               plot_title <- "Percent change of age-standardized prevalence of overweight between 2021 and 2050")
        plot_y <- "Percent change of overweight (%)"
        if(age_grp %in% c("25_125")){
          if(y==2021){
            limits <- c(-100, -40, -20, -0.1, 0, 0.1, 20, 40, 60, 80, 100, 500)
            labels <- c("<-40" ,"-40 to -20" ,"-20 to 0", "insignificant decrease", "insignificant increase", "0 to 20","20 to 40", "40 to 60", "60 to 80", "80 to 100", ">100")
          } else if(y==2050){
            limits <- c(-100, -40, -20, -0.1, 0, 0.1, 20, 40, 60, 80, 100, 500)
            labels <- c("<-40" ,"-40 to -20" ,"-20 to 0", "insignificant decrease", "insignificant increase", "0 to 20","20 to 40", "40 to 60", "60 to 80", "80 to 100", ">100")
          }
        } else if(age_grp %in% c("5_24", "5_14", "15_24")){
          if(y==2021){
            limits <- c(-100, -20, -0.1, 0, 0.1, 20, 40, 60, 80, 100, 120, 500)
            labels <- c("<-20", "-20 to 0", "insignificant decrease", "insignificant increase", "0 to 20","20 to 40", "40 to 60", "60 to 80", "80 to 100", "100 to 120", ">120")
          } else if(y==2050){
            limits <- c(-100, -20, -0.1, 0, 0.1, 20, 40, 60, 80, 100, 500)
            labels <- c("<-20", "-20 to 0", "insignificant decrease", "insignificant increase", "0 to 20","20 to 40", "40 to 60", "60 to 80", "80 to 100", ">100")
          }
        }
      }


      for(s in sex){
        sex_name <- ifelse(s==1, "Males",
                           ifelse(s==2,"Females", "Both sexes"))
        p <- gbd_map(prev[sex_id == s & year_id == y],
                     limits = limits,
                     sub_nat = "none",
                     legend = TRUE,
                     inset = TRUE,
                     col = "Blues",
                     # col.reverse = TRUE,
                     labels = labels,
                     na.color = "grey",
                     title = paste0(plot_title," among ", age_grp,", ", sex_name),
                     legend.title = plot_y,
                     legend.cex = 1.8,
                     legend.columns = 2)
      print(p)
      
      gg1 <- gbd_map_islands(prev[sex_id == s & year_id == y], 
                             limits = limits,
                             sub_nat = "none",
                             legend = TRUE,
                             inset = TRUE,
                             island=TRUE,
                             col = "Blues",
                             # col.reverse = TRUE,
                             isl_cols = 1,
                             labels = labels,
                             na.color = "grey",
                             title = paste0(plot_title," among ", age_grp,", ", sex_name),
                             legend.title = plot_y,
                             legend.cex = 1.9,
                             legend.columns = 2,
                             outdir = "FILEPATH",
                             fname=paste0("F2_",m,"_AS_pct_change_map_",age_grp,"_", sex_name,"_",y,".pdf"))
      print(gg1)
      }
    }
    dev.off()
    
  }
}


#-------------------------------------------------------------------------
# Figure 4. Line plots with the annual rate of change over time
#-------------------------------------------------------------------------
## Adult paper: Figure 4 and Figure S2
## Child paper: Figure 1 and Figure S1

age_groups <- c("5_14", "15_24", "5_24", "25_125") 
metrics <- c("obesity", "ow_and_ob", "ow_not_ob")

locs_ids <- locs[level<=2 & !location_id %in% c(138,159), location_id] %>% unique
locs_sr <- locs[level==2, location_id] %>% unique

# plot results of ow+ob, ob and ow by sex and by region (including global and SR) from 1990 to 2050 with UI
for (age_grp in age_groups){
  
  pdf(paste0("FILEPATH"),width=16,height=10)
  
  for(lid in locs_ids){
    sr <- locs[location_id == lid, location_name] %>% unique
    
    # combine data for all metrics
    ow <- fread("FILEPATH") %>% .[age_group == age_grp & location_id == lid] 
    ob <- fread("FILEPATH") %>% .[age_group == age_grp & location_id == lid]
    ow_ob <- fread("FILEPATH") %>% .[age_group == age_grp & location_id == lid]
    
    prev <- rbindlist(list(ow,ob,ow_ob), use.names = T)
    
    lv <- prev[, location_level] %>% unique
    
    # change metric name
    prev[, metric_name := ifelse(metric == "overweight", "Overweight and Obesity", 
                                 ifelse(metric == "obesity", "Obesity", "Overweight"))]
    
    #yearly change
    prev[, yearly_change := (mean_AS_prev - lag(mean_AS_prev))*100, by = c("location_name", "Sex", "age_group", "metric")]
    prev[, yearly_change_lead := lead(yearly_change), by = c("location_name", "Sex", "age_group", "metric")]
    prev <- prev[is.na(yearly_change), yearly_change := yearly_change_lead]
    prev[, mean_AS_prev := mean_AS_prev*100]
    prev[, lower_AS_prev := lower_AS_prev*100]; prev[, upper_AS_prev := upper_AS_prev*100]
    
    #make colour scheme for gradient
    col_s <- c("darkblue","cornflowerblue","springgreen4","gold","red", "darkred")
    inv_col_s <- c("darkred", "red", "gold","springgreen4" ,"cornflowerblue","darkblue")
    
    sex <- c(1,2,3)
    
    for(s in sex){
      
      set.seed(135)
      sex_name <- ifelse(s==1, "Males", 
                         ifelse(s==2,"Females", "Both sexes"))
      yc_min <- prev[, yearly_change] %>% min
      yc_max <- prev[, yearly_change] %>% max
      
      
      p <- ggplot(prev[sex_id==s],aes(x=year_id, y=mean_AS_prev, group=metric))+
        geom_ribbon(aes(ymin = lower_AS_prev, ymax = upper_AS_prev), fill = "grey", alpha = 0.2) + 
        geom_line(aes(color=yearly_change),size=1.5) + geom_vline(xintercept = 2021, linetype = "dotted", linewidth = 1) +
        theme_bw()+
        geom_text_repel(data = prev[sex_id==s & year_id == 1990],force=0.01,direction="y",
                        aes(label = metric_name, x = 1989) , hjust = 1, fontface = "bold", color = "#888888", size = 3.5)+
        geom_text_repel(data = prev[sex_id==s & year_id == 2050], force=0.01,direction="y",
                        aes(label = metric_name, x = 2051) , hjust = 0, fontface = "bold", color = "#888888", size = 3.5)+
        labs(x = "Year",
             y = "Prevalence Estimate (%)",
             title = paste0("Prevalence Estimates from 1990 to 2021 and Forecasts to 2050"),
             subtitle = paste0(lv, ", ", sr, ", ", age_grp,", ",sex_name)) +
        theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
        theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
        theme(axis.text.x      = element_text(size=14)) +
        theme(axis.text.y      = element_text(size=14)) +
        scale_color_gradientn(colors=col_s, name="Yearly change in prevalence (percentage point)", breaks = round(seq(yc_min, yc_max, length.out=4), digits = 1))+
        scale_x_continuous(breaks = c(1990,2000,2010,2021,2030,2040,2050), expand = c(0.5, 0.5)) +
        coord_cartesian(clip = 'off') +
        theme(legend.position = 'bottom',
              panel.border = element_blank(),
              plot.margin = margin(0.1, 5, 0.1, 0.1, "cm")) #top, right, bottom, and left margins
      print(p)
    }
  }
  dev.off()
}

# plot the lines by age group and by metrics for all super regions (including global)

for(m in metrics){
  
  #yearly change
  prev <- fread(paste0("FILEPATH")) 
  prev[, yearly_change := (mean_AS_prev - lag(mean_AS_prev))*100, by = c("location_name", "Sex", "age_group")]
  prev[, yearly_change_lead := lead(yearly_change), by = c("location_name", "Sex", "age_group")]
  prev <- prev[is.na(yearly_change), yearly_change := yearly_change_lead]
  prev[, mean_AS_prev := mean_AS_prev*100]; prev[, lower_AS_prev := lower_AS_prev*100]; prev[, upper_AS_prev := upper_AS_prev*100]
  prev <- prev[location_level=="Global" | location_level=="Super Region"]
  
  #make colour scheme for gradient
  col_s <- c("darkblue","cornflowerblue","springgreen4","gold","red", "darkred")
  inv_col_s <- c("darkred", "red", "gold","springgreen4" ,"cornflowerblue","darkblue")
  
  sex <- c(1,2,3)
  
  pdf(paste0("FILEPATH"),width=16,height=10)
  for(age_grp in age_groups){
    for(s in sex){
      
      set.seed(135)
      sex_name <- ifelse(s==1, "Males", 
                         ifelse(s==2,"Females", "Both sexes"))
      yc_min <- prev[age_group==age_grp, yearly_change] %>% min
      yc_max <- prev[age_group==age_grp, yearly_change] %>% max
      
      p <- ggplot(prev[sex_id==s & age_group==age_grp],aes(x=year_id, y=mean_AS_prev, group=location_name))+
        geom_ribbon(aes(ymin = lower_AS_prev, ymax = upper_AS_prev), fill = "grey", alpha = 0.2) +
        geom_line(aes(color=yearly_change),linewidth=1.5)+
        geom_vline(xintercept = 2021, linetype = "dotted", linewidth = 1) +
        theme_bw()+
        geom_text_repel(data = prev[sex_id==s & year_id == 2050 & age_group==age_grp], force=0.01,direction="y",
                        aes(label = location_name, x = 2051) , hjust = 0, fontface = "bold", color = "#888888", size = 3.5)+
        labs(x = "Year",
             y = "Prevalence Estimate (%)",
             title = paste0("Prevalence ",  m, " by super regions"),
             subtitle = paste0(age_grp,", ",sex_name)) +
        theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
        theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
        theme(axis.text.x      = element_text(size=14)) +
        theme(axis.text.y      = element_text(size=14)) +
        scale_color_gradientn(colors=col_s, name="Yearly change in prevalence (percentage points)", breaks = round(seq(yc_min, yc_max, length.out=4), digits = 1))+
        scale_x_continuous(breaks = c(1990,2000,2010,2021,2030,2040,2050), expand = c(0.5, 0.5)) +
        coord_cartesian(clip = 'off') +
        theme(legend.position = 'bottom',
              panel.border = element_blank(),
              plot.margin = margin(0.1, 5, 0.1, 0.1, "cm")) #top, right, bottom, and left margins
      print(p)
    } 
  }
  dev.off()
}

