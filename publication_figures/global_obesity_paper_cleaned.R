#### Create figures and tables for GBD2021 global obesity paper
## 1. Final overweight and obese trends (Figure)
## 2. birth cohorts by super region (Figure)
## 3. Sources used (Table)
## 4. Counts overweight (Table)
## 5. Counts obese (Table)


invisible(sapply(list.files("FILEPATH", full.names = T), source))
library(plyr)
library(dplyr)
library(bit64)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(ggpubr)
library(RColorBrewer)
library(kableExtra)

locs <- get_location_metadata(location_set_id=22, release_id=16)[,c("location_id", "location_name_short","location_name", "super_region_id", "super_region_name",
                                                                   "region_id", "region_name", "ihme_loc_id","location_type")]
locs[, country_id := as.integer(as.factor(substr(ihme_loc_id, 1, 3)))]
locs[, country_name := substr(ihme_loc_id, 1, 3)]
countries <- unique(locs[!grepl("_", ihme_loc_id) & (location_type=="admin0"| location_type=="nonsovereign")])

ages <- get_age_metadata(age_group_set_id=24, release_id=16)[, .(age_group_id, age_start = age_group_years_start, age_end = age_group_years_end-1, 
                                                                age_mid = .5*(age_group_years_start + age_group_years_end))]
ages$age_start <- round(ages$age_start)
ages$age_end <- round(ages$age_end)
ages[, age_label := paste0(age_start, "-", age_end, " years (", age_group_id, ")")]
ages$age_label <- factor(ages$age_label, levels = c("2-4 years (34)", "5-9 years (6)", "10-14 years (7)", "15-19 years (8)", "20-24 years (9)", "25-29 years (10)", 
                                                    "30-34 years (11)", "35-39 years (12)", "40-44 years (13)", "45-49 years (14)", "50-54 years (15)", "55-59 years (16)", 
                                                    "60-64 years (17)", "65-69 years (18)", "70-74 years (19)", "75-79 years (20)", "80-84 years (30)", 
                                                    "85-89 years (31)", "90-94 years (32)", "95-124 years (235)"))

## Load in data
out_path <- "FILEPATH"

prev_ow <- fread("FILEPATH")
prev_ow <- rbind(prev_ow, fread("FILEPATH"), fill=T)

prev_ob <- fread("FILEPATH")
prev_ob <- rbind(prev_ob, fread("FILEPATH"), fill=T)

prev_ow$age_group_name <- factor(prev_ow$age_group_name, levels = c("2 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
                                                                    "30 to 34", "35 to 39", "40 to 44","45 to 49", "50 to 54", "55 to 59", 
                                                                    "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84" ,"85 to 89", "90 to 94",  "95 plus"))

prev_ob$age_group_name <- factor(prev_ob$age_group_name, levels = c("2 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
                                                                    "30 to 34", "35 to 39", "40 to 44","45 to 49", "50 to 54", "55 to 59", 
                                                                    "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84" ,"85 to 89", "90 to 94",  "95 plus"))

release <- 9 ## GBD 2021 versions

# # Getting location metadata
locs_3 <- get_location_metadata(22, release_id = release)
locs_3 <- locs_3[level==3, location_id]
# 
# # Getting age metadata
ages2 <- get_age_metadata(24, release_id = release)
ages2[,defined := paste0(age_group_years_start,"-",age_group_years_end)]

# Getting population metadata
pops <- get_population(release_id = release, age_group_id = c(6:20, 30:32, 235,22, 34), sex_id = c(1,2,3),
                       year_id = 1990:2022, location_id = locs_3, forecasted_pop = T)



# load prevalence data by year, age and sex
fhs_ow <- rbind(fread("FILEPATH"), fread("FILEPATH"))
fhs_ob <- rbind(fread("FILEPATH"), fread("FILEPATH"))


fhs_ow <- fhs_ow[age_group_id %in% c(34,6:20,30)]
fhs_ob <- fhs_ob[age_group_id %in% c(34,6:20,30)]
fhs_ow[age_group_name=="80 to 84",age_group_name:="80+"][age_group_id==30, age_start := 80][age_group_id==30, age_end := 85]
fhs_ob[age_group_name=="80 to 84",age_group_name:="80+"][age_group_id==30, age_start := 80][age_group_id==30, age_end := 85]

fhs_ow$age_group_name <- factor(fhs_ow$age_group_name, levels = c("2 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
                                                                  "30 to 34", "35 to 39", "40 to 44","45 to 49", "50 to 54", "55 to 59", 
                                                                  "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80+"))

fhs_ob$age_group_name <- factor(fhs_ob$age_group_name, levels = c("2 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
                                                                  "30 to 34", "35 to 39", "40 to 44","45 to 49", "50 to 54", "55 to 59", 
                                                                  "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80+"))

ages <- c("child","adult")

for(age in ages){
  
if(age=="child") ag <- c(34, 6:9)
if(age=="adult") ag <- c(10:20, 30:32, 235,22)
    
#### 1. Final overweight and obese trends ####
## Adult paper: Figure 3 and S3 adult paper
## Child paper: NA
  
  prev_ow_plot <- prev_ow[year_id>1990 & year_id<=2021 & sex_id %in% c(1,2) & age_group_id %in% ag]
  prev_ob_plot <- prev_ob[year_id>1990 & year_id<=2021 & sex_id %in% c(1,2) & age_group_id %in% ag]
  prev_ow_plot[age_group_name=="80 to 84",age_group_name:="80+"]
  prev_ob_plot[age_group_name=="80 to 84",age_group_name:="80+"]
  prev_ow_plot[,age_group_name:=gsub(" to ", "-", age_group_name)]
  prev_ob_plot[,age_group_name:=gsub(" to ", "-", age_group_name)]
  
  prev_ow_plot$age_group_name <- factor(prev_ow_plot$age_group_name, levels = c("2-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                                                                                "30-34", "35-39", "40-44","45-49", "50-54", "55-59", 
                                                                                "60-64", "65-69", "70-74", "75-79", "80+"))
  
  prev_ob_plot$age_group_name <- factor(prev_ob_plot$age_group_name, levels = c("2-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                                                                                "30-34", "35-39", "40-44","45-49", "50-54", "55-59", 
                                                                                "60-64", "65-69", "70-74", "75-79", "80+"))
  
  prev_ow_plot[,label:=paste0(location_name, ", ", Sex)]
  prev_ob_plot[,label:=paste0(location_name, ", ", Sex)]
  ages_plot <- unique(prev_ob_plot$age_group_name)
  custom_labels <- ifelse(seq_along(levels(prev_ob_plot$age_group_name)) %% 5 == 1, levels(prev_ob_plot$age_group_name), "")
  
  pdf(paste0("FILEPATH"),width=15,height=10)
  
  for(loc in unique(glob_sr$location_id)){
    g1 <- ggplot()  +
      geom_line(data=prev_ob_plot[location_id==loc & year_id==2021], aes(x=age_group_name, y=mean_prev*100, group=as.factor(label), color=as.factor(label)), alpha=0.7, linewidth=1) +
      geom_ribbon(data=prev_ob_plot[location_id==loc & year_id==2021],aes(x=age_group_name, y=mean_prev*100, ymin=lower_prev*100, ymax=upper_prev*100, group=as.factor(label), fill=as.factor(Sex)), alpha=0.1) +
      scale_fill_manual(values=c("slateblue","red3")) +
      scale_color_manual(values=c("slateblue","red3")) +
      scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.05))) + 
      theme_classic() + theme(axis.text.x = element_text(angle = 60, vjust = 1.15, hjust=1.25), plot.title = element_text(hjust = 0.5), 
                              legend.position = "inside", legend.justification=c(0.9,0.05), panel.border = element_rect(fill = NA), axis.ticks.length = unit(0.25, "cm"),
                              axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = 1)) +  
      guides(size = guide_legend(override.aes = list(color="black",fill="black")), fill="none") +
      labs(x="Age", y = "Prevalence (%)", title = "Obesity (BMI>=30 kg/m2)",color=element_blank())
    
    g2 <- ggplot()  +
      geom_line(data=prev_ow_plot[location_id==loc & year_id==2021], aes(x=age_group_name, y=mean_prev*100, group=as.factor(label), color=as.factor(label)), alpha=0.7, linewidth=1) +
      geom_ribbon(data=prev_ow_plot[location_id==loc & year_id==2021],aes(x=age_group_name, y=mean_prev*100, ymin=lower_prev*100, ymax=upper_prev*100, group=as.factor(label), fill=as.factor(Sex)), alpha=0.1) +
      scale_fill_manual(values=c("slateblue","red3")) +
      scale_color_manual(values=c("slateblue","red3")) +
      scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.05))) + 
      theme_classic() + theme(axis.text.x = element_text(angle = 60, vjust = 1.15, hjust=1.25), legend.position="bottom", plot.title = element_text(hjust = 0.5), 
                              panel.border = element_rect(fill = NA), axis.ticks.length = unit(0.25, "cm"), 
                              axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = 1)) +   
      guides(size = guide_legend(override.aes = list(color="black",fill="black")), fill="none", color="none") +
      labs(x="Age", y = "Prevalence (%)", title = "Overweight and obesity (BMI>=25 kg/m2)",color=element_blank())
    
    gg <- ggarrange(g2, g1, ncol=2, nrow=1)
    print(gg)
  }
  dev.off()
  
  ##Plot combined global and super-regions
  prev_ob_plot$location_name <- factor(prev_ob_plot$location_name, levels = c("Global","Southeast Asia, East Asia, and Oceania",
                                                                              "Central Europe, Eastern Europe, and Central Asia","High-income",
                                                                              "Latin America and Caribbean","North Africa and Middle East","South Asia",
                                                                              "Sub-Saharan Africa"  ))
  prev_ob_plot[sex_id==2, Sex1 := "A: Female"][sex_id==1, Sex1 := "B: Male"]
  
  pdf(paste0(out_path,"/Figure_5_agetrends_",age,"_2021_comb_sr_glob.pdf"),width=15,height=10)
  for(v in c("ow","ob")){
    
    tit <- ifelse(v=="ow", "Overweight and obesity (BMI>=25)","Obesity (BMI>=30)")
    plot <- get(paste0("prev_",v,"_plot"))
    plot$location_name <- factor(plot$location_name, levels = c("Global","Southeast Asia, East Asia, and Oceania",
                                                                                "Central Europe, Eastern Europe, and Central Asia","High-income",
                                                                                "Latin America and Caribbean","North Africa and Middle East","South Asia",
                                                                                "Sub-Saharan Africa"  ))
    plot[sex_id==2, Sex1 := "A: Female"][sex_id==1, Sex1 := "B: Male"]
    
    g1 <- ggplot()  +
      geom_line(data=plot[year_id==2021 & location_id %in% unique(glob_sr$location_id) & !is.na(age_group_name)], 
                aes(x=age_group_name, y=mean_prev*100, group=as.factor(location_name), color=as.factor(location_name)), alpha=0.7, linewidth=1) +
      geom_ribbon(data=plot[year_id==2021 & location_id %in% unique(glob_sr$location_id) & !is.na(age_group_name)],
                  aes(x=age_group_name, y=mean_prev*100, ymin=lower_prev*100, ymax=upper_prev*100, group=as.factor(location_name), 
                      fill=as.factor(location_name)), alpha=0.1) +
      # ylim(0,1) + 
      scale_fill_manual(values=c("black", brewer.pal(n = 7, name = "Dark2"))) +
      scale_color_manual(values=c("black", brewer.pal(n = 7, name = "Dark2"))) +
      facet_wrap(~Sex1) +
      scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.05))) + 
      theme_classic() + theme(axis.text.x = element_text(angle = 60, vjust = 1.15, hjust=1.25), plot.title = element_text(hjust = 0.5), 
                              legend.position = "right", 
                              axis.ticks.length = unit(0.25, "cm"),
                              axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = 1),
                              panel.spacing = unit(1, "lines"),panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)) +  
      guides(size = guide_legend(override.aes = list(color="black",fill="black")), fill="none") +
      labs(x="Age", y = "Prevalence (%)", title = tit, color=element_blank())
    print(g1)
  
  }
  dev.off()

#### 2. birth cohorts by super region   ####
## Adult paper: Figure 5 and figure S8
## Child paper: NA
  
all_cohort_data <- data.table()
for(meas in c("ow","ob")){ 
  current <- get(paste0("fhs_",meas))
  current <- current[sex_id!=3  & age_group_id %in% ag]
  
  # calculate birth years (birth_year_end should be used as the start of birth cohort)
  current <- merge(current, ages2, by="age_group_id")
  current[, birth_year_start := year_id - age_group_years_start]
  current[, birth_year_end := year_id - age_group_years_end]
  current[, birth_cohort := paste0(birth_year_end, "-", birth_year_start)]
  
  # keep only 5 year interval for plotting
  current_5 <- current[year_id %in% seq(1960,2050,5)]
  
  # country estimates only
  current_5 <- current_5[location_id %in% locs_3]
  
  setnames(current_5, c("mean_prev", "lower_prev", "upper_prev"), c("prevalence", "lower", "upper"))
  
  # collapse by year, sex, age group and SDI group
  current_by_sr <- current_5[, lapply(.SD, mean), by=c("year_id", "sex_id", "age_group_id", "super_region_name"), .SDcols=c("prevalence","lower","upper")]
  current_global <- current_5[, lapply(.SD, mean), by=c("year_id", "sex_id", "age_group_id"), .SDcols=c("prevalence","lower","upper")]
  current_global[, super_region_name := "Global"]
  
  # include global estimates
  current_by_sr <- rbindlist(list(current_by_sr, current_global), use.names = T)
  
  current_by_sr <- merge(current_by_sr, ages2[,.(age_group_id, age_group_name, age_group_years_start, age_group_years_end)], by="age_group_id")
  current_by_sr[, birth_year_start := year_id - age_group_years_start]
  current_by_sr[, birth_year_end := year_id - age_group_years_end]
  current_by_sr[, birth_cohort := paste0(birth_year_end, "-", birth_year_start)]
  current_by_sr[, sex := ifelse(sex_id==1, "Male", "Female")]
  
  # plots birth cohort after 1900
  current_by_sr <- current_by_sr[birth_year_start>=1900]

  
  col_grad <- colorRampPalette(c("#9E0142", "#F46D43", "#FEE08B", "#E6F598", "#66C2A5", "#5E4FA2"), space = "rgb")
  num <- current_by_sr[age_group_id %in% c(6:20,30, 235), birth_year_start] %>% unique %>% length
  
  # plot results using SR in 2024
  if(meas=="ow"){
    plot_title <- "Prevalence of overweight and obesity by age across birth cohorts"
    plot_y <- "Prevalence overweight"
    save_name <- "FILEPATH"
  } else if(meas=="ob"){
    plot_title <- "Prevalence of obesity by age across birth cohorts"
    plot_y <- "Prevalence obese"
    save_name <- "FILEPATH"
  } 
  ## Save a dataset of all data that goes into cohort plots
  all_cohort_data <- rbind(all_cohort_data, current_by_sr[, measure := meas])
  
  ## Include forecasting results
  pdf(save_name,width=15,height=10)

  for(sr in unique(current_by_sr$super_region_name)){
    for(s in c("Male","Female")){
      figure_1_2020 <- ggplot()+
        geom_line(data = current_by_sr[age_group_id %in% c(6:20,30, 235) & sex==s & super_region_name==sr], aes(x = age_group_years_start, y = prevalence*100, group = factor(birth_year_start),
                                                                                        color=factor(birth_year_start)), linetype = 1)+
        geom_point(data = current_by_sr[age_group_id %in% c(6:20,30, 235)& sex==s & super_region_name==sr], aes(x = age_group_years_start, y = prevalence*100, group = factor(birth_year_start),
                                                                                         color=factor(birth_year_start)), shape=15) +
        scale_colour_manual(values=col_grad(num), name="Birth year",
                            guide = guide_legend(ncol=1), drop = T) +
        theme_classic() +  theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_continuous(breaks=seq(10,120,10)) +
        labs(title=plot_title, subtitle=paste(sr, s), y=plot_y, x="age")

      print(figure_1_2020)
    }
  }
  dev.off()
  
  }

##save dataset
all_cohort_data[measure == "ow", measure:= "Prevalence overweight"][measure == "ob", measure:= "Prevalence obese"]
fwrite(all_cohort_data, "FILEPATH")

}


# Load in data for tables
ow_xwalk <- get_crosswalk_version('FILEPATH')
ow_xwalk <- ow_xwalk[is_outlier==0]

#### 3. Sources used   ####
## Adult and child  paper: Methods appendix

ow_counts <- unique(ow_xwalk[, c("nid", "field_citation_value", "diagnostic")])
ow_counts <- ow_counts[, .N, by=c("nid")]
ow_counts <- ow_counts[N>1]

ow_counts2 <- unique(ow_xwalk[, c("nid", "field_citation_value", "location_id")])
ow_counts2[, nat:=0][location_id %in% countries$location_id, nat:=1]
ow_counts2 <- ow_counts2[, .N, by=c("nid","nat")]
ow_counts3 <- ow_counts2[, .N, by=c("nid")]
ow_both <- ow_counts3[N>1, nid]
ow_nat <- ow_counts2[nat==1 & !nid %in% ow_both, nid] 
ow_subnat <- ow_counts2[nat==0 & N>=1 & !nid %in% ow_both, nid]

ow_sources <- unique(ow_xwalk[, .(ss = sum(sample_size)),c("nid", "field_citation_value", "year_start", "year_end", "location_id", "diagnostic")])
ow_sources[nid %in% ow_counts$nid, diagnostic := "measured"]
ow_sources[nid %in% ow_nat, represent := "National"][nid %in% ow_subnat, represent := "Subnational"][nid %in% ow_both, represent := "Subnational + National"]
ow_sources <- merge(ow_sources, locs[, c("location_id","country_name")], all.x=T)
ow_sources[, Years := paste0(year_start, "-", year_end)]
ow_sources <- unique(ow_sources[, .(ss = sum(ss)), c("nid", "field_citation_value", "Years", "country_name","diagnostic","represent")])
ow_sources <- aggregate(data=ow_sources,Years~.,FUN=paste,collapse=",\n") %>% as.data.table()
ow_sources <- ow_sources[order(-Years,nid),]
ow_sources[, field_citation_value := gsub("<.*?>", "", field_citation_value)]
ow_sources <- merge(ow_sources, unique(locs[location_name %in% countries$location_name,c("location_name","country_name")]), by="country_name", all.x=T)
ow_sources <- ow_sources[country_name!="USA" | (country_name=="USA" & location_name=="United States of America")] ##Remove Georgia state rows
setnames(ow_sources, c("nid", "field_citation_value", "location_name","diagnostic","represent","ss"), 
         c("NID", "Citation", "Country","Data type","Representativeness","Sample size"))
ow_sources <- ow_sources[, c("NID","Citation","Country","Representativeness","Sample size", "Years","Data type")]
ow_sources[,`Sample size`:= round(`Sample size`,0)]
ow_sources$Citation <- enc2utf8(ow_sources$Citation)
Encoding(ow_sources$Citation) <- "unknown"
fwrite(ow_sources, "FILEPATH", bom=T)


#### 4. Counts overweight   ####
## Adult paper: Table S1 and S2
## Child paper: Table 1 and Table S1

ow_5_14 <- rbind(fread("FILEPATH"), fread("FILEPATH")[year_id!=2021], fill=T)
ow_15_24 <- rbind(fread("FILEPATH"), fread("FILEPATH")[year_id!=2021], fill=T)
ow_25up <- rbind(fread("FILEPATH"), fread("FILEPATH")[year_id!=2021], fill=T)

## Table SM 1
years <- c(1990,2021,2050)
ow_5_14 <- ow_5_14[year_id %in% years & Sex!="Both", c("location_name","Sex","year_id","mean_count","lower_count","upper_count")]
ow_15_24 <- ow_15_24[year_id %in% years & Sex!="Both", c("location_name","Sex","year_id","mean_count","lower_count","upper_count")]
ow_25up <- ow_25up[year_id %in% years & Sex!="Both", c("location_name","Sex","year_id","mean_count","lower_count","upper_count")]

ow_5_14[, mean_count := formatC(round(mean_count,1), format="d", big.mark=",")][, lower_count := formatC(round(lower_count,1), format="d", big.mark=",")][, upper_count := formatC(round(upper_count,1), format="d", big.mark=",")]
ow_15_24[, mean_count := formatC(round(mean_count,1), format="d", big.mark=",")][, lower_count := formatC(round(lower_count,1), format="d", big.mark=",")][, upper_count := formatC(round(upper_count,1), format="d", big.mark=",")]
ow_25up[, mean_count := formatC(round(mean_count,1), format="d", big.mark=",")][, lower_count := formatC(round(lower_count,1), format="d", big.mark=",")][, upper_count := formatC(round(upper_count,1), format="d", big.mark=",")]

## Pivot wider
ow_5_14_w <- pivot_wider(ow_5_14, names_from = year_id, values_from = c(mean_count,lower_count, upper_count)) %>% as.data.table()
ow_15_24_w <- pivot_wider(ow_15_24, names_from = year_id, values_from = c(mean_count,lower_count, upper_count)) %>% as.data.table()
ow_25up_w <- pivot_wider(unique(ow_25up), names_from = year_id, values_from = c(mean_count,lower_count, upper_count)) %>% as.data.table()

##Format 25+ age group
comb_sr<- ow_25up_w[location_name %in% countries$super_region_name | location_name=="Global"]
comb_ctry<- ow_25up_w[!location_name %in% countries$super_region_name & !location_name %in% countries$region_name & location_name!="Global"]
comb_ctry <- comb_ctry[order(location_name),]
comb <- rbind(comb_sr,comb_ctry)
comb2 <- merge(comb[Sex=="Male", -c("Sex")], comb[Sex=="Female", -c("Sex")], by=c("location_name"))
comb2 <- comb2[unique(comb$location_name),]

comb2[, ci_1990.x := paste0("(", lower_count_1990.x, "-", upper_count_1990.x, ")")][, ci_1990.y := paste0("(", lower_count_1990.y, "-", upper_count_1990.y, ")")]
comb2[, ci_2021.x := paste0("(", lower_count_2021.x, "-", upper_count_2021.x, ")")][, ci_2021.y := paste0("(", lower_count_2021.y, "-", upper_count_2021.y, ")")]
comb2[, ci_2050.x := paste0("(", lower_count_2050.x, "-", upper_count_2050.x, ")")][, ci_2050.y := paste0("(", lower_count_2050.y, "-", upper_count_2050.y, ")")]
comb2 <- comb2[, c("location_name","mean_count_1990.x","ci_1990.x","mean_count_2021.x","ci_2021.x","mean_count_2050.x","ci_2050.x",
                   "mean_count_1990.y","ci_1990.y","mean_count_2021.y","ci_2021.y","mean_count_2050.y","ci_2050.y")]

write.xlsx(comb2, "FILEPATH")

# Counts obese
ob_5_14 <- rbind(fread("FILEPATH"), fread("FILEPATH")[year_id!=2021], fill=T)
ob_15_24 <- rbind(fread("FILEPATH"), fread("FILEPATH")[year_id!=2021], fill=T)
ob_25up <- rbind(fread("FILEPATH"), fread("FILEPATH")[year_id!=2021], fill=T)

## Table SM 2
years <- c(1990,2021,2050)
ob_5_14 <- ob_5_14[year_id %in% years & Sex!="Both", c("location_name","Sex","year_id","mean_count","lower_count","upper_count")]
ob_15_24 <- ob_15_24[year_id %in% years & Sex!="Both", c("location_name","Sex","year_id","mean_count","lower_count","upper_count")]
ob_25up <- ob_25up[year_id %in% years & Sex!="Both", c("location_name","Sex","year_id","mean_count","lower_count","upper_count")]

ob_5_14[, mean_count := formatC(round(mean_count,1), format="d", big.mark=",")][, lower_count := formatC(round(lower_count,1), format="d", big.mark=",")][, upper_count := formatC(round(upper_count,1), format="d", big.mark=",")]
ob_15_24[, mean_count := formatC(round(mean_count,1), format="d", big.mark=",")][, lower_count := formatC(round(lower_count,1), format="d", big.mark=",")][, upper_count := formatC(round(upper_count,1), format="d", big.mark=",")]
ob_25up[, mean_count := formatC(round(mean_count,1), format="d", big.mark=",")][, lower_count := formatC(round(lower_count,1), format="d", big.mark=",")][, upper_count := formatC(round(upper_count,1), format="d", big.mark=",")]

## Pivot wider
ob_5_14_w <- pivot_wider(ob_5_14, names_from = year_id, values_from = c(mean_count,lower_count, upper_count)) %>% as.data.table()
ob_15_24_w <- pivot_wider(ob_15_24, names_from = year_id, values_from = c(mean_count,lower_count, upper_count)) %>% as.data.table()
ob_25up_w <- pivot_wider(unique(ob_25up), names_from = year_id, values_from = c(mean_count,lower_count, upper_count)) %>% as.data.table()

comb_sr<- ob_25up_w[location_name %in% countries$super_region_name | location_name=="Global"]
comb_ctry<- ob_25up_w[!location_name %in% countries$super_region_name & !location_name %in% countries$region_name & location_name!="Global"]
comb_ctry <- comb_ctry[order(location_name),]
comb <- rbind(comb_sr,comb_ctry)

comb2 <- merge(comb[Sex=="Male", -c("Sex")], comb[Sex=="Female", -c("Sex")], by=c("location_name"))
comb2 <- comb2[unique(comb$location_name),]

comb2[, ci_1990.x := paste0("(", lower_count_1990.x, "-", upper_count_1990.x, ")")][, ci_1990.y := paste0("(", lower_count_1990.y, "-", upper_count_1990.y, ")")]
comb2[, ci_2021.x := paste0("(", lower_count_2021.x, "-", upper_count_2021.x, ")")][, ci_2021.y := paste0("(", lower_count_2021.y, "-", upper_count_2021.y, ")")]
comb2[, ci_2050.x := paste0("(", lower_count_2050.x, "-", upper_count_2050.x, ")")][, ci_2050.y := paste0("(", lower_count_2050.y, "-", upper_count_2050.y, ")")]
comb2 <- comb2[, c("location_name","mean_count_1990.x","ci_1990.x","mean_count_2021.x","ci_2021.x","mean_count_2050.x","ci_2050.x",
                   "mean_count_1990.y","ci_1990.y","mean_count_2021.y","ci_2021.y","mean_count_2050.y","ci_2050.y")]

write.xlsx(comb2, "FILEPATH")



