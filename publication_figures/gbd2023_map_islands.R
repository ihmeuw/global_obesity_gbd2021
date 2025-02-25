################################################################################
## Map Pacific islands to make colored squares for each island
## Description: mapping function that takes the following arguments:
##
##  REQUIRED INPUT:
##    data        ->  A data frame or table with two variables, 'location_id' and 'mapvar'.
##    limits      ->  A vector that defines the bins for the map. This should be
##                    the minimum value, each of the break points in ascending order
##                    and then the maximum value. By default, the bins are currently
##                    assigned such that a value equal to a breakpoint is included in
##                    the bin where that breakpoint is the lower bound.
##
##  OPTIONAL INPUT:
##    sub_nat ->      Prints map with capstone level subnationals("capstone"), topic paper level subnations ("topic"), all subnationals for vetting ("all"), or without subnationals("none").
##                    Defaults to map with capstone level subnationals
##    legend ->      Logical. Prints map with or without legend. Defaults to map with legend.
##    inset ->       Logical. Prints map with or without inset at bottom. Defaults to map without insets
##    labels      ->  Vector of the labels to use in the legend for each group.
##                    If this is not specified, these are assigned automatically
##                    based on the provided values in 'limits'.
##    pattern     ->  A vector, with length equal to the number of bins, of
##                    of patterns to be used for shading in countries. 0 = no
##                    shading (the default), 1=horizontal lines, 2=positively
##                    sloping lines at a 45 degree angle, 3 = vertical lines,
##                    4 = negatively sloping lines at a 45 degree angle.
##    col         ->  Either a string specifying the name of a color palette from
##                    ColorBrewer () or a vector of colors.
##    col.reverse ->  Logical. Should the order of the colors be reversed (this
##                    may be useful when specifying a palette from ColorBrewer)
##    na.color    ->  The color to be used for countries not in the dataset
##                    (defaults to grey)
##    title       ->  A string specifying the title (defaults to "")
##    title.cex   ->  Multiplier for the font size of the title
##    fname       ->  The filename for saving the map. Currently supports '.tif'
##                    '.eps', and '.pdf'. If nothing is specifed, the map is
##                    printed to screen so long as there is not a pdf device
##                    already open (if there is a pdf device already open, the
##                    map is saved to this device; this is to allow for making
##                    pdfs with multiple figures using the onefile=T option in
##                    the pdf function)
##    legend.title -> The title for the legend
##    legend.columns -> The number of columns in the lengend. Defaults to 1 for
##                    7 or less groups and 2 otherwise.
##    legend.cex   -> Multiplier for the size of the legend
##    legend.shift -> Shift the legend (vector of size 2: horizontal shift and
##                    vertical shift)
##  OVERRIDES col:
##    midpoint     -> Set a midpoint of your data to set neutral colors
##    col1         -> If midpoint is set, sets a color scheme for below midopints
##    col2         -> If midpoint is set, sets a color scheme for above midopints
################################################################################


gbd_map_islands <- function(data, limits, sub_nat="capstone", legend=TRUE, inset=TRUE,
                    labels=NULL,
                    pattern=NULL,
                    col="RdYlBu", col.reverse=FALSE, na.color = "grey20",
                    title="",
                    title.cex = 1,
                    island = FALSE,
                    fname = NULL,
                    outdir = NULL,
                    isl_cols = 3,
                    inset_countries = NULL,
                    legend.title=NULL, legend.columns = NULL, legend.cex=1, legend.shift=c(0,0)) {

  ## load libraries
  library(RColorBrewer)
  library(maptools)
  library(ggplot2)
  require(sp)

  ## set default for legend and inset and sub_nat
  if(missing(legend)) {
    legend==TRUE
  }

  if(missing(inset)) {
    inset==TRUE
  }

  if(missing(sub_nat)) {
    sub_nat=="capstone"
  }

  ## test for consistency & give warnings
  if (length(limits)-1 != length(labels) & !is.null(labels)) stop("length(limits) must equal length(labels) + 1")
  if (length(limits)-1 != length(col) & length(col)>1) stop("number of colors does not match number of groups")
  if (length(limits)-1 != length(pattern) & !is.null(pattern)) stop("number of patterns does not match number of groups")
  if (!"location_id" %in% names(data) | !"mapvar" %in% names(data)) stop("data is missing location_id or mapvar variables")
  
  data <- as.data.frame(data)
  data$loc_id <- data$location_id
  data$location_id <- NULL

  ## load shapefile and prep data and inset data
  load(paste('FILEPATH'))
  main_data <- merge(data, map@data[,c("loc_id", "ID")], by="loc_id", sort= TRUE, all=T)
  kmlPolygon(border=.01)


  if(sub_nat=="none") {
    load(paste('FILEPATH'))
    nat_map <- merge(data, nosubmap@data[,c("loc_id", "ID")],by="loc_id", sort= TRUE, all=T)
    kmlPolygon(border=.01)
  }

  if(sub_nat=="topic") {
    load(paste('FILEPATH'))
    topic_map <- merge(data, topicmap@data[,c("loc_id", "ID")],by="loc_id", sort= TRUE, all=T)
    kmlPolygon(border=.01)
  }

  if(sub_nat=="all") {
    load(paste('FILEPATH'))
    all_map <- merge(data, allmap@data[,c("loc_id", "ID")],by="loc_id", sort= TRUE, all=T)
    kmlPolygon(border=.01)
  }

  load(paste('FILEPATH'))
  if(sub_nat=="none") caribbean_central_america_background <- caribbean_central_america_background[caribbean_central_america_background@data$level ==  3,]
  caribbean_central_america_data <- merge(data, caribbean_central_america_background@data[, c("loc_id", "ID")], by = "loc_id", sort = TRUE, all.y=T)
  kmlPolygon(border=.01)

  load(paste('FILEPATH'))
  w_africa_data <- merge(data, w_africa_background@data[, c("loc_id", "ID")], by="loc_id", all.y=TRUE, sort= TRUE)
  kmlPolygon(border=.01)

  load(paste('FILEPATH'))
  if(sub_nat=="none" | sub_nat =="topic") persian_gulf_background <- persian_gulf_background[persian_gulf_background@data$level ==  3,]
  persian_gulf_data <- merge(data, persian_gulf_background@data[, c("loc_id", "ID")], by="loc_id", sort= TRUE, all.y=T)
  kmlPolygon(border=.01)

  load(paste('FILEPATH'))
  balkan_data <- merge(data, balkan_background@data[, c("loc_id", "ID")], by="loc_id", all.y=T, sort= TRUE)
  kmlPolygon(border=.01)

  load(paste('FILEPATH'))
  if(sub_nat=="none") se_asia_background <- se_asia_background[se_asia_background@data$level ==  3,]
  se_asia_data <- merge(data, se_asia_background@data[, c("loc_id", "ID")], by="loc_id", all.y=T, sort= TRUE)
  kmlPolygon(border=.01)

  load(paste('FILEPATH'))
  if(sub_nat=="none" | sub_nat =="topic") n_europe_background <- n_europe_background[n_europe_background@data$level ==  3,]
  n_europe_data <- merge(data, n_europe_background@data[, c("loc_id", "ID")], by="loc_id", all.y=T, sort= TRUE)
  kmlPolygon(border=.01)

  load(paste('FILEPATH'))
  e_mediterranean_data <- merge(data, e_mediterranean_background@data[, c("loc_id", "ID")], by="loc_id", sort= TRUE, all.y=T)
  kmlPolygon(border=.01)


  ## assign colors and patterns
  main_data$color <- main_data$pattern <- NA
  n <- length(limits) - 1

  if (length(col)==1) {
    if (brewer.pal.info[col,1] >= n) mapcolors <- brewer.pal(n, col)
    else mapcolors <- colorRampPalette(brewer.pal(brewer.pal.info[col,1], col))(n)
  } else {
    mapcolors <- col
  }
  if (col.reverse) mapcolors <- mapcolors[n:1]
  if (is.null(pattern)) pattern <- rep(0, n)

  for (g in 1:n) {
    ii <- (main_data$mapvar >= limits[g] & main_data$mapvar <= limits[g+1])
    main_data$color[ii] <- mapcolors[g]
    main_data$pattern[ii] <- pattern[g]
  }

  main_data$color[is.na(main_data$color)] <- na.color
  main_data$pattern[is.na(main_data$pattern)] <- 0

  main_data$density <- ifelse(main_data$pattern==0, 0, 30)
  main_data$angle <- as.numeric(as.character(factor(main_data$pattern, levels=1:4, label=c(0,45,90,135))))

  ### assign colors and patterns for nosubmap
  if(sub_nat=="none") {
    for (g in 1:n) {
      ii <- (nat_map$mapvar >= limits[g] & nat_map$mapvar <= limits[g+1])
      nat_map$color[ii] <- mapcolors[g]
      nat_map$pattern[ii] <- pattern[g]
    }
    
    nat_map$color[is.na(nat_map$color)] <- na.color
    nat_map$pattern[is.na(nat_map$pattern)] <- 0

    nat_map$density <- ifelse(nat_map$pattern==0, 0, 30)
    nat_map$angle <- as.numeric(as.character(factor(nat_map$pattern, levels=1:4, label=c(0,45,90,135))))
  }

  if(sub_nat=="topic") {
    for (g in 1:n) {
      ii <- (topic_map$mapvar >= limits[g] & topic_map$mapvar <= limits[g+1])
      topic_map$color[ii] <- mapcolors[g]
      topic_map$pattern[ii] <- pattern[g]
    }

    topic_map$color[is.na(topic_map$color)] <- na.color
    topic_map$pattern[is.na(topic_map$pattern)] <- 0

    topic_map$density <- ifelse(topic_map$pattern==0, 0, 30)
    topic_map$angle <- as.numeric(as.character(factor(topic_map$pattern, levels=1:4, label=c(0,45,90,135))))
  }

  if(sub_nat=="all") {
    for (g in 1:n) {
      ii <- (all_map$mapvar >= limits[g] & all_map$mapvar <= limits[g+1])
      all_map$color[ii] <- mapcolors[g]
      all_map$pattern[ii] <- pattern[g]
    }

    all_map$color[is.na(all_map$color)] <- na.color
    all_map$pattern[is.na(all_map$pattern)] <- 0

    all_map$density <- ifelse(all_map$pattern==0, 0, 30)
    all_map$angle <- as.numeric(as.character(factor(all_map$pattern, levels=1:4, label=c(0,45,90,135))))
  }
  ## assign colors and patterns and location for inset

  caribbean_central_america_data$color <- caribbean_central_america_data$pattern <- NA
  for (g in 1:n) {
    ii <- (caribbean_central_america_data$mapvar >= limits[g] & caribbean_central_america_data$mapvar <= limits[g+1])
    caribbean_central_america_data$color[ii] <- mapcolors[g]
    caribbean_central_america_data$pattern[ii] <- pattern[g]
  }

  caribbean_central_america_data$color[is.na(caribbean_central_america_data$color)] <- na.color
  caribbean_central_america_data$pattern[is.na(caribbean_central_america_data$pattern)] <- 0

  caribbean_central_america_data$density <- ifelse(caribbean_central_america_data$pattern==0, 0, 30)
  caribbean_central_america_data$angle <- as.numeric(as.character(factor(caribbean_central_america_data$pattern, levels=1:4, label=c(0,45,90,135))))

  balkan_data$color <- balkan_data$pattern <- NA
  for (g in 1:n) {
    ii <- (balkan_data$mapvar >= limits[g] & balkan_data$mapvar <= limits[g+1])
    balkan_data$color[ii] <- mapcolors[g]
    balkan_data$pattern[ii] <- pattern[g]
  }

  balkan_data$color[is.na(balkan_data$color)] <- na.color
  balkan_data$pattern[is.na(balkan_data$pattern)] <- 0

  balkan_data$density <- ifelse(balkan_data$pattern==0, 0, 30)
  balkan_data$angle <- as.numeric(as.character(factor(balkan_data$pattern, levels=1:4, label=c(0,45,90,135))))

  n_europe_data$color <- n_europe_data$pattern <- NA

  for (g in 1:n) {
    ii <- (n_europe_data$mapvar >= limits[g] & n_europe_data$mapvar <= limits[g+1])
    n_europe_data$color[ii] <- mapcolors[g]
    n_europe_data$pattern[ii] <- pattern[g]
  }

  n_europe_data$color[is.na(n_europe_data$color)] <- na.color
  n_europe_data$pattern[is.na(n_europe_data$pattern)] <- 0

  n_europe_data$density <- ifelse(n_europe_data$pattern==0, 0, 30)
  n_europe_data$angle <- as.numeric(as.character(factor(n_europe_data$pattern, levels=1:4, label=c(0,45,90,135))))

  persian_gulf_data$color <- persian_gulf_data$pattern <- NA
  for (g in 1:n) {
    ii <- (persian_gulf_data$mapvar >= limits[g] & persian_gulf_data$mapvar <= limits[g+1])
    persian_gulf_data$color[ii] <- mapcolors[g]
    persian_gulf_data$pattern[ii] <- pattern[g]
  }

  persian_gulf_data$color[is.na(persian_gulf_data$color)] <- na.color
  persian_gulf_data$pattern[is.na(persian_gulf_data$pattern)] <- 0

  persian_gulf_data$density <- ifelse(persian_gulf_data$pattern==0, 0, 30)
  persian_gulf_data$angle <- as.numeric(as.character(factor(persian_gulf_data$pattern, levels=1:4, label=c(0,45,90,135))))

  e_mediterranean_data$color <- e_mediterranean_data$pattern <- NA

  for (g in 1:n) {
    ii <- (e_mediterranean_data$mapvar >= limits[g] & e_mediterranean_data$mapvar <= limits[g+1])
    e_mediterranean_data$color[ii] <- mapcolors[g]
    e_mediterranean_data$pattern[ii] <- pattern[g]
  }

  e_mediterranean_data$color[is.na(e_mediterranean_data$color)] <- na.color
  e_mediterranean_data$pattern[is.na(e_mediterranean_data$pattern)] <- 0

  e_mediterranean_data$density <- ifelse(e_mediterranean_data$pattern==0, 0, 30)
  e_mediterranean_data$angle <- as.numeric(as.character(factor(e_mediterranean_data$pattern, levels=1:4, label=c(0,45,90,135))))

  se_asia_data$color <- se_asia_data$pattern <- NA
  for (g in 1:n) {
    ii <- (se_asia_data$mapvar >= limits[g] & se_asia_data$mapvar <= limits[g+1])
    se_asia_data$color[ii] <- mapcolors[g]
    se_asia_data$pattern[ii] <- pattern[g]
  }

  se_asia_data$color[is.na(se_asia_data$color)] <- na.color
  se_asia_data$pattern[is.na(se_asia_data$pattern)] <- 0

  se_asia_data$density <- ifelse(se_asia_data$pattern==0, 0, 30)
  se_asia_data$angle <- as.numeric(as.character(factor(se_asia_data$pattern, levels=1:4, label=c(0,45,90,135))))

  w_africa_data$color <- w_africa_data$pattern <- NA

  for (g in 1:n) {
    ii <- (w_africa_data$mapvar >= limits[g] & w_africa_data$mapvar <= limits[g+1])
    w_africa_data$color[ii] <- mapcolors[g]
    w_africa_data$pattern[ii] <- pattern[g]
  }

  w_africa_data$color[is.na(w_africa_data$color)] <- na.color
  w_africa_data$pattern[is.na(w_africa_data$pattern)] <- 0

  w_africa_data$density <- ifelse(w_africa_data$pattern==0, 0, 30)
  w_africa_data$angle <- as.numeric(as.character(factor(w_africa_data$pattern, levels=1:4, label=c(0,45,90,135))))

  ####disputed locations
  load(paste('FILEPATH'))
  disputed_data <- merge(data, disputed@data[,c("ADM0_NAME", "ID", "DISP_AREA")], all.y=T)
  kmlPolygon(border=.01)

  load(paste('FILEPATH'))
  disputed_border@data$linetype <- ifelse(disputed_border@data$DISP_AREA==1,2,1)
  disputed_border_non <- disputed_border[disputed_border@data$DISP_AREA==0,]
  kmlPolygon(border=.01)
  disputed_border <-disputed_border[disputed_border@data$DISP_AREA==1,]
  kmlPolygon(border=.01)


  ## generate labels if necessary
  if (is.null(labels)) {
    for (g in 1:n) {
      labels <- c(labels, paste(limits[g], " to ", limits[g+1]))
    }
  }

  # graphics.off()
  par("mar")
  par(mar=c(1,1,1,1))

if(island == TRUE){
  
  
  loc_meta = get_location_metadata(location_set_id = 35, release_id=9)
  
  lids = loc_meta[level > 2 | location_id == 0, location_id]
  #list of island nations we want to include for plotting. 
  if(missing(inset_countries)){
    inset_countries <- c(298, 320,  22, 351,  23,  24,  25, 369, 374, 376, 380,  27,  28, 413,  29, 416,  30)
  }

  inset_data = data.table(main_data)
  inset_data = merge(inset_data, loc_meta[,.(location_id, lancet_label, location_name, location_name_short)], by.x = 'loc_id', by.y = 'location_id')
  inset_data = inset_data[loc_id %in% inset_countries]
  setorder(inset_data,'lancet_label')
  
  # Prep for 2 columns
  if(missing(isl_cols)){
    n_cols = 3
  }else{
    n_cols = isl_cols
  }
  
  inset_n_rows <- ceiling(nrow(inset_data) / n_cols)
  
  inset_data[, row := 1:.N]
  inset_data[,pt_x := 0]
  for(i in seq(1:n_cols)){
    inset_data[row > inset_n_rows * i, pt_x := i * (1/n_cols)]
  }
  
  inset_data[, label_x := pt_x + 0.03 ]
  inset_data[, pt_y := 1 - (((.I - 1) %% inset_n_rows) / inset_n_rows) ]
  
  n <- length(limits) - 1
  
  inset_data$color <- inset_data$pattern <- NA
  for (g in 1:n) {
    ii <- (inset_data$mapvar >= limits[g] & inset_data$mapvar <= limits[g+1])
    inset_data$color[ii] <- g
  }

  inset_data$color = as.character(inset_data$color)
  
  inset_data$color = factor(inset_data$color, levels = as.character(seq(1,n)))
  #Using location name short for plotting. Lancet label and location name are also available in the location_metadata
  inset_keys_plot <- ggplot(data = inset_data) +
    geom_point(aes(x=pt_x, y=pt_y, fill=color), shape=22, size=8) +
    geom_text(aes(x=label_x, y=pt_y, label=lancet_label), hjust=0, size=8) +
    scale_fill_manual('', values = mapcolors, drop = FALSE) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1.05)) +
    theme_minimal() +
    theme(
      axis.text.x=element_blank(), axis.ticks.x=element_blank(),
      axis.text.y=element_blank(), axis.ticks.y=element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      legend.position = 'none'
    ) +
    labs(x=NULL, y=NULL, fill=NULL, title=NULL, subtitle=NULL)

  return(inset_keys_plot)
}

}
