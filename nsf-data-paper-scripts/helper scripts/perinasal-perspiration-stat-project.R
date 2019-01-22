#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
# install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
library(grid)
library(dplyr)


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
concatePath <- function(path1, path2) {
  file.path(path1, path2)
}

findFileAndSetPath <- function(file_dir, file_name_pattern) {
  file_name <- list.files(path=file_dir, pattern=file_name_pattern, recursive=FALSE)
  concatePath(file_dir, file_name)
}

getData <- function(file_dir, file_name_pattern) {
  file_path <- findFileAndSetPath(file_dir, file_name_pattern)
  read.csv(file_path)
}

# get_data_frame <- function(session_name, file_pattern, label_name) {
#   file_path <- findFileAndSetPath(session_name, file_pattern)
#   
#   if(identical(file_path, character(0))) {
#     data_frame <- get_empty_perspiration_df()
#   } else {
#     data_frame <- read.csv(file_path)
#     data_frame$label <- label_name
#   }
#   return(data_frame)
# }

save_image <- function(image_name, plot) {
  image_path <- concatePath(root_image_dir, image_name)
  ggsave(image_path, plot, width=12, height=10)
}

convertToCamelCase <- function(x){
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, "\\."), function(x) paste(capit(x), collapse=""))
}

get_empty_perspiration_df <- function() {
  return(data.frame("Time"=character(0), "Perspiration"=integer(0)))
}

getDataFrameExceptNanPerspiration <- function(df) {
  return(df[!is.na(df$Perspiration),])
}

addNextSecondMeanPerspiration <- function(genuine_df, new_df, i) {
  data_per_sec <- genuine_df %>% filter (i-1 <= Time & Time < i)
  new_df <- rbind(new_df, data.frame(Time = i-1, Perspiration = mean(data_per_sec$Perspiration, na.rm=T)))
  return(new_df)
}

getPerSecondData <- function(base_df) {
  per_sec_df <- get_empty_perspiration_df()
  # per_sec_df <- lapply(1:trunc(max(base_df$Time)), addNextSecondMeanPerspiration, genuine_df = base_df, new_df = per_sec_df)
  for(i in 1 : ceiling(max(base_df$Time))) {
    #####################################
    # per_sec_df <- rbind(per_sec_df,genuine_df %>% filter (i-1 <= Time & Time < i) %>% mutate(Perspiration = mean(Perspiration, na.rm=T), Time=i-1) %>% select(Time, Perspiration) %>% slice(1))
    #####################################
    per_sec_df <- addNextSecondMeanPerspiration(base_df, per_sec_df, i)
  }
  #THIS WILL RETURN DATA FRAME AFTER DISCARDING THE NA ROWS
  return(per_sec_df[!is.na(per_sec_df$Perspiration),])
}


#-------------------------#
#-------Main Program------#
#-------------------------#
data_dir <- "T:/Google Drive/University of Houston/CS - UH/@@Spring 2018/Statistical Methods for Research/@Project-Stats/Code/Project-Statistical-Methods/Data"
root_image_dir <- "T:/Google Drive/University of Houston/CS - UH/@@Spring 2018/Statistical Methods for Research/@Project-Stats/Code/Project-Statistical-Methods/2.Quality Control/Perinasal Persipiration"
grid_image_dir <- "GridGraphs"

setwd(data_dir)
getwd()


baseline_file_pattern <- ".*Baseline..csv"
cutting_file_pattern <- ".*Cutting..csv"
suturing_file_pattern <- ".*Suturing..csv"


#USE APPLY FAMILY
subj_list <- list.dirs(path=data_dir, full.names=FALSE, recursive=FALSE)
subj_list


for(subj_idx in 1 : length(subj_list)) {
  # for(subj_idx in 1 : 1) {
  
  x_max_subj <- 0
  y_max_subj <- 0
  max_legend_subj <- 0
  # max_legend_session_idx <- 1
  
  plot_list <- list()
  df_list <- list()
  session_name_list <- list()
  color_vector_list <- list()
  
  # plot_df <- data.frame(matrix(ncol = 3, nrow = 0))
  # temp <- c("plot_data", "session_name", "legend_color")
  # colnames(df) <- temp
  
  subj_dir <- file.path(data_dir, subj_list[subj_idx])
  session_list <- list.dirs(path=subj_dir, full.names=FALSE, recursive=FALSE)
  session_list
  
  for(sess_idx in 1 : length(session_list)) {
    # for(sess_idx in 1 : 1) {
    
    session_name <- session_list[sess_idx]
    total_legend_session <- 0
    
    # baseline_df <- get_data_frame(session_name, baseline_file_pattern, "Baseline")
    # cutting_df <- get_data_frame(session_name, cutting_file_pattern, "Cutting")
    # suturing_df <- get_data_frame(session_name, suturing_file_pattern, "Sututing")
    
    color_vector <- vector(mode="character", length=0)
    session_path <- file.path(subj_list[subj_idx], session_name)
    
    baseline_file_path <- findFileAndSetPath(session_path, baseline_file_pattern)
    cutting_file_path <- findFileAndSetPath(session_path, cutting_file_pattern)
    suturing_file_path <- findFileAndSetPath(session_path, suturing_file_pattern)
    
    if(identical(baseline_file_path, character(0))) {
      baseline_df <- get_empty_perspiration_df()
    } else {
      baseline_df <- getPerSecondData(read.csv(baseline_file_path))
      baseline_df$label <- "Baseline"
      color_vector <- union(color_vector, c("black"))
      x_max_subj <- max(x_max_subj, max(baseline_df$Time))
      y_max_subj <- max(y_max_subj, max(baseline_df$Perspiration))
      total_legend_session <- total_legend_session + 1
    }
    
    
    if(identical(cutting_file_path, character(0))) {
      cutting_df <- get_empty_perspiration_df()
    } else {
      cutting_df <- getPerSecondData(read.csv(cutting_file_path))
      cutting_df$label <- "Cutting"
      color_vector <- union(color_vector, c("green"))
      x_max_subj <- max(x_max_subj, max(cutting_df$Time))
      y_max_subj <- max(y_max_subj, max(cutting_df$Perspiration))
      total_legend_session <- total_legend_session + 1
    }
    
    
    if(identical(suturing_file_path, character(0))) {
      suturing_df <- get_empty_perspiration_df()
    } else {
      suturing_df <- getPerSecondData(read.csv(suturing_file_path))
      suturing_df$label <- "Suturing"
      color_vector <- union(color_vector, c("red"))
      x_max_subj <- max(x_max_subj, max(suturing_df$Time))
      y_max_subj <- max(y_max_subj, max(suturing_df$Perspiration))
      total_legend_session <- total_legend_session + 1
    }
    
    ## FIND WHICH SESSION HAS THE MAX LEGEND
    ## SAVE THE SESSION INDEX TO PULL OUT THE LEGEND
    if (total_legend_session > max_legend_subj) {
      max_legend_subj <- total_legend_session
      max_legend_session_idx <- sess_idx
    }
    
    grouped_df <- rbind(baseline_df, cutting_df, suturing_df)
    grouped_df
    
    # plot_df <- rbind(plot_df, data.frame(plot_data = grouped_df, 
    #                                      session_name = session_name, 
    #                                      legend_color = color_vector))
    
    
    ## NEED TO CHNAGE THE CODE HERE!!!
    ## KEEPING DATA, SESSION NAME AND SIGNAL COLOR TO DRAW LATER
    df_list[[sess_idx]] <- grouped_df
    session_name_list[[sess_idx]] <- session_name
    color_vector_list[[sess_idx]] <- color_vector
    
    ## THIS IS FOR NO CONFLICT WITH EMPTY DATAFARME ON NEXT ITERATION OF THE SESSION
    rm(baseline_df, cutting_df, suturing_df)
  }
  
  
  ## AFTER GETIING ALL THE DATAFRAME OF ALL SESSIONS OF ONE SUBJECT
  ## DRAWING SINGLE PLOTS AND COMBINING THEM
  for(df_idx in 1 : length(df_list)) {
    
    current_df <- as.data.frame(df_list[df_idx])
    session_name <- unlist(session_name_list[df_idx])
    color_vec <- unlist(color_vector_list[df_idx])
    
    x_lab <- ""
    y_lab <- ""
    
    ## PUTTING X-LABEL FOR THE LAST PLOT ONLY
    if (df_idx == length(df_list)) {
      x_lab <- "Time [s]"
    }
    
    if (nrow(current_df) != 0) {
      
      single_plot <- ggplot(current_df) +
        # geom_line(data=getDataFrameExceptNanPerspiration(current_df), aes(Time, Perspiration, colour=label)) +
        geom_line(aes(Time, Perspiration, colour=label)) +
        theme_bw() +
        xlab(x_lab) +
        ylab(y_lab) +
        xlim(0, x_max_subj) +
        ylim(0, y_max_subj) +
        labs(title=convertToCamelCase(session_name)) +
        scale_colour_manual(values=color_vec, name="",
                            guide=F)
      
      ## SAVING THE PLOTS TO MAKE A GRID GRAPH
      plot_list[[df_idx]] <- single_plot
      
      ## SAVING SINGLE PLOT
      # image_name <- paste(subj_list[subj_idx], "_", session_list[df_idx], ".png", sep="")
      # save_image(image_name, single_plot)
      
    }
  }
  
  
  ##-- PULLING OUT THE LEGEND --##
  current_df <- as.data.frame(df_list[max_legend_session_idx])
  session_name <- unlist(session_name_list[max_legend_session_idx])
  color_vec <- unlist(color_vector_list[max_legend_session_idx])
  
  single_plot <- ggplot(current_df) +
    geom_line(aes(Time, Perspiration, colour=label)) +
    xlim(0, x_max_subj) +
    ylim(0, y_max_subj) +
    labs(title=convertToCamelCase(session_name)) +
    theme(legend.title = element_blank()) +
    scale_colour_manual(values=color_vec, name="",
                        guide=guide_legend(ncol=4))
  
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  mylegend <- g_legend(single_plot)
  
  
  ## GRID GRAPH WITH ALL THE PLOTS FROM EACH SESSION OF A SUBJECT
  ?arrangeGrob
  ?grid.arrange
  
  # total_plot <- length(plot_list)
  # n_col <- floor(sqrt(total_plot))
  
  ## THIS GRID PLOT CONTAINS ALL THE SINGLE PLOT
  grid_plot <- do.call("grid.arrange", c(plot_list, ncol=1))
  
  ## THIS GRID PLOT CONTAINS THE GRID PLOT AND THE LEGEND
  grid_plot_title <- bquote(paste("Perinasal Perspiration [",""^"o","C",""^2,"]: ", .(convertToCamelCase(subj_list[subj_idx]))))
  grid_plot <- grid.arrange(grid_plot, mylegend, nrow=2,heights=c(20, 1), top=textGrob(grid_plot_title, gp=gpar(fontsize=18, font=1)))
  
  ## SAVING GRID PLOT
  image_path <- file.path(grid_image_dir, paste(subj_list[subj_idx], ".pdf", sep=""))
  save_image(image_path, grid_plot)
}