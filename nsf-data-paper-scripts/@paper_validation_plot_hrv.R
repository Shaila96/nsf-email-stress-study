#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(directlabels)
library(scales)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

source(file.path(dirname(current_dir), "@common_functions.R"))

data_dir <- 'data'
tamu_dir <- 'data-from-tamu'
plots_dir <- 'plots'

hrv_file_name <- 'hrv.csv'

plot_df <- tibble()
plot_list <- list()


session_list <- c('RestingBaseline', 'BaselineWriting', 'StressCondition', 'Presentation', 'DualTask')
# session_list <- c('StressCondition')



#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
print_msg <- function(df) {
  print(df)
  message(df)
}

save_plot <- function(plot_name, plot) {
  plot_path <- file.path(current_dir, paste0(plot_name, '.png'))
  ggsave(plot_path, plot, width=10, height=10)
  
  plot_path <- file.path(current_dir, paste0(plot_name, '.pdf'))
  ggsave(plot_path, plot, device=cairo_pdf, width=10, height=10)
}

convert_to_csv <- function(df, file_name) {
  write.table(df, file = file.path(file_name), row.names=F, sep = ',')
}

figure_out_title <- function(group) { 
  if (group == 'BH') {
    return('Batch High Group/BH')
  } else if (group == 'BL') {
    return('Batch Low Group/BL')
  } else if (group == 'IH') {
    return('Continual High Group/CH')
  } else if (group == 'IL') {
    return('Continual Low Group/CL')
  }  
} 

get_significance <- function(p_value) { 
  if (p_value > 0.05) { 
    return(" ") 
  } else if (p_value <= 0.001) { 
    return("***") 
  } else if (p_value <= 0.01) { 
    return("**") 
  } else if (p_value <= 0.05) { 
    return("*") 
  } 
} 

give.n <- function(x) { 
  return(c(y=-Inf, vjust = -1, label=length(x))) 
} 

plot_rmssd <- function() {
  hrv_df <- read_csv(file.path(data_dir, tamu_dir, hrv_file_name)) %>% 
    rename(Group=Treatment)
  print(str(hrv_df))
  
  hrv_df <- hrv_df %>% 
    spread(Session, RMSSD) %>% 
    mutate(WB.RB = ST - RB, 
           SC.RB = PM - RB, 
           DT.RB = DT - RB, 
           P.RB = PR - RB) %>% 
    gather(Comparison, Value, -Participant, -Group,
           -RB, -ST, -PM, -DT, - PR) %>% 
    filter(!is.na(Comparison))
  
  print(str(hrv_df))
  # convert_to_csv(hrv_df, file.path(data_dir, tamu_dir, 'hrv_validation_plot_df.csv'))
  
  comparison_levels <- c( "WB.RB", "SC.RB", "DT.RB", "P.RB")
  for (group in c('BH', 'BL', 'IH', 'IL')) {
    
    temp_hrv_df <- hrv_df %>% 
      filter(Group==group)
    
    if (group == "IL" || group == "BL") {
      break_cond <- "RV-RB"
      plot_margin_left <- 1.5
    } else {
      break_cond <- "Stroop-RB"
      plot_margin_left <- 0.5
    }
    
    labels_vec <- gsub(".", "-", comparison_levels, fixed = TRUE)
    # print(levels(factor(hrv_df$Comparison)))
    labels_vec <- replace(labels_vec, labels_vec=="SC-RB", break_cond)
    labels_vec <- replace(labels_vec, labels_vec=="WB-RB", "ST-RB")
    labels_vec <- replace(labels_vec, labels_vec=="P-RB", "PR-RB")
    
    sign <- c()
    for (comparison in comparison_levels) {
      temp_comparison_df <- temp_hrv_df %>% 
        filter(Comparison==comparison)
      print(get_significance(t.test(temp_comparison_df$Value)$p.value))
      sign <- c(sign, get_significance(t.test(temp_comparison_df$Value)$p.value))
    }
    
    gg <- ggplot(temp_hrv_df, aes(x = Comparison, y = Value)) + 
      geom_boxplot() + 
      labs(title = figure_out_title(group), y = expression(Delta~'RMSSD [ms]')) +
        theme_bw(base_size = 18) +
        theme(axis.title.x = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x=element_text(size=16, face='bold'),
              plot.margin = unit(c(0.5, 0.5, 0.5, plot_margin_left), "lines"),  ##top, right, bottom, left
              axis.line = element_line(colour = "black")) +
        geom_hline(yintercept=0, linetype="dashed", color = "red", alpha = 0.6, size=1) +
        scale_x_discrete(labels=labels_vec) +
        stat_summary(fun.y = mean, color = "darkred", geom = "point", shape = 3, size = 4, show_guide = FALSE) +
        stat_summary(fun.data = give.n, geom = "text", size = 6) +
        scale_y_continuous(expand = c(0.15, 0, 0.15, 0)) +
        annotate("text", x=1, y=Inf, label= sign[1], vjust = 1.2, size = 10) +
        annotate("text", x=2, y=Inf, label= sign[2], vjust = 1.2, size = 10) +
        annotate("text", x=3, y=Inf, label= sign[3], vjust = 1.2, size = 10) +
        annotate("text", x=4, y=Inf, label= sign[4], vjust = 1.2, size = 10)
    
    plot_list[[length(plot_list) + 1]] <<- gg
  }
  
  grid_plot <- do.call("grid.arrange", c(plot_list, ncol=2))
  plot_path <- file.path(plots_dir, 'hrv-validation-plot')
  save_plot(plot_path, grid_plot)
}


plot_nn <- function() {
  
}



#-------------------------#
#-------Main Program------#
#-------------------------#
# plot_rmssd()
plot_nn()





