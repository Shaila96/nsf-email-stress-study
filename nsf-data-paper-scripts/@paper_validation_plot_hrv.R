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

data_file_name <- 'hrv.csv'
# grid_plot_title <- bquote(paste('QC1', ' signal sets'))
# y_axis_label <- bquote(paste('log'[10], '(HRV [IBI])'))
# y_axis_label <- 'HRV [IBI]'

plot_df <- tibble()
plot_list <- list()



# session_color_code <- c('', '', '#229954', '', '', '')


# session_atr <- 'all-session'
session_list <- c('RestingBaseline', 'BaselineWriting', 'StressCondition', 'Presentation', 'DualTask')
# session_list <- c('StressCondition')

# session_atr <- 'non-dual-session'
# session_list <- c('RestingBaseline', 'BaselineWriting', 'StressCondition', 'Presentation')

# session_atr <- 'dual-session'
# session_list <- c('DualTask')







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









#---- Add one space if it finds any CamelCase ----#
get_session_name <- function(session_name) {
  if (session_name == 'BaselineWriting') {
    return('Single Task')
  } else if (session_name == 'StressCondition') {
    return('Priming')
  }
  return(gsub("([a-z])([A-Z])", "\\1 \\2", session_name))
}

get_abbr_session_name <- function(session_name) {
  if (session_name=='RestingBaseline') {
    return('RB')
  } else if (session_name=='BaselineWriting') {
    return('ST')
  } else if (session_name == 'StressCondition') {
    return('PM')
  } else if (session_name == 'DualTask') {
    return('DT')
  } else if (session_name == 'Presentation') {
    return('PR')
  }
  return(gsub("([a-z])([A-Z])", "\\1 \\2", session_name))
}

generate_hrv_plot <- function() {
  pp_all_df <- read_csv(file.path(data_dir, tamu_dir, data_file_name))
  # pp_all_df <- pp_all_df[, c(seq(1, 6))]
  # print(str(pp_all_df))
  
  pp_all_df <- pp_all_df %>% 
    rename(Subject='Participant ID',
           Condition=Group,
           Session=Treatment,
           CovertedTime=Time,
           Task=TaskMarkers,
           N.EDA=EDA,
           HR='Chest HR',
           N.HR='Wrist HR') %>% 
    mutate(Session = recode(Session,
                            'RB' = 'RestingBaseline',
                            'ST' = 'BaselineWriting',
                            'PM' = 'StressCondition',
                            'DT' = 'DualTask',
                            'PR' = 'Presentation'),
           Task = recode(Task, 'Report' = 'Essay')) %>% 
    select(c('Subject', 'Condition', 'Session', 'CovertedTime', 'TimeElapsed', 'HRV'))
  message(str(pp_all_df))
  
  # subj_list <- levels(factor(pp_all_df$Subject))
  
  
  #################### CHECK THIS  ################### 
  pp_all_df <- pp_all_df[complete.cases(pp_all_df), ] %>% filter(Session %in% session_list)
  pp_non_dual_df <- pp_all_df %>% filter(Session != 'DualTask')
  
  max_y <- max(pp_all_df$HRV)
  max_x <- max(pp_non_dual_df$TimeElapsed)
  
  # subj_no_annot_x_pos = max_x-max_x/40
  # subj_no_annot_y_pos = max_y-max_y/8
  # print(min(pp_all_df$PP))
  # print(max(pp_all_df$PP))

  for(sess_idx in 1 : length(session_list)) {
    session_name <- session_list[sess_idx]
    session_df <- pp_all_df %>% 
      filter(Session==session_name) %>% 
      mutate(StressCondition=case_when(is_match(Condition, 'H') ~ 'High',
                                       is_match(Condition, 'L') ~ 'Low'
                                       ))
    # print(str(session_df))
    
    if (session_name == 'DualTask') {
      max_x <- max(session_df$TimeElapsed)
    }
    

    #---- SETUPPING FOR PLOTS ----#
    # print(nrow(session_df %>% distinct(Subject)))
    # subj_no <- session_df %>% distinct(Subject) %>% summarize(n = n())
    subj_no <- nrow(session_df %>% distinct(Subject))
    subj_no_annot <- paste("n =", subj_no)
    
    x_axis_label <- ''
    
    #---- PUTTING X-LABEL FOR THE LAST PLOT ONLY ----#
    if (sess_idx == length(session_list)) {
      x_axis_label <- 'Time [s]'
    }
    
    if (nrow(session_df) != 0) {
      
      
      ###########################################################################
      ## With SC Color
      ###########################################################################
      # if (session_name == 'StressCondition') {
      #   session_plot <- ggplot(data=session_df, aes(TimeElapsed, PP, group=Subject, colour=StressCondition))
      # } else {
      #   session_plot <- ggplot(data=session_df, aes(TimeElapsed, PP, group=Subject))
      # }
      ###########################################################################
      
      # session_plot <- session_plot +                                                ## With SC Color
      session_plot <- ggplot(data=session_df, aes(TimeElapsed, HRV, group=Subject)) +  ## Without SC Color
        geom_line(alpha=0.7) +
        annotate("text", 
                 x=max_x, 
                 y=max_y, 
                 hjust=1, 
                 vjust=1, 
                 size=5,
                 label=subj_no_annot,
                 fontface = 'italic')
      
      if (session_name != 'DualTask') {
        session_plot <- session_plot +
          theme_bw() +
          theme(axis.line = element_line(colour = "black"))
      }
      
      session_plot <- session_plot +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y.right = element_blank(),
              axis.ticks.y.right = element_blank(),
              axis.title.y.right = element_text(angle=0, vjust=0.5, face='bold'),
              text=element_text(size=14),
              axis.text.x=element_text(size=16),
              axis.text.y=element_text(size=12),
              # axis.title=element_text(size=20),
              # strip.text.x = element_text(size = 20),
              legend.position='none'
              ) +
        # scale_color_manual(values = c("High"="black", "Low"=session_color_code[3])) +
        # scale_y_continuous(trans='log10',
        #                    breaks=pretty_breaks(),
        #                    sec.axis=sec_axis(~.+1, name=get_abbr_session_name(session_name))) +
        scale_y_continuous(limits=c(0, max_y),
                           sec.axis=sec_axis(~.+1, name=get_abbr_session_name(session_name))) +
        xlim(0, max_x) +
        xlab(x_axis_label) +
        ylab(y_axis_label)

      

      
      # print(session_plot)
   
      #---- SAVING THE PLOTS IN A LIST TO MAKE A GRID GRAPH ----#
      plot_list[[sess_idx]] <- session_plot
    }
  }
  
  
  #----------------------------------------------------------------#
  #---- MAKING GRID GRAPH WITH ALL THE PLOTS FROM EACH SESSION ----#
  #----------------------------------------------------------------#
  
  #---- THIS GRID PLOT CONTAINS ALL THE SINGLE PLOT----#
  grid_plot <- do.call('grid.arrange', c(plot_list, ncol=1))
  grid_plot <- grid.arrange(grid_plot, 
                            top=textGrob(grid_plot_title, gp=gpar(fontsize=16, font=1)))
  
  print(grid_plot)
  
  #---- SAVING GRID PLOT ----#
  # plot_path <- file.path(plots_dir, paste0('pp-time-series-', format(Sys.Date(), format='%m-%d-%y'), '.pdf'))
  plot_path <- file.path(plots_dir, paste0('hrv-time-series'))
  save_plot(plot_path, grid_plot)
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

make_data_set <- function() {
  hrv_df <- read_csv(file.path(data_dir, tamu_dir, data_file_name)) %>% 
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
      # append(sign, get_significance(t.test(temp_comparison_df$Value)$p.value))
    }
    
    print(sign)
    
    # temp_comparison_df <- temp_hrv_df %>% 
    #   group_by(Comparison) %>% 
    #   mutate(Significance=get_significance(t.test(temp_comparison_df)$p.value))
    
    
    gg <- ggplot(temp_hrv_df, aes(x = Comparison, y = Value)) + 
      geom_boxplot() + 
      labs(title = figure_out_title(group), y = 'HRV') +
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
    print(gg)
    
    plot_list[[length(plot_list) + 1]] <<- gg
  }
  
  
  
  
  grid_plot <- do.call("grid.arrange", c(plot_list, ncol=2))
  
  # plot_path <- file.path(plots_dir, paste0(tolower(signal_name), '-validation-plot-', format(Sys.Date(), format='%m-%d-%y'), '.pdf'))
  plot_path <- file.path(plots_dir, 'hrv-validation-plot')
  save_plot(plot_path, grid_plot)

}



#-------------------------#
#-------Main Program------#
#-------------------------#
make_data_set()
# generate_hrv_validation_plot()





