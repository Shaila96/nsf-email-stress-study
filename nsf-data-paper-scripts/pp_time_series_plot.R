#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(readr)
library(dplyr)
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
plots_dir <- 'plots'

data_file_name <- 'full_df_second_phase_filtered.csv'
# grid_plot_title <- bquote(paste('Perinasal Perspiration [',''^'o','C',''^2,']:  QC'[0], ' signal sets'))
grid_plot_title <- bquote(paste('Perinasal Perspiration [',''^'o','C',''^2,']:  QC0', ' signal sets'))
y_axis_label <- bquote(paste('log'[10], '(PP [',''^'o','C',''^2,'])'))

plot_list <- list()



session_color_code <- c('', '', '#229954', '', '', '')


session_atr <- 'all-session'
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

generate_pp_plot <- function() {
  pp_all_df <- read_csv(file.path(data_dir, data_file_name))
  pp_all_df <- pp_all_df[, c(seq(1, 6))]
  # print(str(pp_all_df))
  
  # subj_list <- levels(factor(pp_all_df$Subject))
  
  
  #################### CHECK THIS  ################### 
  pp_all_df <- pp_all_df[complete.cases(pp_all_df), ] %>% filter(Session %in% session_list)
  pp_non_dual_df <- pp_all_df %>% filter(Session != 'DualTask')
  
  max_y <- max(pp_all_df$PP)
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
    subj_no <- session_df %>% distinct(Subject) %>% summarize(n = n())
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
      session_plot <- ggplot(data=session_df, aes(TimeElapsed, PP, group=Subject)) +  ## Without SC Color
        geom_line(alpha=0.7) +
        annotate("text", 
                 x=max_x, 
                 y=max_y, 
                 hjust=1, 
                 vjust=1, 
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
              axis.title.y.right = element_text(angle=0, vjust=0.5),
              legend.position='none'
              ) +
        scale_color_manual(values = c("High"="black", "Low"=session_color_code[3])) +
        scale_y_continuous(trans='log10', 
                           breaks=pretty_breaks(),
                           sec.axis=sec_axis(~.+1, name=get_abbr_session_name(session_name))) +
        # scale_y_continuous(limits=c(0, max_y), 
        #                    sec.axis=sec_axis(~.+1, name=get_session_name(session_name)) +
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
                            top=textGrob(grid_plot_title, gp=gpar(fontsize=10, font=1)))
  
  print(grid_plot)
  
  #---- SAVING GRID PLOT ----#
  # plot_path <- file.path(plots_dir, paste0('pp-time-series-', format(Sys.Date(), format='%m-%d-%y'), '.pdf'))
  plot_path <- file.path(plots_dir, paste0('pp-time-series'))
  save_plot(plot_path, grid_plot)
}




#-------------------------#
#-------Main Program------#
#-------------------------#
generate_pp_plot()





