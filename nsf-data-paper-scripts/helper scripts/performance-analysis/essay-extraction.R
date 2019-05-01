#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(XLConnect)
library(xlsx)
library(tibble)



#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
data_dir <- 'nsf-stress-study' 
super_session_pattern <- '^SuperSession$'

testing_df_file_path <- "nsf-stress-study-scripts/@Datasets/testing_df.csv"
essay_df <- tibble()


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
convert_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ',')
}

getAllDirectoryList <- function(directory) {
  return(list.dirs(path=directory, full.names=F, recursive=F))
}

isMatchedString <- function(pattern, str) {
  return(grepl(pattern, str, perl=TRUE))
}

isEmptyDataFrame <- function(df) {
  return(is.data.frame(df) && nrow(df)==0)
}

getMatchedFileNames <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=F))
}

getEssayTibble <- function(subj_name, session, essay_content) {
  return(tibble('Subject'=subj_name, 'Session'=session, 'Essay'=essay_content))
}

addRowForEssay <- function(df, tibble_df) {
  if (isEmptyDataFrame(df)) {
      return(tibble_df)
    } else {
      return(rbind(df, tibble_df))
    }
}

extractEssayForEachSubject <- function(session_dir, subj_name) {
  subj_interface_file_pattern <- paste0('.*-', subj_name, '.xlsx')
  subj_interface_file_name <- getMatchedFileNames(session_dir, subj_interface_file_pattern)
  subj_interface_df <- readWorksheet(XLConnect::loadWorkbook(file.path(session_dir, subj_interface_file_name)), sheet = 'Sheet1')
  
  essay_df <<- addRowForEssay(essay_df, getEssayTibble(subj_name, "WB", subj_interface_df$Essay.Baseline.Content))
  essay_df <<- addRowForEssay(essay_df, getEssayTibble(subj_name, "DT", subj_interface_df$Essay.Dualtask.Content))
}


extractEssays <- function() {
  grp_list <- getAllDirectoryList(data_dir)
  testing_df <- read.csv(testing_df_file_path)[, c('Subject')]
  good_subj_list <- as.list(levels(testing_df))
  
  sapply(grp_list, function(grp_name) {
    # sapply(grp_list[1], function(grp_name) {
    
    grp_dir <- file.path(data_dir, grp_name)
    subj_list <- getAllDirectoryList(grp_dir)
    
    sapply(subj_list, function(subj_name) {
      # sapply(subj_list[3], function(subj_name) {
      # good_subj_list <- list('T003')
      subj_dir <- file.path(grp_dir, subj_name)
      session_list <- getAllDirectoryList(subj_dir)
      session_list <- session_list[isMatchedString(super_session_pattern, session_list)]
      
      sapply(session_list, function(session_name) {
        # sapply(session_list[3], function(session_name) {
        session_dir <- file.path(getwd(), subj_dir, session_name)
        
        tryCatch({
          if(subj_name %in% good_subj_list) {
            extractEssayForEachSubject(session_dir, subj_name)
            # message(paste('Good Subject: ', subj_name))
          } else {
            # message(paste('Not good Subject: ', subj_name))
          }
          
        },
        error=function(cond) {
          
          # write('----------------------------------------------------------', file=log.file, append=TRUE)
          # write(paste0(grp_name, '-', subj_name, '-', session_name, ': ERROR!'), file=log.file, append=TRUE)
          # write(paste0(cond, '\n'), file=log.file, append=TRUE)
          
          message('----------------------------------------------------------')
          message(paste0(grp_name, '-', subj_name, '-', session_name, ': ERROR!'))
          message(paste0(cond, '\n'))
        })
      })
    })
  })

  # essay_df <- as.data.frame(essay_df)
  # row.names(essay_df) <- NULL
  # rownames(essay_df) <- NULL
  # print(class(essay_df))
  # print(rownames(essay_df))
  
  # convert_to_csv(essay_df, "nsf-stress-study-scripts/@Datasets/essay_all.csv")
  write.xlsx(as.data.frame(essay_df), "nsf-stress-study-scripts/@Datasets/essay_all.xlsx")
} 



#-------------------------#
#-------Main Program------#
#-------------------------#
current_dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path)) 
setwd(current_dir)

extractEssays() 


