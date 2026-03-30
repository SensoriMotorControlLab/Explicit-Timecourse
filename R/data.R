#download data from OSF

library(osfr)

#find project 

#downloadData <- function() {
  #project <- osf_retrieve_node("6g3h7")
  #files <- osf_ls_files(project)
 # osf_download(files, path = "data/", conflicts = "overwrite")
#}

getData <- function() {
  
  Reach::downloadOSFdata(
    repository = '6g3h7',
    filelist = list(
      'data/' = c(
        'demographics.zip',
        'Instructed_summary.zip',
       'Group_two_summary.zip',
       'LearningClassifier.zip'
        )),
    folder = 'data/',
    overwrite = TRUE,
    unzip = TRUE,
    removezips = TRUE)
}
  
