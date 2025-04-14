#download data from OSF

library(osfr)

# Authenticate (only if private repo)
Sys.setenv(OSF_PAT = "IqrzYi7u2W7S6DEOmesigZz7AQRndT47ZsEL2fONAsNDSd8Uig09CSgiVbodcTORM9nmRI") 
osf_auth() 

#find project 
project <- osf_retrieve_node("6g3h7")

# List files in the project to see what you can download
files <- osf_ls_files(project)

# Download files (use the file names from 'files')
osf_download(files, path = "data/", conflicts = "overwrite")

getData <- function() {
  
  
  Reach::downloadOSFdata(
    repository = '6g3h7',
    filelist = list(
      'data' = c(
        'demographics.zip',
        'Instructed_all.zip',
        'Instructed_summary.zip',
        'pilot_all.zip',
        'pilot_summary.zip')),
    folder = 'data/',
    overwrite = TRUE,
    unzip = TRUE,
    removezips = TRUE)
  
}
  
