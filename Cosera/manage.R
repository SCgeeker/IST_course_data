source("D:\\core\\Research\\projects\\rpp\\osfr\\login_osf.R")
## Get osf id of main project
main <- search_osf(title = "Improving your statistical inferences")
## Get osf id of every week
weeks_id <- recurse_node(id = main$id)
## Get meta data of every week
weeks_meta <- search_osf(id = weeks_id)



## Upload week 7 files
## Switch to week 7 folder
setwd("D:\\core\\workout\\Practice\\Improving your statistical inferences\\week07")
## Upload the file
upload_file(id = weeks_meta[grep("Week 7",weeks_meta$title),"id"], filename = "7-3.vtt")
