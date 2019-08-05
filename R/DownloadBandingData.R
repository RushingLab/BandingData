#' DownloadBandingData


DownloadBandingData <- function(species, year) {
  if(!file.exists(paste0("data-raw/", species, "/", year))){
    dir.create(paste0("data-raw/", species, "/", year))
  }

  if(species == "LAZB"){
    googledrive::drive_download(paste0("~/LAZB/", year, "_Banding_data"), path = paste0("data-raw/LAZB/", year, "/Banding_data.csv"), overwrite = TRUE)
  }

  if(species == "BCCH"){
    googledrive::drive_download(paste0("~/BCCH/", year, "_Banding_data"), path = paste0("data-raw/BCCH/", year, "/Banding_data.csv"), overwrite = TRUE)
  }

  if(species == "PABU"){
    googledrive::drive_download(paste0("~/PABU/", year, "_Banding_data"), path = paste0("data-raw/PABU/", year, "/Banding_data.csv"), overwrite = TRUE)
  }
}
