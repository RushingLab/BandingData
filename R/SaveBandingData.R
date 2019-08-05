#' SaveBandingData


SaveBandingData <- function(species, year) {

  if(species == "LAZB"){
    data <- read.csv(paste0("./data-raw/LAZB/", year, "/Banding_data.csv"),
                     na.strings = c("", "-"),
                     stringsAsFactors = FALSE)
    if(year == 2019) data$RECAP <- "N"

    data$DATE <- suppressWarnings(lubridate::ymd(data$DATE))

    data$TIME[nchar(data$TIME) == 3 & !is.na(data$TIME)] <- paste0("0", data$TIME[nchar(data$TIME) == 3 & !is.na(data$TIME)])

    data$TIME <- gsub("(\\d\\d)(\\d\\d)", "\\1:\\2", data$TIME)

    data$TIME <- suppressWarnings(lubridate::hm(data$TIME))

    data$BAND_PR <- suppressWarnings(as.integer(data$BAND_PR))
    data$BAND_N <- suppressWarnings(as.integer(data$BAND_N))

    data$LAT <- suppressWarnings(as.numeric(data$LAT))
    data$LONG <- suppressWarnings(as.numeric(data$LONG))

    data$FAT <- suppressWarnings(as.integer(data$FAT))
    data$BM <- suppressWarnings(as.integer(data$BM))
    data$WT <- suppressWarnings(as.numeric(data$WT))
    data$BILL_L <- suppressWarnings(as.numeric(data$BILL_L))
    data$BILL_W <- suppressWarnings(as.numeric(data$BILL_W))
    data$BILL_D <- suppressWarnings(as.numeric(data$BILL_D))
    data$WING <- suppressWarnings(as.numeric(data$WING))
    data$TAIL <- suppressWarnings(as.numeric(data$TAIL))
    data$TARS <- suppressWarnings(as.numeric(data$TARS))

    dataOld <- readRDS("data/LAZB_banding_log.rds")

    data <- suppressWarnings(dplyr::bind_rows(dataOld, data))
    data <- dplyr::distinct(data)

    saveRDS(object = data, "data/LAZB_banding_log.rds")
  }

  if(species == "PABU"){
    data <- read.csv(paste0("./data-raw/PABU/", year, "/Banding_data.csv"),
                     na.strings = c("", "-"),
                     stringsAsFactors = FALSE)
    data$DATE <- suppressWarnings(lubridate::ymd(data$DATE))

    data$TIME[nchar(data$TIME) == 3 & !is.na(data$TIME)] <- paste0("0", data$TIME[nchar(data$TIME) == 3 & !is.na(data$TIME)])

    data$TIME <- gsub("(\\d\\d)(\\d\\d)", "\\1:\\2", data$TIME)

    data$TIME <- suppressWarnings(lubridate::hm(data$TIME))

    data$BAND_PR <- suppressWarnings(as.integer(data$BAND_PR))
    data$BAND_N <- suppressWarnings(as.integer(data$BAND_N))

    data$LAT <- suppressWarnings(as.numeric(data$LAT))
    data$LONG <- suppressWarnings(as.numeric(data$LONG))

    data$FAT <- suppressWarnings(as.integer(data$FAT))
    data$BM <- suppressWarnings(as.integer(data$BM))

    data$WT <- suppressWarnings(as.numeric(data$WT))
    data$BILL_L <- suppressWarnings(as.numeric(data$BILL_L))
    data$BILL_W <- suppressWarnings(as.numeric(data$BILL_W))
    data$BILL_D <- suppressWarnings(as.numeric(data$BILL_D))
    data$WING <- suppressWarnings(as.numeric(data$WING))
    data$TAIL <- suppressWarnings(as.numeric(data$TAIL))
    data$TARS <- suppressWarnings(as.numeric(data$TARS))

    dataOld <- readRDS("data/PABU_banding_log.rds")

    data <- suppressWarnings(dplyr::bind_rows(dataOld, data))
    data <- dplyr::distinct(data)

    saveRDS(data, "data/PABU_banding_log.rds")
  }

  if(species == "BCCH"){
    data <- read.csv(paste0("./data-raw/BCCH/", year, "/Banding_data.csv"),
                     na.strings = c("", "-"),
                     stringsAsFactors = FALSE)
    if(year == 2019) data$RECAP <- "N"
    data$DATE <- suppressWarnings(lubridate::ymd(data$DATE))

    data$TIME[nchar(data$TIME) == 3 & !is.na(data$TIME)] <- paste0("0", data$TIME[nchar(data$TIME) == 3 & !is.na(data$TIME)])

    data$TIME <- gsub("(\\d\\d)(\\d\\d)", "\\1:\\2", data$TIME)

    data$TIME <- suppressWarnings(lubridate::hm(data$TIME))

    data$LAT <- suppressWarnings(as.numeric(data$LAT))
    data$LONG <- suppressWarnings(as.numeric(data$LONG))

    data$BAND_PR <- suppressWarnings(as.integer(data$BAND_PR))
    data$BAND_N <- suppressWarnings(as.integer(data$BAND_N))

    data$FAT <- suppressWarnings(as.integer(data$FAT))
    data$BM <- suppressWarnings(as.integer(data$BM))

    data$WT <- suppressWarnings(as.numeric(data$WT))
    data$BILL_L <- suppressWarnings(as.numeric(data$BILL_L))
    data$BILL_W <- suppressWarnings(as.numeric(data$BILL_W))
    data$BILL_D <- suppressWarnings(as.numeric(data$BILL_D))
    data$WING <- suppressWarnings(as.numeric(data$WING))
    data$TAIL <- suppressWarnings(as.numeric(data$TAIL))
    data$TARS <- suppressWarnings(as.numeric(data$TARS))

    dataOld <- readRDS("data/BCCH_banding_log.rds")

    data <- suppressWarnings(dplyr::bind_rows(dataOld, data))
    data <- dplyr::distinct(data)

    saveRDS(data, "data/BCCH_banding_log.rds")
  }
}
