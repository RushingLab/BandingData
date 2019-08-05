#' CombineBandingData


CombineBandingData <- function() {

    LAZBdata <- readRDS("data/LAZB_banding_log.rds")
    PABUdata <- readRDS("data/PABU_banding_log.rds")
    BCCHdata <- readRDS("data/BCCH_banding_log.rds")

    Alldata <- suppressWarnings(dplyr::bind_rows(LAZBdata, PABUdata, BCCHdata))
    write.csv(Alldata, "data/Master_banding_log.csv")

}
