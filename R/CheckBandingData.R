#' CheckBandingData


CheckBandingData <- function(species, year) {
  op <- options(warn = 1)
  on.exit(options(op))
  if(species == "LAZB"){
    data <- read.csv(paste0("./data-raw/LAZB/", year, "/Banding_data.csv"),
                     na.strings = c("", "-"),
                     stringsAsFactors = FALSE)
    data$DATE <- lubridate::ymd(data$DATE)

    data$TIME[nchar(data$TIME) == 3 & !is.na(data$TIME)] <- paste0("0", data$TIME[nchar(data$TIME) == 3 & !is.na(data$TIME)])

    data$TIME <- gsub("(\\d\\d)(\\d\\d)", "\\1:\\2", data$TIME)

    data$TIME <- lubridate::hm(data$TIME)

    data$WT <- as.numeric(data$WT)
    data$BILL_L <- as.numeric(data$BILL_L)
    data$BILL_W <- as.numeric(data$BILL_W)
    data$BILL_D <- as.numeric(data$BILL_D)
    data$WING <- as.numeric(data$WING)
    data$TAIL <- as.numeric(data$TAIL)
    data$TARS <- as.numeric(data$TARS)

    if(!all(data$SPECIES %in% c("LAZB", "BALO"))){
      errors <- paste((which(!(data$SPECIES %in% c("LAZB", "BALO")))), collapse=", ")
      print(paste("STOP: Check species code(s): ", errors))
    }
  }

  if(species == "PABU"){
    data <- read.csv(paste0("./data-raw/PABU/", year, "/Banding_data.csv"),
                     na.strings = c("", "-"),
                     stringsAsFactors = FALSE)
    data$DATE <- lubridate::ymd(data$DATE)

    data$TIME[nchar(data$TIME) == 3 & !is.na(data$TIME)] <- paste0("0", data$TIME[nchar(data$TIME) == 3 & !is.na(data$TIME)])

    data$TIME <- gsub("(\\d\\d)(\\d\\d)", "\\1:\\2", data$TIME)

    data$TIME <- lubridate::hm(data$TIME)

    data$WT <- as.numeric(data$WT)
    data$BILL_L <- as.numeric(data$BILL_L)
    data$BILL_W <- as.numeric(data$BILL_W)
    data$BILL_D <- as.numeric(data$BILL_D)
    data$WING <- as.numeric(data$WING)
    data$TAIL <- as.numeric(data$TAIL)
    data$TARS <- as.numeric(data$TARS)

    if(!all(data$SPECIES %in% c("PABU", "BALO"))){
      errors <- paste((which(!(data$SPECIES %in% c("PABU", "BALO")))), collapse=", ")
      print(paste("STOP: Check species code(s): ", errors))
    }
  }

  if(species == "BCCH"){
    data <- read.csv(paste0("./data-raw/BCCH/", year, "/Banding_data.csv"),
                     na.strings = c("", "-"),
                     stringsAsFactors = FALSE)
    data$DATE <- lubridate::ymd(data$DATE)

    data$TIME[nchar(data$TIME) == 3 & !is.na(data$TIME)] <- paste0("0", data$TIME[nchar(data$TIME) == 3 & !is.na(data$TIME)])

    data$TIME <- gsub("(\\d\\d)(\\d\\d)", "\\1:\\2", data$TIME)

    data$TIME <- lubridate::hm(data$TIME)

    data$WT <- as.numeric(data$WT)
    data$BILL_L <- as.numeric(data$BILL_L)
    data$BILL_W <- as.numeric(data$BILL_W)
    data$BILL_D <- as.numeric(data$BILL_D)
    data$WING <- as.numeric(data$WING)
    data$TAIL <- as.numeric(data$TAIL)
    data$TARS <- as.numeric(data$TARS)

    if(!all(data$SPECIES %in% c("BCCH", "BALO"))){
      errors <- paste((which(!(data$SPECIES %in% c("PABU", "BALO")))), collapse=", ")
      print(paste("STOP: Check species code(s): ", errors))
    }
  }

  ###########################################
  ### Check date (year, month, and day) ----
  ###########################################

  if(!all(lubridate::year(data$DATE) %in% year)){
    errors <- paste(which(!(lubridate::year(data$DATE) %in% year)), collapse=", ")
    print(paste0("STOP: Check year(s): ", errors))
  }

  if(species == "LAZB" | species == "PABU"){
    if(!all(lubridate::month(data$DATE) %in% 4:9)){
      errors <- paste(which(!(lubridate::month(data$DATE) %in% 4:9)), collapse=", ")
      print(paste0("STOP: Check month(s): ", errors))
    }
  }

  if(species == "BCCH"){
    if(!all(lubridate::month(data$DATE) %in% 1:12)){
      errors <- paste(which(!(lubridate::month(data$DATE) %in% 1:12)), collapse=", ")
      print(paste0("STOP: Check month(s): ", errors))
    }
  }

    if(!all(lubridate::day(data$DATE) %in% 1:31)){
      errors <- paste(which(!(lubridate::day(data$DATE) %in% 1:31)), collapse=", ")
      print(paste0("STOP: Check day(s): ", errors))
    }


  ###########################################
  ### Check state ----
  ###########################################

   if(species == "LAZB"| species == "BLRF"| species == "BCCH"){
     if(!all(data$STATE %in% "UT")){
       errors <- paste(which(!(data$STATE %in% "UT")), collapse=", ")
       print(paste0("STOP: Check STATE(s): ", errors))
     }
   }

  if(species == "PABU"){
    if(!all(data$STATE %in% c("NC", "SC", "GA", "FL"))){
      errors <- paste(which(!(data$STATE %in% c("NC", "SC", "GA", "FL"))), collapse=", ")
      print(paste0("STOP: Check STATE(s): ", errors, " (possible values include NC, SC, GA, FL"))
    }
  }

  ###########################################
  ### Check time (hour and minutes) ----
  ###########################################

  if(!all(lubridate::hour(data$TIME) %in% 1:24)){
    errors <- paste(which(!(lubridate::hour(data$TIME) %in% 1:24)), collapse=", ")
    print(paste0("STOP: Check hour(s): ", errors))
  }

  if(!all(lubridate::minute(data$TIME) %in% 0:59)){
    errors <- paste(which(!(lubridate::minute(data$TIME) %in% 0:59)), collapse=", ")
    print(paste0("STOP: Check minute(s): ", errors))
  }

  ###########################################
  ### Check locations (year, month, and day) ----
  ###########################################

  if(species == "LAZB"|species=="BCCH"){
    if(!all(data$LOC %in% c("CC", "DC", "CG", "TG", "JQL"))){
      errors <- paste(which(!(data$LOC %in% c("CC", "DC", "CG", "TG", "JQL"))), collapse=", ")
      print(paste0("STOP: Check LOC(s): ", errors, " (possible values include CC, DC, CG, TG, JQL)"))
    }
  }

  if(species == "PABU"){
    if(!all(data$LOC %in% c("BHI", "SPIS", "LTSP", "LSSI", "BFT"))){
      errors <- paste(which(!(data$LOC %in% c("NC", "SC", "GA", "FL"))), collapse=", ")
      print(paste0("STOP: Check LOC(s): ", errors, " (possible values include BHI, SPIS, LTSP, LSSI, BFT)"))
    }
  }



  ###########################################
  ### Check bands (prefix, number) ----
  ###########################################

   if(species == "PABU"){
    if(!all(data$BAND_PR %in% c(2521, 2811, 2631, 2020, 1831))){
      errors <- paste(which(!(data$BAND_PR %in% c(2521, 2811, 2020, 2631))), collapse=", ")
      print(paste0("STOP: Check BAND_PR(s): ", errors))
    }
   }

  if(species == "LAZB"){
    if(!all(data$BAND_PR %in% c(2811))){
      errors <- paste(which(!(data$BAND_PR %in% c(2811))), collapse=", ")
      print(paste0("STOP: Check BAND_PR(s): ", errors))
    }
  }

  if(species == "BCCH"){
    if(!all(data$BAND_PR %in% c(2870))){
      errors <- paste(which(!(data$BAND_PR %in% c(2870))), collapse=", ")
      print(paste0("STOP: Check BAND_PR(s): ", errors))
    }
  }

  if(species == "PABU"|species == "LAZB"){
    if(!all(if(data$BAND_PR == 2811) data$BAND_N %in% 18101:18599)){
      errors <- paste(which(!(if(data$BAND_PR == 2811) data$BAND_N %in% 18101:18599)), collapse=", ")
      print(paste0("STOP: Check BAND_N(s): ", errors))
    }
  }

  if(species == "BCCH"){
    if(!all(data$BAND_N %in% 35201:35300)){
      errors <- paste(which(!(data$BAND_N %in% 35201:35300)), collapse=", ")
      print(paste0("STOP: Check BAND_N(s): ", errors))
    }
  }

  ###########################################
  ### Check age and sex ----
  ###########################################

    if(!all(data$AGE %in% c("ASY", "SY", "AHY", "HY"))){
      errors <- paste(which(!(data$AGE %in% c("ASY", "SY", "AHY", "HY"))), collapse=", ")
      print(paste0("STOP: Check AGE(s): ", errors, " (possible values: ASY, SY, AHY, HY)"))
    }

   if(!all(data$SEX %in% c("M", "F", "U"))){
     errors <- paste(which(!(data$SEX %in% c("M", "F", "U"))), collapse=", ")
     print(paste0("STOP: Check SEX(s): ", errors, " (possible values: M, F, U)"))
   }

  ###########################################
  ### Check RFID ----
  ###########################################
  if(!all(nchar(data$RFID) == 10)){
    errors <- paste(which(!(nchar(data$RFID) == 10)), collapse=", ")
    print(paste0("STOP: Check RFID(s): ", errors, " (some ID's contain less than 10 characters)"))
  }

  states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  mp <- ggplot() +
         geom_sf(data = states, fill = NA, color = "grey25") +
         coord_sf() +
         geom_point(data = data, aes(x = as.numeric(LONG), y = as.numeric(LAT)), size = 3, color = "red") +
         theme_minimal() +
         theme(axis.title = element_blank())
  p <- ggplot(data, aes(x = WT)) + geom_histogram(binwidth = 0.25) + theme_minimal()
  q <- ggplot(data, aes(x = BILL_D)) + geom_histogram(binwidth = 0.25) + theme_minimal()
  r <- ggplot(data, aes(x = BILL_W)) + geom_histogram(binwidth = 0.25) + theme_minimal()
  s <- ggplot(data, aes(x = BILL_L)) + geom_histogram(binwidth = 0.25) + theme_minimal()
  u <- ggplot(data, aes(x = WING)) + geom_histogram(binwidth = 1) + theme_minimal()
  v <- ggplot(data, aes(x = TAIL)) + geom_histogram(binwidth = 1) + theme_minimal()
  w <- ggplot(data, aes(x = TARS)) + geom_histogram(binwidth = 0.25) + theme_minimal()

  print(cowplot::plot_grid(mp, p, q, r, s, u, v, w, nrow= 2))
}
