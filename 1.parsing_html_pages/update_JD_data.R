
### ----Update the JD data-------------------------------------------------
### This script will update the df (data.frame), reading the lastest pages
### (depending on the days between the script running and the date last 
### update, esimate the average records of exist dataset) calculate by the
### following formula: 
### pages = 2 * average_records_per_day (old) * days_to_last_update / 10
###
### 1. move the old 20150101_01_01_01.JD_comments.rda to "old_version"
### 2. create the new *.rda file. (data.frame, df)

### ----parse Comments on JD wedsite---------------------------------------
### example url: http://club.jd.com/allconsultations/779369-1-1.html

######################
### Update dataset ###
######################
update_jd_comments <- function(dir = "./", ndays = 7, updateAll = FALSE) {
  if(! isTRUE(file.info(dir)$isdir)) {
    stop(paste0("Input dir is not exists: ", dir))
  }
  setwd(dir)
  ### ----estimate the days to last update, and how many pages to read-------
  ### parsing last version of data
  fn  <- list.files("./", "*.rda$")
  if(length(fn) == 0) stop("Find 0 *.rda file in current directory")
  f   <- file.info(fn[1])$mtime
  dd  <- difftime(Sys.Date(), f, units = "days")
  dd2 <- round(as.numeric(dd), 2)
  ###
  if(dd2 < ndays && ! updateAll) {
    warning("Too close to last update: < 7 days.")
  }else {
    ### ----load old data, guess how many pages to read
    load(fn[1]) # load df (data.frame)
    last_recs <- nrow(df)
    ### move old *.rda files to old_versions
    mv_to_old <- paste0("old_version", "/", basename(fn))
    my_mv(from = fn, to = mv_to_old)
    p2r <- 2 * ceiling(as.numeric(dd)) * 10 / avg_recs(df) # pages_to_read
    if(updateAll) p2r <- 1000 ### read 1000 pages
    ### ----Parsing a few pages, expected to all the newest data---------------
    ### ----Loop over all the pages: read url----------------------------------
    #library(RCurl)
    num <- p2r
    dn  <- data.frame()
    baseurl <- "http://club.jd.com/allconsultations/779369-"
    for (i in 1:num) {
      page <- paste0(baseurl, i, '-1.html')
      #  if(url.exists(page)) { # someting it is not stable
      print(paste0(i, ' of ', num, ' : OK'))
      lines <- readLines(page, warn = FALSE, encoding = "utf-8")
      dd <- parsePage(lines)
      dn <- rbind(dn, dd)
      dd <- character()
      Sys.sleep(0.5)
      #  } else {
      #    print(paste0(i, ' of ', num, ' : skip, url not exist'))
      #  }
    }
    ### ----save data.frame to file: for further analysis--------------------
    df <- rbind(df, dn)
    df <- unique(df)
    curr_recs <- nrow(df)
    ### save data.frame object ot newfile
    currdate    <- format(Sys.time(), "%Y%m%d_%H_%M_%S")
    update_file <- paste0(currdate, '.JD_comments.rda')
    save(df, file = update_file, compress = "xz")
    ###
    print(paste0("Previous records: ", last_recs))
    print(paste0("Find Records: ", curr_recs))
    print(paste0("Update : ", curr_recs - last_recs))
  }
}
### ----Function, parse content from one page------------------------------
parsePage <- function(x) {
  #x     <- iconv(x, from = "gb2312", to = "utf-8")
  uid   <- x[grep("class=\"r_info\"", x) + 1]
  uid   <- gsub("\\t+|\\s+", "", uid)
  ###
  uask  <- x[grep("class=\"ask\"", x) + 3]
  uask  <- gsub("\\t+|\\s+|\\t+$|\\s+$", "", uask)
  ###
  utime <- x[grep("em id=\"level\"", x)]
  ut1   <- do.call(rbind, strsplit(utime, ";", fixed=TRUE))
  ut2   <- gsub("<.*>", "", ut1[, 9], perl = TRUE)
  ###
  jdans <- x[grep("class=\"answer\"", x) + 4]
  jdans <- gsub("^\\t+|^\\s+|\\t+$|\\s+$", "", jdans, perl = TRUE)
  ###
  df    <- cbind(uid, uask, jdans, ut2)
  df    <- as.data.frame(df)
  names(df) <- c("User_id", "User_ask", "JD_answer", "Time")
  return(df)
}
### ----Calculate the average records per day------------------------------
avg_recs <- function(x) {
  #x <- df[1:10, ]
  xa <- data.frame(do.call(rbind, strsplit(as.character(x$Time), " ")))
  xdays <- length(unique(xa[, 1]))
  xavg <- ceiling(nrow(x) / xdays)
  #rm(xa, xdays)
  return(xavg)
}

### ----Move files to new folder-------------------------------------------
my_mv <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}

### ----END OF FILE--------------------------------------------------------
