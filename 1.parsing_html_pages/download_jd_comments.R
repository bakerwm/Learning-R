
### ----parse Comments on JD wedsite---------------------------------------
### example url: http://club.jd.com/allconsultations/779369-1-1.html

### ----prepare data-------------------------------------------------------
setwd("c:/Users/bakerwm/Desktop/R_work/get_URL/JD_comments/")
baseurl <- "http://club.jd.com/allconsultations/779369-"

### ----Loop over all the pages: read url----------------------------------
library(RCurl)
### want to parse total pages
num <- 10 # supposed to 1000 
df  <- data.frame()
for (i in 1:num) {
  page <- paste0(baseurl, i, '-1.html')
  if(url.exists(page)) {
    print(paste0(i, ' of ', num, ' : OK'))
    #lines <- readLines(page, warn = FALSE, encoding = "gb2312")
    lines <- readLines(page, warn = FALSE, encoding = "utf-8")
    dd    <- parsePage(lines)
    df    <- rbind(df, dd)
    dd    <- character()
  } else {
    print(paste0(i, ' of ', num, ' : skip, url not exist'))
  }
}

### ----save data.frame to file: for further analysis----------------------
dt <- format(Sys.time(), "%Y%m%d_%H_%M_%S")
ff <- paste0(dt, '.JD_comments.rda')
save(df, file = ff, compress = "xz")
load(ff)

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

### ----Analysis, and presentation ----------------------------------------

load("20150723_06_59_50.JD_comments.rda")

### ----1. date string-----------------------------------------------------
### (1) counts per Day,Week,Month,Year...
### (2) Month vs Year
library(ggplot2)
tt <- as.Date(df$Time)
t1 <- data.frame(do.call(rbind, strsplit(as.character(df$Time), " ", fixed = TRUE)), 
                 stringsAsFactors = FALSE)
th <- data.frame(do.call(rbind, strsplit(t1$X2, ":", fixed = TRUE)), 
                 stringsAsFactors = FALSE)
colnames(th) <- c("hour", "minute", "second")
t2 <- data.frame(do.call(rbind, strsplit(as.character(tt), "-", fixed = TRUE)), 
                 stringsAsFactors = FALSE)
colnames(t2) <- c("year", "month", "day")
df2 <- cbind(df, t2, th)
df2$weekday <- as.factor(weekdays(as.Date(df2$Time)))
df2$date    <- tt

### customized theme()
mytheme <-  theme_bw() +
  theme(axis.line  = element_line(colour="black", size = 0.8),
        axis.text  = element_text(size=16, colour="black"),
        axis.title = element_text(size=18, colour="black", face = "bold"),
        plot.title = element_text(size=24, colour="black", face = "bold"),
        strip.text = element_text(size=16, face="bold"),
        strip.background = element_rect(fill="gray80", colour="black", 
                                        size=0.5) 
        )

p1 <- ggplot(df2, aes(x = day) ) + 
  geom_histogram(aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  facet_grid(year ~ month) + 
  scale_x_discrete(breaks = c("01", "15", "30")) +
  mytheme

### supp: hour everyday, weekday, month, year
### ----hourly----
p1_0 <- ggplot(df2, aes(x = hour)) + 
  geom_histogram(aes(fill = ..count..), binwidth = 1) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  scale_x_discrete(breaks = c("00", "08", "12", "18", "22")) +
  mytheme
p1_0
p1_0 + facet_grid(year ~ month) 

### ----everyday----
p1_1 <- ggplot(df2, aes(x = date)) + 
  geom_histogram(aes(fill = ..count..), binwidth = 1) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  mytheme
p1_1
p1_1 + facet_grid(year ~ month) 

### ----day of month (DOM)----
p1_2 <- ggplot(df2, aes(x = day)) + 
  geom_histogram(aes(fill = ..count..), binwidth = 1) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  scale_x_discrete(breaks = c("01", "10", "20", "30")) +
  mytheme
p1_2
p1_2 + facet_grid(year ~ month) 

### ----weekly----
p1_3 <- ggplot(df2, aes(x = weekday)) + 
  geom_histogram(aes(fill = ..count..), binwidth = 1) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                   labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  mytheme
p1_3
p1_3 + facet_grid(year ~ month) 

### ----monthly----
p1_4 <- ggplot(df2, aes(x = month)) + 
  geom_histogram(aes(fill = ..count..), binwidth = 1) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  mytheme
p1_4
p1_4 + facet_grid(year ~ .) 

### ----yearly----
p1_5 <- ggplot(df2, aes(x = year)) + geom_histogram(aes(fill = ..count..), binwidth = 1) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  mytheme
p1_5

### ----2. Length of Asks and Answers--------------------------------------
### ask
p2_1 <- ggplot(df2, aes(x = nchar(as.character(User_ask)))) + 
  geom_histogram(binwidth = 2) +
  ggtitle("Length of Ask") + 
  xlim(0, 150) +
  mytheme
p2_1
p2_1 + facet_grid(year ~ month)

### answer
p2_2 <- ggplot(df2, aes(x = nchar(as.character(JD_answer)))) + 
  geom_histogram(binwidth = 2) +
  ggtitle("Length of JD reply") + 
  xlim(0, 150) +
  mytheme
p2_2
p2_2 + facet_grid(year ~ month)

### correlation between ask and reply
p2_3 <- ggplot(df2, aes(x = nchar(as.character(User_ask)), 
                        y = nchar(as.character(JD_answer)))) +
  stat_smooth(method = lm, level = 0.99) +
  geom_point(size = 2, position = "jitter") +
  xlim(0, 150) + ylim(0, 300) +
  labs(x = "Length of Ask", y = "Length of Reply") +
  mytheme 
p2_3  
p2_3 + facet_wrap( ~ year)

