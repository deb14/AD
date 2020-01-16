raw_df <- read.csv(".././data/st_with_zeros.csv",stringsAsFactors=FALSE)
raw_df$timestamp <- as.POSIXct(raw_df$timestamp, tz="UTC")
raw_df$timestamp<-with_tz(raw_df$timestamp,"America/Chicago")


# raw_df$timestamp <- as.POSIXct(raw_df$timestamp, format = "%m/%d/%y", tz="America/Chicago")
max_date <- substr(as.character(max(raw_df$timestamp) + 86400), 1, 10)
min_date <- substr(as.character(min(raw_df$timestamp)), 1, 10)

date_format_df <- 
  data.frame(
    timestamp = 
      seq(as.POSIXct(min_date, tz="America/Chicago"),
          as.POSIXct(max_date, tz="America/Chicago"),
          by=(5*60)))

nrow(date_format_df)

filled_df_full <- merge(x = date_format_df, y = raw_df, by.x = "timestamp", by.y = "timestamp", all.x = TRUE)
nrow(filled_df_full)

filled_df_ts <-
  msts(filled_df_full$metricvalue,
       seasonal.periods = c(288, 2016))

filled_df_full_ts <- na.seadec(filled_df_ts)

df_clean<-tsclean(filled_df_full_ts)
# df_clean_merged<-data.frame(cbind(date.format.df,as.data.frame(as.matrix(df_clean))))
df_clean_merged<-data.frame(cbind(date_format_df,as.data.frame(as.matrix(filled_df_full_ts))))
df_clean_merged$V1<-abs(df_clean_merged$V1)

df_ts <-
  msts(raw_df$metricvalue,
       seasonal.periods = c(288, 2016))
