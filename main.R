setwd("/home/walle/Desktop/STDA/project2/")
names <- c("train_weekly.csv", "train_quarterly.csv", "train_monthly.csv", "train_hourly.csv", "train_yearly.csv")
data <- lapply(names, read.csv) %>% bind_rows() # If the typo error is not solved, this function won't take the ID row of train_hourly.csv

info <- read.csv("TSinfo.csv")
info

smape <- function(true, pred, h, n){
  val = 0
  for (i in (n+1):(n + h)){
    res <- abs(true[i] - pred[i])/(abs(true[i]) + abs(pred[i]))
    val <- val + res
  }
  val <- val * 2 / h * 100
  return(val)
}

preprocess <- function(data, info){
  data_ts <-list()
  data_ids <- list()
  for (i in 1:length(data[,1])){
    vector <- data[i,]
    vector <- vector[!is.na(vector)] # Take NAs out
    non_numeric <- grepl("[^0-9.]", vector) # Take non numeric numbers out
    id <- vector[non_numeric] # Save the ID
    info_v <- info[info$ID == id,] # Take info from the vector
    ts <- ts(as.numeric(vector[!non_numeric]), frequency = info_v$Frequency) #Create ts
    data_ts[[length(data_ts) + 1]] <-  ts
    data_ids[[length(data_ids) + 1]] <- id
  }
  return(list(data_ts, data_ids))
}
preprocessed <- preprocess(data, info)

data_ts <- preprocessed[[1]]
data_ids <- preprocessed[[2]]
