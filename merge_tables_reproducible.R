
library(dplyr)

file_names <- list.files(path = "data/", pattern = ".csv")
file_names

data <- list()

for (f in 1:length(file_names)) {
  data[[f]] <- read.csv(
    paste0("data/", file_names[f]),
    header = FALSE,
    sep = "," 
  )
}

names(data) <- file_names

# Loop format
dataset_number <- length(data)
for (i in 1:dataset_number) {
  data[[i]] <- t(
    data[[i]]
  ) # transpose the dataframe

  colnames(data[[i]]) <- data[[i]][1,] # make the first row the header

  data[[i]] <- as.data.frame(
    data[[i]][-1, -c(2:4)]
  ) # remove unecessary rows and cols

  data[[i]][,1] <- substr(
    as.character(data[[i]][,1]), 
    1, 
    (nchar(data[[i]][,1]) - 3)
  ) # remove the "_-1" characters
  
  data[[i]][,1] <- as.character(
    formatC(
      as.double(data[[i]][,1]), 
      digits = 3,
      format = "f"
    )
  ) # round characters to 3 decimals  
  
  colnames(data[[i]])[1] <- "mz"

  data[[i]][1,1] <- "Group"

} # make the data trans

merged_df <- left_join(data[[1]], data[[2]], by = "mz") %>%
  left_join(., data[[3]], by = "mz") %>%
  left_join(., data[[4]], by = "mz") %>%
  left_join(., data[[5]], by = "mz")

for (z in 2:ncol(merged_df)) {
  merged_df[is.na(merged_df[,z]), z] <- 0  
} # remove NAs
write.csv(merged_df, "results/merged.csv", row.names = FALSE)


write.csv(merged_df, "results/merged_NAs.csv", row.names = FALSE)
