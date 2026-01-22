
library(dplyr)

file_names <- list.files(path = "data_pixels", pattern = ".txt")

data <- list()

for (i in 1:length(file_names)) {
  data[[i]] <- t(read.delim(
    paste0("data_pixels/", file_names[1]),
    header = FALSE
  ))
} # Read files

names(data) <- file_names

for (i in 1:length(data)) {
  data[[i]][1,] <- paste0(
    "p",
    data[[i]][2,], "_",
    data[[i]][3,], "_", names(data[i])
  ) ## Create pixels ID

  rownames(data[[i]]) <- NULL
  data[[i]] <- as.data.frame(data[[i]][-c(2,3), -c(1,2)])
  
  data[[i]][,1] <- as.character(
    formatC(
      as.double(data[[i]][,1]), 
      digits = 3,
      format = "f"
    )
  ) ## round characters to 3 decimals  
  
  data[[i]][1,1] <- "mz"

  colnames(data[[i]]) <- data[[i]][1, ]
  data[[i]] <- as.data.frame(data[[i]][-1, ])
  
  data[[i]] <- data[[i]][-grep(x = data[[i]][,1], pattern = "NA"), ]
  
} # Reformat each individual dataframe


mergedA <- left_join(data[[1]], data[[2]], by = "mz", relationship = "many-to-many") %>%
  left_join(., data[[3]], by = "mz", relationship = "many-to-many") %>%
  left_join(., data[[4]], by = "mz", relationship = "many-to-many") %>%
  left_join(., data[[5]], by = "mz", relationship = "many-to-many") %>%
  left_join(., data[[6]], by = "mz", relationship = "many-to-many") 
  
mergedB <- left_join(data[[7]], data[[8]], by = "mz", relationship = "many-to-many") %>%
  left_join(., data[[9]], by = "mz", relationship = "many-to-many") %>%
  left_join(., data[[10]], by = "mz", relationship = "many-to-many") %>%
  left_join(., data[[11]], by = "mz", relationship = "many-to-many") %>%
  left_join(., data[[12]], by = "mz", relationship = "many-to-many") %>%
  left_join(., data[[13]], by = "mz", relationship = "many-to-many")
  
  
mergedC <- left_join(data[[14]], data[[15]], by = "mz", relationship = "many-to-many") %>%
  left_join(., data[[16]], by = "mz", relationship = "many-to-many") %>%
  left_join(., data[[17]], by = "mz", relationship = "many-to-many") %>%
  left_join(., data[[18]], by = "mz", relationship = "many-to-many")

mergedD <- left_join(data[[19]], data[[20]], by = "mz", relationship = "many-to-many")

merged_mainA <- left_join(mergedA, mergedB, by = "mz", relationship = "many-to-many")
merged_mainB <- left_join(mergedC, mergedD, by = "mz", relationship = "many-to-many")

merged_main <- left_join(merged_mainA, merged_mainB, by = "mz", relationship = "many-to-many")

# ADDING GROUPS
colnames(merged_main) <- gsub(
  x = colnames(merged_main), 
  pattern = ".txt", 
  replacement = ""
) # clean colnames

group_slide <- vector()
group_region <- vector()

for (i in 2:length(colnames(merged_main))) {
  group_slide[i] <- strsplit(colnames(merged_main), split = "_")[[i]][3]
  group_region[i] <- strsplit(colnames(merged_main), split = "_")[[i]][4]
} # set groups

main <- rbind.data.frame(
  group_slide, group_region, merged_main
) # Merge everything together
main[1:2, 1] <- c("Slide", "Region")

write.csv(main, "results/merged_main_byPixel.csv", row.names = FALSE)
