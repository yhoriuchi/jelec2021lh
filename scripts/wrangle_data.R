#' Download, merge, and wrangle municipality-level election data
#' 2021 House of Representatives Election in Japan
#' Step 2: Read and wrangle files

# Initial settings --------------------------------------------------------

library(tidyverse)
library(readxl)

# A list of files in the downloaded folders
files1 <- list.files("downloaded/smd")
files2 <- list.files("downloaded/pr")

# Function to read and wrangle data ---------------------------------------

.file <- "000785193.xls"
.type <- "smd"

read_data <- function(.file, .type){
  
  filename <- paste0("downloaded/", .type, "/", .file)
  
  districts <- excel_sheets(filename)
  
  if (.type == "smd"){
    
    out <- NULL
    
    for (i in seq_along(districts)){
      
      data <- suppressMessages(read_excel(filename, sheet = districts[i]))
      ncols <- ncol(data)
      names(data) <- c("municipality", 1:(ncols - 1))
      
      location1 <- which(data$municipality == "候補者名")
      location2 <- which(data$municipality == "市区町村名＼政党名")
      
      candidates <- data[location1, 2:ncols]
      parties    <- data[location2, 2:ncols]
      
      information <- data.frame(
        candidate = data[location1, 2:ncols] %>% t(),
        parties   = data[location2, 2:ncols] %>% t()
      ) %>% 
        mutate(id = row_number())
      
      data2 <- data %>% 
        filter(row_number() > location2) %>% 
        mutate(across(.cols = everything(), .fns = as.character)) %>% 
        pivot_longer(cols = all_of(2:ncols), names_to = "id", values_to = "votes") %>% 
        mutate(id = as.numeric(id)) %>% 
        left_join(information, by = "id") %>% 
        select(-id) %>% 
        filter(!is.na(votes) & !is.na(parties)) %>% 
        mutate(votes = as.numeric(votes),
               file = .file, 
               district = districts[i])
      
      out <- bind_rows(out, data2)
      
    }
    
    return(out)
    
    
  } else if (.type == "pr"){
    
    data <- suppressMessages(read_excel(filename, sheet = districts[1]))
    ncols <- ncol(data)
    names(data) <- c("municipality", 1:(ncols - 1))
    
    location <- which(data$municipality == "市区町村名＼政党名")
    
    parties    <- data[location, 2:ncols]
    
    information <- data.frame(
      parties   = data[location, 2:ncols] %>% t()
    ) %>% 
      mutate(id = row_number())
    
    out <- data %>% 
      filter(row_number() > location) %>% 
      mutate(across(.cols = everything(), .fns = as.character)) %>% 
      pivot_longer(cols = all_of(2:ncols), names_to = "id", values_to = "votes") %>% 
      mutate(id = as.numeric(id)) %>% 
      left_join(information, by = "id") %>% 
      select(-id) %>% 
      filter(!is.na(votes) & !is.na(parties)) %>% 
      mutate(votes = as.numeric(votes),
             file = .file, 
             district = districts[1])
    
    return(out)
    
  } else{
    
    print(".type must be either smd or pr")
    break
    
  }
  
  
}

# Merge all data ----------------------------------------------------------

smd_data <- NULL
pr_data  <- NULL

for (i in 1:47){
  
  smd_data <- bind_rows(smd_data, read_data(files1[i], "smd"))
  pr_data  <- bind_rows(pr_data,  read_data(files2[i], "pr"))
  
}

# Save data ---------------------------------------------------------------

write_rds(smd_data, "output/smd_data.rds")
write_rds(pr_data, "output/pr_data.rds")

write_excel_csv(smd_data, "output/smd_data.csv")
write_excel_csv(pr_data, "output/pr_data.csv")

