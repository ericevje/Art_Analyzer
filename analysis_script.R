library(EBImage)
library(readr)
library(stringr)
library(R.utils)
library(CulturalAnalytics)

img_data = read.csv("gitrepos/Art_Analyzer/cleaned.csv", stringsAsFactors = FALSE)
img_data = subset(img_data, Is.painting=="yes")

mean_red = vector()
mean_green = vector()
mean_blue = vector()

median_red = vector()
median_green = vector()
median_blue = vector()

per_red = vector()
per_green = vector()
per_blue = vector()

sd_red = vector()
sd_green = vector()
sd_blue = vector()

per_edges = vector()
k_vals = vector()
k_edges = vector()

Benses = vector()

paths = vector()
IDs = vector()

evjes = vector()

fhi = matrix(1, nrow = 3, ncol = 3)
fhi[2, 2] = -8

for(ID in img_data$ID){
  path = paste0("/Users/ericevje/gitrepos/Art_Analyzer/", ID, ".jpg")
  print(path)
  img = readImage(path)

  #Basic image analysis
  if(length(dim(img)) == 3){
    IDs = c(IDs, ID)
    
    mean_red = c(mean_red, mean(img[,,1]))
    mean_green = c(mean_green, mean(img[,,2]))
    mean_blue = c(mean_blue, mean(img[,,3]))

    median_red = c(median_red, median(img[,,1]))
    median_green = c(median_green, median(img[,,2]))
    median_blue = c(median_blue, median(img[,,3]))

    total_red = sum(img[,,1])
    total_green = sum(img[,,2])
    total_blue = sum(img[,,3])
    total = total_red + total_green + total_blue

    per_red = c(per_red, total_red / total)
    per_green = c(per_green, total_green / total)
    per_blue = c(per_blue, total_blue / total)

    sd_red = c(sd_red, sd(img[,,1]))
    sd_green = c(sd_green, sd(img[,,2]))
    sd_blue = c(sd_blue, sd(img[,,3]))

    grey = channel(img, "gray")
    path_grey = paste0("/Users/ericevje/gitrepos/Art_Analyzer/", ID, "-grey.jpg")
    writeImage(grey, path_grey)
    
    
    img_fhi = filter2(grey, fhi)
    img_thresh = img_fhi > 0.25
    per_edge = sum(img_thresh)
    per_edge = per_edge/(per_edge + sum(img_thresh==FALSE))
    per_edges = c(per_edges, per_edge)

    #Compression algorithm analysis
    #Compute compressed size of image
    command = paste0("ls -l ", path, " | cut -d \" \" -f 7")
    size = system(command, intern=TRUE)
    if(size == ""){
      command = paste0("ls -l ", path, " | cut -d \" \" -f 8")
      size = system(command, intern=TRUE)
    }
    #print(size)
    #Kolmogorov calculation
    size = as.integer(size)
    max_info = 3 * sum(img_thresh==F | img_thresh==T)
    k_val = (max_info - size) / max_info
    print(k_val)
    k_vals = c(k_vals, k_val)

    #Compute compressed size of edge images
    path_thresh = paste0("/Users/ericevje/gitrepos/Art_Analyzer/", ID, "-thresh.jpg")
    writeImage(img_thresh, path_thresh)
    command = paste0("ls -l ", path_thresh, " | cut -d \" \" -f 7")
    size_con = system(command, intern=TRUE)
    if(size_con == ""){
      command = paste0("ls -l ", path_thresh, " | cut -d \" \" -f 8")
      size_con = system(command, intern=TRUE)
    }
    #Compute Kolmogrov Mk for edge image
    size_con = as.integer(size_con)
    contour_val = (max_info - size_con) / max_info
    print(contour_val)
    k_edges = c(k_edges, contour_val)
    
    #Compute Bense aesthetic measure based on shannon entropy
    entropy = imageEntropy(hist(grey))
    max_entropy = 8
    Bense = (max_entropy - entropy) / max_entropy
    print(Bense)
    Benses = c(Benses, Bense)
    
    #Compute Evjes
    command = paste0("ls -l ", path_grey, " | cut -d \" \" -f 7")
    size_con = system(command, intern=TRUE)
    if(size_con == ""){
      command = paste0("ls -l ", path_grey, " | cut -d \" \" -f 8")
      size_con = system(command, intern=TRUE)
    }

    #delete any images saved during analysis
    command = paste0("rm ", path_thresh)
    system(command)
  }
  else{
    paths = c(paths, path)
  }
}
  
results_df = data.frame(IDs, stringsAsFactors = F)
results_df$mean_red = mean_red
results_df$mean_green =  mean_green
results_df$mean_blue =  mean_blue

results_df$median_red = median_red
results_df$median_green = median_green
results_df$median_blue = median_blue

results_df$per_red = per_red
results_df$per_green = per_green
results_df$per_blue = per_blue

results_df$sd_red = sd_red
results_df$sd_green = sd_green
results_df$sd_blue = sd_blue

results_df$per_edges = per_edges
results_df$k_vals = k_vals
results_df$k_edges = k_edges

results_df$Benses = Benses



