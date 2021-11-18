rm( list=ls() )
gc()
# ------------------------------------------------------------------------------------------------------------
# Imports
# ------------------------------------------------------------------------------------------------------------
# install.packages('pacman')
library(pacman)
p_load(this.path, dplyr, tidyr, tidyverse, cluster, Rtsne, ggplot2)
setwd(this.path::this.dir())
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Global variables
# ------------------------------------------------------------------------------------------------------------
DATASET_PATH  <- '../../dataset/'

# INPUT_FILE  <- 'dataset_discharge_yes_no'
INPUT_FILE  <- 'discharges'
INPUT_PATH  <- paste(DATASET_PATH, INPUT_FILE, '.csv', sep='')

DATA_PATH         <- '../../data/'
IMPORTANCE_PATH  <- paste(DATA_PATH, INPUT_FILE, '.csv', sep='')

PROFILE_PATH      <- paste(DATA_PATH, 'profiles.csv', sep='')
PROFILE_IMG_PATH  <- paste(DATA_PATH, 'profiles.png', sep='')

exclude <- c('clase_ternaria', 'target', 'foto_mes', 'X')
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------------------------------------
load_file <- function(path) {
  print(paste('Load', path, 'dataset...'))
  read.csv(path)
}

write_csv <- function(df, path) write.csv(df, path, row.names = FALSE, quote=FALSE)

discharges_summary <- function(df) {
  print(df %>% group_by(clase_ternaria) %>% tally())
}

plot_clusters <- function(gower_df, classes) {
  tsne_object <- Rtsne(gower_df, is_distance = TRUE)
  
  tsne_df <- tsne_object$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(classes))
  
  ggplot(aes(x = X, y = Y), data = tsne_df) +
    geom_point(aes(color = cluster))
}
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------------------------------------
# https://towardsdatascience.com/clustering-datasets-having-both-numerical-and-categorical-variables-ed91cdca0677
dataset <- load_file(INPUT_PATH)
print('Dataset loaded...')

nrow(dataset)

print('Sample observations...')
sample_percent <- 1
sample_size <- round(sample_percent * nrow(dataset))
dataset_sample <- dataset %>% sample_n(sample_size) %>% drop_na()
rm(dataset)

nrow(dataset_sample)

columns <- load_file(IMPORTANCE_PATH) %>% 
  select(Feature) %>% 
  pull()

print('Select importante columns...')
ds <- dataset_sample %>% select(columns) # %>% select(-exclude)
ds_target <- dataset_sample %>% 
  mutate(target = ifelse(clase_ternaria == "BAJA+2", 1, 0)) %>% 
  select(target) %>% pull()
rm(dataset_sample)




print('Calculate distances...')
gower_df <- daisy(
  ds,
  metric = "gower" ,
  type   = list(logratio = 2)
)


silhouette <- c()
silhouette <- c(silhouette, NA)
for(i in 2:10) {
  pam_clusters <- pam(
    as.matrix(gower_df),
    diss = TRUE,
    k = i
  )
  silhouette <- c(silhouette ,pam_clusters$silinfo$avg.width)
}

plot(
  1:10, 
  silhouette,
  xlab = "Clusters",
  ylab = "Silhouette Width"
)
lines(1:10, silhouette)


pam_result = pam(gower_df, diss = TRUE, k = 2)

profiles <- ds[pam_result$medoids, ]
write_csv(profiles, PROFILE_PATH)
View(profiles)

print('Plot clusters...')
png(PROFILE_IMG_PATH, width = 1000, height = 1000, pointsize = 30)
plot_clusters(gower_df, pam_result$clustering)
dev.off()
