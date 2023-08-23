library(cluster)
library(vegan)

min_max_norm <- function(x, pow = 2, min = -1, max = 1){
  browser()
  y <- (x - min(x))/(max(x) - min(x))
  y * (max - min) + min
}

get_style_cluster <- function(){
  data <- wjd_features_hardcore[setdiff(1:nrow(wjd_features_hardcore), c(185,187,188,321,335,336,337,338,402,403,404,433)),]
  data <- wjd_features_hardcore %>% 
    filter(!is.na(line), style != "FREE", style != "FUSION", performer != "Warne Marsh")
  metadata <- data %>% 
    select(id, title, full_title, performer, style) %>% 
    group_by(performer) %>% 
    mutate(n_solos = n()) %>% 
    ungroup() %>% 
    left_join(wjd_meta %>% 
                mutate(decade = sprintf("'%02ds", 10*floor((recordingyear-1900)/10) %% 10)) %>% select(id, decade))
  
  features <- 
    data %>% 
    select(-id, -title, -full_title, -performer, -style) #%>% 
    #mutate(across(where(is.numeric), function(x) scale(x) %>% as.numeric()))
  
  dist_gower <- cluster::daisy(features,
                      metric = "euclidean")
  mat_gower <- as.matrix(dist_gower)
  mds_nonmetric <- MASS::isoMDS(dist_gower)
  df_mds_nonmetric <- cbind(metadata, 
                            x = mds_nonmetric$points[,1], 
                            y = mds_nonmetric$points[,2])
  browser()
  df_style <- 
    df_mds_nonmetric %>%
    group_by(style) %>%
    summarise(x = median(x), 
              y = median(y))
  df_performer <- 
    df_mds_nonmetric %>%
    group_by(performer) %>%
    summarise(x = median(x), 
              y = median(y),
              n = n())
  df_decade <- 
    df_mds_nonmetric %>%
    group_by(decade) %>%
    summarise(x = median(x), 
              y = median(y))
  ggplot(df_mds_nonmetric, aes(x, y)) +
    ggrepel::geom_label_repel(data = df_performer %>% filter(n > 5), 
                              aes(x, y, label = performer), color = "black", size = 3) + 
    ggrepel::geom_label_repel(data = df_style, 
                              aes(x, y, label = style, fill = style), size = 10, alpha = .2) + 
    ggrepel::geom_text_repel(data = df_decade, 
                              aes(x, y, label = decade), col = "black", size = 12, alpha = .7) + 
    geom_point(aes(col = style), alpha = .5) + theme_bw() + theme(legend.position = "none") + coord_cartesian(xlim = c(-1, 1))
}