library(tidyverse)
library(jazzodata)
library(parkR)

messagef <- function(...) message(sprintf(...))

scales_steps <- list(
  "ionian"= c(0, 2, 4, 5, 7, 9, 11),
  "lydian"= c(0, 2, 4, 6, 7, 9, 11),
  "mixolydian"= c(0, 2, 4, 5, 7, 9, 10),
  "aeolian"= c(0, 2, 3, 5, 7, 8, 10),
  "dorian"= c(0, 2, 3, 5, 7, 9, 10),
  "phrygian"= c(0, 1, 3, 5, 7, 8, 10),
  "locrian"= c(0, 1, 3, 5, 6, 8, 10),
  "melodic-min"= c(0, 2, 3, 5, 7, 9, 11),
  "dorian-b2"= c(0, 1, 3, 5, 7, 9, 10),
  "lydian-aug"= c(0, 2, 4, 6, 8, 9, 11),
  "lydian-dom"= c(0, 2, 4, 6, 7, 9, 10),
  "mixo-b6"= c(0, 2, 4, 5, 7, 8, 10),
  "half-dim"= c(0, 2, 3, 5, 6, 8, 10),
  "alt"= c(0, 1, 3, 4, 6, 8, 10),
  "harmonic-min"= c(0, 2, 3, 5, 7, 8, 11),
  "locrian-#6"= c(0, 1, 3, 5, 6, 9, 10),
  "ionian-aug"= c(0, 2, 4, 5, 8, 9, 11),
  "ukr-dorian"= c(0, 2, 3, 6, 7, 9, 10),
  "phrygian-dom"= c(0, 1, 4, 5, 7, 8, 10),
  "lydian-#2"= c(0, 3, 4, 6, 7, 9, 11),
  "super-locrian-bb7"= c(0, 1, 3, 4, 6, 8, 9),
  "wt"= c(0, 2, 4, 6, 8, 10),
  "htwt"= c(0, 1, 3, 4, 6, 7, 9, 10),
  "wtht"= c(0, 2, 3, 5, 6, 8, 9, 11),
  "maj-pent" = c(0, 2, 4, 7, 9),
  "min-pent" = c(0, 3, 5, 7, 10),
  "maj-blues" = c(0, 2, 3, 4, 7, 9),  
  "min-blues" = c(0, 3, 5, 6, 7, 10)  
)

get_standard_weights <- function(x, maj = 2, min = 1){
  w <- rep(min, 12)
  if(is.character(x)){
    x <- scales_steps[[x]]
  }
  w[1] <- maj
  if(3 %in% x){
    w[4] <- maj
  }
  if(4 %in% x){
    w[5] <- maj
  }
  if(6 %in% x){
    w[7] <- maj
  }
  if(7 %in% x){
    w[8] <- maj
  }
  if(10 %in% x){
    w[11] <- maj
  }
  if(11 %in% x){
    w[12] <- maj
  }
  w
}
bin_scale_sim <- function(x, y){
  x <- x %% 12
  y <- y %% 12
  xb <- rep(0, 12)
  xb[x] <- 1
  yb <- rep(0, 12)
  yb[y] <- 1
  #sum(xb * yb)/sqrt(sum(xb)*sum(yb))  
  cor(xb, yb)
}

cos_sim <- function(x, y){
  x <- x %% 12
  y <- y %% 12
  xb <- rep(0, 12)
  xb[x] <- 1
  yb <- rep(0, 12)
  yb[y] <- 1
  #sum(xb * yb)/sqrt(sum(xb)*sum(yb))  
  sum(xb*yb)/sqrt(sum(xb*xb)*sum(yb*yb))
  
}
jaccard_sim <- function(x, y){
  length(intersect(x, y))/length(union(x, y))
}

total_variation_sim <- function(x, y, xw = rep(1, 12), yw = rep(1, 12)){
  xn <- (table(factor(x %% 12, levels = 0:11)) * xw) %>% prop.table()  
  yn <- (table(factor(y %% 12 , levels = 0:11)) * yw) %>% prop.table()  
  1 - .5*sum(abs(xn -yn))
}

get_scale_similarity <- function(as_dist_matrix = T, sim_type = c("jaccard", "bin_cos")){
  sim_type <- match.arg(sim_type)
  n <- length(scales_steps)
  d_mat <- matrix(rep(0, n*n), ncol = n, nrow = n)
  ret <- 
    map_dfr(1:n, function(i){
      map_dfr(1:n, function(j){
        if(sim_type == "jaccard"){
          sim  <- jaccard_sim(scales_steps[[i]], scales_steps[[j]])
        }
        else{ 
          sim <- bin_scale_sim(scales_steps[[i]], scales_steps[[j]])
        }
        d_mat[i,j] <<- 1- sim
        tibble(x = names(scales_steps)[[i]], y = names(scales_steps)[[j]], sim = sim, d = 1- sim, type = sim_type)
      })
    })
  if(as_dist_matrix) return(d_mat %>% as.dist())
  ret
}

tone_names_norm <- c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B")

plot_scales_mds <- function(sim_type = "jaccard", with_transpositions = F){
  if(with_transpositions){
    all_scales <- get_all_scale_transpositions()
    all_scales <- all_scales[!str_detect(names(all_scales), "pent|blues")]
    browser()
    all_scales_df <- 
      map_dfr(1:length(all_scales), ~{
        tibble(name = names(all_scales)[[.x]],
               root = str_extract(name, "^[A-Z]+[b]?"), 
               scale = paste(all_scales[[.x]], collapse = ","))})
    
    norm_scales <- all_scales_df %>% 
      group_by(scale) %>% 
      filter(name == select_norm_name(name)) %>% 
      pull(name)
    
    mds <-  get_Scale_similarity_with_transpositions() %>% 
    as.dist() %>% 
    cmdscale(add = F, k = 2)  %>% 
    as.data.frame.matrix() %>% 
    as_tibble() %>%
    mutate(labels = norm_scales)
  }
  else{
    mds <-  get_scale_similarity(sim_type = sim_type) %>% 
      as.dist() %>% 
      cmdscale(add = F, k = 2)  %>% 
      as.data.frame.matrix() %>% 
      as_tibble() %>%
      mutate(labels = names(scales_steps))
    
  }
  q <- mds %>% ggplot(aes(x = V1, y = V2)) 
  q <- q + ggrepel::geom_text_repel(aes(label =labels)) 
  q <- q + theme_minimal() 
  q <- q + geom_point()
  q
}

get_all_scale_transpositions <- function(){
  map(1:length(scales_steps), function(i){
    scales <- map(0:11, function(t){
      ((scales_steps[[i]] + t) %% 12) %>% sort()
    }) 
    #browser()
    scales %>% setNames(sprintf("%s-%s", tone_names_norm[1:12], names(scales_steps)[[i]]))
  }) %>% unlist(recursive = F)
}

select_norm_name <- function(scale_names){
  scale_names <- sort(scale_names)
  c_str <- scale_names[str_detect(scale_names, "C-")]
  
  if(length(c_str) != 0){
    return(c_str[1])
  }
  c_str <- scale_names[str_detect(scale_names, "Db-")]
  
  if(length(c_str) != 0){
    return(c_str[1])
  }
  c_str <- scale_names[str_detect(scale_names, "D-")]
  
  if(length(c_str) != 0){
    return(c_str[1])
  }
  return(scale_names[1])
}

get_Scale_similarity_with_transpositions <- function(as_dist_matrix = T){
  all_scales <- get_all_scale_transpositions()
  all_scales <- all_scales[!str_detect(names(all_scales), "pent|blues")]
  
  all_scales_df <- 
    map_dfr(1:length(all_scales), ~{
      tibble(name = names(all_scales)[[.x]],
             root = str_extract(name, "^[A-Z]+[b]?"), 
             scale = paste(all_scales[[.x]], collapse = ","))})
  
  norm_scales <- all_scales_df %>% 
    group_by(scale) %>% 
    filter(name == select_norm_name(name)) %>% 
    pull(name)
  
  all_scales <- all_scales[norm_scales]
  n <- length(all_scales)
  d_mat <- matrix(rep(0, n*n), ncol = n, nrow = n, dimnames = list(norm_scales, norm_scales))
  ret <- 
    map_dfr(1:(n-1), function(i){
      map_dfr((i):n, function(j){
            sim  <- jaccard_sim(all_scales[[i]], all_scales[[j]])
            #browser()
            #messagef("%s <-> %s", names(all_scales)[[i]], names(all_scales)[[j]])
            d_mat[i, j] <<- 1 - sim
            d_mat[j, i] <<- 1 - sim
            bind_rows(
              tibble(
                x = names(all_scales)[[i]], 
                y = names(all_scales)[[j]], 
                sim = sim, 
                d = 1- sim),
              tibble(
                x = names(all_scales)[[j]], 
                y = names(all_scales)[[i]], 
                sim = sim, 
                d = 1- sim)
            )
      })      
    })
  if(as_dist_matrix) return(d_mat %>% as.dist())
  ret
  
}

get_top_ranks <- function(x, top_n = 4, collapse = ","){
  #browser()
  tab <- table(x) %>% sort() %>% prop.table()
  r <- length(tab) + 1 - rank(tab)
  cum_prop <- tab[names(r[r <= top_n])] %>% sum
  tibble(label = paste(sort(names(rev(r))[1:top_n]), collapse = collapse), cum_prop = cum_prop)
}

get_chord_tones_cum_prop <- function(x, chord_tones = c("1", "3", "5", "7"), collapse = ""){
  #browser()
  tab <- table(x) %>%  prop.table()
  cum_prop <- tab[intersect(names(tab), chord_tones)] %>% sum
  tibble(label = paste(intersect(names(tab), chord_tones), collapse = collapse), cum_prop = cum_prop)
}

patch_analysis <- function(analysis, cmp){
  #browser()
  if(nrow(cmp) == 0){
    return(analysis)
  }
  for(i in 1:nrow(cmp)){
    analysis[analysis$bar == cmp[i,]$bar & analysis$chord == cmp[i,]$chord,]$local_scale_degree <- cmp[i,]$local_scale_degree.x 
    analysis[analysis$bar == cmp[i,]$bar & analysis$chord == cmp[i,]$chord,]$local_key <- cmp[i,]$local_key.x
  }
  analysis
}

prepare_harmonic_analysis_eval <- function(){
  old_ids <- c(19, 31, 53, 107, 113, 115, 168, 242, 268, 311, 315, 421)
  new_ids <- c(3, 97, 130, 137, 163, 172, 194, 235, 243, 264, 383, 441, 45)
  ids <- union(old_ids, new_ids) %>% sort()
  #browser()
  #ids <- 19
  anas <- map(ids, function(i) {
    sv <- jazzodata::wjd_tpc %>% filter(melid == i) %>% pull(id) %>% unique()
    messagef("Preparing %s (%d)...", sv, i)
    ret <- create_leadsheet_from_wjd_db(compid = i) %>% 
      key_analysis() %>% 
      select(section, bar, beat, chord, local_key, local_scale_degree) %>% 
      filter(chord != "NC") %>% 
      mutate(evaluation = 0) 
    cmp <- 
      wjd_tpc %>% filter(melid == i) %>% 
      distinct(bar, chord, local_scale_degree, local_key) %>% 
      left_join(ret, by = c("bar", "chord")) %>% 
      filter(local_scale_degree.x != local_scale_degree.y | local_key.x != local_key.y)
    if(nrow(cmp) > 0){
      messagef("Patching %d lines", nrow(cmp))
    }
    ret <- patch_analysis(ret, cmp)
    ret
    }) %>% 
    set_names(sprintf("%d", ids)) 
  openxlsx::write.xlsx(anas, "harmonic_analysis_evaluation.xlsx")
}
enumerate_sequence <- function(seq){
  rl <- rle(seq)
  rep(1:length(rl$lengths), rl$lengths)
}
get_chord_tone_connection <- function(data){
  tmp <- data %>% pull(cdpcx_raw) #%>% mutate(is_ct = cdpcx_raw %in% c("1", "3", "5", "7"))
  tmp <- c(tmp, "F")
  ctid <- c(which(tmp %in% c("1", "3", "5", "7", "F")))
  #browser()
  ctl <- diff(ctid)
  tmp <- 
    map2_chr(ctid[1:(length(ctid)-1)], ctl, function(x,y){
      paste(tmp[x:(x + y)], collapse = "")
    })
  tmp[!str_detect(tmp,"F")]  
}
get_all_chord_tone_connection <- function(data){
  ids <- unique(data$id)
  conn_tab <-
    map(ids, function(i){
      tmp <- data %>% filter(id == i) %>% mutate(chord_id = enumerate_sequence(chords_raw)) 
      map(unique(tmp$chord_id), function(ci){
        tmp %>% filter(chord_id == ci) %>% get_chord_tone_connection()
      }) 
    }) %>% 
    unlist() %>% 
    table()
  conn_tab
}