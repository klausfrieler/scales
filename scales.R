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
  "maj-pent"   = c(0, 2, 4, 7, 9),
  "maj-pent-2" = c(0, 2, 5, 7, 9),
  "maj-pent-3" = c(0, 2, 5, 7, 10),
  "min-pent"   = c(0, 3, 5, 7, 10),
  "min-pent-2" = c(0, 3, 5, 7, 9),
  "min-pent-3" = c(0, 3, 5, 8, 10),
  "maj-blues" = c(0, 2, 3, 4, 7, 9),  
  "min-blues" = c(0, 3, 5, 6, 7, 10),  
  "aug" = c(0, 3, 4, 7, 8, 11),  
  "aug-inv" = c(0, 1, 4, 5, 8, 9),  
  #"lydian-dim" = c(0, 2, 3, 6, 7, 9, 10),  
  "dorian-b5" = c(0, 2, 3, 5, 6, 9, 10),
  "mela-ramapriya" = c(0, 1, 4, 6, 7, 9, 10),
  "harm-min-inv" = c(0, 1, 4, 5, 7, 9, 10),
  "mela-sadvidharmargini" = c(0, 1, 3, 6, 7, 9, 10),
  "locrian-bb7" = c(0, 1, 3, 5, 6, 8, 9),
  "kathian" = c(0, 1, 3, 4, 7, 9, 10),
  "phrygian-b4" = c(0, 1, 3, 4, 7, 8, 10),
  "alt-hepta" = c(0, 1, 3, 4, 6, 7, 9),
  "lydian-b3" = c(0, 2, 3, 6, 7, 9, 11),
  "epylian" = c(0, 1, 3, 4, 6, 7, 10),
  "bebop-dom"= c(0, 2, 4, 5, 7, 9, 10, 11),
  "bebop-maj"= c(0, 2, 4, 5, 7, 8, 9, 11),
  "bebop-min"= c(0, 2, 3, 5, 7, 8, 9, 11),
  "bebop-harm-min"= c(0, 2, 3, 5, 7, 8, 10, 11),
  "dom-b5-dim"= c(0, 2, 4, 5, 6, 8, 10, 11),
  "hung-min" = c(0, 2, 3, 6, 7, 8, 11),
  "aeol-#4" = c(0, 2, 3, 6, 7, 8, 10)
)
scales_short  <- lapply(scales_steps, function(x) as.character(x) %>% str_replace("10", "A") %>% str_replace("11", "B") %>% paste(collapse = "")) 
scales_map <- names(scales_short)
names(scales_map) <- as.character(scales_short)

get_scale_name <- function(scale_pc_str){
  scales_map[scale_pc_str] %>% as.character()
}

lsd_map <- c(
"i" = "i7",
"I" = "Ij7",
"i7" = "i7", 
"I7" = "I7",
"ii" = "ii7", 
"ii7" = "ii7", 
"IIb7" = "IIb7", 
"iibo" = "iibo7", 
"iibo7" = "iibo7",
"iii" = "iii7", 
"iii7" = "iii7", 
"iim7b5" = "iim7b5", 
"iio" = "iio", 
"ij7" = "ij7", 
"Ij7" = "Ij7", 
"io" = "io7", 
"io7" = "io7",
"iv" = "iv7", 
"IV" = "IVj7", 
"iv7" = "iv7", 
"IV7" = "IV7", 
"IVj7" = "IVj7", 
"ivo" = "ivo7", 
"ivo7" = "ivo7",
"V" = "V7", 
"V7" = "V7", 
"vi" = "vi7", 
"vi7" = "vi7", 
"VIb" = "VIbj7", 
"VIbj7" = "VIbj7", 
"vibo" = "vibo7", 
"vibo7" = "vibo7", 
"VII7" = "VII7", 
"VIIb" = "VIIb", 
"viio" = "viio7", 
"viio7"= "viio7")

cpc_map <- c(as.character(0:9), "A", "B", "X")

freq_table_group <- function(x, group_var, prop_var, sort = T) {
  group_var <- enquo(group_var)
  prop_var  <- enquo(prop_var)
  tmp <- x %>%
    group_by(!!group_var) %>% 
    mutate(n_group = n()) %>% 
    ungroup() %>% 
    group_by(!!group_var, !!prop_var, n_group) %>%
    summarise(n = n(), 
              .groups = "drop" ) %>%
    mutate(freq = n /sum(n), freq_group = n/n_group)
  if(sort){
    tmp <- tmp %>% arrange(desc(n))
  }
  tmp
  #tmp %>% ggplotf(aes_string(x = rlang::quo_text(prop_var), y= "freq")) + geom_col()
}

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

cos_sim_mod12 <- function(x, y){
  x <- x %% 12
  y <- y %% 12
  xb <- rep(0, 12)
  xb[x] <- 1
  yb <- rep(0, 12)
  yb[y] <- 1
  #sum(xb * yb)/sqrt(sum(xb)*sum(yb))  
  sum(xb*yb)/sqrt(sum(xb*xb) * sum(yb*yb))
  
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
  d_mat <- matrix(rep(0, n * n), ncol = n, nrow = n, dimnames = list(norm_scales, norm_scales))
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

shifter <- function(x, n = 1) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}

bin_to_pcs <- function(x){
  paste(c(as.character(0:9), "A", "B")[which(x == 1)] %>% na.omit(), collapse = "")
}

pc_vec_to_str <- function(pcs){
  as.character(pcs) %>% str_replace("10", "A")  %>% str_replace("11", "B") %>% paste(collapse = "")
}

pc_str_to_vec <- function(pcs){
  ret <- lapply(pcs %>% str_split(""), function(x) x %>% str_replace("A", "10")  %>% str_replace("B", "11") %>% as.integer())
  if(length(pcs) == 1){
    ret <- ret[[1]]
  }
  ret
}

generate_all_scales <- function(pitch_classes = 12, 
                               scale_lengths = 5:8, 
                               intervals = 1:3, 
                               no_consecutive_semitones = T){
  N <- 2^pitch_classes
  mat <- matrix(as.integer(intToBits(0:(N - 1))), nrow = 32) %>% t()
  rs <- rowSums(mat)
  good_rows <- which(rs %in% scale_lengths)
  mat <- mat[good_rows,]
  mat <- mat[which(mat[,1] == 1),]
  mat <- mat[, 1:12]
  well_formed <- function(bit_vec){
    tmp <- rle(bit_vec)
    tmp_shift1 <- rle(shifter(bit_vec, 1))
    tmp_shift_1 <- rle(shifter(bit_vec, -1))
    # if(paste(bit_vec, collapse = "") == "100101110010"){
    #    browser()
    # }
    bad <- any(tmp$length[which(tmp$value == 1)] > (3 - as.integer(no_consecutive_semitones))) | any(tmp$length[which(tmp$value == 0)] > 2)
    bad2 <- any(tmp_shift1$length[which(tmp_shift1$value == 1)] > (3 - as.integer(no_consecutive_semitones))) | any(tmp_shift1$length[which(tmp_shift1$value == 0)] > 2)
    bad3 <- any(tmp_shift_1$length[which(tmp_shift_1$value == 1)] > (3 - as.integer(no_consecutive_semitones))) | any(tmp_shift_1$length[which(tmp_shift_1$value == 0)] > 2)
    iv <- c(which(bit_vec == 1), 13) %>% diff()
    iv_rle <- rle(iv)
    bad  <- bad | bad2 | bad3 | any(iv_rle$length[which(iv_rle$values == 3)] > 1)
    # if(!bad){
    #   browser()
    # }
    return(!bad)
  }
  wf <- apply(mat, 1, well_formed)
  mat <- mat[wf,]
  tibble(PCS = apply(mat, 1, bin_to_pcs),
         BIV = apply(mat, 1, function(x) paste(x, collapse = "")), 
         IV = apply(mat, 1, function(x) paste(c(which(x == 1), 13) %>% diff(), collapse = "")),
         n_iv = apply(mat, 1, function(x) length(unique(c(which(x == 1), 13) %>% diff()))),
         length = rowSums(mat),
         has_semitone_cluster = str_detect(IV, "11") | (substr(IV, 1, 1) == "1" & substr(IV, nchar(IV), nchar(IV)) == "1"),
         has_threeone = str_detect(IV, "13") | str_detect(IV, "31")
  ) %>% 
    mutate(name = get_scale_name(PCS)) %>% 
    arrange(length, IV) %>% 
    mutate(good_scale = (!has_semitone_cluster | !is.na(name)) & (!has_threeone | !is.na(name)))
  #browser()
}

#all_scales <- generate_all_scales(no_consecutive_semitones = F)

cos_sim <- function(x, y){
  sum(x*y)/sqrt(sum(x*x) * sum(y*y))
}

vec_norm <- function(x){
  x/sqrt(sum(x*x))
}

vec_norm_max <- function(x){
  x/max(abs(x))
}

weight_biv <- function(biv){
  weights <- c(3, 1, 1, 2, 2, 1, 3, 3, 1, 1, 2, 2)
  #weights <- weights/sqrt(sum(weights*weights))
  #browser()
  if(is.list(biv)){
    lapply(biv, function(x) vec_norm_max(x * weights))
  }
  else{
    vec_norm_max(biv * weights)
  }
}

get_scale_fits <- function(cpc_vec, test_scales = NULL, ret_top_n = 1, weighting = T, norm = c("euclid", "max")){
  norm <- match.arg(norm)
  if(is.null(test_scales) || !("biv_vec" %in% names(test_scales))){
    test_scales <- all_scales %>%
      filter(good_scale,  n_iv == 2, !has_threeone) %>% 
      bind_rows(all_scales %>% filter(str_detect(name, "blues|^wt$"))) %>% 
      mutate(biv_vec =  lapply(str_split(BIV, ""), as.integer))
  }
  if(weighting){
    test_scales <- test_scales %>% mutate(biv_vec = weight_biv(biv_vec)) 
  }
  test_scales <- test_scales %>% 
    mutate(scale_value = as.integer(str_detect(name, "[0-9]+")) + as.integer(str_detect(name, "-")) - as.integer(str_detect(name, "pent")))
  cpc_tab <- table(factor(cpc_vec, levels = 0:11))
  browser()
  
  scale_mat <- test_scales$biv_vec %>% 
    unlist()  %>% 
    matrix(ncol = nrow(test_scales), nrow = 12) %>% 
    t()
  
  if(norm == "euclid"){
    scale_mat_norm <- t(apply(scale_mat, 1, vec_norm))
    cpc_tab_norm <- cpc_tab/sqrt(sum(cpc_tab * cpc_tab))
  }
  else{
    scale_mat_norm <- t(apply(scale_mat, 1, vec_norm_max))
    cpc_tab_norm <- cpc_tab/max(cpc_tab)
  }
  sims <- scale_mat_norm %*% cpc_tab_norm
  sims <- (t(sims) * (1 - .1 * test_scales$scale_value/2)) %>% as.vector()
  ret <- test_scales %>%
    select(-biv_vec, -has_semitone_cluster, -has_threeone) %>%
    mutate(sim = sims,
           n_vec = length(cpc_vec),
           cpc_vec = pc_vec_to_str(cpc_vec)) %>%
    arrange(desc(sim))
  # ret <- map_dfr(1:nrow(test_scales), function(i){
  #   # if(pc_vec_to_str(cpc_vec) == "73764"){
  #   #   browser()
  #   # }
  #   #browser()
  #   biv <- test_scales$biv_vec[i][[1]]
  #   sim <- cos_sim(cpc_tab, biv) * ( 1- .1 * test_scales$scale_value[i]/2)
  #   test_scales[i,] %>%
  #     select(-biv_vec, -has_semitone_cluster, -has_threeone) %>%
  #     mutate(sim = sim,
  #            n_vec = length(cpc_vec),
  #            cpc_vec = pc_vec_to_str(cpc_vec))
  # }) %>%
  #   arrange(desc(sim))
  
  if(!is.null(ret_top_n)){
    ret <- ret %>% top_n(ret_top_n, sim)
  }
  ret
}

get_all_scale_fits <- function(data = wjd_tpc){
  ids <- unique(data$id)
  map_dfr(ids, function(i){
    tmp <- data %>% filter(id == i)
    messagef("Checking %s", i)
    map_dfr(unique(tmp$chord_id), function(cid) {
      tmp2 <- tmp[tmp$chord_id == cid,]
      get_scale_fits(tmp2$cpc) %>% 
        mutate(chord_id = cid, 
               local_scale_degree =  unique(tmp2$local_scale_degree))
    })
  })
}

get_all_scale_fits_fast <- function(data = wjd_tpc, 
                                    test_scales = NULL, 
                                    ret_top_n = 1, 
                                    weighting = c("chordal", "flat"), 
                                    cpc_tab = c("freq", "indicator"),
                                    norm = c("euclid", "max")){
  ids <- unique(data$id)
  weighting <- match.arg(weighting)
  norm <- match.arg(norm)
  cpc_tab <- match.arg(cpc_tab)
  
  if(is.null(test_scales) || !("biv_vec" %in% names(test_scales))){
    test_scales <- all_scales %>%
      filter(good_scale,  n_iv == 2, !has_threeone) %>% 
      bind_rows(all_scales %>% filter(str_detect(name, "blues|^wt$"))) %>% 
      distinct() %>% 
      mutate(biv_vec =  lapply(str_split(BIV, ""), as.integer))
    #browser()
  }
  if(weighting == "chordal"){
    test_scales <- test_scales %>% mutate(biv_vec = weight_biv(biv_vec)) 
  }
  test_scales <- test_scales %>% 
    mutate(scale_value = as.integer(str_detect(name, "[0-9]+")) + as.integer(str_detect(name, "-")) - as.integer(str_detect(name, "pent")))
  
  if(length(ids) > 1){
    return(map_dfr(ids, function(id) {
      get_all_scale_fits_fast(data[data$id == id,], 
                              test_scales = test_scales, 
                              weighting = weighting,
                              ret_top_n = ret_top_n,
                              norm = norm)
    }))
  }
  
  #browser()
  data <- data %>% 
    group_by(id) %>% 
    mutate(chord_id = enumerate_sequence(chord),
           event_id = 1:n()) %>% ungroup() 
  
  split_df <- data %>%  
    split(data$chord_id)
  
  cpc_vecs <- map_dfr(1:length(split_df), function(i) {
    tibble(chord_id = i,
           local_scale_degree = unique(split_df[[i]]$local_scale_degree),
           local_key = unique(split_df[[i]]$local_key),
           chord = unique(split_df[[i]]$chord),
           n_vec = length(split_df[[i]]$cpc), 
           cpc_vec = pc_vec_to_str(split_df[[i]]$cpc))
    })
  if(norm == "euclid"){
    norm_func = vec_norm
  }
  else{
    norm_func = vec_norm_max
  }
  cpc_tab_norm <- lapply(split_df, 
                     function(x){
                       if(cpc_tab == "freq"){
                         tab <- table(factor(x$cpc, levels = 0:11))
                       }
                       else{
                         tab <- table(factor(unique(x$cpc), levels = 0:11))
                       }
                       norm_func(tab)} ) %>% 
    unlist() %>% 
    matrix(nrow = 12) 
  
  #browser()
  scale_mat <- test_scales$biv_vec %>% 
    unlist()  %>%
    matrix(ncol = nrow(test_scales), nrow = 12) %>% 
    t()
  scale_mat_norm <- t(apply(scale_mat, 1, norm_func))
  sims <- scale_mat_norm %*% cpc_tab_norm
  sims_weighted <- apply(sims, 2, function(x) x*(1 - .1 * test_scales$scale_value/2)) %>% t()
  #browser()
  
  ret <- sims_weighted %>% 
    as.data.frame.matrix %>% 
    as_tibble() %>% 
    set_names(test_scales$name) %>% 
    mutate(chord_id = 1:nrow(.)) %>% 
    pivot_longer(-chord_id) %>% 
    rename(sim = value)
  
  if(!is.null(ret_top_n)){
    ret <- ret %>% 
      group_by(chord_id) %>% 
      top_n(ret_top_n, sim) %>%
      ungroup()
  }
  
  ret <- ret %>% left_join(cpc_vecs, by = "chord_id", relationship = "many-to-many") 
  ret$id <- unique(data$id)
  ret <- ret %>%  left_join(
    test_scales %>%
      select(-biv_vec, -has_semitone_cluster, -has_threeone), 
    by = "name") %>% 
    select(id, chord_id, chord, local_scale_degree, cpc_vec, sim, everything())
  if(!is.null(ret_top_n)){
    ret <- ret %>% 
      group_by(chord_id) %>% 
      top_n(-ret_top_n, length) %>%
      ungroup()
  }
  #   arrange(desc(sim))
  #browser()
  ret
}
  