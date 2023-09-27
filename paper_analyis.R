library(tidyverse)
library(jazzodata)
library(parkR)

style_map <- c("TRADITIONAL" = "Traditional", 
               "SWING" = "Traditional", 
               "BEBOP" = "Modern",
               "COOL" = "Modern", 
               "HARDBOP" = "Modern", 
               "FREE" = "Postmodern", 
               "POSTBOP" = "Postmodern", 
               "FUSION" = "Postmodern")

cdpcx_class <- c("-" = "chrom", 
                 "%" = "chrom", 
                 "<" = "chrom", 
                 ">" = "chrom", 
                 "1" = "chord", 
                 "2" = "dia", 
                 "3" = "chord", 
                 "4" = "dia", 
                 "5" = "chord", 
                 "6" = "dia", 
                 "7" = "chord", 
                 "B" = "chrom", 
                 "L" = "chrom", 
                 "T" = "chrom", 
                 "X" = "X")

get_chromatic_approach_table <- function(){
  tab <- wjd_transforms %>% 
    left_join(wjd_meta %>% select(id, style)) %>% 
    select(id, int_raw, cdpcx_raw, style) %>% 
    mutate(style = style_map[style], 
           cdpcx_type = cdpcx_class[cdpcx_raw]) %>% 
    mutate(lag_int_raw = lag(int_raw), same_int = abs(int_raw) == abs(lag_int_raw)) %>% 
    mutate(poi = abs(lag_int_raw) == 1 & !same_int) %>% 
    filter(cdpcx_type != "X") %>% 
    with(., table(poi, cdpcx_type, style)) %>% 
    as.data.frame() %>% 
    group_by(style, poi) %>%
    mutate(rel_freq = Freq/sum(Freq)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols = c(poi, style), names_from = cdpcx_type, values_from = rel_freq) %>% 
    mutate(poi = c("Other", "Chromatic Approach")[poi]) %>% 
    knitr::kable(format = "latex", digits = 3)  
  tab
  
}

plot_pitch_type_by_year <- function(){
  tmp <- wjd_transforms %>% 
    mutate(cdpcx_type = cdpcx_class[cdpcx_raw]) %>% 
    left_join(wjd_meta %>% select(id, style, recordingyear), by = "id") %>% 
    filter(cdpcx_raw != "X")
  tmp_freq <- map_dfr(unique(tmp$id),
                      ~{
                        tmp %>% filter(id == .x) %>% freq_table(cdpcx_type) %>% select(-n) %>% mutate(id = .x)
                        })  %>% 
    left_join(wjd_meta %>% 
                select(id, recordingyear, style)) 
  tab <- tmp_freq %>% 
    filter(style != "FREE", style != "FUSION") %>% 
    mutate(cdpcx_type = factor(cdpcx_type, labels = c("Chord Tone", "Chromatic", "Diatonic")) %>% 
             fct_reorder(freq), 
           style = factor(style_map[style]) %>% 
             fct_reorder(recordingyear)) %>% 
    group_by(style, cdpcx_type) %>% 
    summarise(m = mean(freq))  %>% 
    mutate(cdpcx_type = fct_rev(cdpcx_type)) %>% 
    pivot_wider(id_cols = style, names_from = cdpcx_type, values_from = m) %>% 
    select(c(1, 4, 3, 2)) %>% 
    knitr::kable(format = "latex", digits = 2)
  q <- tmp_freq %>% 
    filter(style != "FREE", style != "FUSION") %>% 
    mutate(cdpcx_type = factor(cdpcx_type, labels = c("Chord Tone", "Chromatic", "Diatonic")) %>% fct_reorder(freq), style = style_map[style]) %>% 
    ggplot(aes(x = recordingyear, y = freq, color = cdpcx_type, shape = style)) 
  q <- q + geom_point(size = 2) 
  q <- q + geom_smooth(method = "lm", aes(color  = cdpcx_type, group = cdpcx_type), size = 1, se = T) 
  q <- q + theme_bw() 
  q <- q + labs( x = "Recording Year", y = "Rel. Frequency", color = "Pitch Type", shape = "Style")
  q
}

get_sixth_comparison_plot <- function(){
  q <- 
    wjd_tpc %>% 
    filter(local_scale_degree %in% c("ii7", "iii7", "iv7", "vi7", "i7", "v7"), 
           tonality_type != "MODAL") %>% 
    group_by(melid, bar, local_scale_degree) %>% 
    summarise(n = sum(cpc %in% c(8,9)), m8 = mean(cpc == 8), m9 = mean(cpc == 9), d_f = (m9 - m8), .groups = "drop") %>%
    filter(m9 > 0 | m8 > 0) %>% 
    select(local_scale_degree, `Minor 6` = m8, `Major 6` = m9, `Maj. 6 - Min. 6` = d_f) %>% 
    pivot_longer(-local_scale_degree) %>% 
    mutate(type = factor(name == "Maj. 6 - Min. 6", labels = c("Densities", "Bar-wise Differences"))) %>% 
    ggplot(aes(x = value, y = local_scale_degree, fill = name)) 
  q <- q + ggridges::geom_density_ridges(alpha = .2) 
  q <- q + theme_bw()
  q <- q + scale_x_continuous(limits = c(-1, 1))
  q <- q + labs(x = "Rel. Frequency", y = "Scale Degree", fill = "")
  q <- q + facet_wrap(~type)
  q
}
cpc_cdpcx_map <-  list("0" = "1", 
                       "1" = "-", 
                       "2" = "2",
                       "3" = list("min" = "3",
                                  "7"   = "B",
                                  "maj" = "B"),
                       "4" = list("min" = ">",
                                  "7"   = "3",
                                  "maj" = "3"),
                       "5" = "4",
                       "6" = "T",
                       "7" = "5",
                       "8" = "%",
                       "9" = "6",
                       "10" = list("min" = "7", 
                                   "7"   = "7",
                                   "maj" = "<"),
                       "11" = list("min" = "L",
                                   "7"   = "L",
                                   "maj" = "7")
)
chord_type_map <- c("min" = "min",
                    "min6" = "min",
                    "min7" = "min",
                    "m7b5" = "min",
                    "o" = "min",
                    "o7" = "min",
                    "minmaj" = "min",
                    "minmaj7" = "min",
                    "6" = "maj",
                    "7" = "7",
                    "maj" = "maj",
                    "maj7" = "maj",
                    "NC" = NA)

recalc_cdpcx <- function(data = wjd_tpc){
  chord_dictionary <- tibble(
    chord = data$chord %>% 
      unique()) %>% 
    bind_cols(data$chord %>% 
                unique() %>% 
                parkR::parse_chord())
  
  tmp <- data %>% 
    left_join(chord_dictionary %>% 
                select(-pc, -bass, -bass_pc, -ext, chord_type = type), by = "chord") %>% 
    mutate(cpc = (pitch - root_pc) %% 12)
  
  elements <- tmp %>% 
    distinct(cpc, chord_type) %>% 
    mutate(chord_type_short = chord_type_map[chord_type])
  
  elements$cdpcx <- 
    map2_chr(as.character(elements$cpc), elements$chord_type_short, function(cpc, ct){
      #browser()
      if(is.na(ct)){
        return("X")
      }
      tmp <- cpc_cdpcx_map[[cpc]]
      if(length(tmp) > 1){
        return(tmp[[ct]])
      }
      else{
        return(tmp[[1]])
      }
    }) 
  tmp %>% select(-cdpcx) %>% left_join(elements %>% select(cpc, chord_type, cdpcx)) 
}

scale_lsd_heat_map <- function(scale_fits, min_n = 100, fname = "", sort_scales = T, sort_scale_degrees = T){
  scale_fits <- scale_fits %>% 
    left_join(wjd_meta %>% 
                select(id, style, tonality_type), 
              by = "id") %>% 
    mutate(red_style = style_map[style])
  prep <- scale_fits 
   
  # prep <- scale_fits %>% 
  # mutate(style = style_map[style]) %>%  
  #   filter(style %in% "Postmodern") 
  prep <- prep %>% 
    filter(n_vec > 4, !is.na(local_scale_degree)) %>% 
    mutate(local_scale_degree = lsd_map[local_scale_degree],
           name = factor(name, levels = unique(test_scales$name) )) %>% 
    group_by(local_scale_degree) %>% 
    mutate(n_lsd = n())  %>% 
    group_by(local_scale_degree, name) %>% 
    summarise(n = n(), n_tot = max(n_lsd), freq = n/n_tot, .groups = "drop") 

  prep <- prep %>%  
    filter(n_tot >= min_n) %>% 
    mutate(text_colour = factor(freq <= .5 * max(freq), labels = c("white", "black")))
  if(sort_scales){
    prep <- prep %>% 
      mutate(name = fct_reorder(name, freq, sum))
  }
  if(sort_scale_degrees){
    prep <- prep %>% 
      mutate(local_scale_degree = fct_reorder(local_scale_degree, n_tot, median) %>% fct_rev())
  }
  q <- prep %>% 
    ggplot(aes(x = local_scale_degree, 
               y = name, 
               fill = freq))
  q <- q + geom_tile() 
  #q <- q + scale_fill_viridis_c(option = "F", direction = -1) 
  q <- q + scale_fill_gradient(low = "white", high = "#083e81")
  q <- q + geom_text(aes(label = sprintf("%02d", round(100 * freq, 0)), colour = text_colour))
  q <- q + scale_colour_manual(values = c("white", "black"), guide = "none")
  q <- q + theme_minimal() 
  q <- q + labs(x = "Local Scale Degree", y = "Scale", fill = "Rel. Freq.")  
  q <- q + theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_rect(fill = "white"), 
                 axis.text.x = element_text(size = 11),  
                 axis.text.y = element_text(size = 11))
  if(nchar(fname) > 0 ){
    ggsave(fname, plot = q, dpi = 600)
  }
  q
}

plot_all_heat_maps <- function(recalc = F){
  if(recalc || !exists("all_scale_fits_weighted_freq")){
    all_scale_fits_weighted_freq <-    
      get_all_scale_fits_fast(wjd_tpc, ret_top_n = 1, weighting = "chordal", cpc_tab = "freq")
    assign("all_scale_fits_weighted_freq", all_scale_fits_weighted_freq, globalenv())
    writexl::write_xlsx(all_scale_fits_weighted_freq, "all_scale_fits_weighted_freq.xlsx")
  }
  if(recalc || !exists("all_scale_fits_weighted_ind")){
    all_scale_fits_weighted_ind <-    
      get_all_scale_fits_fast(wjd_tpc, ret_top_n = 1, weighting = "chordal", cpc_tab = "indicator")
    assign("all_scale_fits_weighted_ind", all_scale_fits_weighted_ind, globalenv())
    writexl::write_xlsx(all_scale_fits_weighted_ind, "all_scale_fits_weighted_ind.xlsx")
  }
  if(recalc || !exists("all_scale_fits_flat_freq")){
    all_scale_fits_flat_freq <-    
      get_all_scale_fits_fast(wjd_tpc, ret_top_n = 1, weighting = "flat", cpc_tab = "freq")
    assign("all_scale_fits_flat_freq", all_scale_fits_flat_freq, globalenv())
    writexl::write_xlsx(all_scale_fits_flat_freq, "all_scale_fits_flat_freq.xlsx")
  }
  if(recalc || !exists("all_scale_fits_flat_ind")){
    all_scale_fits_flat_ind <-    
      get_all_scale_fits_fast(wjd_tpc, ret_top_n = 1, weighting = "flat", cpc_tab = "indicator")
    assign("all_scale_fits_flat_ind", all_scale_fits_flat_ind, globalenv())
    writexl::write_xlsx(all_scale_fits_flat_ind, "all_scale_fits_flat_ind.xlsx")
  }
  q <- scale_lsd_heat_map(all_scale_fits_weighted_freq)
  ggsave(plot = q, filename = "figs/all_scale_fits_weighted_freq.png", dpi = 600)

  q <- scale_lsd_heat_map(all_scale_fits_weighted_ind)
  ggsave(plot = q, filename = "figs/all_scale_fits_weighted_ind.png", dpi = 600)
  
  q <- scale_lsd_heat_map(all_scale_fits_flat_freq)
  ggsave(plot = q, filename = "figs/all_scale_fits_flat_freq.png", dpi = 600)
  
  q <- scale_lsd_heat_map(all_scale_fits_flat_ind)
  ggsave(plot = q, filename = "figs/all_scale_fits_flat_ind.png", dpi = 600)
  return("Done")
}

blues_trigrams_succ_pred <- function(data = wjd_tpc){
  tmp <- wjd_tpc %>%  
    group_by(id) %>% 
    mutate(cdpcx_bigrams = sprintf("%s%s", 
                                   cdpcx, 
                                   lead(cdpcx, default = "$")), 
           cdpcx_trigrams = sprintf("%s%s%s", cdpcx, 
                                    lead(cdpcx, default = "$"), 
                                    lead(cdpcx, 2, default = "$")))  %>% 
    ungroup() %>% 
    mutate(is_blues = factor(tonality_type == "BLUES", labels = c("no_blues", "blues"))) %>% filter(str_detect(substr(cdpcx_trigrams, 2,2), "B"))  %>% 
    mutate(pred = substr(cdpcx_trigrams, 1, 1), 
           succ = substr(cdpcx_trigrams, 3, 3)) 
  blues_succ <- tmp %>% 
    select(pred, succ, cdpcx_trigrams, is_blues)  %>% 
    filter(is_blues == "blues") %>% 
    group_by(succ) %>% 
    summarise(n = n(), tot = nrow(.), freq = n/tot) %>% 
    arrange(desc(freq))
  blues_pred <- tmp %>% 
    select(pred, succ, cdpcx_trigrams, is_blues)  %>% 
    filter(is_blues == "blues") %>% 
    group_by(pred) %>% 
    summarise(n = n(), tot = nrow(.), freq = n/tot) %>% 
    arrange(desc(freq))
  no_blues_succ <- tmp %>% 
    select(pred, succ, cdpcx_trigrams, is_blues)  %>% 
    filter(is_blues != "blues") %>% 
    group_by(succ) %>% 
    summarise(n = n(), tot = nrow(.), freq = n/tot) %>% 
    arrange(desc(freq))
  no_blues_pred <- tmp %>% 
    select(pred, succ, cdpcx_trigrams, is_blues)  %>% 
    filter(is_blues != "blues") %>% 
    group_by(pred) %>% 
    summarise(n = n(), tot = nrow(.), freq = n/tot) %>% 
    arrange(desc(freq))
  succ <- no_blues_succ %>% select(succ, freq_no_blues = freq) %>% left_join(blues_succ %>% select(succ, freq_blues = freq)) %>% mutate(OR = freq_blues/freq_no_blues) %>% arrange(desc(OR))
  pred <-no_blues_pred %>% select(pred, freq_no_blues = freq) %>% left_join(blues_pred %>% select(pred, freq_blues = freq)) %>% mutate(OR = freq_blues/freq_no_blues) %>% arrange(desc(OR))
  browser()   
}

blues_trigrams_plot <- function(data = wjd_tpc, cut_off = .015, save  = T, blue_note = "B"){
  tmp <- data %>% 
    group_by(id) %>% 
    mutate(cdpcx_bigrams = sprintf("%s%s", cdpcx, lead(cdpcx, default = "$")), 
           cdpcx_trigrams = sprintf("%s%s%s", cdpcx, lead(cdpcx, default = "$"), lead(cdpcx, 2, default = "$")))  %>%
    ungroup() %>% 
    mutate(is_blues = factor(tonality_type == "BLUES", labels = c("no_blues", "blues"))) %>% 
    filter(str_detect(substr(cdpcx_trigrams, 2, 2), blue_note)) %>% 
    freq_table_group(is_blues, cdpcx_trigrams) %>% 
    filter(freq_group > cut_off)  %>%  
    pivot_wider(id_cols = c(cdpcx_trigrams), 
                names_from = is_blues, 
                values_from = freq_group)  %>% 
    mutate(blues = set_na(blues, 0), 
           no_blues = set_na(no_blues, 0),  
           or = blues - no_blues, dominance = factor(or > 0, labels = c("Other", "Blues"))) %>% 
    arrange(desc(or)) 
  q <- tmp %>% ggplot(aes(blues, no_blues))
  q <- q + ggrepel::geom_text_repel(aes(color = dominance, 
                                        label = cdpcx_trigrams), 
                                    size = 4, 
                                    max.overlaps = 20) 
  q <- q + theme_bw()  
  q <- q + stat_function(mapping = aes(group = 1), fun = function(x) x) 
  q <- q + geom_point() 
  q <- q + labs(x = "Rel. Freq. Blues", y = "Rel. Freq. Other", color = "")  
  q <- q + scale_color_manual(values = c("lightblue4", "indianred4"), guide = "none")
  if(save){
    ggsave(plot = q, filename = sprintf("figs/blues_trigrams_%s.png", blue_note), dpi = 600)
  }
  q
}

blues_bigrams_plot <- function(data = wjd_tpc, cut_off = .015, save  = T, blue_note = "B"){
  tmp <- data %>% 
    group_by(id) %>% 
    mutate(cdpcx_bigrams = sprintf("%s%s", cdpcx, lead(cdpcx, default = "$")), 
           cdpcx_trigrams = sprintf("%s%s%s", cdpcx, lead(cdpcx, default = "$"), lead(cdpcx, 2, default = "$")))  %>%
    ungroup() %>% 
    mutate(is_blues = factor(tonality_type == "BLUES", labels = c("no_blues", "blues"))) %>% 
    filter(str_detect(cdpcx_bigrams, blue_note)) %>% 
    freq_table_group(is_blues, cdpcx_bigrams) %>% 
    filter(freq_group > cut_off)  %>%  
    pivot_wider(id_cols = c(cdpcx_bigrams), 
                names_from = is_blues, 
                values_from = freq_group)  %>% 
    mutate(blues = set_na(blues, 0), 
           no_blues = set_na(no_blues, 0),  
           or = blues - no_blues, 
           dominance = factor(or > 0, labels = c("Other", "Blues"))) %>% 
    arrange(desc(or)) 
  
  q <- tmp %>% ggplot(aes(blues, no_blues))
  q <- q + ggrepel::geom_text_repel(aes(color = dominance, 
                                        label = cdpcx_bigrams), 
                                    size = 4, 
                                    max.overlaps = 20) 
  q <- q + theme_bw()  
  q <- q + stat_function(mapping = aes(group = 1), fun = function(x) x) 
  q <- q + geom_point() 
  q <- q + labs(x = "Rel. Freq. Blues", y = "Rel. Freq. Other", color = "")  
  q <- q + scale_color_manual(values = c("lightblue4", "indianred4"), guide = "none")
  if(save){
    ggsave(plot = q, filename = sprintf("figs/blues_bigrams_%s.png", blue_note), dpi = 600)
  }
  q
}
blub <- function(){
  tmp <- wjd_transforms %>% 
    group_by(id) %>% 
    mutate(cdpcx_bigrams = sprintf("%s%s", cdpcx_raw, lead(cdpcx_raw, default = "$"))) 
  b3_dist <- tmp %>% 
    filter(str_detect(cdpcx_bigrams, "3B")) %>% 
    freq_table(ioiclass_abs_raw) %>% 
    filter(!is.na(ioiclass_abs_raw))
  
  no_b3_dist <- tmp %>% 
    filter(!str_detect(cdpcx_bigrams, "3B")) %>% 
    freq_table(ioiclass_abs_raw) %>% 
    filter(!is.na(ioiclass_abs_raw))
  browser()
}
blues_cpc_plot <- function(data = wjd_tpc){
  tmp <- data %>% 
    mutate(is_blues = factor(tonality_type != "BLUES", labels = c("Blues", "Other")),
           cpc = factor(cpc, levels = 0:11),
           cdpcx = nice_cdpcx(cdpcx), 
           is_major = chord_type %in% c("6", "7", "maj", "maj7")) 
  browser()
  #q <- tmp %>% ggplot(aes(x = cpc, y = after_stat(count)/tapply(after_stat(count),after_stat(PANEL),sum)[after_stat(PANEL)]))
  q <- tmp %>% ggplot(aes(x = cdpcx, y = after_stat(count)/tapply(after_stat(count),after_stat(PANEL),sum)[after_stat(PANEL)]))
  q <- q + geom_bar()
  q <- q + theme_bw()
  q <- q + scale_y_continuous(labels = scales::percent, name = "Percentage (%)")
  
  q <- q + labs(x = "CPC", y = "Rel. Freq.")
  q <- q  + facet_wrap(~is_blues)
  q <- q + theme_bw()
  q
}