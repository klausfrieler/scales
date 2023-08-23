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
    jazzodata::wjd_tpc %>% 
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