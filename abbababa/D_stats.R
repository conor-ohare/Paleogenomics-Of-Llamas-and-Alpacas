library(tidyverse)
library(dplyr)

## --------
# READ DATA
## --------

# Lama genus (accidentally contains vicuña, so needs to be removed)
abbababa_stats <- as_tibble(read.csv("inclDamaged.txt", header = TRUE, sep = "\t")) %>%
  filter(!grepl("Vicugna", H1) & !grepl("Vicugna", H2) & !grepl("Vicugna", H3))

# Swap mislabeled cacsilensis based on PCA
abbababa_modified <- abbababa_stats %>%
  mutate(H1 = ifelse(str_detect(H1, "SRR11905261|SRR11905272"), "converted_LamaGuanicoeGuanicoe", H1),
         H2 = ifelse(str_detect(H2, "SRR11905261|SRR11905272"), "converted_LamaGuanicoeGuanicoe", H2))


# Vicugna genus
vicugna.abbababa <- as_tibble(read.csv("vicugna.txt", header = TRUE, sep = "\t"))


## --------
# FUNCTIONS
## --------

get_second_element <- function(x) {
  return(x[2])
}

abbababa <- function(statistics, h3_ind, plot_name){
  # (LAMA GENUS) Produce and save D-statistics plot for a sample in the H3 position
  dedamaged <- h3_ind
  if(substr(h3_ind, 1, 1) == "d") {
    dedamaged <- substr(h3_ind, 2, nchar(h3_ind))
  }
  
  ancient_stats <- statistics %>% 
    filter(H3==h3_ind) %>% 
    filter(!grepl("C21|C72", H1) & !grepl("C21|C72", H2) & !grepl(dedamaged, H1) & !grepl(dedamaged, H2)) %>%
    filter(sapply(strsplit(H1, "_"), get_second_element) != sapply(strsplit(H2, "_"), get_second_element)) %>%
    mutate(is_bc = grepl("Glama", H2) | (!grepl("Glama", H1) & !grepl("Glama", H2) & !grepl("GuanicoeGuanicoe", H2))) %>% 
    mutate(col1_new = if_else(is_bc, H2, H1),
           col2_new = if_else(is_bc, H1, H2),
           col6_new = if_else(is_bc, -Dstat, Dstat)) %>% 
    select(-c(H1, H2, Dstat, is_bc)) %>% 
    rename(H1 = col1_new, H2 = col2_new, Dstat = col6_new) %>%
    mutate(Comparison = paste(sapply(strsplit(H1, "_"), get_second_element), sapply(strsplit(H2, "_"), get_second_element), sep="-"))
  
  print(distinct(ancient_stats, select=Comparison))
  ancient_stats$Comparison <- as.factor(ancient_stats$Comparison)
  
  
  
  p <- ggplot(ancient_stats, aes(x=Comparison, y=Dstat)) + 
    geom_jitter(position=position_jitter(0.1), aes(colour = abs(Z) > 3)) + 
    scale_colour_manual(name = '|Z| > 3', values = setNames(c('tomato','black'),c(T, F))) +
    scale_x_discrete(labels= c("A = Llama, B = Cacsilensis",
                               "A = Llama, B = Guanaco",
                               "A = Cacsilensis, B = Guanaco")) +
    scale_y_continuous(limits = c(-0.5, 0.5)) +
    labs( 
         x = "Comparisons",
         y = "D-statistic",
         size = 10) +
    theme_bw() + 
    theme(legend.position = c(.9, .8)) +
    theme(axis.text.x = element_text(size = 9))  +
    theme(axis.text.y = element_text(size = 10))
  
  plot(p)
  
  # Save plot
  ggsave(plot_name, width = 6.5, height = 4)
}


abbababa_vicugna <- function(statistics, h3_ind, plot_name){
  # (VICUGNA GENUS) Produce and save D-statistics plot for a sample in the H3 position
  
  dedamaged <- h3_ind
  if(substr(h3_ind, 1, 1) == "d") {
    dedamaged <- substr(h3_ind, 2, nchar(h3_ind))
  }
  
  ancient_stats <- statistics %>% 
    filter(H3==h3_ind) %>% 
    filter(!grepl("C20", H1) & !grepl("C20", H2) & !grepl(dedamaged, H1) & !grepl(dedamaged, H2)) %>%
    filter(sapply(strsplit(H1, "_"), get_second_element) != sapply(strsplit(H2, "_"), get_second_element)) %>%
    mutate(is_bc = grepl("Pacos", H2) | (!grepl("Pacos", H1) & !grepl("Pacos", H2) & !grepl("VicugnaVicugna", H2))) %>% 
    mutate(col1_new = if_else(is_bc, H2, H1),
           col2_new = if_else(is_bc, H1, H2),
           col6_new = if_else(is_bc, -Dstat, Dstat)) %>% 
    select(-c(H1, H2, Dstat, is_bc)) %>% 
    rename(H1 = col1_new, H2 = col2_new, Dstat = col6_new) %>%
    mutate(Comparison = paste(sapply(strsplit(H1, "_"), get_second_element), sapply(strsplit(H2, "_"), get_second_element), sep="-"))
  
  print(distinct(ancient_stats, select=Comparison))
  ancient_stats$Comparison <- as.factor(ancient_stats$Comparison)
  
  print(ancient_stats$Comparison)
  
  
  p <- ggplot(ancient_stats, aes(x=Comparison, y=Dstat)) + 
    geom_jitter(position=position_jitter(0.1), aes(colour = abs(Z) > 3)) + 
    scale_colour_manual(name = '|Z| > 3', values = setNames(c('tomato','black'),c(T, F))) +
    scale_x_discrete(labels= c("A = Alpaca, B = Mensalis",
                               "A = Alpaca, B = Vicuña",
                               "A = Vicuña, B = Mensalis")) +
    scale_y_continuous(limits = c(-0.5, 0.5)) +
    labs( 
      x = "Comparisons",
      y = "D-statistic",
      size = 20) +
    theme_bw() + 
    theme(legend.position = c(.9, .8)) +
    theme(axis.text.x = element_text(size = 9))  +
    theme(axis.text.y = element_text(size = 10))
  
  plot(p)
  
  # Save plot
  ggsave(plot_name, width = 6.5, height = 4)
}

compare_damage <- function(statistics, h3_ind, plot_name){
  # (LAMA GENUS) Compare modern sample D-statistics to its damaged modern counterpart
  tb_nodamage <- statistics %>% 
    filter(startsWith(H3, paste0(h3_ind, collapse = ""))) %>% 
    filter(!grepl("C21|C72", H1) & !grepl("C21|C72", H2) & !grepl(h3_ind, H1) & !grepl(h3_ind, H2)) %>%
    filter(sapply(strsplit(H1, "_"), get_second_element) != sapply(strsplit(H2, "_"), get_second_element)) %>%
    mutate(is_bc = grepl("Glama", H2) | (!grepl("Glama", H1) & !grepl("Glama", H2) & !grepl("Cacsilensis", H2))) %>% 
    mutate(col1_new = if_else(is_bc, H2, H1),
           col2_new = if_else(is_bc, H1, H2),
           col6_new = if_else(is_bc, -Dstat, Dstat)) %>% 
    select(-c(H1, H2, Dstat, is_bc)) %>% 
    rename(H1 = col1_new, H2 = col2_new, Dstat = col6_new) %>%
    mutate(Comparison = paste(sapply(strsplit(H1, "_"), get_second_element), sapply(strsplit(H2, "_"), get_second_element), sep="-")) %>%
    arrange(H1, H2) %>%
    rename("D_nodamage" = Dstat)
  
  tb_nodamage$Comparison <- as.factor(tb_nodamage$Comparison)
  
  damaged_version <- paste("d",h3_ind, sep="")
  print(damaged_version)
  
  tb_damage <- statistics %>% 
    filter(startsWith(H3, paste0(damaged_version, collapse = ""))) %>% 
    filter(!grepl("C21|C72", H1) & !grepl("C21|C72", H2) & !grepl(h3_ind, H1) & !grepl(h3_ind, H2)) %>%
    filter(sapply(strsplit(H1, "_"), get_second_element) != sapply(strsplit(H2, "_"), get_second_element)) %>%
    mutate(is_bc = grepl("Glama", H2) | (!grepl("Glama", H1) & !grepl("Glama", H2) & !grepl("Cacsilensis", H2))) %>% 
    mutate(col1_new = if_else(is_bc, H2, H1),
           col2_new = if_else(is_bc, H1, H2),
           col6_new = if_else(is_bc, -Dstat, Dstat)) %>% 
    select(-c(H1, H2, Dstat, is_bc)) %>% 
    rename(H1 = col1_new, H2 = col2_new, Dstat = col6_new) %>%
    mutate(Comparison = paste(sapply(strsplit(H1, "_"), get_second_element), sapply(strsplit(H2, "_"), get_second_element), sep="-")) %>%
    arrange(H1, H2) %>%
    rename("D_damage" = Dstat)
  
  
  tb_damage$Comparison <- as.factor(tb_damage$Comparison)
  
  print(tb_damage)
  
  combined_df <- bind_cols(select(tb_nodamage, D_nodamage), select(tb_damage, D_damage))
  
  print(combined_df)
  
  
  
  p <- ggplot(combined_df, aes(x=D_nodamage, y=D_damage)) + 
    geom_point() + 
    labs( 
      x = "D-statistic (no damage)",
      y = "D-statistic (damage)") +
    theme_bw()
  
  plot(p)
  
  # Save plot
  ggsave(plot_name, width = 6.5, height = 4)
}

## ------------------------------------------

# --------------
## COMPUTE PLOTS
# --------------

# D-statistics for H3 sample

#abbababa(abbababa_modified, "dSRR11905273_LamaGuanicoeCacsilensis", "dSRR11905273_LamaGuanicoeCacsilensis.ABBABABA.png")
#abbababa(abbababa_modified, "C72", "C72.ABBABABA_modified.png")
#abbababa(abbababa_modified, "dSRR11905250_LamaGuanicoeGuanicoe", "dGuanaco.ABBABABA_modified.png")
#abbababa(abbababa_modified, "SRR11905273_LamaGuanicoeCacsilensis", "Cacsilensis.ABBABABA.png")
#abbababa_vicugna(vicugna.abbababa, "C20", "C20.ABBABABA.png")
#abbababa(abbababa_modified, "SRR11905251_LamaGuanicoeGuanicoe", "SRR11905251.ABBABABA.png")
#abbababa_vicugna(vicugna.abbababa, "SRR11905263_VicugnaVicugnaVicugna", "SRR11905263.ABBABABA.png")
#abbababa(abbababa_modified, "C21", "C21.ABBABABA_modified.png")
#abbababa(abbababa_modified, "SRR11905270_LamaGlama", "SRR11905270_LamaGlama.ABBABABA.png")
#unique(abbababa_modified$H1)

# Comparative plots

#compare_damage(abbababa_modified, "SRR11905250", "SRR11905250_compare.png")
#compare_damage(abbababa_modified, "SRR11905273", "SRR11905273_compare.png")
