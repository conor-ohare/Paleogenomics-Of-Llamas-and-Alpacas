library(tidyverse)
library(cowplot)

## ---------------------------------------
# Read in bin files outputted by ADMIXFROG
## ---------------------------------------

# Ancient Samples
C72_GuaVic <- read_csv("bin_files//C72_GuaVic.bin.xz")
C72_GuaCac <- read_csv("bin_files//C72_GuaCac.bin.xz")
C21_GuaVic <- read_csv("bin_files//C21_GuaVic.bin.xz")
C21_GuaCac <- read_csv("bin_files//C21_GuaCac.bin.xz")
C20_GuaVic <- read_csv("bin_files//C20_GuaVic.bin.xz")

# Modern damaged samples
modernGua_GuaCac <- read_csv("bin_files//modernGua_GuaCac.bin.xz")
modernCac_GuaCac <- read_csv("bin_files//modernCac_GuaCac.bin.xz")
modernGua_GuaVic <- read_csv("bin_files//modernGua_GuaVic.bin.xz")
modernCac_GuaVic <- read_csv("bin_files//modernCac_GuaVic.bin.xz")
modernVic_GuaVic <- read_csv("bin_files//modernVic_GuaVic.bin.xz")

# Modern samples
LLA_GuaCac <- read_csv("bin_files/LLA.bin.xz")

# Find the row index of the fifth occurrence of 0 in 'map' column
fifth_zero_row <- which(LLA_GuaCac$map == 0)[5]

# Find the row index of the sixth occurrence of 0 in 'map' column
sixth_zero_row <- which(LLA_GuaCac$map == 0)[6]

# Filter the rows between the fifth and sixth occurrence of 0
filtered_data <- LLA_GuaCac[fifth_zero_row:sixth_zero_row, ]

# View the filtered data
View(filtered_data)
## --------------------------------

## --------
# FUNCTIONS
## --------

admixture_plot <- function(sample_set, chroms=10, plot_name){
  # Plot and save admixture plot for the given bin file
  
  # Find the index of the fifth occurrence of 0 in the 'map' column
  index <- which(sample_set$map == 0)[chroms]
  
  # Remove rows from the index onwards
  sample_set <- sample_set %>%
    slice(1:(index - 1))
  
  zero_indices <- unlist(lapply(which(sample_set$map == 0), function(x) (x / 100) - 0.01))
  
  sample_set <- sample_set %>%
    mutate(map = seq(0, length.out = n(), by = 0.01))
  
  nice_plot <- sample_set %>% gather(k, v, -chrom:-n_snps) %>% 
    filter(k!="AFR", v>.1) %>%
    ggplot(aes(x=map, y=v, fill=k)) + geom_col() +
    # geom_vline(xintercept = zero_indices, color = "black", size=0.3) +
    xlab(expression(paste("Base-pair distance ", (10^8)))) +
    ylab("Pr(Local Ancestry)") +
    labs(fill = "Ancestry") +
    scale_fill_manual(values = c('GUA' = 'steelblue', 'GUAVIC' = 'darkorange', 'VIC' = 'forestgreen',
                                 'CAC' = 'salmon', 'GUACAC' = 'maroon')) +
    theme_minimal()
    
  plot(nice_plot)
  
  ggsave(plot_name, nice_plot, width = 10, height = 2)
  
}

## ----------------------------------------------------

# Create admixture plots and save in local directory
admixture_plot(C20_GuaVic, 62, "C20_GuaVic.png")
admixture_plot(C21_GuaCac, 62, "C21_GuaCac.png")
admixture_plot(C21_GuaVic, 62, "C21_GuaVic.png")
admixture_plot(C72_GuaCac, 62, "C72_GuaCac.png")
admixture_plot(C72_GuaVic, 62, "C72_GuaVic.png")

admixture_plot(modernCac_GuaCac, 62, "modernCac_GuaCac.png")
admixture_plot(modernGua_GuaCac, 62, "modernGua_GuaCac.png")
admixture_plot(modernGua_GuaVic, 62, "modernGua_GuaVic.png")
admixture_plot(modernCac_GuaVic, 62, "modernCac_GuaVic.png")
admixture_plot(modernVic_GuaVic, 62, "modernVic_GuaVic.png")

admixture_plot(LLA_GuaCac, 62, "LLA_GuaCac.png")




