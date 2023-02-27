#plot.no_genome_deposited.R


library(tidyverse)
library(lubridate)

list20220915 <- read_csv("aquatic_organism_genome_size_2022_0915.csv")
list20230221 <- read_csv("aquatic_organism_genome_size_2023_0221.csv")

No_sp_genome_20100331 <- 1
No_sp_genome_20220915 <- sum(!is.na(list20220915$Species_genome_size)) 
No_sp_genome_20230221 <- sum(!is.na(list20230221$Species_genome_size)) 

No_sp_genus_20100331 <- 3
No_genus_genome_20220915 <- sum(!is.na(list20220915$Genus_genome_size)) 
No_genus_genome_20230221 <- sum(!is.na(list20230221$Genus_genome_size)) 


genome_chronology <- tibble(date = c(ymd("2010-03-31"), ymd("2022-09-15"), ymd("2023-02-21")), 
            Species = c(No_sp_genome_20100331, No_sp_genome_20220915,No_sp_genome_20230221),
            Genus = c(No_sp_genus_20100331, No_genus_genome_20220915, No_genus_genome_20230221)
            )


genome_chronology <- genome_chronology %>% tidyr::gather(Levels, Value, -date) 


plot_sp_level <- ggplot(data = genome_chronology, aes(x=date, y = Value, color=Levels, group=Levels)) + 
  geom_point() + 
  geom_line(linetype = "dotted") +
  labs(y="No. fisheries species", title="No. fisheries species with genome sequence deposited in GenBank (last update: 2023/2/21)")

png("No_sp_genome_deposited.png", width = 600, height = 400)
plot_sp_level
dev.off()
