#plot.no_genome_deposited.R


library(tidyverse)
library(ggplot2)
library(lubridate)

list20220915 <- read_csv("aquatic_organism_genome_size_2022_0915.csv")
list20230221 <- read_csv("aquatic_organism_genome_size_2023_0221.csv")
list20230313 <- read_csv("aquatic_organism_genome_size_2023_0313.csv")
list20230329 <- read_csv("aquatic_organism_genome_size_2023_0329.csv")
list20230421 <- read_csv("aquatic_organism_genome_size_2023_0421.csv")
list20230515 <- read_csv("aquatic_organism_genome_size_2023_0515.csv")
list20230531 <- read_csv("aquatic_organism_genome_size_2023_0531.csv")
list20230724 <- read_csv("aquatic_organism_genome_size_2023_0724.csv")


No_sp_genome_20100331 <- 1
No_sp_genome_20200421 <- 18
No_sp_genome_20220915 <- sum(!is.na(list20220915$Genome_size_of_the_species_Mbp)) 
No_sp_genome_20230221 <- sum(!is.na(list20230221$Genome_size_of_the_species_Mbp)) 
No_sp_genome_20230313 <- sum(!is.na(list20230313$Genome_size_of_the_species_Mbp)) 
No_sp_genome_20230329 <- sum(!is.na(list20230329$Genome_size_of_the_species_Mbp)) 
No_sp_genome_20230421 <- sum(!is.na(list20230421$Genome_size_of_the_species_Mbp)) 
No_sp_genome_20230515 <- sum(!is.na(list20230515$Genome_size_of_the_species_Mbp)) 
No_sp_genome_20230531 <- sum(!is.na(list20230531$Genome_size_of_the_species_Mbp)) 
No_sp_genome_20230724 <- sum(!is.na(list20230724$Genome_size_of_the_species_Mbp)) 


No_sp_genus_20100331 <- 3
No_sp_genus_20200421 <- 43
No_genus_genome_20220915 <- sum(!is.na(list20220915$Average_genome_size_of_the_genus_Mbp)) 
No_genus_genome_20230221 <- sum(!is.na(list20230221$Average_genome_size_of_the_genus_Mbp)) 
No_genus_genome_20230313 <- sum(!is.na(list20230313$Average_genome_size_of_the_genus_Mbp)) 
No_genus_genome_20230329 <- sum(!is.na(list20230329$Average_genome_size_of_the_genus_Mbp)) 
No_genus_genome_20230421 <- sum(!is.na(list20230421$Average_genome_size_of_the_genus_Mbp)) 
No_genus_genome_20230515 <- sum(!is.na(list20230515$Average_genome_size_of_the_genus_Mbp)) 
No_genus_genome_20230531 <- sum(!is.na(list20230531$Average_genome_size_of_the_genus_Mbp)) 
No_genus_genome_20230724 <- sum(!is.na(list20230724$Average_genome_size_of_the_genus_Mbp)) 


genome_chronology <- tibble(
  date = c(ymd("2010-03-31"),ymd("2020-04-21"),ymd("2022-09-15"), 
           ymd("2023-02-21"),ymd("2023-03-13"),ymd("2023-04-21"),
           ymd("2023-05-15"),ymd("2023-05-31"),ymd("2023-07-24")), 
  Species = c(No_sp_genome_20100331, No_sp_genome_20200421, No_sp_genome_20220915,
              No_sp_genome_20230221,No_sp_genome_20230313, No_sp_genome_20230421,
              No_sp_genome_20230515,No_sp_genome_20230531,No_sp_genome_20230724),
  Genus = c(No_sp_genus_20100331,No_sp_genus_20200421,No_genus_genome_20220915,
            No_genus_genome_20230221,No_genus_genome_20230313,No_genus_genome_20230421,
            No_genus_genome_20230515,No_genus_genome_20230531,No_genus_genome_20230724)
            )

Taxonomic_class_lab <- c("Species","Genus")

genome_chronology <- genome_chronology %>% 
  tidyr::gather(Levels, Value, -date) %>% mutate(Class = factor(Levels, levels=Taxonomic_class_lab))

current_day <-  Sys.Date()
title_lab <- paste0("No. species with genome sequence deposited in GenBank: last update@",current_day)
plot_sp_level <- ggplot(data = genome_chronology, aes(x=date, y = Value, color=Class, group=Class)) + 
  geom_point(size=4.5) + 
  geom_line(linetype = "dashed", linewidth = 1) +
  labs(y="No. species", title=title_lab)

png("No_sp_genome_deposited.png", width = 600, height = 400)
plot_sp_level
dev.off()
