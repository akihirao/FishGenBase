#plot.no_genome_deposited.R


library(tidyverse)
library(ggplot2)
library(lubridate)

last_day <- "2024-02-07"

list20220915 <- read_csv("aquatic_organism_genome_size_2022_0915.csv")
list20230221 <- read_csv("aquatic_organism_genome_size_2023_0221.csv")
list20230313 <- read_csv("aquatic_organism_genome_size_2023_0313.csv")
list20230329 <- read_csv("aquatic_organism_genome_size_2023_0329.csv")
list20230421 <- read_csv("aquatic_organism_genome_size_2023_0421.csv")
list20230515 <- read_csv("aquatic_organism_genome_size_2023_0515.csv")
list20230531 <- read_csv("aquatic_organism_genome_size_2023_0531.csv")
list20230724 <- read_csv("aquatic_organism_genome_size_2023_0724.csv")
list20231201 <- read_csv("aquatic_organism_genome_size_2023_1201.csv")
list20240109 <- read_csv("aquatic_organism_genome_size_2024_0109.csv")
list20240207 <- read_csv("aquatic_organism_genome_size_2024_0207.csv")

last_list <- list20240207


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
No_sp_genome_20231201 <- sum(!is.na(list20231201$Genome_size_of_the_species_Mbp)) 
No_sp_genome_20240109 <- sum(!is.na(list20240109$Genome_size_of_the_species_Mbp)) 
No_sp_genome_20240207 <- sum(!is.na(list20240207$Genome_size_of_the_species_Mbp)) 


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
No_genus_genome_20231201 <- sum(!is.na(list20231201$Average_genome_size_of_the_genus_Mbp)) 
No_genus_genome_20240109 <- sum(!is.na(list20240109$Average_genome_size_of_the_genus_Mbp)) 
No_genus_genome_20240207 <- sum(!is.na(list20240207$Average_genome_size_of_the_genus_Mbp)) 


genome_chronology <- tibble(
  date = c(ymd("2010-03-31"),ymd("2020-04-21"),ymd("2022-09-15"), 
           ymd("2023-02-21"),ymd("2023-03-13"),ymd("2023-04-21"),
           ymd("2023-05-15"),ymd("2023-05-31"),ymd("2023-07-24"),
           ymd("2023-12-01"),ymd("2024-01-09"),ymd("2024-02-07")), 
  Species = c(No_sp_genome_20100331, No_sp_genome_20200421, No_sp_genome_20220915,
              No_sp_genome_20230221,No_sp_genome_20230313, No_sp_genome_20230421,
              No_sp_genome_20230515,No_sp_genome_20230531,No_sp_genome_20230724,
              No_sp_genome_20231201,No_sp_genome_20240109,No_sp_genome_20240207),
  Genus = c(No_sp_genus_20100331,No_sp_genus_20200421,No_genus_genome_20220915,
            No_genus_genome_20230221,No_genus_genome_20230313,No_genus_genome_20230421,
            No_genus_genome_20230515,No_genus_genome_20230531,No_genus_genome_20230724,
            No_genus_genome_20231201,No_genus_genome_20240109,No_genus_genome_20240207)
            )

Taxonomic_class_lab <- c("Species","Genus")

genome_chronology <- genome_chronology %>% 
  tidyr::gather(Levels, Value, -date) %>% mutate(Class = factor(Levels, levels=Taxonomic_class_lab))

current_day <-  Sys.Date()
title_lab <- paste0("No. species with genome sequence deposited in GenBank: last update@",last_day)
plot_sp_level <- ggplot(data = genome_chronology, aes(x=date, y = Value, color=Class, group=Class)) + 
  geom_point(size=4.5) + 
  geom_line(linetype = "dashed", linewidth = 1) +
  labs(x = "Date", y="No. species", title=title_lab)+
  theme(legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

png("No_sp_genome_deposited.png", width = 600, height = 400)
plot_sp_level
dev.off()


# No. species/genus with genome assembly
no_target_species <- length(last_list$Scientific_name)
no_species_assembly <- sum(last_list$Genome_size_of_the_species_Mbp > 0,na.rm=TRUE)
no_genus_assembly <- sum(last_list$No_species_with_assembly_within_the_genus > 0)
cat("No. species with genome assembly")
print(no_species_assembly)
cat("Percentage of no. species with genome assembly")
print(no_species_assembly/no_target_species)

cat("No. genus with genome assembly")
print(no_genus_assembly)
cat("Percentage of no. genus with genome assembly")
print(no_genus_assembly/no_target_species)


last_list_with_assembly <- last_list %>% filter(Genome_size_of_the_species_Mbp > 0)
gg_assembly_len_dist <- ggplot(last_list_with_assembly,
                               aes(x= Genome_size_of_the_species_Mbp)) +
  geom_histogram() +
  labs(x = "Genome size (Mbp)", y = "Count") +
  theme(text = element_text(size = 24))



png("genome_size_dist.png", width = 600, height = 400)
plot(gg_assembly_len_dist)
dev.off()

