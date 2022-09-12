-   [Scraping genome assembly information for fish species on
    GenBank](#scraping-genome-assembly-information-for-fish-species-on-genbank)
    -   [Loading the packages](#loading-the-packages)
    -   [Loading data-set](#loading-data-set)
    -   [Preparing storages](#preparing-storages)
    -   [Scraping on GenBank](#scraping-on-genbank)
    -   [Write a result file](#write-a-result-file)

# Scraping genome assembly information for fish species on GenBank

## Loading the packages

``` r
# Loading packages
library(tidyverse)
library(rvest)
```

## Loading data-set

``` r
fishes_raw <- read_csv("FRA200List_Latin.csv")
#fishes <- read_csv("FRA200List_Latin.test.csv")

#filtering out species with NA
fishes <- fishes_raw %>% drop_na()
#no_fish <- nrow(fishes) #number of fishes in the list
no_fish <- 5 # for test
fishes <- fishes[1:no_fish,]
```

## Preparing storages

``` r
genome_assembly_exist <- vector()
genome_size_vec <- vector() # target species / a related species / mean within genus
genome_assembly_genus_exist <- vector()
no_related_exist <- vector()
```

## Scraping on GenBank

``` r
# example https://www.ncbi.nlm.nih.gov/genome/?term=Gadus+morhua
genbank_genome_head <- "https://www.ncbi.nlm.nih.gov/genome/?term="


for(i in 1:no_fish){
  Species_vec <- strsplit(fishes[i,]$Latin_name, " ") %>% unlist # split genus/species names
  
  genbank_genome_url <- paste0(genbank_genome_head,Species_vec[1],"+",Species_vec[2])
  genbank_html <- read_html(genbank_genome_url)
  #extracting title element
  target_title <- genbank_html %>% html_element(xpath = "/html/head/title") %>% html_text()
  
  if(!(stringr::str_detect(target_title,pattern="No items found"))){
    genome_assembly_exist[i] <- 1 # genome assembly found at species level
    genome_assembly_genus_exist <- 1  # genome assembly found at genus level
    summary_element <- genbank_html %>% html_element(xpath = '//*[@id="mmtest1"]/div/div/div/table') %>% html_table()
    target_size <- summary_element$X2[grep("total length", summary_element$X2)]
    target_size_vec <- strsplit(target_size, ":") %>% unlist
    genome_size_vec[i] <- round(as.numeric(target_size_vec[2]),digits=0)
    
    #Check related species
    genbank_genome_genus_url <- paste0(genbank_genome_head,Species_vec[1])
    genbank_genus_html <- read_html(genbank_genome_genus_url)
    target_genus_title <- genbank_genus_html %>% html_element(xpath = "/html/head/title") %>% html_text()
    
    if(!(stringr::str_detect(target_genus_title,pattern="ID"))){ #if multiple related species found
      target_genus_items <- genbank_genus_html %>% html_element(xpath = '//*[@id="maincontent"]/div/div[3]/div/h3') %>% html_text()
      items_vec <- strsplit(target_genus_items, " ") %>% unlist
      no_related_exist[i]  <- items_vec[2]
    }else{
      no_related_exist[i] <- 1
    }
  
  Sys.sleep(1) #
    
  }else{
    genome_assembly_exist[i] <- 0 # no genome assembly found at species level
    genome_size_vec[i] <- NA
    
    # search at genus level 
    genbank_genome_genus_url <- paste0(genbank_genome_head,Species_vec[1])
    genbank_genus_html <- read_html(genbank_genome_genus_url)
    target_genus_title <- genbank_genus_html %>% html_element(xpath = "/html/head/title") %>% html_text()
    
  
    if(!(stringr::str_detect(target_genus_title,pattern="No items found"))){
      no_related_exist[i] <- 1 # items found at genus level
      
      if(stringr::str_detect(target_genus_title,pattern="ID")){ #a single related species found
        summary_related_element <-genbank_genus_html %>% html_element(xpath = '//*[@id="mmtest1"]/div/div/div/table') %>% html_table()
        target_related_size <- summary_related_element$X2[grep("total length", summary_related_element$X2)]
        target_related_size_vec <- strsplit(target_related_size, ":") %>% unlist
        genome_size_vec[i] <- round(as.numeric(target_related_size_vec[2]),digits=0)
      
      }else{
        target_genus_items <- genbank_genus_html %>% html_element(xpath = '//*[@id="maincontent"]/div/div[3]/div/h3') %>% html_text()
        items_vec <- strsplit(target_genus_items, " ") %>% unlist
        no_related_exist[i]  <- items_vec[2]
      }
    }else{# No items found in the genus
      no_related_exist[i] <- 0
    }
      
  Sys.sleep(1)
  }
  

}

fishes_genome <- fishes %>% mutate (Assembly_GenBank = genome_assembly_exist, Assembly_related_GenBank = no_related_exist, Genome_size = genome_size_vec)
```

## Write a result file

``` r
write_csv(fishes_genome, "fishes_genome_size.csv")
```
