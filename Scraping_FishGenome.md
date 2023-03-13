-   <a
    href="#scraping-genome-assembly-information-for-fish-species-on-genbank"
    id="toc-scraping-genome-assembly-information-for-fish-species-on-genbank">Scraping
    genome assembly information for fish species on GenBank</a>
    -   <a href="#loading-the-packages" id="toc-loading-the-packages">Loading
        the packages</a>
    -   <a href="#loading-dataset" id="toc-loading-dataset">Loading dataset</a>
    -   <a href="#preparing-storages" id="toc-preparing-storages">Preparing
        storages</a>
    -   <a href="#defining-scraping-function"
        id="toc-defining-scraping-function">Defining scraping function</a>
    -   <a href="#scraping-on-genbank" id="toc-scraping-on-genbank">Scraping on
        GenBank</a>
    -   <a href="#write-a-result-file" id="toc-write-a-result-file">Write a
        result file</a>

# Scraping genome assembly information for fish species on GenBank

## Loading the packages

``` r
# Loading packages
library(tidyverse)
library(rvest)
library(rentrez)
```

## Loading dataset

``` r
species_raw <- read_csv("FRA200List_Latin.csv")

#filtering out species with NA
species <- species_raw %>% drop_na()
no_species <- nrow(species) #number of fishes in the list

#Set a small dataset for test procedure
#no_species <- 20 # for test
#species <- species[1:no_species,]
```

``` r
# genome size estimate from animal genome size database
#http://www.genomesize.com/index.php
AGSDB_List <- read_csv("AGSDB_List.csv")
no_AGSDB_List <- nrow(AGSDB_List)
AGSDB_C_vec <- vector()
AGSDB_genus_vec <- vector()

for(i in 1:no_AGSDB_List){
  AGSDB_C_values <- strsplit(AGSDB_List$'C-value'[i], "-") %>% unlist
  AGSDB_C_vec[i] <- as.numeric(AGSDB_C_values[1])
  AGSDB_species_unlist <-strsplit(AGSDB_List$Species[i]," ") %>% unlist
  AGSDB_genus_vec[i] <- AGSDB_species_unlist[1]
}
AGSDB_List <- AGSDB_List %>% mutate(Genus=AGSDB_genus_vec, C_value=AGSDB_C_vec)

AGSDB_genus_ave_genome_size <- tapply(AGSDB_List$C_value,AGSDB_List$Genus, mean)
AGSDB_no_genus_avail <- tapply(AGSDB_List$C_value,AGSDB_List$Genus, length)
```

## Preparing storages

``` r
target_sp_genome_assembly_exist <- vector()
target_sp_genome_size_vec <- vector() # genome size of the target species
related_spp_genome_size_vec <- vector() # mean genome size of related species
related_spp_genome_assembly_exist <- vector()
no_related_spp_exist <- vector()

No_deposited_sp_level_genome_vec <- vector()
representative_assembly_ID_vec <- vector()
representative_assembly_status_vec <- vector()
representative_contigN50_vec <- vector()
representative_scaffoldN50_vec <- vector()

AGSDB_genus_genome_size_vec <- vector()
AGSDB_no_genus_avail_vec <- vector()
```

## Defining scraping function

``` r
#https://bioconnector.github.io/workshops/r-ncbi.html#introduction
#https://pediatricsurgery.hatenadiary.jp/entry/2018/01/10/205737
#entrez_db_searchable(db ="genome")

#-------------------------------------------------------------------------
#Scraping information of genome-assembly of a target species using rentrez
genome_info_sp_NIH <- function(genus, species){
  
  query_species_lab <- paste0(genus," ",species,"[Organism]")
  spp_genome_info <- entrez_search(db="assembly", term =query_species_lab)
  no_assembly <- spp_genome_info$count
  
  if(no_assembly > 1){
  
    spp_genome_info_list <- entrez_summary(db="assembly", id=spp_genome_info$ids)
        
      for(i in 1:no_assembly){
    
        if(spp_genome_info_list[[i]]$refseq_category=="representative genome"){
          representative_sp_genome <- spp_genome_info_list[[i]] #Primary genome-assembly
          representative_sp_genome_accession <- spp_genome_info_list[[i]]$assemblyaccession
        }
      }
    
  }else if(no_assembly == 1){
    representative_sp_genome <- entrez_summary(db="assembly", id=spp_genome_info$ids)
    representative_sp_genome_accession <- representative_sp_genome$assemblyaccession
  }else{#(no_assembly == 0)
     representative_sp_genome <- "NA"
     representative_sp_genome_accession <- "NA"
  } 
  output <- list(no_assembly,representative_sp_genome,representative_sp_genome_accession)
  return(output)
  Sys.sleep(1) #
}
#-------------------------------------------------------------------------


genome_size_sp_NIH <- function(Taxon_ID){
  NIH_genome_head <- "https://www.ncbi.nlm.nih.gov/data-hub/genome/?taxon="
  NIH_genome_url <- paste0(genbank_genome_head,genus,Taxon_ID)
  NIH_html <- read_html(genbank_genome_url)
  
  summary_element_pre <- NIH_html %>% 
      html_element(xpath = '//*[@id="page_content"]/section[3]/div[2]')

  #new_genome_page_in NIH, https://www.ncbi.nlm.nih.gov/data-hub/genome/?taxon=
  #for example of Japanese eel, https://www.ncbi.nlm.nih.gov/data-hub/genome/?taxon=7937
  
  #target_title <- NIH_html %>% html_element(xpath = "/html/head/title") %>% html_text()
  
  if(!(stringr::str_detect(target_title,pattern="No items found"))){
    summary_element_pre <- genbank_html %>% 
      html_element(xpath = '//*[@id="mmtest1"]/div/div/div/table')
    if(!(length(summary_element_pre)==0)){ #if web format unavaialable
      summary_element <- summary_element_pre %>% html_table()
      
      #browser()
      
      target_size <- summary_element$X2[grep("total length", summary_element$X2)]
      target_size_vec <- strsplit(target_size, ":") %>% unlist
      genome_size_out <- round(as.numeric(target_size_vec[2]),digits=0)
    }else{
      genome_size_out <- NA #
    }
       

    
  }else{
    genome_size_out <- NA
  }
  
  return(genome_size_out)
  Sys.sleep(1) #
}


#=======




genome_size_sp_GenBank <- function(genus, species){
  genbank_genome_head <- "https://www.ncbi.nlm.nih.gov/genome/?term="
  genbank_genome_url <- paste0(genbank_genome_head,genus,"+",species)
  genbank_html <- read_html(genbank_genome_url)
  
  #new_genome_page_in NIH, https://www.ncbi.nlm.nih.gov/data-hub/genome/?taxon=
  #for example of Japanese eel, https://www.ncbi.nlm.nih.gov/data-hub/genome/?taxon=7937
  
  target_title <- genbank_html %>% html_element(xpath = "/html/head/title") %>% html_text()
  
  if(!(stringr::str_detect(target_title,pattern="No items found"))){
    summary_element_pre <- genbank_html %>% 
      html_element(xpath = '//*[@id="mmtest1"]/div/div/div/table')
    if(!(length(summary_element_pre)==0)){ #if web format unavaialable
      summary_element <- summary_element_pre %>% html_table()
      
      #browser()
      
      target_size <- summary_element$X2[grep("total length", summary_element$X2)]
      target_size_vec <- strsplit(target_size, ":") %>% unlist
      genome_size_out <- round(as.numeric(target_size_vec[2]),digits=0)
    }else{
      genome_size_out <- NA #
    }
       

    
  }else{
    genome_size_out <- NA
  }
  
  return(genome_size_out)
  Sys.sleep(1) #
}


multi_species_genome_list <- function(genus_name){

  multi_genome_head <- "https://www.ncbi.nlm.nih.gov/genome/?term="
  multi_genome_url <- paste0(multi_genome_head,genus_name)
  multi_html <- read_html(multi_genome_url)
  
  #no_related
  multi_items <- multi_html %>% 
    html_element(xpath = '//*[@id="maincontent"]/div/div[3]/div/h3') %>% 
    html_text()
  items_vec <- strsplit(multi_items, " ") %>% unlist
  no_related  <- items_vec[2]
  
  species_vec <- vector()
  for(i in 1:no_related){
    xpath_head1 <- '//*[@id="maincontent"]/div/div[5]/div['
    xpath_head2 <- ']/div[2]/p'
    xpath_lab <- paste0(xpath_head1,i,xpath_head2)
    species_vec[i] <- multi_html %>% 
      html_element(xpath = xpath_lab) %>% html_text()
  }
  output <- list(no_related,species_vec)
  return(output)
  Sys.sleep(1) #
}
```

## Scraping on GenBank

``` r
# example https://www.ncbi.nlm.nih.gov/genome/?term=Gadus+morhua
genbank_genome_head <- "https://www.ncbi.nlm.nih.gov/genome/?term="


for(i in 1:no_species){
  # split genus/species names
  genus_species_name <- strsplit(species[i,]$Scientific_name, " ") %>% unlist 
  genus_name <- genus_species_name[1]
  species_name <- genus_species_name[2]
  
  
  #------------------------------------------
  #scraping representative genome using rentrez
  rentrez_scraping_out <- genome_info_sp_NIH(genus_name, species_name)
  if(rentrez_scraping_out[[1]]>=1){
    representative_assembly_ID_vec[i] <- rentrez_scraping_out[[3]]
    representative_assembly_status_vec[i] <- rentrez_scraping_out[[2]]$assemblystatus
    representative_contigN50_vec[i] <- rentrez_scraping_out[[2]]$contign50
    representative_scaffoldN50_vec[i] <- rentrez_scraping_out[[2]]$scaffoldn50
  }else{
    representative_assembly_ID_vec[i] <- NA
    representative_assembly_status_vec[i] <- NA
    representative_contigN50_vec[i] <- NA
    representative_scaffoldN50_vec[i] <- NA
  }
  No_deposited_sp_level_genome_vec[i] <- rentrez_scraping_out[[1]]
  #------------------------------------------
  
  
  AGSDB_genus_ID <- which(names(AGSDB_genus_ave_genome_size)==genus_name)
  
  if(!(identical(AGSDB_genus_ID,integer(0)))){
    AGSDB_genus_genome_size_vec[i] <- round(as.numeric(AGSDB_genus_ave_genome_size[AGSDB_genus_ID])*978,digits=0)
    AGSDB_no_genus_avail_vec[i] <- as.numeric(AGSDB_no_genus_avail[AGSDB_genus_ID])
  }else{
    AGSDB_genus_genome_size_vec[i] <- NA
    AGSDB_no_genus_avail_vec[i] <- NA
  }
 
  NIH_organism_lab1 <- "https://www.ncbi.nlm.nih.gov/genome?term=%22"
  NIH_organism_lab2 <- "%20"
  NIH_organism_lab3 <- "%22%5BOrganism%5D%20&cmd=DetailsSearch"
  
  
  
  NIH_sp_url <- paste0(NIH_organism_lab1,genus_name,NIH_organism_lab2,species_name,NIH_organism_lab3)
  
  NIH_sp_html <- read_html(NIH_sp_url)
  #extracting title element
  target_title <- NIH_sp_html %>% html_element(xpath = "/html/head/title") %>% html_text()
  
  # if genome assembly of the target species exist
  if(!(stringr::str_detect(target_title,pattern="No items found"))){
    target_sp_genome_assembly_exist[i] <- 1 # genome assembly found at species level
    
    target_sp_genome_size_vec[i] <- genome_size_sp_GenBank(genus_name,species_name)
    
    #Check related species
    genbank_genome_genus_url <- paste0(genbank_genome_head,genus_name)
    genbank_genus_html <- read_html(genbank_genome_genus_url)
    target_genus_title <- genbank_genus_html %>% html_element(xpath = "/html/head/title") %>% html_text()
    
    #if genome assemblies of multiple related species found
    if(!(stringr::str_detect(target_genus_title,pattern="ID"))){ 
      multi_related_list <- multi_species_genome_list(genus_name)
      no_related_spp_exist[i]  <- multi_related_list[[1]]
      multi_related_list <- multi_related_list[[2]]
      multi_related_spp_genome_size <- vector()
      
      for(j in 1:no_related_spp_exist[i]){
        sp_name_recover <- strsplit(multi_related_list[j], " ") %>% unlist
        multi_related_spp_genome_size[j] <- genome_size_sp_GenBank(sp_name_recover[1],sp_name_recover[2])
      }
      related_spp_genome_size_vec[i] <- round(mean(multi_related_spp_genome_size, na.rm=TRUE),digits=0)
      
    }else{
      no_related_spp_exist[i] <- 1
      related_spp_genome_size_vec[i] <- genome_size_sp_GenBank(genus_name,species_name)
    }
  
  Sys.sleep(1) #
    
  }else{
    target_sp_genome_assembly_exist[i] <- 0 # no genome assembly found at species level
    target_sp_genome_size_vec[i] <- NA
    
    # search at genus level 
    genbank_genome_genus_url <- paste0(genbank_genome_head,genus_name)
    genbank_genus_html <- read_html(genbank_genome_genus_url)
    target_genus_title <- genbank_genus_html %>% html_element(xpath = "/html/head/title") %>% html_text()
    
    if(!(stringr::str_detect(target_genus_title,pattern="No items found"))){
     
      if(stringr::str_detect(target_genus_title,pattern="ID")){ #a single related species found
        no_related_spp_exist[i] <- 1 # a single related species found within the same genus
        
        summary_element <- genbank_genus_html %>% 
          html_element(xpath = '//*[@id="mmtest1"]/div/div/div/table') %>% 
          html_table()
        target_size <- summary_element$X2[grep("total length", summary_element$X2)]
        target_size_vec <- strsplit(target_size, ":") %>% unlist
        related_spp_genome_size_vec[i] <- round(as.numeric(target_size_vec[2]),digits=0)
        
      
      }else{ #if genome assemblies of multiple related species found
        multi_related_list <- multi_species_genome_list(genus_name)
        no_related_spp_exist[i]  <- multi_related_list[[1]]
        multi_related_list <- multi_related_list[[2]]
        multi_related_spp_genome_size <- vector()
        for(j in 1:no_related_spp_exist[i]){
          sp_name_recover <- strsplit(multi_related_list[j], " ") %>% unlist
          multi_related_spp_genome_size[j] <- genome_size_sp_GenBank(sp_name_recover[1],sp_name_recover[2])
        }
        
        related_spp_genome_size_vec[i] <- round(mean(multi_related_spp_genome_size, na.rm=TRUE),digits=0)
      }
    }else{# No items found in the genus
      target_sp_genome_size_vec[i] <- NA
      no_related_spp_exist[i] <- 0
      related_spp_genome_size_vec[i] <- NA
    }
      
  Sys.sleep(1)
  }
  

}

species_genome <- species %>%
  mutate (Species_genome_size = target_sp_genome_size_vec,
          No_target_sp_genome = No_deposited_sp_level_genome_vec,
          Representative_assembly = representative_assembly_ID_vec,
          Representative_assembly_status = representative_assembly_status_vec,
          Contig_N50 = representative_contigN50_vec,
          Scaffold_N50 = representative_scaffoldN50_vec,
          Genus_genome_size = related_spp_genome_size_vec, 
          No_related_spp_with_assemblies = no_related_spp_exist,
          AGSDB_genus_genome_size = AGSDB_genus_genome_size_vec,
          AGSDB_No_genus_avail = AGSDB_no_genus_avail_vec)
```

## Write a result file

``` r
write_csv(species_genome, "aquatic_organism_genome_size.csv")
```
