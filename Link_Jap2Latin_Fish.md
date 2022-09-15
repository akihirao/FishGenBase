-   [Linking Japanes/scientific names in aquatic
    species](#linking-japanesscientific-names-in-aquatic-species)
    -   [Loading the packages](#loading-the-packages)
    -   [Loading dataset](#loading-dataset)
    -   [Linking Japanese name and scientific
        name](#linking-japanese-name-and-scientific-name)
    -   [Write a taxonomy list file](#write-a-taxonomy-list-file)

# Linking Japanes/scientific names in aquatic species

## Loading the packages

``` r
# Loading packages
library(tidyverse)
library(rvest) #for scraping
```

## Loading dataset

``` r
# Current standard Japanese/scientific names of all fish species recorded from Japanese waters: https://www.museum.kagoshima-u.ac.jp/staff/motomura/jaf.html
JAFList <- read_csv("20220821_JAFList.csv")

NonFish_List <- read_csv("NonFishList.csv")

# A list of aquatic species for fisheries resource assessment
FRA200List <- read_csv("FRA200List.csv")
FRA200List <- FRA200List %>% filter(SingleMulti == "Single")
FRA200List$Category <- factor(FRA200List$Category, levels=c("Fish","NonFish"))

# Taxonomic rank in fish species
TaxonRankFish <- read_csv("TaxonRankFish.csv")
```

## Linking Japanese name and scientific name

``` r
no_fish <- nrow(FRA200List)
#no_fish　<- 10

scientific_name_vec <- vector()
genus_name_vec <- vector()
family_name_vec <- vector()
order_name_vec <- vector()
class_name_vec <- vector()
phylum_name_vec <- vector()

for(i in 1:no_fish){

    target_fish <- FRA200List[i,1][[1]]
    Category <- as.vector(FRA200List$Category)[i]
    
    if(Category == "Fish"){
      target_fish_ID <- which(JAFList$和名==target_fish)
      scientific_name_check <- JAFList$学名[target_fish_ID]
      Family_unlist <-JAFList$Family[target_fish_ID] %>% strsplit("\n") %>% unlist 
      Family_name <- as.character(Family_unlist[1])

      target_Family_ID <- which(TaxonRankFish$Family==Family_name)
      
      Phylum_name <- TaxonRankFish$Phylum[target_Family_ID]
      if(identical(Phylum_name,character(0))){
        Phylum_name <-NA
      }
      
      Class_name <- TaxonRankFish$Class[target_Family_ID]
      if(identical(Class_name,character(0))){
        Class_name <-NA
      }
      
      Order_name <- TaxonRankFish$Order[target_Family_ID]
      if(identical(Order_name,character(0))){
        Order_name <-NA
      }
      
      #if(str_detect(Family_name, pattern="Epinephelidae")){
    #  Family_name <- "Serranidae"  
      #}
      
      if(!(identical(scientific_name_check,character(0)))){#if non-fish species
      scientific_unlist <- scientific_name_check %>% strsplit("\n") %>% unlist 
      scientific_name <- scientific_unlist[1]
      phylum_name_vec[i] <- Phylum_name
      class_name_vec[i] <- Class_name
      order_name_vec[i] <- Order_name
      family_name_vec[i] <- Family_name
      scientific_name_vec[i] <- scientific_name

    }else{
      phylum_name_vec[i] <- NA
        class_name_vec[i] <- NA
        order_name_vec[i] <- NA
        family_name_vec[i] <- NA
        scientific_name_vec[i] <- NA
      }
      
    }else if(Category == "NonFish"){
      target_NonFish_ID <- which(NonFish_List$和名==target_fish)
      scientific_name_check <- NonFish_List$Scientific_name[target_NonFish_ID]
      Family_unlist <-NonFish_List$Family[target_NonFish_ID] %>% strsplit("\n") %>% unlist 
      Family_name <- Family_unlist[1]
      Order_name <- NonFish_List$Order[target_NonFish_ID]
      Class_name <- NonFish_List$Class[target_NonFish_ID]
      Phylum_name <- NonFish_List$Phylum[target_NonFish_ID]
      
      if(!(identical(scientific_name_check,character(0)))){#if non-fish species
        scientific_unlist <- scientific_name_check %>% strsplit("\n") %>% unlist 
      scientific_name <- scientific_unlist[1]
      phylum_name_vec[i] <- NonFish_List$Phylum[target_NonFish_ID]
      class_name_vec[i] <- NonFish_List$Class[target_NonFish_ID]
      order_name_vec[i] <- NonFish_List$Order[target_NonFish_ID]
        family_name_vec[i] <- Family_name
        scientific_name_vec[i] <- scientific_name

    }else{
      phylum_name_vec[i] <- NA
      class_name_vec[i] <- NA
      order_name_vec[i] <- NA
        family_name_vec[i] <- NA
        scientific_name_vec[i] <- NA
    }
      
    }else{
      phylum_name_vec[i] <- NA
    class_name_vec[i] <- NA
    order_name_vec[i] <- NA
    family_name_vec[i] <- NA
      scientific_name_vec[i] <- NA
    }  
}

# Checking number of phylum in the dataset
No_phylum <- length(unique(phylum_name_vec))
Phylum_order <- c("Chordata","Arthropoda","Mollusca","Echinodermata")
if(!(No_phylum==length(Phylum_order))){
  print("Number of phylum-levels does not match the dataset")
}

# Checking number of phylum in the dataset
No_class <- length(unique(class_name_vec))
Class_order <- c("Actinopterygii","Malacostraca","Cephalopoda","Gastropoda","Bivalvia","Holothuroidea")
if(!(No_class=length(Class_order))){
  print("Number of class-levels does not match the dataset")
}

# Summarizing a taxonomy list
FRA200List_Latin <- FRA200List %>% 
  mutate(Phylum = factor(phylum_name_vec, levels = Phylum_order),
         Class = factor(class_name_vec,levels = Class_order), 
                         Order = order_name_vec, Family = family_name_vec,
                         Scientific_name = scientific_name_vec) %>%
  select(-Category,-SingleMulti,-TAC,-Target) %>% 
  arrange(Phylum, Class, Order, Family, Scientific_name)
```

## Write a taxonomy list file

``` r
write_csv(FRA200List_Latin, "FRA200List_Latin.csv")
```
