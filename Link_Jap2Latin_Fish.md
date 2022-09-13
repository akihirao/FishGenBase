-   [Linking Japanese name to scientific name in
    fishes](#linking-japanese-name-to-scientific-name-in-fishes)
    -   [Loading the packages](#loading-the-packages)
    -   [Loading data-set](#loading-data-set)
    -   [Linking Japanese name and scientific
        name](#linking-japanese-name-and-scientific-name)
    -   [Write a list file](#write-a-list-file)

# Linking Japanese name to scientific name in fishes

## Loading the packages

``` r
# Loading packages
library(tidyverse)
library(rvest)
```

## Loading data-set

``` r
# Current standard Japanese/scientific names of all fish species recorded from Japanese waters
#https://www.museum.kagoshima-u.ac.jp/staff/motomura/jaf.html
JAFList <- read_csv("20220821_JAFList.csv")

# Species list of fisheries resources
FRA200List <- read_csv("FRA200List.csv")
FRA200List$Category <- factor(FRA200List$Category, levels=c("Fish","Shellfish","Cuttlefish"))
```

## Linking Japanese name and scientific name

``` r
no_fish <- nrow(FRA200List)
#no_fish　<- 10

scientific_name_vec <- vector()
genus_name_vec <- vector()
family_name_vec <- vector()
order_name_vec <- vector()

for(i in 1:no_fish){

    target_fish <- FRA200List[i,1][[1]]
    target_fish_ID <- which(JAFList$和名==target_fish)
    scientific_name_check <- JAFList$学名[target_fish_ID]
    Family_unlist <-JAFList$Family[target_fish_ID] %>% strsplit("\n") %>% unlist 
    Family_name <- Family_unlist[1]


    if(!(identical(scientific_name_check,character(0)))){#if non-fish species
      scientific_unlist <- scientific_name_check %>% strsplit("\n") %>% unlist 
      scientific_name <- scientific_unlist[1]
      family_name_vec[i] <- Family_name
      scientific_name_vec[i] <- scientific_name

    }else{
      family_name_vec[i] <- NA
      scientific_name_vec[i] <- NA
    }

}

FRA200List_Latin <- FRA200List %>% 
  mutate(Family = family_name_vec, Scientific_name = scientific_name_vec) %>%
  arrange(Category, Family, Scientific_name)
```

## Write a list file

``` r
write_csv(FRA200List_Latin, "FRA200List_Latin.csv")
```
