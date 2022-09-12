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
```

## Loading data-set

``` r
# Current standard Japanese/scientific names of all fish species recorded from Japanese waters
#https://www.museum.kagoshima-u.ac.jp/staff/motomura/jaf.html
JAFList <- read_csv("20220821_JAFList.csv")

# Species list of fisheries resources
FRA200List <- read_csv("FRA200list.csv")
```

## Linking Japanese name and scientific name

``` r
no_fish <- nrow(FRA200List)
#no_fish <- 6 # for test

species_name_vec <- vector()


for(i in 1:no_fish){

    target_fish <- FRA200List[i,1][[1]]
    target_fish_ID <- which(JAFList$和名==target_fish)
    species_name <- JAFList$学名[target_fish_ID]

    if(!(identical(species_name,character(0)))){#if non-fish species
        species_name_vec[i] <- species_name

    }else{
      species_name_vec[i] <- NA
    }

}
FRA200List_Latin <- FRA200List %>% mutate(Latin_name = species_name_vec)
```

## Write a list file

``` r
write_csv(FRA200List_Latin, "FRA200List_Latin.pre.csv")
```
