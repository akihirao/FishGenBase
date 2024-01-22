## Genome size information and demographic histories of aquatic organisms for fisheries resource management in FRA


* [Genome size of comercially important aquatic species](aquatic_organism_genome_size.csv)
* [Number of fisheries species with genome sequence deposited in GenBank](https://github.com/akihirao/FishGenBase/blob/main/chronology/No_sp_genome_deposited.png)
* [Generation time for fish species](species_generation_time_age.csv)


### Work flow for scaping genome size information

* [Step 1](Link_Jap2Latin_Fish.md)：Link the standard Japanese name to the scientific name in each of species
* [Step 2](Scraping_FishGenome.md)：Scrape genome assembly infomation depsited on GenBank


### Estimation of generation time for fish species
* [R code](fish_species_generation_time.md)


### References
* [日本産魚類全種リスト(JAFリスト)](https://www.museum.kagoshima-u.ac.jp/staff/motomura/jaf.html)：日本産全魚種の標準和名—学名—科名の参照先
* [Eschmeyer's Catalog of Fishes](https://www.calacademy.org): 魚類の科—目レベルの分類階級の参照先
* [Animal Genome Size Database](http://www.genomesize.com/index.php): 動物6222種（脊椎動物2793種; 無脊椎動物2429種）を対象としたハプロイドDNA含量に関するデータベース
