## Genome size information and demographic histories of aquatic organisms for fisheries resource management in FRA


* [Genome size of comercially important aquatic species](aquatic_organism_genome_size.csv)
* [Number of fisheries species with genome sequence deposited in GenBank](https://github.com/akihirao/FishGenBase/blob/main/chronology/No_sp_genome_deposited.png)
* [Generation time for fish species](species_generation_time_age.csv)


### Work flow for scaping genome size information

* Step 1：Link the standard Japanese name to the scientific name in each of species [Rcode](Link_Jap2Latin_Fish.md)
* Step 2：Scrape genome assembly infomation depsited on GenBank [Rcode](Scraping_FishGenome.md)


### Generation time of fish species
* The generation time for each fish species was inferred based on the equation proposed by Pacoureau et al (2021)[R code](fish_species_generation_time.md)


### [Historical effective population size](harmonic_mean_Ne_PSMC.csv)
* The history of effective population size (Ne) for each species was inferred using PSMC. The historical Ne was represented as the harmonic mean across temporal estimates (see detailed in Wilder et al 2023).


### References
* [日本産魚類全種リスト(JAFリスト)](https://www.museum.kagoshima-u.ac.jp/staff/motomura/jaf.html)：日本産全魚種の標準和名—学名—科名の参照先
* [Eschmeyer's Catalog of Fishes](https://www.calacademy.org): 魚類の科—目レベルの分類階級の参照先
* [Animal Genome Size Database](http://www.genomesize.com/index.php): 動物6222種（脊椎動物2793種; 無脊椎動物2429種）を対象としたハプロイドDNA含量に関するデータベース

* Pacoureau et al. (2021) Half a century of global decline in oceanic sharks and rays. Nature 589, 567–571. [link](https://www.nature.com/articles/s41586-020-03173-9)
* Wilder et al. (2023) The contribution of historical processes to contemporary extinction risk in placental mammals. Sicence 380 eabn5856. [link](https://www.science.org/doi/10.1126/science.abn5856)
