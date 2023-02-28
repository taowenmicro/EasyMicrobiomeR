

#------构建phyloseq对象

#-----med1#----------
# 从文件读取
metadata = read.delim("./metadata.tsv")
head(metadata)
row.names(metadata) = metadata$SampleID


otutab = read.table("./result/otutab.txt", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)

# colnames(otutab) <- gsub("X","",colnames(otutab))
taxonomy = read.table("./result/taxonomy.txt", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)
head(taxonomy )

library(ggtree)
tree = read.tree("./result/otus.tree")

library(Biostrings)
rep = readDNAStringSet("result/otus.fa")

# 导入phyloseq(ps)对象
library(phyloseq)

ps = phyloseq(sample_data(metadata),
              otu_table(as.matrix(otutab), taxa_are_rows=TRUE), 
              tax_table(as.matrix(taxonomy)),
              phy_tree(tree),
              refseq(rep)
              )
ps


saveRDS(ps,"./ps_16s.rds")


#-----med2#----------
# 从文件读取
map = read.delim("./data/分组文件.txt")
head(map)
colnames(map)[1] = "ID"
map$ID = paste("A",map$ID,sep = "")
row.names(map) = map[[1]]
library(readxl)
otutax = read.delim("./data/16s/otu_taxa_table.xls",row.names = 1)
head(otutax)
library(tidyverse)
otu = otutax[,1:length(map$ID)]
colnames(otu) = gsub("^X","",colnames(otu))
colnames(otu) = paste("A",colnames(otu) ,sep = "")

tax = data.frame(tax = otutax$taxonomy,row.names = row.names(otutax))
head(tax)
tax <- tax %>% 
  tidyfst::separate_dt(tax,
                    into = c("Kingdom","Phylum","Class","Order","Family","Genus","Species"),
                    sep = "\\;",
                    remove = F) %>% select(-tax) %>% as.tibble() 

tax = tax %>% as.data.frame()
row.names(tax) = row.names(otutax)
head(tax)
# library(ggtree)
# tree = read.tree("./result/otus.tree")

library(Biostrings)
rep = readDNAStringSet("data/16s/otu_rep.fasta")
names(rep) = sapply(strsplit(names(rep), "[ ]"), `[`, 1)
# 导入phyloseq(ps)对象
library(phyloseq)

ps = phyloseq(
  sample_data(map),
              otu_table(as.matrix(otu), taxa_are_rows=TRUE), 
              tax_table(as.matrix(tax)),
              # phy_tree(tree),
              refseq(rep)
)
ps


saveRDS(ps,"./data/ps_16s.rds")


#-----med2#----------
# 从文件读取
map = read.delim("./data/分组文件.txt")
head(map)
colnames(map)[1] = "ID"
map$ID = paste("A",map$ID,sep = "")
row.names(map) = map[[1]]
library(readxl)
otutax = read.delim("./data/ITS//otu_taxa_table.xls",row.names = 1)
head(otutax)
library(tidyverse)
otu = otutax[,1:length(map$ID)]
colnames(otu) = gsub("^X","",colnames(otu))
colnames(otu) = paste("A",colnames(otu) ,sep = "")

tax = data.frame(tax = otutax$taxonomy,row.names = row.names(otutax))
head(tax)
tax <- tax %>% 
  tidyfst::separate_dt(tax,
                       into = c("Kingdom","Phylum","Class","Order","Family","Genus","Species"),
                       sep = "\\;",
                       remove = F) %>% select(-tax) %>% as.tibble() 

tax = tax %>% as.data.frame()
row.names(tax) = row.names(otutax)
head(tax)
# library(ggtree)
# tree = read.tree("./result/otus.tree")

library(Biostrings)
rep = readDNAStringSet("data/ITS//otu_rep.fasta")
names(rep) = sapply(strsplit(names(rep), "[ ]"), `[`, 1)
# 导入phyloseq(ps)对象
library(phyloseq)

ps = phyloseq(
  sample_data(map),
  otu_table(as.matrix(otu), taxa_are_rows=TRUE), 
  tax_table(as.matrix(tax)),
  # phy_tree(tree),
  refseq(rep)
)
ps


saveRDS(ps,"./data/ps_ITS.rds")


