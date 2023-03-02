

# #---构建方式一远程文件直接制作#------
# library(phyloseq)
# # library(ggClusterNet)
# library(tidyverse)
# library(Biostrings)
# 
# 
# metadata = read.delim("https://raw.githubusercontent.com/taowenmicro/R-_function/main/metadata.tsv",row.names = 1)
# otutab = read.delim("https://raw.githubusercontent.com/taowenmicro/R-_function/main/otutab.txt", row.names=1)
# taxonomy = read.table("https://raw.githubusercontent.com/taowenmicro/R-_function/main/taxonomy.txt", row.names=1)
# tree  = read_tree("https://raw.githubusercontent.com/taowenmicro/R-_function/main/otus.tree")
# rep = readDNAStringSet("https://raw.githubusercontent.com/taowenmicro/R-_function/main/otus.fa")
# 
# ps = phyloseq(sample_data(metadata),
#               otu_table(as.matrix(otutab), taxa_are_rows=TRUE),
#               tax_table(as.matrix(taxonomy)),
#               phy_tree(tree),
#               refseq(rep)
# )



#--本地文件制作#-----------
library(phyloseq)
# library(ggClusterNet)
library(tidyverse)


metadata = read.delim("./metadata.tsv",row.names = 1)
metadata$ID = row.names(metadata)
dim(metadata)
head(metadata)

otutab = read.delim("./otutab.txt", row.names=1)
head(otutab)
dim(otutab)


taxonomy = read.table("./taxonomy.txt", row.names=1,header = T)
head(taxonomy)


tree  = read_tree("./otus.tree")
tree
# 
library(Biostrings)
rep = readDNAStringSet("./otus.fa")

ps = phyloseq(
              sample_data(metadata),
              otu_table(as.matrix(otutab), taxa_are_rows=TRUE),
              tax_table(as.matrix(taxonomy)),
              phy_tree(tree),
              refseq(rep)
)

ps

saveRDS(ps,"ps_TS.rds")

