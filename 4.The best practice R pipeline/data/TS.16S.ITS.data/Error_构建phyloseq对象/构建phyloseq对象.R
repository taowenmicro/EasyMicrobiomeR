

# #---构建方式一--远程文件直接制作#------
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


metadata = read.delim("./metadata1.txt",row.names = 1)
metadata$ID = row.names(metadata)

otutab = read.delim("./otutable.txt", row.names=1)
head(otutab)


taxonomy = read.delim("./taxonomy1.txt",row.names = 1)
head(taxonomy)




ps = phyloseq(
  sample_data(metadata),
              otu_table(as.matrix(otutab), taxa_are_rows=TRUE),
              tax_table(as.matrix(taxonomy))
         
)

saveRDS(ps,"ps.rds")

