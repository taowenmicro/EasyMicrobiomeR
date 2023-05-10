
#--R语言最佳微生物组数据挖掘实战-安装R包

# #-查看目前安装的全部R包,保存全部R包列表，用使用者查看是否全部安装成功
# tab = as.data.frame(installed.packages())$Package
# write.csv(tab,"./all.install.p[ackages.csv",quote = F,row.names = F)

# #-查看目前安装的全部R包,保存全部R包列表，用使用者查看是否全部安装成功
# tab = as.data.frame(installed.packages())$Package
# write.csv(tab,"./all.install.p[ackages.csv",quote = F,row.names = F)

#-批量全部安装R包#--------
tab= read.csv("./all.install.packages.csv")
#--查看已经加载的R包
(.packages())
#--判断用户有的R包，然后再安用户缺乏的R包
tem = as.data.frame(installed.packages())$Package
# 找到tab$x中有，但是tem中没有的包
id = setdiff(tab$x,tem)
length(id)

#-提取不好安装的R包或者github的R包
tab.gb = c("basilisk","ConQuR","EasyMicroPlot",
           "EasyStat" ,"ggClusterNet","ggtern",
           "metacoder","MicrobiomeAnalystR", "pulsar" ,
           "SpiecEasi","Tax4Fun","Tax4Fun2")


# 查看装包路径或者修改装包路径
# .libPaths(new = "C:/Users/asus/AppData/Local/R/win-library/4.3")
.libPaths()

for (i in 1:length(id)) {

  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager",update = FALSE)
  }
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools",update = FALSE)
  }
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes",update = FALSE)
  }
  if (id[i] %in% tab.gb) {
    print("github packages")
  } else{
    if (!requireNamespace(id[i], quietly = TRUE)) {
      try(BiocManager::install(id[i],update = FALSE))
    }
  }

  tem = as.data.frame(installed.packages())$Package
  # 找到tab$x中有，但是tem中没有的包
  id = setdiff(tab$x,tem)
  length(id)


}


#-单独安装github，或者不好安装的R包
devtools::install_github("taowenmicro/EasyStat",upgrade = "never")
devtools::install_github("taowenmicro/ggClusterNet",upgrade = "never")
devtools::install_github("zdk123/SpiecEasi")
devtools::install_github("fjossandon/Tax4Fun2")
devtools::install_github("kylebittinger/qiimer")
devtools::install_github("joey711/biom",upgrade = "never",force = TRUE)
install.packages("./install.packages.for.local/Tax4Fun/", repos = NULL, type = "source")
devtools::install_github("xia-lab/MicrobiomeAnalystR",
                         build = TRUE,
                         build_opts = c("--no-resave-data", "--no-manual"))

install.packages("./install.packages.for.local/ggtern/", repos = NULL, type = "source")
# Error: Failed to install 'ggtern' from GitHub:
#   ! System command 'Rcmd.exe' failed
remotes::install_github("https://github.com/xielab2017/EasyMicroPlot",
                        subdir='Version_0.5')
install.packages("microeco")














#挨个安装R包过程#--------
# install.packages("BiocManager")
# library(BiocManager)
# install("phyloseq")
# install("ggtree")
# install("ggthemes")
# install("tidyverse")
# install("randomForest")
# install("devtools")
# install.packages("ggalluvial")
# .libPaths()
# library("devtools")
# devtools::install_github("taowenmicro/ggClusterNet")
# devtools::install_github("taowenmicro/EasyStat")
# BiocManager::install("ggtern")
# BiocManager::install("ggtreeExtra")
# BiocManager::install("MicrobiotaProcess")
# BiocManager::install("VennDiagram")
# BiocManager::install("pheatmap")
# BiocManager::install("circlize")
# BiocManager::install("picante")
# BiocManager::install("factoextra")
# BiocManager::install("edgeR")
# BiocManager::install("DESeq2")
# BiocManager::install("metagenomeSeq")
# BiocManager::install("ALDEx2")
# BiocManager::install("ANCOMBC")
# BiocManager::install("microbiome")
# BiocManager::install("corncob")
# BiocManager::install("Maaslin2")
# BiocManager::install("exactRankTests")
# BiocManager::install("ROCR")
# devtools::install_github("zdk123/SpiecEasi")
# BiocManager::install("WGCNA")
# BiocManager::install("ggraph")
# BiocManager::install("tidyfst")
# devtools::install_github("fjossandon/Tax4Fun2")
# BiocManager::install("eulerr")
# BiocManager::install("minpack.lm")
# install.packages('NST')
# BiocManager::install("gcookbook")
# BiocManager::install("qs")
# BiocManager::install('metacoder')
# BiocManager::install('pryr')
# BiocManager::install('metagenomeSeq')
# BiocManager::install('globaltest')
# devtools::install_github("cran/ppcor")
# devtools::install_github("kylebittinger/qiimer")
# devtools::install_github("kylebittinger/qiimer")
# install.packages("./Tax4Fun-master/", repos = NULL, type = "source")
# devtools::install_github("xia-lab/MicrobiomeAnalystR",
#                          build = TRUE,
#                          build_opts = c("--no-resave-data", "--no-manual"))
# BiocManager::install("animalcules")
# if(!requireNamespace("microeco", quietly = T))
#   install_github("ChiLiubio/microeco")
# install.packages('remotes')
# remotes::install_github("https://github.com/xielab2017/EasyMicroPlot",subdir='Version_0.5')







