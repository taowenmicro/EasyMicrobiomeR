# 参数文件; Tao Wen, 2022.10.05
library(tidyverse)
library(phyloseq)
library(ggClusterNet)

#--输入和设定文件
ps0 = base::readRDS("./data/dataNEW/ps_16s.rds")

# data(ps)
# ps0 = ps


change.rank.name(ps0)

# # 高丰度微生物挑选
# ps0 %>% scale_micro() %>%
#   filter_taxa(function(x) sum(x ) > 0.01 , TRUE)
# #低丰度微生物挑选
# ps0 %>% scale_micro() %>%
# filter_taxa(function(x) sum(x ) < 0.0001 , TRUE)


# #---解决错误
# ps0 = base::readRDS("./Error/221121/ps_its.rds")
# ps0
# .libPaths()
# .libPaths(new="C:/Program Files/R/R-4.1.1/library")




# 基于phyloseq对象进行分析
# 设定主题，颜色，分组数量，分组排序，比较类型等内容
# 如果是微生物数据，可以定义进化树展示的微生物数量，默认150个
# 同时设定lefse用的微生物的数量

#---扩增子环境布置
#-主题--颜色等
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\total_amplicon.R")
res = theme_my(ps0)
mytheme1 = res[[1]]
mytheme2 = res[[2]]; 
colset1 = res[[3]];colset2 = res[[4]];colset3 = res[[5]];colset4 = res[[6]]
result<- dir.amp(ps0 = ps0,smart = TRUE)#--如果出现错误，设定smart = F；是由于注释信息不符合本代码标准导致的
res1path = result[[1]];res1path
id = result[[2]];id
#--OTU base diversity 保存文件夹
otupath = paste(res1path,"/OTU/",sep = "");otupath
dir.create(otupath)

# #--如何筛选样本:去除sample1
# ps_sub <- subset_samples.wt(ps0,"ID",c("sample1"),T);ps_sub
# #--如何筛选微生物
# ps0 <- ps0 %>% subset_taxa.wt("Kingdom", id) ;ps0

#--最终确定的phyloseq对象定义为ps
ps = ps0 %>% filter_taxa(function(x) sum(x ) > 0 , TRUE)
#--提取分组因子数量
gnum = phyloseq::sample_data(ps)$Group %>% unique() %>% length()
gnum
#--设定排序顺序
axis_order =  phyloseq::sample_data(ps)$Group %>%unique();axis_order
# axis_order = c("Group2","Group3","Group1")

#--物种分类树展示的OTU数量
Top_micro = 150

#--堆叠柱状图展示前Top的微生物,j 展示的微生物分类等级
Top = 12
jj = j = "Phylum"

#韦恩网络设置过滤阈值
ps_biost = ggClusterNet::filter_OTU_ps(ps = ps,Top = 500)

#--差异分析设定两两比对
# group1 = c("Gro1","Gro2")
# group2 = c("Gro1","Gro2")
# b= data.frame(group1,group2)
# b
b = NULL# 如果每两个组之间都做差异，那就指定b为NULL

# 热图展示的OTU数量
heatnum　=　30


#--R语言做lefse的过滤数量
ps_Rlefse = ggClusterNet::filter_OTU_ps(ps = ps,Top = 400)

#--机器学习部分
ROC = FALSE# ROC是三种机器学习的ROC曲线，但是只能跑两个分组，如果两组，可以选择为T。
rfcv = FALSE# 是否做交叉检验
optimal = 40# 选择多少个重要变量

#--功能预测

if (is.null(ps0@refseq)) {
  Tax4Fun2 = FALSE
} else if(!is.null(ps0@refseq)){
  Tax4Fun2 = TRUE
}

ps.t = ps0 %>% ggClusterNet::filter_OTU_ps(1000)
if (Tax4Fun2) {
  dir.create("data")
  otu = ps.t %>% 
    # ggClusterNet::filter_OTU_ps(1000) %>%
    ggClusterNet:: vegan_otu() %>%
    t() %>%
    as.data.frame()
  # write.table(otu,"./data/otu.txt",quote = F,row.names = T,col.names = T,sep = "\t")
  rep = ps.t %>% 
    # ggClusterNet::filter_OTU_ps(1000) %>%
    phyloseq::refseq()
  rep
  # library(Biostrings)
  Biostrings::writeXStringSet(rep,"./data/otu.fa")
  #开头空一格字符保存
  write.table("\t", "./data/otu.txt",append = F, quote = F, eol = "", row.names = F, col.names = F)
  # 保存统计结果，有waring正常
  write.table(otu, "./data/otu.txt", append = T, quote = F, sep="\t", eol = "\n", na = "NA", dec = ".", row.names = T, col.names = T)     

}


#-----选择性功能#---------

#设置CK，用于双向柱状图绘制-目前不绘制
CK = unique(phyloseq::sample_data(ps)$Group)[1]

# 用python做lefse
lefse.py = T
if (lefse.py) {
  lefsenum = 0
  ps_lefse <- ps %>%
    phyloseq::subset_taxa(
      # Kingdom == "Fungi"
      Kingdom == id
      # Genus  == "Genus1"
      # Species %in%c("species1") 
      # row.names(tax_table(ps0))%in%c("OTU1")
    )
  
  ps_lefse = ggClusterNet::filter_OTU_ps(ps = ps_lefse,Top = 400)
  
}






