

#---文中Fig4#---------
#1 Amplicon的稀释曲线#------
alppath = paste(otupath,"/alpha/",sep = "")
dir.create(alppath)

rare <- mean(phyloseq::sample_sums(ps))/80

source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\alpha_rare_all.R",encoding = "utf-8")
result = alpha_rare_all(ps = ps, group = "Group", method = "Richness", start = 100, step = rare)
#--提供单个样本溪稀释曲线的绘制
p2_1 <- result[[1]] +
  mytheme1 +
  guides(fill = guide_legend(title = NULL))+
  scale_fill_manual(values = colset1)


## 提供数据表格，方便输出
raretab <- result[[2]]
head(raretab)


#--按照分组展示稀释曲线
p2_2 <- result[[3]] +
  mytheme1 +
  guides(fill = guide_legend(title = NULL))+
  scale_fill_manual(values = colset1)
#--按照分组绘制标准差稀释曲线
p2_3 <- result[[4]] +
  mytheme1 +
  guides(fill = guide_legend(title = NULL))+
  scale_fill_manual(values = colset1)

FileName <- paste(alppath,"Alpha_rare_group", ".pdf", sep = "")
ggsave(FileName, p2_2, width = 8, height =6)
FileName <- paste(alppath,"Alpha_rare_groupwithSD", ".pdf", sep = "")
ggsave(FileName, p2_3, width = 8, height =6)


FileName <- paste(alppath,"/Alpha_rare_data.csv", sep = "")
write.csv(raretab,FileName,sep = "")

#2 排序分析#--------
betapath = paste(otupath,"/beta/",sep = "")
dir.create(betapath)


source("E:\\Shared_Folder\\Function_local\\R_function\\micro/BetaDiv.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/MicroTest.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/pairMicroTest.R")

method = "PCoA"
result = BetaDiv(ps = ps, group = "Group", dist = "bray",
                 method = method, Micromet = "anosim", pvalue.cutoff = 0.05,
                 pair = F)

# 提取出图数据
plotdata = result[[2]]

head(plotdata)
# 求均值
cent <- aggregate(cbind(x,y) ~Group, data = plotdata, FUN = mean)
cent
# 合并到样本坐标数据中
segs <- merge(plotdata, setNames(cent, c('Group','oNMDS1','oNMDS2')),
              by = 'Group', sort = FALSE)

# p2$layers[[2]] = NULL
# library(ggcor)
library(ggsci)
p3_1 = result[[1]] + 
  scale_fill_manual(values = colset1)+
  scale_color_manual(values = colset1,guide = F) +
  mytheme1 
p3_3 = p3_1 +geom_segment(data = segs,
                          mapping = aes(xend = oNMDS1, yend = oNMDS2,color = Group),show.legend=F) + # spiders
  geom_point(mapping = aes(x = x, y = y),data = cent, size = 5,pch = 24,color = "black",fill = "yellow") +
  scale_fill_manual(values = colset1)+
  scale_color_manual(values = colset1,guide = F) + 
  mytheme1 
p3_3

FileName <- paste(betapath,"/a2_",method,"bray_star.pdf", sep = "")
ggsave(FileName, p3_3, width = 8, height = 7)

#--物种分类树#-----
library(ggstar)
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/phy_tree_micro.R")

barpath = paste(otupath,"/phy_tree_micro/",sep = "");print(barpath)
dir.create(barpath)
library(ggClusterNet)
# rank.names(ps)
# Top_micro = 150
result <- phy_tree_micro(ps = ps,Top = 150)

# p0 = result[[1]]
# p1 = result[[2]]
# p2 = result[[3]]
# p3 = result[[4]]
# p4 = result[[5]]
p5 = result[[6]]
# p6 = result[[7]]
# p7 = result[[8]]
detach("package:ggstar")
FileName <- paste(barpath,Top_micro,"phy_tree_micro5_2", ".pdf", sep = "")
ggsave(FileName, p5, width = 18, height = 15)




#--lefse#-------

lefsepath = paste(otupath,"/lefse_R_plot/",sep = "")
dir.create(lefsepath)

# library(ggpubr)
# library(patchwork)
# library(MicrobiotaProcess)
library(ggtree)
source("E:/Shared_Folder/Function_local/R_function/micro/R_lefse_SAV.R",encoding = "utf-8")
# source("../micro/R_lefse_SAV.R",encoding = "utf-8")



source("E:/Shared_Folder/Function_local/R_function/micro/R_lefse_allRank.R",encoding = "utf-8")
library(patchwork)
lefsepath = paste(otupath,"/lefse_R_plot/",sep = "")
dir.create(lefsepath)

j = 6
  
  p1 <- p_base(ps,Top = 100,ranks =j)
  p1
  
  tablda = LDA_Micro(ps = ps,
                     Top = 100,
                     ranks = j,
                     p.lvl = 0.01,
                     lda.lvl = 3,
                     seed = 11,
                     adjust.p = F)
  
  p2 <- clade.anno_wt(p1, tablda[[1]], alpha=0.3,anno.depth = 2)
  p2
  FileName <- paste(lefsepath,j,"_tree_lefse", ".pdf", sep = "")
  ggsave(FileName,p2,width = 15,height = 10)

  p <- lefse_bar(taxtree = tablda[[2]])
  FileName <- paste(lefsepath,j,"_bar_lefse", ".pdf", sep = "")
  ggsave(FileName, p, width = 15, height =9)

  #----微生物桑基图#--------
  library(ggClusterNet)
  snapath =  paste(otupath,"/sankeyNetwork/",sep = "");otupath
  dir.create(snapath)
  source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\sankey.m.Group.R",encoding = "utf-8")
  
  data(ps)
  ps = ps %>% filter_OTU_ps(200)
  map = sample_data(ps)
  head(map)
  map = map[,c(1,2,3)]
  sample_data(ps) = map
  
  result = sankey.m.Group(
    ps = ps,
    rank = 6,
    Top = 50,

  )
  
  p = result[[1]]
  dat = result[[2]]
  
  FileName <-paste(snapath,"/sankey_Group.csv", sep = "")
  write.csv(dat,FileName,sep = "")
  
  saveNetwork(p,paste(snapath,"/sankey_Group.html", sep = ""))
  library(webshot)
  # webshot::install_phantomjs()
  # webshot(paste(snapath,"/sankey1.html", sep = "") ,paste(snapath,"/sankey1.png", sep = ""))
  webshot(paste(snapath,"/sankey_Group.html", sep = "") , paste(snapath,"/sankey_Group.pdf", sep = ""))
  
  
  
  #--环状物种堆叠柱状图#-----
  barpath = paste(otupath,"/circle_Micro_strack_bar/",sep = "");print(barpath)
  dir.create(barpath)
  
  source("E:\\Shared_Folder\\Function_local\\R_function\\micro/circle_stark_bar.R")
  library(ggtree)
  # j = "Phylum"
  dev.off()
  p2 = circle_starc_bar(
    ps = readRDS("./data/16S/ps.rds"),
    Top = 10,
    dist = "bray",
    cuttree = 3,
    hcluter_method = "complete")
  p2 
  
  FileName2 <- paste(barpath,"/a2_","_bar",".pdf", sep = "")
  ggsave(FileName2, p2, width = 10, height =8 )
  
  
  

  #---------stemp_差异分析#-------
  source("E:\\Shared_Folder\\Function_local\\R_function\\micro/stemp_diff.R")
  
  # library(phyloseq)
  # sample_data(ps)$Group
  
  diffpath = paste(otupath,"/stemp_diff/",sep = "")
  dir.create(diffpath)
  data(ps)
  map = phyloseq::sample_data(ps)

  allgroup <- combn(unique(map$Group),2)
    i = 1
    ps_sub <- phyloseq::subset_samples(ps,Group %in% allgroup[,i]);ps_sub
    
    j = 6
      p <- stemp_diff(ps = ps_sub,Top = 15,ranks = j)
      p
      # filename = paste(diffpath,"/",paste(allgroup[,i][1],allgroup[,i][2],sep = "_"),"stemp_P_plot.csv",sep = "")
      # write.csv(diff.mean,filename)
filename = paste(diffpath,"/",paste(allgroup[,i][1],
                                          allgroup[,i][2],sep = "_"),phyloseq::rank.names(ps)[j],"stemp_plot.pdf",sep = "")
ggsave(filename,p,width = 10,height = 6)
      

  detach("package:patchwork")
  library(ggClusterNet)
  library(tidyverse)
  library(phyloseq)


#--edger--曼哈顿图绘制#-------
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\edge_Manhattan.R")

diffpath = paste(otupath,"/diff_Manhattan/",sep = "")
dir.create(diffpath)

edge_Manhattan (
  ps = ps,
  pvalue = 0.05,
  lfc = 0,
  diffpath = diffpath 
)


#--维恩网络#-------
library(ggClusterNet)
library(phyloseq)
biospath = paste(otupath,"/biospr_network_Ven/",sep = "")
dir.create(biospath)
N = 0.5
ps = readRDS("./data/ven.network/ps_16s.rds")
map = sample_data(ps)
head(map)
map$Group = map$Group1
sample_data(ps) = map
result = ggClusterNet::div_network(ps)
edge = result[[1]]
data = result[[3]]


result <- ggClusterNet::div_culculate(table = result[[3]],distance = 1.1,distance2 = 1.5,distance3 = 1.3,order = FALSE)
# result <- div_culculate(table = result[[3]],distance = 1,distance2 = 1.2,distance3 = 1.1,order = FALSE)
edge = result[[1]]

plotdata = result[[2]]

#--这部分数据是样本点数据
groupdata <- result[[3]]
# table(plotdata$elements)
node =  plotdata[plotdata$elements == unique(plotdata$elements), ]

otu_table = as.data.frame(t(ggClusterNet::vegan_otu(ps)))
tax_table = as.data.frame(ggClusterNet::vegan_tax(ps))
res = merge(node,tax_table,by = "row.names",all = F)
row.names(res) = res$Row.names
res$Row.names = NULL
plotcord = res

xx = data.frame(mean  =rowMeans(otu_table))

plotcord = merge(plotcord,xx,by = "row.names",all = FALSE)
head(plotcord)
# plotcord$Phylum
row.names(plotcord) = plotcord$Row.names
plotcord$Row.names = NULL
head(plotcord)
library(ggrepel)
head(plotcord)
head(groupdata)
p = ggplot() + geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2),
                            data = edge, size = 0.3,color = "#E6AB02",alpha = 0.2) +
  geom_point(aes(X1, X2,fill = Phylum,size =mean ),pch = 21, data = plotcord) +
  geom_point(aes(X1, X2),pch = 21, data = groupdata,size = 2,fill = "#7570B3",color = "black") +
  geom_text(aes(X1, X2,label = elements ), data = groupdata,hjust = 1,vjust = -1) +
  theme_void()

p

filename = paste(biospath,"/","biostr_Ven_network.pdf",sep = "")
ggsave(filename,p,width = 9,height = 7)


detach("package:ggClusterNet")
detach("package:phyloseq")
  

#----多组差异分析火山图#------

source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\EdgerSuper.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\EdgerSuper2.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\Mui.cluster-group.volcano.R")

diffpath.1 = paste(otupath,"/Mui.Group.v/",sep = "")
dir.create(diffpath.1)
data(ps)

res = EdgerSuper2 (ps = ps %>% filter_OTU_ps(250),group  = "Group",artGroup =NULL,
                   j = "OTU",
                   path = diffpath.1
)

head(res)


result = Mui.Group.volcano (res = res)
p = result[[2]]
p
filename = paste(diffpath.1,"/","Mui.group.volcano.pdf",sep = "")
ggsave(filename,p,width = 6,height = 4,limitsize = FALSE)

p = result[[1]]
p
filename = paste(diffpath.1,"/","Mui.group.volcano_label.pdf",sep = "")
ggsave(filename,p,width = 6,height = 4,limitsize = FALSE)




#--机器学习#------
matpath = paste(otupath,"/Machine_learing/",sep = "")
dir.create(matpath )
data(ps)
ps = ps %>% subset_samples.wt(Group = "Group","KO",T) %>% filter_OTU_ps(600)

source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\MicroMachine_learning.R")

library(randomForest)
library(caret)
library(ROCR) ##用于计算ROC
library(e1071)

  #--三种机器学习方法评测
  result = MicroRoc( ps = ps,group  = "Group")
  #--提取roc曲线
  p <- result[[1]] + 
    mytheme1
  p
  #提取AUC值
  data <- result[[2]]
  
  filename = paste(matpath,"/three_method_AUCvalue.csv",sep = "")
  write.csv(data,filename,quote = F)
  
  data <- result[[3]]
  filename = paste(matpath,"/three_method_AUCdata.csv",sep = "")
  write.csv(data,filename,quote = F)
  
  filename = paste(matpath,"/three_method_AUC_plot.pdf",sep = "")
  ggsave(filename,p,width = 8,height = 8)
  filename = paste(matpath,"/three_method_AUC_plot.jpg",sep = "")
  ggsave(filename,p,width = 8,height = 8)
  
  


mapping = as.data.frame(phyloseq::sample_data(ps))
#--随机森林全套-如果圈图尚未显示前面几个，就设定max大一点
ps_rela = ps
result = MicroRF(ps = ps,
                 group  = "Group",
                 optimal = optimal,
                 rfcv = T,
                 nrfcvnum = 5,
                 min = -5,max = 5)
#火柴图展示前二十个重要的OTU
p <- result[[1]] + 
  mytheme1
p


# 圈图展示
p <- result[[2]]
p
filename = paste(matpath,"/randonforest_loading_circle.pdf",sep = "")
ggsave(filename,p,width = 8,height = 10)
filename = paste(matpath,"/randonforest_loading_circle.jpg",sep = "")
ggsave(filename,p,width = 8,height = 10)

p <- result[[6]]
p
filename = paste(matpath,"/Show_model.pdf",sep = "")
ggsave(filename,p,width = 8,height = 4)
filename = paste(matpath,"/Show_model.jpg",sep = "")
ggsave(filename,p,width = 8,height = 4)


  # 展示交叉验证结果
  p <- result[[3]] +theme_classic()
  filename = paste(matpath,"/randonforest_cross_check.pdf",sep = "")
  ggsave(filename,p,width = 5,height = 3)
  data <- result[[4]]
  filename = paste(matpath,"/randomforest_cross_data.csv",sep = "")
  write.csv(data,filename,quote = F)


data <- result[[5]]
filename = paste(matpath,"/randomforest_data.csv",sep = "")
write.csv(data,filename,quote = F)




#---网络展示#------

netpath = paste(otupath,"/network_igraph/",sep = "")
dir.create(netpath)
# map = sample_data(ps)
# map$Group = "one"
# sample_data(ps16s) = map
library(igraph)
library(ggnewscale)
data(ps)

result = network.i(ps =  ps,
                   N = 1000,
                   r.threshold=0.6,
              
                   big = T,
                   select_layout = T,
                   method = "pearson",
                   scale = FALSE,
                   layout_net = "model_igraph2",
                   p.threshold=0.1,
                   label = FALSE,
                   path = netpath ,
                   ncol = gnum,
                   nrow = 1,
                   zipi = F,
                   order = NULL
)

p1 = result[[1]]

dat = result[[2]]

p = result[[5]]
gnum = 3
plotname1 = paste(netpath,"/network_all.pdf",sep = "")
ggsave(plotname1, p1,width = 3*gnum,height = 2,limitsize = FALSE)

plotname1 = paste(netpath,"/network_all2.pdf",sep = "")
ggsave(plotname1, p,width = 6*gnum,height = 5.5,limitsize = FALSE)


# rm(list=ls())
# library(tidyverse)
# library(ggClusterNet)
# library(phyloseq)
# library(igraph)
# library(tidyfst)
# 
# ps.st = readRDS("./ps_TS.rds")
# ps.st
# ps.st %>% sample_data() %>% head()
# 
# #时空组网络-分面网络图-解决填充颜色不一致问题
# data(ps)
# map = sample_data(ps)
# map$Group = "A"
# sample_data(ps) = map
# library(igraph)
# res = Facet.network (
#   ps.st= ps,# phyloseq对象
#   N = 1000,
#   g1 = "Group",# 分组1
#   # g2 = "space",# 分组2
#   # g3 = "time",# 分组3
#   # ord.g1 = c("WT","KO","OE"),# 排序顺序
#   # ord.g2 = c("B","R") ,# 排序顺序
#   # ord.g3 = c("T1","T2","T3") ,# 排序顺序
#   # order = "time", # 出图每行代表的变量
#   fill = "Phylum",
#   size = "igraph.degree",
#   layout_net = "model_igraph2",
#   r.threshold=0.8,
#   p.threshold=0.05,
#   method = "spearman",
#   select_layout = TRUE,
#   clu_method = "cluster_fast_greedy",
#   maxnode = 5
# )
# 
# p = res[[1]]
# 
# p



#------FEAST溯源分析#-------
betapath = paste(otupath,"/Feast/",sep = "")
dir.create(betapath)

data(ps)
# source("E:\\Shared_Folder\\Function_local\\R_function\\Liu_project\\amplicon-master\\R\\开发花絮\\FEAST-master\\FEAST_src\\src.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\FEAST.R",encoding = "UTF-8")


result = FEAST(ps = ps,
               group = "Group",
               sinkG = "KO",
               sourceG = c("WT","OE"),
               path = "E:/Shared_Folder/Function_local/R_function/micro/" # 注意按照自己设定的路径进行修改
)

# result
p <- Plot_FEAST(data = result)
p
p2 = MuiPlot_FEAST(data = result)

p2


FileName <- paste(betapath,"Feast_data.csv", sep = "")
write.csv(result,FileName, quote = F)

FileName <- paste(betapath,"Feast_Group.pdf", sep = "")
ggsave(FileName, p, width = 3, height = 3)

p2 = MuiPlot_FEAST(data = result)
p2
FileName <- paste(betapath,"Feast_sample.pdf", sep = "")
ggsave(FileName, p2, width = 8, height = 5)


#---science组合图表#-----

env = read.csv("./data/dataNEW/env.csv")
head(env)
env = env[,1:12]
envRDA = env

head(env)
row.names(envRDA) = env$ID
envRDA$ID = NULL
head(envRDA)

otupath


#--微生物群落和环境Science组合图表#---------
compath = paste(otupath,"/Conbine_env_plot/",sep = "")
dir.create(compath)

otu1 = as.data.frame(t(ggClusterNet::vegan_otu(ps)))
head(otu1)

tabOTU1 = list(bac = otu1
)

# tabOTU1 = list(b = otu)
# tabOTU1 = list(f = otu2)

p0 <- ggClusterNet:: MatCorPlot(env.dat = envRDA,
                                tabOTU = tabOTU1,
                                diag = F,
                                range = 0.1,
                                numpoint = 22,
                                sig = TRUE,
                                siglabel = FALSE,
                                shownum = F,
                                curvature = 0.1,
                                numsymbol = NULL,
                                lacx = "left",
                                lacy = "bottom",
                                p.thur = 0.05,
                                onlysig = F
)
p0

p0 <- p0 + scale_colour_manual(values = c("blue","red")) + 
  scale_fill_distiller(palette="PRGn")
p0

FileName <- paste(compath,"Conbine_envplot", ".pdf", sep = "")
ggsave(FileName, p0,width = 8,height =6)# , device = cairo_pdf, family = "Song"

FileName <- paste(compath,"Conbine_envplot", ".png", sep = "")
ggsave(FileName, p0,width = 15,height = 10)# , device = cairo_pdf, family = "Song"






