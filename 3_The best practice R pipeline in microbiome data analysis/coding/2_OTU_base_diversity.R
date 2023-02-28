
# source("./coding/OTU_parameter.R")


# #-分析共四个部分，这是第二个部分，代号2，第一部分是扩增子原始序列处理
# #---result1 base_diversity analyses-------------
# otupath = paste(res1path,"/OTU_220715/",sep = "");otupath
# dir.create(otupath)

#--基本表格保存#----------
tabpath = paste(otupath,"/report_table/",sep = "")
dir.create(tabpath)
#--raw otu tab
otu = as.data.frame(t(ggClusterNet::vegan_otu(ps)))
head(otu)
FileName <- paste(tabpath,"/otutab.csv", sep = "")
write.csv(otu,FileName,sep = "")
# tax table
tax = as.data.frame((ggClusterNet::vegan_tax(ps)))
head(tax)
FileName <- paste(tabpath,"/tax.csv", sep = "")
write.csv(otu,FileName,sep = "")

ps_rela  = phyloseq::transform_sample_counts(ps, function(x) x / sum(x) );ps_rela 
#--norm otu tab
otu_norm = as.data.frame(t(ggClusterNet::vegan_otu(ps_rela)))
FileName <- paste(tabpath,"/otutab_norm.csv", sep = "")
write.csv(otu_norm,FileName,sep = "")

otutax <- cbind(as.data.frame(t(ggClusterNet::vegan_otu(ps_rela))),as.data.frame((ggClusterNet::vegan_tax(ps_rela))))
FileName <- paste(tabpath,"/otutax_norm.csv", sep = "")
write.csv(otutax,FileName,sep = "")


for (i in 2: length(phyloseq::rank_names(ps))) {
  psi  <- ggClusterNet::tax_glom_wt(ps = ps,ranks = phyloseq::rank_names(ps)[i])
  #--raw otu tab
  otu = as.data.frame(t(ggClusterNet::vegan_otu(psi)))
  FileName <- paste(tabpath,"/otutab",phyloseq::rank_names(ps)[i],".csv", sep = "")
  write.csv(otu,FileName,sep = "")
  # tax table
  tax = as.data.frame((ggClusterNet::vegan_tax(ps)))
  FileName <- paste(tabpath,"/tax",phyloseq::rank_names(ps)[i],".csv", sep = "")
  write.csv(otu,FileName,sep = "")
  
  psi_rela  = phyloseq::transform_sample_counts(psi, function(x) x / sum(x) );psi_rela 
  #--norm otu tab
  otu_norm = as.data.frame(t(ggClusterNet::vegan_otu(psi_rela)))
  FileName <- paste(tabpath,"/otutab_norm",phyloseq::rank_names(psi)[i],".csv", sep = "")
  write.csv(otu_norm,FileName,sep = "")
  
  otutax <- cbind(as.data.frame(t(ggClusterNet::vegan_otu(psi_rela))),as.data.frame((ggClusterNet::vegan_tax(psi_rela))))
  FileName <- paste(tabpath,"/otutax_norm",phyloseq::rank_names(ps)[i],".csv", sep = "")
  write.csv(otutax,FileName,sep = "")
}


#如果导入数据是测序原始count，可以选择后面这几种标准化方法# sampling, log,TMM,RLE,upperquartile
# 下面是运行案例，直接降otu表格导出即可。
# ps1  = ps %>%
#   ggClusterNet::scale_micro(method = "TMM" )
# otu = as.data.frame(t(ggClusterNet::vegan_otu(ps1)))
# head(otu)



#--alpha多样性#---------
alppath = paste(otupath,"/alpha/",sep = "")
dir.create(alppath)


# source("../micro/alpha-diversity.R")

#---多种指标alpha多样性分析加出图-标记显著性
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/alpha-diversity.R")
index = c("Shannon","Inv_Simpson","Pielou_evenness","Simpson_evenness" ,"Richness" ,"Chao1","ACE" )

#--多种组合alpha分析和差异分析出图
alp = alpha(ps = ps,inde="Shannon",group = "Group",Plot = TRUE )
index= alp
head(index)

#--提取三个代表指标作图
all.alpha = T

if (all.alpha) {
  sel = c(match("Inv_Simpson",colnames(index)),
          match("Pielou_evenness",colnames(index)),
          match("Simpson_evenness",colnames(index)),
          match("Richness",colnames(index)),
          match("Chao1",colnames(index)),
          match("ACE",colnames(index)),
          match("Shannon",colnames(index))
          
  )
  h = 3
} else{
  sel = c(match("Shannon",colnames(index)),match("Richness",colnames(index)),
          match("Pielou_evenness",colnames(index)))
  h = 1
}

n = length(sel) + 3


data = cbind(data.frame(ID = 1:length(index$Group),group = index$Group),index[sel])
head(data)



result = EasyStat::MuiKwWlx2(data = data,num = c(3:(n -1)))

FileName <- paste(alppath,"/alpha_diversity_different_label.csv", sep = "")
write.csv(result,FileName,sep = "")
FileName <- paste(alppath,"/alpha_diversity_index.csv", sep = "")
write.csv(index,FileName,sep = "")


result1 = EasyStat::FacetMuiPlotresultBox(data = data,num = c(3:(n -1)),
                                result = result,
                                sig_show ="abc",ncol = 3 )
p1_1 = result1[[1]] + 
  ggplot2::scale_x_discrete(limits = axis_order) + 
  mytheme1 +
  ggplot2::guides(fill = guide_legend(title = NULL)) +
  ggplot2::scale_fill_manual(values = colset1)
p1_1

#如何升级展示-提取数据用小提琴图展示
p1_0 = result1[[2]] %>% ggplot(aes(x=group , y=dd )) + 
  geom_violin(alpha=1, aes(fill=group)) +
  geom_jitter( aes(color = group),position=position_jitter(0.17), size=3, alpha=0.5)+
  labs(x="", y="")+
  facet_wrap(.~name,scales="free_y",ncol  = 3) +
  # theme_classic()+
  geom_text(aes(x=group , y=y ,label=stat)) +
  ggplot2::scale_x_discrete(limits = axis_order) + 
  mytheme1 +
  guides(color=guide_legend(title = NULL),
         shape=guide_legend(title = NULL),
         fill = guide_legend(title = NULL)
         ) +
  ggplot2::scale_fill_manual(values = colset1)
p1_0


res = EasyStat::FacetMuiPlotresultBar(data = data,num = c(3:(n -1)),result = result,sig_show ="abc",ncol = 3)
p1_2 = res[[1]]+ scale_x_discrete(limits = axis_order) + guides(color = FALSE) +
  mytheme1+ 
  guides(fill = guide_legend(title = NULL))+
  scale_fill_manual(values = colset1)
p1_2

res = EasyStat::FacetMuiPlotReBoxBar(data = data,num = c(3:(n -1)),result = result,sig_show ="abc",ncol = 3)
p1_3 = res[[1]]+ scale_x_discrete(limits = axis_order) + 
  mytheme1 + 
  guides(fill = guide_legend(title = NULL))+
  scale_fill_manual(values = colset1)
p1_3


FileName <- paste(alppath,"Alpha_Facet_vl", ".pdf", sep = "")
ggsave(FileName, p1_0, width = ((1 + gnum) * 3), height =4 *h,limitsize = FALSE)
FileName <- paste(alppath,"Alpha_Facet_vl", ".png", sep = "")
ggsave(FileName, p1_0, width = ((1 + gnum) * 3), height =4 *h,limitsize = FALSE)


FileName <- paste(alppath,"Alpha_Facet_box", ".pdf", sep = "")
ggsave(FileName, p1_1, width = ((1 + gnum) * 3), height =4 *h,limitsize = FALSE)

FileName <- paste(alppath,"Alpha_Facet_bar", ".pdf", sep = "")
ggsave(FileName, p1_2, width = ((1 + gnum) * 3), height = 4 *h,limitsize = FALSE)

FileName <- paste(alppath,"Alpha_Facet_boxbar", ".pdf", sep = "")
ggsave(FileName, p1_3, width = ((1 + gnum) * 3), height = 4 *h,limitsize = FALSE)

FileName <- paste(alppath,"Alpha_Facet_box", ".jpg", sep = "")
ggsave(FileName, p1_1, width = ((1 + gnum) * 3), height =4*h,limitsize = FALSE)

FileName <- paste(alppath,"Alpha_Facet_bar", ".jpg", sep = "")
ggsave(FileName, p1_2, width = ((1 + gnum) * 3), height = 4*h,limitsize = FALSE)

FileName <- paste(alppath,"Alpha_Facet_boxbar", ".jpg", sep = "")
ggsave(FileName, p1_3, width = ((1 + gnum) * 3), height = 4*h,limitsize = FALSE)


#--总体差异检测alpha多样性
krusk1 = ggpubr::compare_means( Shannon ~ group, data=data, method = "kruskal.test")
krusk2 = ggpubr::compare_means( Richness ~ group, data=data, method = "kruskal.test")
krusk3 = ggpubr::compare_means( Pielou_evenness ~ group, data=data, method = "kruskal.test")

dat = rbind(krusk1,krusk2,krusk3) %>% as.data.frame()
FileName <- paste(alppath,"/alpha_diversity_index_all_p_Kruskal-Wallis.csv", sep = "")
write.csv(dat,FileName,sep = "")

#--系统发育多样性#------
alppath = paste(otupath,"/alpha/",sep = "")
dir.create(alppath)
com_2020 <- ps %>% vegan_otu() %>% 
  as.data.frame()
library(ape)
library(picante) 

## 加载系统发育分析包
rooted <- phy_tree(ps0)  
## 读取系统发育树文件
# rooted <- root(phylo,outgroup = "Amborella_trichopoda",resolve.root = T)  ## 转化为有根树
## 转化为距离矩阵

cover2020.pd<-pd(com_2020,rooted,include.root=F) 

map = sample_data(ps)
head(map)

data = cbind(map[,c("ID","Group")],pd = cover2020.pd[,1])
head(data)
colnames(data)[2] = "group"
data$group = as.factor(data$group)
result = EasyStat::MuiKwWlx2(data = data,num = c(3))

FileName <- paste(alppath,"/pd_diversity_different_label.csv", sep = "")
write.csv(result,FileName,sep = "")
FileName <- paste(alppath,"/pd_diversity_index.csv", sep = "")
write.csv(data,FileName,sep = "")

result1 = EasyStat::FacetMuiPlotresultBox(data = data,num = c(3),
                                          result = result,
                                          sig_show ="abc",ncol = 1 )
p1_1 = result1[[1]] + 
  ggplot2::scale_x_discrete(limits = axis_order) + 
  mytheme2 +
  ggplot2::guides(fill = guide_legend(title = NULL)) +
  ggplot2::scale_fill_manual(values = colset1)
p1_1

#如何升级展示-提取数据用小提琴图展示
p1_1 = result1[[2]] %>% ggplot(aes(x=group , y=dd )) + 
  geom_violin(alpha=1, aes(fill=group)) +
  geom_jitter( aes(color = group),position=position_jitter(0.17), size=3, alpha=0.5)+
  labs(x="", y="")+
  facet_wrap(.~name,scales="free_y",ncol  = 3) +
  # theme_classic()+
  geom_text(aes(x=group , y=y ,label=stat)) +
  ggplot2::scale_x_discrete(limits = axis_order) + 
  mytheme2 +
  guides(color=guide_legend(title = NULL),
         shape=guide_legend(title = NULL),
         fill = guide_legend(title = NULL)
  ) +
  ggplot2::scale_fill_manual(values = colset1)
p1_1

res = EasyStat::FacetMuiPlotresultBar(data = data,num = c(3),result = result,sig_show ="abc",ncol = 1)
p1_2 = res[[1]]+ scale_x_discrete(limits = axis_order) + guides(color = FALSE) +
  mytheme2+ 
  guides(fill = guide_legend(title = NULL))+
  scale_fill_manual(values = colset1)
p1_2

res = EasyStat::FacetMuiPlotReBoxBar(data = data,num = c(3),result = result,sig_show ="abc",ncol =1)
p1_3 = res[[1]]+ scale_x_discrete(limits = axis_order) + 
  mytheme2 + 
  guides(fill = guide_legend(title = NULL))+
  scale_fill_manual(values = colset1)
p1_3


FileName <- paste(alppath,"pd_Facet_box", ".pdf", sep = "")
ggsave(FileName, p1_1, width = ((1 + gnum) ), height =4,limitsize = FALSE)

FileName <- paste(alppath,"pd_Facet_bar", ".pdf", sep = "")
ggsave(FileName, p1_2, width = ((1 + gnum) ), height = 4,limitsize = FALSE)

FileName <- paste(alppath,"pd_Facet_boxbar", ".pdf", sep = "")
ggsave(FileName, p1_3, width = ((1 + gnum) ), height = 4,limitsize = FALSE)

FileName <- paste(alppath,"pd_Facet_box", ".jpg", sep = "")
ggsave(FileName, p1_1, width = ((1 + gnum) ), height =4,limitsize = FALSE)

FileName <- paste(alppath,"pd_Facet_bar", ".jpg", sep = "")
ggsave(FileName, p1_2, width = ((1 + gnum) ), height = 4,limitsize = FALSE)

FileName <- paste(alppath,"pd_Facet_boxbar", ".jpg", sep = "")
ggsave(FileName, p1_3, width = ((1 + gnum) ), height = 4,limitsize = FALSE)



rare <- mean(phyloseq::sample_sums(ps))/10

#--alpha稀释曲线绘制#-----
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

p2_4 <- result[[5]] +
  mytheme1 +
  guides(fill = guide_legend(title = NULL))+
  scale_fill_manual(values = colset1)


FileName <- paste(alppath,"Alpha_rare_sample", ".pdf", sep = "")
ggsave(FileName, p2_1, width = 8, height =6)

FileName <- paste(alppath,"Alpha_rare_group", ".pdf", sep = "")
ggsave(FileName, p2_2, width = 8, height =6)
FileName <- paste(alppath,"Alpha_rare_groupwithSD", ".pdf", sep = "")
ggsave(FileName, p2_3, width = 8, height =6)
FileName <- paste(alppath,"Alpha_rare_sample", ".jpg", sep = "")
ggsave(FileName, p2_1, width = 8, height =6)
FileName <- paste(alppath,"Alpha_rare_group", ".jpg", sep = "")
ggsave(FileName, p2_2, width = 8, height =6)
FileName <- paste(alppath,"Alpha_rare_groupwithSD", ".jpg", sep = "")
ggsave(FileName, p2_3, width = 8, height =6)


FileName <- paste(alppath,"/Alpha_rare_data.csv", sep = "")
write.csv(raretab,FileName,sep = "")


#---排序分析beta-diversity#-----
betapath = paste(otupath,"/beta/",sep = "")
dir.create(betapath)

# "unifrac" "wunifrac" "dpcoa" "jsd" "manhattan" "euclidean"   "canberra" "bray" "kulczynski" 
# "jaccard" "gower" "altGower" "morisita" "horn" "mountford"  "raup" "binomial" 
# "chao"  "cao" "w"  "-1"  "c" "wb"  "r"   "I"  "e" "t" "me"   "j"  "sor"  "m"   "-2"  "co"
# DCA, CCA, RDA, NMDS, MDS, PCoA, PCA, LDA tsne 

source("E:\\Shared_Folder\\Function_local\\R_function\\micro/BetaDiv.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/MicroTest.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/pairMicroTest.R")

methodlist = c("NMDS","PCoA", "PCA")

# methodlist = c("LDA")
for (method in methodlist) {
  result = BetaDiv(ps = ps, group = "Group", dist = "bray",
                   method = method, Micromet = "anosim", pvalue.cutoff = 0.05,
                   pair = T)
  p3_1 = result[[1]] + 
    scale_fill_manual(values = colset1)+
    scale_color_manual(values = colset1,guide = F) +
    mytheme1 
    # theme(legend.position = c(0.2,0.2))
  p3_1
  #带标签图形出图
  p3_2 = result[[3]] +
    scale_fill_manual(values = colset1)+
    scale_color_manual(values = colset1,guide = F) + 
    mytheme1 
    # theme(legend.position = c(0.2,0.2))
  p3_2
  
  FileName <- paste(betapath,"/a2_",method,"bray.pdf", sep = "")
  ggsave(FileName, p3_1, width = 8, height = 7)
  FileName1 <- paste(betapath,"/a2_",method,"",method,"bray.jpg", sep = "")
  ggsave(FileName1 , p3_1, width = 12, height = 11)
  
  FileName <- paste(betapath,"/a2_",method,"bray_label.pdf", sep = "")
  ggsave(FileName, p3_2, width = 12, height = 12)
  FileName1 <- paste(betapath,"/a2_",method,"bray_label.jpg", sep = "")
  ggsave(FileName1 , p3_2, width = 12, height = 11)
  
  # 提取出图数据
  plotdata = result[[2]]
  FileName <-  paste(betapath,"/a2_",method,"bray.csv", sep = "")
  write.csv(plotdata,FileName)
  #---------排序-精修图
  plotdata =result[[2]]
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
  p3_3 = p3_1 +geom_segment(data = segs,
                            mapping = aes(xend = oNMDS1, yend = oNMDS2,color = Group),show.legend=F) + # spiders
    geom_point(mapping = aes(x = x, y = y),data = cent, size = 5,pch = 24,color = "black",fill = "yellow") +
    scale_fill_manual(values = colset1)+
    scale_color_manual(values = colset1,guide = F) + 
    mytheme1 
    # theme(legend.position = c(0.2,0.2))
  p3_3
  
  FileName <- paste(betapath,"/a2_",method,"bray_star.pdf", sep = "")
  ggsave(FileName, p3_3, width = 8, height = 7)
  FileName1 <- paste(betapath,"/a2_",method,"bray_star.jpg", sep = "")
  ggsave(FileName1 , p3_3, width = 8, height = 7)
  
}

map

#提取总体比较
TResult =result[[5]]
head(TResult)

# 提取两两检测结果
pair = result[[4]]
pair
FileName <- paste(betapath,"Pair_anosim.csv", sep = "")
write.csv(pair,FileName)
FileName <- paste(betapath,"Total_anosim.csv", sep = "")
write.csv(TResult,FileName)

#--换用adonis差异分析
# title1 = MicroTest(ps = ps, Micromet = "adonis", dist = "bray")
# title1
# FileName <- paste(betapath,"Total_adonis.csv", sep = "")
# write.csv(title1,FileName)
# pairResult = pairMicroTest(ps = ps, Micromet = "adonis", dist = "bray")
# FileName <- paste(betapath,"Pair_anosim.csv", sep = "")
# write.csv(pair,FileName)

#---普氏分析#------
map = phyloseq::sample_data(ps)
samegroup = map$Group %>% table() %>% as.data.frame() %>% .$Freq %>% unique() %>% length() == 1


if (samegroup) {
  source("E:\\Shared_Folder\\Function_local\\R_function\\micro/matel_pro_plot.R")
  #--门特尔检验-普氏分析
  if (length(unique(phyloseq::sample_data(ps)$Group)) < 6) {
    # library(vegan)
    size = combn(unique(phyloseq::sample_data(ps)$Group),2) %>% dim()
    size
    result <- mantal.micro(ps = ps,
                           method =  "spearman",
                           group = "Group",
                           ncol = size[2],
                           nrow = 1
    )
    data <- result[[1]]
    
    p3_7 <- result[[2]] +  mytheme1 
    p3_7
    
    FileName <- paste(betapath,"mantel_pro.csv", sep = "")
    write.csv(data,FileName)
    FileName1 <- paste(betapath,"/a2_","Mantel_Pro.pdf", sep = "")
    ggsave(FileName1 , p3_7, width = size[2] *6, height = 6,limitsize = FALSE)
    FileName1 <- paste(betapath,"/a2_","Mantel_Pro.jpg", sep = "")
    ggsave(FileName1 , p3_7, width = size[2] *6, height =6,limitsize = FALSE)
  }
}


#---物种分类树分析#-----
# detach("package:treeio")
# detach("package:vegan")
library(ggstar)
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/phy_tree_micro.R")

barpath = paste(otupath,"/phy_tree_micro/",sep = "");print(barpath)
dir.create(barpath)
library(ggClusterNet)
# rank.names(ps)
# Top_micro = 150
result <- phy_tree_micro(ps = ps,Top = 150)

p0 = result[[1]]
p1 = result[[2]]
p2 = result[[3]]
p3 = result[[4]]
p4 = result[[5]]
p5 = result[[6]]
p6 = result[[7]]
p7 = result[[8]]

detach("package:ggstar")

FileName <- paste(barpath,Top_micro,"phy_tree_micro1", ".pdf", sep = "")
ggsave(FileName, p0, width = 4, height = 4)
FileName <- paste(barpath,Top_micro,"phy_tree_micro2", ".pdf", sep = "")
ggsave(FileName, p1, width = 6, height =4)
FileName <- paste(barpath,Top_micro,"phy_tree_micro3", ".pdf", sep = "")
ggsave(FileName, p2, width = 6, height = 4)

FileName <- paste(barpath,Top_micro,"phy_tree_micro4", ".pdf", sep = "")
ggsave(FileName, p3, width = 12, height = 10)
FileName <- paste(barpath,Top_micro,"phy_tree_micro5", ".pdf", sep = "")
ggsave(FileName, p4, width = 15, height = 13)
FileName <- paste(barpath,Top_micro,"phy_tree_micro5_2", ".pdf", sep = "")
ggsave(FileName, p5, width = 18, height = 15)
FileName <- paste(barpath,Top_micro,"phy_tree_micro6", ".pdf", sep = "")
ggsave(FileName, p6, width = 5, height = 4)
FileName <- paste(barpath,Top_micro,"phy_tree_micro7", ".pdf", sep = "")
ggsave(FileName, p7, width = 6, height = 5)

# library(cowplot)
# save_plot(FileName, p2, base_height = 7, base_width =7)

FileName <- paste(barpath,Top_micro,"phy_tree_micro1", ".png", sep = "")
ggsave(FileName, p0, width = 4, height = 4)
FileName <- paste(barpath,Top_micro,"phy_tree_micro2", ".png", sep = "")
ggsave(FileName, p1, width = 6, height =4)
FileName <- paste(barpath,Top_micro,"phy_tree_micro3", ".png", sep = "")
ggsave(FileName, p2, width = 7, height = 7,dpi = 72)
FileName <- paste(barpath,Top_micro,"phy_tree_micro4", ".png", sep = "")
ggsave(FileName, p3, width = 12, height = 12,dpi = 72)
FileName <- paste(barpath,Top_micro,"phy_tree_micro5", ".png", sep = "")
ggsave(FileName, p4, width = 15, height = 15,dpi = 72)
FileName <- paste(barpath,Top_micro,"phy_tree_micro5_2", ".png", sep = "")
ggsave(FileName, p5, width = 18, height = 15,dpi = 72)
FileName <- paste(barpath,Top_micro,"phy_tree_micro6", ".png", sep = "")
ggsave(FileName, p6, width = 5, height = 4,dpi = 72)
FileName <- paste(barpath,Top_micro,"phy_tree_micro7", ".png", sep = "")
ggsave(FileName, p7,width = 6, height = 5,dpi = 72)


#--和弦图#-----
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/cir_plot.R")

# otupath = paste(res1path,"/OTU/",sep = "");otupath
# dir.create(otupath)

barpath =  paste(otupath,"/circle_plot/",sep = "");otupath
dir.create(barpath)

for (i in 2:7) {
  cir_plot(ps  = ps,Top = 10,rank = i,
           path = barpath)
}


#-------物种组成展示#---------
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/barMainplot.R")
barpath = paste(otupath,"/Microbial_composition/",sep = "")
dir.create(barpath)

phyloseq::rank_names(ps)

for (j in c("Phylum" , "Class" ,  "Order"  , "Family" , "Genus")) {
  result = barMainplot(ps = ps,
                       j = j,
                       # axis_ord = axis_order,
                       label = FALSE,
                       sd = FALSE,
                       Top = Top)
  p4_1 <- result[[1]] + 
    # scale_fill_brewer(palette = "Paired") + 
    scale_fill_manual(values = colset3) +
    scale_x_discrete(limits = axis_order) +
    mytheme1
  p4_1

  p4_2  <- result[[3]] + 
    # scale_fill_brewer(palette = "Paired") + 
    scale_fill_manual(values = colset3) +
    scale_x_discrete(limits = axis_order) + 
    mytheme1
  p4_2
  
  databar <- result[[2]] %>% group_by(Group,aa) %>%
    summarise(sum(Abundance)) %>% as.data.frame()
  head(databar)
  colnames(databar) = c("Group",j,"Abundance(%)")
  
  
  FileName1 <- paste(barpath,"/a2_",j,"_barflow",".pdf", sep = "")
  ggsave(FileName1, p4_2, width = (5+ gnum), height =8 )
  FileName2 <- paste(barpath,"/a2_",j,"_barflow",".jpg", sep = "")
  ggsave(FileName2, p4_2, width = (5+ gnum), height =8 )
  
  FileName1 <- paste(barpath,"/a2_",j,"_bar",".pdf", sep = "")
  ggsave(FileName1, p4_1, width = (5+ gnum), height =8 )
  FileName2 <- paste(barpath,"/a2_",j,"_bar",".jpg", sep = "")
  ggsave(FileName2, p4_1, width = (5+ gnum), height =8 )
  
  FileName <- paste(barpath,"/a2_",j,"_bar_data",".csv", sep = "")
  write.csv(databar,FileName,quote = F)
}


detach("package:ggalluvial")

#----微生物桑基图#--------
library(ggClusterNet)
snapath =  paste(otupath,"/sankeyNetwork/",sep = "");otupath
dir.create(snapath)
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\sankey.m.Group.R",encoding = "utf-8")


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



source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\sankey.micro.R",encoding = "utf-8")

sankey.micro (ps = ps,
              rank = 6,# 参数目前不可修改
              Top = 50,
              snapath = snapath
              
              )



#--环状物种堆叠柱状图#-----
barpath = paste(otupath,"/circle_Micro_strack_bar/",sep = "");print(barpath)
dir.create(barpath)

source("E:\\Shared_Folder\\Function_local\\R_function\\micro/circle_stark_bar.R")
library(ggtree)
# j = "Phylum"
p2 = circle_starc_bar(
  ps = ps,
  Top = 15,
  dist = "bray",
  cuttree = 3,
  hcluter_method = "complete")

FileName2 <- paste(barpath,"/a2_","_bar",".jpg", sep = "")
ggsave(FileName2, p2, width = 10, height =8 )

FileName2 <- paste(barpath,"/a2_","_bar",".pdf", sep = "")
ggsave(FileName2, p2, width = 10, height =8 )


#--距离和丰度合并#-------
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/cluMicro.bar.R")

for (j in c("Phylum" , "Class" ,  "Order"  , "Family" , "Genus")) {
  
  result <-  cluMicro.bar (dist = "bray",
                           ps = ps,
                           j = j,
                           Top = Top, # 提取丰度前十的物种注释
                           tran = TRUE, # 转化为相对丰度值
                           hcluter_method = "complete",
                           Group = "Group",
                           cuttree = length(unique(phyloseq::sample_data(ps)$Group))
  )
  
  p5_1 <- result[[1]]
  p5_2 <- result[[2]]
  p5_3 <- result[[3]]
  p5_4 <- result[[4]]
  clubardata <- result[[5]]
  
  
  FileName1 <- paste(barpath,"/a2_",j,"_cluster_sample",".pdf", sep = "")
  ggsave(FileName1, p5_1, width = 6, height = dim(phyloseq::sample_data(ps))[1]/4,limitsize = FALSE)
  FileName1 <- paste(barpath,"/a2_",j,"_cluster_bar_sample",".pdf", sep = "")
  ggsave(FileName1, p5_2, width = 12, height = dim(phyloseq::sample_data(ps))[1]/4 ,limitsize = FALSE)
  FileName1 <- paste(barpath,"/a2_",j,"_cluster_sample",".jpg", sep = "")
  ggsave(FileName1, p5_1, width = 6, height =dim(phyloseq::sample_data(ps))[1]/4 ,limitsize = FALSE)
  FileName1 <- paste(barpath,"/a2_",j,"_cluster_bar_sample",".jpg", sep = "")
  ggsave(FileName1, p5_2, width = 12, height =dim(phyloseq::sample_data(ps))[1]/4 ,limitsize = FALSE)
  
  FileName1 <- paste(barpath,"/a2_",j,"_cluster_Group",".pdf", sep = "")
  ggsave(FileName1, p5_3, width = 6, height = gnum,limitsize = FALSE)
  FileName1 <- paste(barpath,"/a2_",j,"_cluster_bar_Group",".pdf", sep = "")
  ggsave(FileName1, p5_4, width = 12, height = gnum ,limitsize = FALSE)
  FileName1 <- paste(barpath,"/a2_",j,"_cluster_Group",".jpg", sep = "")
  ggsave(FileName1, p5_3, width = 6, height = gnum ,limitsize = FALSE)
  FileName1 <- paste(barpath,"/a2_",j,"_cluster_bar_Group",".jpg", sep = "")
  ggsave(FileName1, p5_4, width = 12, height = gnum ,limitsize = FALSE)
  
  FileName <- paste(barpath,"/a2_",j,"_cluster_bar_data",".csv", sep = "")
  write.csv(clubardata,FileName)
  
}



#--三元图#--------
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/Micro_tern.R")
library(ggtern)
ternpath = paste(otupath,"/ggtern/",sep = "")
dir.create(ternpath)

Micro_tern(ps = ps,ternpath = ternpath )
detach("package:ggtern")

#---共有微生物特有微生物
#---flower plot#-------
flowpath = paste(otupath,"/flowplot/",sep = "")
dir.create(flowpath)


source("E:\\Shared_Folder\\Function_local\\R_function\\micro/ggflowerplot.R")
p0_1 <- ggflower(ps = ps,
                 # rep = 1,
                 group = "ID",
                 start = 1, # 风车效果
                 m1 = 2, # 花瓣形状，方形到圆形到棱形，数值逐渐减少。
                 a = 0.2, # 花瓣胖瘦
                 b = 1, # 花瓣距离花心的距离
                 lab.leaf = 1, # 花瓣标签到圆心的距离
                 col.cir = "yellow",
                 N = 0.5
) 

p0_1 

# p + scale_fill_brewer(palette = "Paired")
FileName1 <- paste(flowpath,"ggflowerID.pdf", sep = "")
ggsave(FileName1, p0_1, width = 9, height = 9)
FileName2 <- paste(flowpath,"ggflowerID.jpg", sep = "")
ggsave(FileName2, p0_1, width = 8, height = 8 )



p0_2 <- ggflower(ps = ps,
                 # rep = 1,
                 group = "Group",
                 start = 1, # 风车效果
                 m1 = 0.8, # 花瓣形状，方形到圆形到棱形，数值逐渐减少
                 a = 0.3, # 花瓣胖瘦
                 b = 1, # 花瓣距离花心的距离
                 lab.leaf = 1, # 花瓣标签到圆心的距离
                 col.cir = "yellow",
                 N = 0.1
) + scale_fill_manual(values = colset1) 
p0_2

FileName1 <- paste(flowpath,"ggflowerGroup.pdf", sep = "")
ggsave(FileName1, p0_2, width = 14, height = 14)
FileName2 <- paste(flowpath,"ggflowerGroup.jpg", sep = "")
ggsave(FileName2, p0_2, width = 14, height = 14 )


# ggplot升级版本韦恩图和Upset#-------

source("E:\\Shared_Folder\\Function_local\\R_function\\micro/Ven.Upset.gg.R")
map = as.data.frame(phyloseq::sample_data(ps_Ven))
gnumven <- map[,group] %>% unique() %>% dim()

if (gnumven[1] < 6) {
  Venpath = paste(otupath,"/Ven_Upset_super/",sep = "")
  dir.create(Venpath)
  
  library(ggVennDiagram)
  res = Ven.Upset(ps =  ps,
                  group = "Group",
                  N = 0.5,
                  size = 3)
  
  p1 = res[[1]]
  p2 = res[[2]]
  
  filename3 <- paste(Venpath,"Ven_gg.pdf", sep = "")
  ggsave(filename3, p1, width = 8, height = 8)
  filename3 <- paste(Venpath,"Upset_gg.pdf", sep = "")
  ggsave(filename3, p2, width = 8, height = 8)
}

#---Ven-Upset#----------
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/VenSeper.R")
source("E:/Shared_Folder/Function_local/R_function/micro/barMainplot.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/Ven-Upset.R")
# j = "Genus"
group = "Group"
ps_Ven = ps
# BiocManager::install("VennDiagram")
# otutab = as.data.frame(otu_table(ps))


if (gnumven[1] < 6) {
  

  Venpath = paste(otupath,"/Ven_Upset_super/",sep = "")
  dir.create(Venpath)

  
  result = VenUpset(ps = ps_Ven,
                    group = group,
                    path = Venpath
  )
  
  #---每个部分
  result = VenSeper(ps = ps_Ven,
                   path = Venpath,
                   group = group,
                   j = j,
                   Top = 10
                   
  )
  # 提取韦恩图中全部部分的otu极其丰度做门类柱状图
  p7_1 <- result[[1]]
  #每个部分序列的数量占比，并作差异
  p8 <- result[[2]]
  # 每部分的otu门类冲积图
  p7_2 <- result[[3]]
  
  
  FileName <- paste(Venpath,j,"count_Facet_ven", ".pdf", sep = "")
  ggsave(FileName, p7_1, width = 15, height = 12)
  
  FileName <- paste(Venpath,j,"diff_count_box", ".pdf", sep = "")
  ggsave(FileName, p8, width = 15, height = 12)
  
  FileName <- paste(Venpath,j,"count_Facet_ven_flow", ".pdf", sep = "")
  ggsave(FileName, p7_2, width = 15, height = 12)
  
  FileName <- paste(Venpath,j,"count_Facet_ven", ".jpg", sep = "")
  ggsave(FileName, p7_1, width = 15, height = 12)
  
  FileName <- paste(Venpath,j,"diff_count_box", ".jpg", sep = "")
  ggsave(FileName, p8, width = 15, height = 12)
  
  FileName <- paste(Venpath,j,"count_Facet_ven_flow", ".jpg", sep = "")
  ggsave(FileName, p7_2, width = 15, height = 12)
  
}



#--维恩网络#-------
library(ggClusterNet)
library(phyloseq)
biospath = paste(otupath,"/biospr_network_Ven/",sep = "")
dir.create(biospath)
N = 0.5
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
                            data = edge, size = 0.3,color = "yellow") +
  geom_point(aes(X1, X2,fill = Phylum,size =mean ),pch = 21, data = plotcord) +
  geom_point(aes(X1, X2),pch = 21, data = groupdata,size = 5,fill = "blue",color = "black") +
  geom_text(aes(X1, X2,label = elements ), data = groupdata,hjust = 1,vjust = -1) +
  theme_void()

p

filename = paste(biospath,"/","biostr_Ven_network.pdf",sep = "")
ggsave(filename,p,width = (15),height = (12))
filename = paste(biospath,"/","biostr_Ven_network.jpg",sep = "")
ggsave(filename,p,width = (15),height = (12))

detach("package:ggClusterNet")
detach("package:phyloseq")


#--差异分析edgeR#----
diffpath = paste(otupath,"/diff_tax/",sep = "")
dir.create(diffpath)

diffpath.1 = paste(diffpath,"/DEsep2/",sep = "")
dir.create(diffpath.1)
# 准备脚本
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\EdgerSuper.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\DESep2_micro.R")
# source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\Plot.CompareWithCK.R",encoding = "utf-8")

res = DESep2_Meta2(ps = ps,group  = "Group",artGroup =NULL,
                   j = "OTU",
                   path = diffpath.1
)
head(res)

filename = paste(diffpath.1,"/","_","OTU","_","DESep2_all.csv",sep = "")
write.csv(res,filename,quote = F)

for (j in 2:6) {
  res = DESep2_Meta2(ps = ps,group  = "Group",artGroup =NULL,
                     j = j,
                     path = diffpath.1
  )
  head(res)
  
  filename = paste(diffpath.1,"/","_",j,"_","DESep2_all.csv",sep = "")
  write.csv(res,filename,quote = F)
}

diffpath.2 = paste(diffpath,"/EDgeR/",sep = "")
dir.create(diffpath.2)

res = EdgerSuper(ps = ps,group  = "Group",artGroup = NULL,
                 j = "OTU",
                 path = diffpath.2
)
head(res)
filename = paste(diffpath.2,"/","_","OTU","_","edger_all.csv",sep = "")
write.csv(res,filename)

for (j in c(2:6)) {
  res = EdgerSuper(ps = ps,group  = "Group",artGroup = NULL,
                   j = j,
                   path = diffpath.2
  )
  head(res)
  filename = paste(diffpath.2,"/","_",j,"_","edger_all.csv",sep = "")
  write.csv(res,filename)
}


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



# # library(dplyr)
# psedger =  filter_OTU_ps(ps = ps,Top = 2000)
# psedger
# res1 <- res[row.names(tax_table(psedger)),]
# result <- Plot.CompareWithCK(ps = psedger,CK = CK,j = "Genus",abun = 0.001,result = res1 )
# p = result[[1]]
# 
# data = result[[2]]
# 
# filename = paste(diffpath,"/","edger_001_diff_bio_plot.pdf",sep = "")
# ggsave(filename,p,width = 10,height = dim(data)[1]/8)
# filename = paste(diffpath,"/","edger_001_diff_bio_plot.jpg",sep = "")
# ggsave(filename,p,width = 10,height = dim(data)[1]/8)
# 
# filename = paste(diffpath,"/","edger_Top_2000_plotdata.csv",sep = "")
# write.csv(data,filename)


#---------stemp_差异分析#-------
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/stemp_diff.R")

# library(phyloseq)
# sample_data(ps)$Group

diffpath = paste(otupath,"/stemp_diff/",sep = "")
dir.create(diffpath)
# https://mp.weixin.qq.com/s/DTOz37JgH80kuLNi6Ae6-g
#---分组两两提取
# map = phyloseq::sample_data(ps)
# map$Group = as.factor(map$Group)
# sample_data(ps) = map


allgroup <- combn(unique(map$Group),2)
for (i in 1:dim(allgroup)[2]) {
  ps_sub <- phyloseq::subset_samples(ps,Group %in% allgroup[,i]);ps_sub
  
  for (j in 2:6) {
    p <- stemp_diff(ps = ps_sub,Top = 20,ranks = j)
    p
    # filename = paste(diffpath,"/",paste(allgroup[,i][1],allgroup[,i][2],sep = "_"),"stemp_P_plot.csv",sep = "")
    # write.csv(diff.mean,filename)
    filename = paste(diffpath,"/",paste(allgroup[,i][1],
                                        allgroup[,i][2],sep = "_"),phyloseq::rank.names(ps)[j],"stemp_plot.pdf",sep = "")
    ggsave(filename,p,width = 14,height = 6)
    
    filename = paste(diffpath,"/",paste(allgroup[,i][1],
                                        allgroup[,i][2],sep = "_"),phyloseq::rank.names(ps)[j],"stemp_plot.jpg",sep = "")
    ggsave(filename,p,width = 14,height = 6)
  }
}

detach("package:patchwork")

library(ggClusterNet)
library(tidyverse)
library(phyloseq)



source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\EdgerSuper.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\EdgerSuper2.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\Mui.cluster-group.volcano.R")

# # 聚类差异火山图指定分组#-------
# # source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\Plot.CompareWithCK.R",encoding = "utf-8")
# id = sample_data(ps)$Group %>% unique()
# # group1 = c("Group1","Group2")
# # b= data.frame(group1)
# diffpath.1 = paste(otupath,"/Mui.cluster.v/",sep = "")
# dir.create(diffpath.1)
# 
# for (i in 1:length(id)) {
#   aaa = combn(id,2)
#   diffpathv = paste(diffpath.1,"/Mui.cluster.v",paste(aaa[,i][1],aaa[,i][2],sep = "_"),sep = "")
#   dir.create(diffpathv)
# 
# 
#   res = EdgerSuper(ps = ps,group  = "Group",artGroup = NULL,
#                    j = "OTU",
#                    path = diffpathv
#   )
# 
#   head(res)
# 
#   result = Mui.cluster.volcano(res = res)
#   p = result[[1]]
#   # p
# 
#   p1 = result[[2]]
#   # p
#   filename = paste(diffpathv,"/","Mui.cluster.volcano.label.pdf",sep = "")
#   ggsave(filename,p,width = 8,height = 4)
#   filename = paste(diffpathv,"/","Mui.cluster.volcano.pdf",sep = "")
#   ggsave(filename,p1,width = 8,height = 4)
# }


#----多组差异分析火山图#------
diffpath.1 = paste(otupath,"/Mui.Group.v/",sep = "")
dir.create(diffpath.1)
res = EdgerSuper2 (ps = ps,group  = "Group",artGroup =NULL,
                   j = "OTU",
                   path = diffpath.1
)

head(res)


result = Mui.Group.volcano (res = res)
p = result[[2]]
p
filename = paste(diffpath.1,"/","Mui.group.volcano.pdf",sep = "")
ggsave(filename,p,width = 12,height = 6,limitsize = FALSE)


#----热图和气泡图展示微生物丰度--------

heatpath = paste(otupath,"/heapmap_boplot/",sep = "")
dir.create(heatpath)

#--注意map文件中一定要有ID列
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\Microheatmap.R",encoding = "utf-8")

#提取丰度最高的前20个OTU做展示
# ps_rela  = ps %>% scale_micro(method = "TMM")
# j = 2
# rank.names(ps)[j]

map = phyloseq::sample_data(ps)
map$ID = row.names(map)
phyloseq::sample_data(ps) = map
for (j in 2:6) {
  ps_tem = ps %>% 
    ggClusterNet::scale_micro(method = "TMM") %>%
    ggClusterNet::tax_glom_wt(ranks = j) 
  rowSD = function(x){
    apply(x,1, sd)
  }
  
  rowCV = function(x){
    rowSD(x)/rowMeans(x)
  }
  
  id <- ps %>% 
    ggClusterNet::scale_micro(method = "TMM") %>%
    ggClusterNet::tax_glom_wt(ranks = j) %>%
    ggClusterNet::filter_OTU_ps(100) %>%
    ggClusterNet::vegan_otu() %>%
    t() %>% as.data.frame() %>%rowCV %>%
    sort(decreasing = TRUE) %>%
    head(20) %>%
    names()
  
  result <- Microheatmap(ps_rela = ps_tem,id = id ,col_cluster = FALSE)
  
  p1 <- result[[1]] 
  p1
  # p1 +  scale_fill_gradientn(colours =colorRampPalette(RColorBrewer::brewer.pal(11,"Set3"))(60))
  p2 <- result[[2]]
  p2
  filename = paste(heatpath,"/",phyloseq::rank.names(ps)[j],"Topggheatmap.pdf",sep = "")
  ggsave(filename,p1,width = 14,height = (6 + heatnum/10))
  
  filename = paste(heatpath,phyloseq::rank.names(ps)[j],"Topggbubble.pdf",sep = "")
  ggsave(filename,p2,width = 14,height = (6 + heatnum/10))
  
  filename = paste(heatpath,"/",phyloseq::rank.names(ps)[j],"Topggheatmap.png",sep = "")
  ggsave(filename,p1,width = 14,height = (6 + heatnum/10))
  
  filename = paste(heatpath,phyloseq::rank.names(ps)[j],"Topggbubble.png",sep = "")
  ggsave(filename,p2,width = 14,height = (6 + heatnum/10))
  
  # filename = paste(heatpath,"/",rank.names(ps)[j],"/","Topggheatmap.jpg",sep = "")
  # ggsave(filename,p1,width = 14,height = (6 + heatnum/10))
  # filename = paste(heatpath,"/",rank.names(ps)[j],"/","Topggbubble.jpg",sep = "")
  # ggsave(filename,p2,width = 14,height = (6 + heatnum/10))
}


#----lefse--py-在win子系统中可以运行#------------
if (T) {
  source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\lefse_py_pre.R",encoding = "utf-8")
  lefpath = paste(otupath,"/lefse_py/",sep = "")
  dir.create(lefpath)
  
  # library(phyloseq)
  # library(EasyMicrobiome)
  # library("tidyverse")
  
  tablefse <- lefse_py_pre(ps = ps,taxGlomRank = "Genus",filter = 250)
  dim(tablefse)
  filename = paste(lefpath,"/LEFSE_to_run_G_level.txt",sep = "")
  write.table(tablefse,filename,append = F, quote = F,col.names= F,sep = "\t")
  
  # #文件预处理
  # format_input.py LEFSE_to_run_G_level.txt pri_lefse.in -c 1 -u 2 -o 1000000
  # 注意这里 –c用来指定分组信息-u 1指定样品信息
  # 文件分析,这里-l设置LDA阈值，默认为2，我们使用4 会更加严格
  # ~/src/nsegata-lefse/run_lefse.py pri_lefse.in pri_lefse_2.res  -l 2
  # #柱状图绘制
  # plot_res.py pri_lefse_2.res lefse_barplot.pdf --format pdf
  # #树状图绘制
  # plot_cladogram.py pri_lefse_2.res lefse_tree.pdf --format pdf
  # #做每个差异的柱状图
  # mkdir biomarkers_raw_images
  # plot_features.py pri_lefse.in pri_lefse_2.res biomarkers_raw_images/
  
}

# #文件预处理
# conda activate qiime1
# lefse-format_input.py LEFSE_to_run_G_level.txt pri_lefse.in -c 1 -u 2 -o 1000000
# run_lefse.py pri_lefse.in pri_lefse_2.res  -l 2
# lefse-plot_res.py pri_lefse_2.res lefse_barplot.pdf --format pdf
# lefse-plot_cladogram.py pri_lefse_2.res lefse_tree.pdf --format pdf
# mkdir biomarkers_raw_images
# lefse-plot_features.py pri_lefse.in pri_lefse_2.res biomarkers_raw_images/


lefsepath = paste(otupath,"/lefse_R_plot/",sep = "")
dir.create(lefsepath)

# library(ggpubr)
# library(patchwork)
# library(MicrobiotaProcess)
library(ggtree)
source("E:/Shared_Folder/Function_local/R_function/micro/R_lefse_SAV.R",encoding = "utf-8")
# source("../micro/R_lefse_SAV.R",encoding = "utf-8")


p1 <- p_base(ps,Top = 100)
p1$data

tablda = LDA_Micro(ps = ps,
                   Top = 100,
                   p.lvl = 0.05,
                   lda.lvl = 1,
                   seed = 11, 
                   adjust.p = F)

p <- lefse_bar(taxtree = tablda[[2]])

tem = tablda[[2]]


FileName <- paste(lefsepath,"bar_lefse", ".pdf", sep = "")
ggsave(FileName, p, width = 25, height =dim(tem)/4,limitsize = FALSE)
FileName <- paste(lefsepath,"bar_lefse", ".png", sep = "")
ggsave(FileName, p, width = 60, height =dim(tem)/4,dpi = 72,limitsize = FALSE)

res = tablda[[2]]
FileName <- paste(lefsepath,"tree_lefse_data", ".csv", sep = "")
write.csv(res,FileName,quote = F)

# 注释树
library(patchwork)
p2 <- clade.anno_wt(p1, tablda[[1]], alpha=0.3, anno.depth = 7)
FileName <- paste(lefsepath,"tree_lefse", ".pdf", sep = "")
ggsave(FileName,p2,width = 15,height = 10)

FileName <- paste(lefsepath,"tree_lefse", ".png", sep = "")
ggsave(FileName,p2,width = 15,height = 10,dpi = 72)


source("E:/Shared_Folder/Function_local/R_function/micro/R_lefse_allRank.R",encoding = "utf-8")

lefsepath = paste(otupath,"/lefse_R_plot/",sep = "")
dir.create(lefsepath)


for (j in 2:6) {
  
  p1 <- p_base(ps,Top = 200,ranks =j)
  p1
  
  tablda = LDA_Micro(ps = ps,
                     Top = 200,
                     ranks = j,
                     p.lvl = 0.05,
                     lda.lvl = 2,
                     seed = 11,
                     adjust.p = F)
  
  p2 <- clade.anno_wt(p1, tablda[[1]], alpha=0.3,anno.depth = 2)
  p2
  FileName <- paste(lefsepath,j,"_tree_lefse", ".pdf", sep = "")
  ggsave(FileName,p2,width = 15,height = 10)
  FileName <- paste(lefsepath,j,"_tree_lefse", ".png", sep = "")
  ggsave(FileName,p2,width = 15,height = 10)
  p <- lefse_bar(taxtree = tablda[[2]])
  FileName <- paste(lefsepath,j,"_bar_lefse", ".pdf", sep = "")
  ggsave(FileName, p, width = 15, height =9)
  
  FileName <- paste(lefsepath,j,"_bar_lefse", ".png", sep = "")
  ggsave(FileName, p, width = 15, height =9)
  
  res = tablda[[2]]
  FileName <- paste(lefsepath,j,"_tree_lefse_data", ".csv", sep = "")
  write.csv(res,FileName,quote = F)
}


#--机器学习#------
matpath = paste(otupath,"/Machine_learing/",sep = "")
dir.create(matpath )


source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\MicroMachine_learning.R")

# library(randomForest)
# library(caret)
# library(ROCR) ##用于计算ROC
# library(e1071)

if (ROC ){
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
  

}


mapping = as.data.frame(phyloseq::sample_data(ps))
#--随机森林全套-如果圈图尚未显示前面几个，就设定max大一点
result = MicroRF(ps = ps_Rlefse,
                 group  = "Group",
                 optimal = optimal,rfcv = rfcv,nrfcvnum = 5,
                 min = -1,max = 5)
#火柴图展示前二十个重要的OTU
p <- result[[1]] + 
  mytheme1
p


filename = paste(matpath,"/randonforest_loading.pdf",sep = "")
ggsave(filename,p,width = 8,height = optimal/2)
filename = paste(matpath,"/randonforest_loading.jpg",sep = "")
ggsave(filename,p,width = 8,height = optimal/2)

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


if (rfcv){
  # 展示交叉验证结果
  p <- result[[3]]
  filename = paste(matpath,"/randonforest_cross_check.pdf",sep = "")
  ggsave(filename,p,width = 8,height = 12)
  data <- result[[4]]
  filename = paste(matpath,"/randomforest_cross_data.csv",sep = "")
  write.csv(data,filename,quote = F)
}

data <- result[[5]]
filename = paste(matpath,"/randomforest_data.csv",sep = "")
write.csv(data,filename,quote = F)


#--网络分析1#-----------
# source("E:\\Shared_Folder\\Function_local\\R_function\\my_R_packages\\ggClusterNet\\R\\networkplot.R")

netpath = paste(otupath,"/network2/",sep = "")
dir.create(netpath)

library(igraph)
library(sna)
library(phyloseq)
library(ggClusterNet)

result = ggClusterNet::network.2(ps = ps, 
                   N = 500,
                   big = TRUE,
                   maxnode = 5,
                   select_layout = TRUE,
                   layout_net = "model_maptree2",
                   r.threshold=0.6,
                   p.threshold=0.05,
                   label = FALSE,
                   path = netpath,
                   zipi = F,
                   ncol = gnum,
                   nrow = 1,
                   # method = "sparcc",
                   fill = "Phylum"
)


# 全部样本的网络比对
p4_1 = result[[1]] + mytheme1
# 全部样本网络参数比对
data = result[[2]]
plotname1 = paste(netpath,"/network_all.pdf",sep = "")
ggsave(plotname1, p4_1,width = 6*gnum,height = 6,limitsize = FALSE)
# plotname1 = paste(netpath,"/network_all.jpg",sep = "")
# ggsave(plotname1, p4_1,width = 16*gnum,height = 16)
tablename <- paste(netpath,"/co-occurrence_Grobel_net",".csv",sep = "")
write.csv(data,tablename)
# 全部样本的网络比对
p4_2 = result[[3]] + 
  scale_fill_brewer(palette = "Paired") +
  mytheme1
plotname1 = paste(netpath,"/network_all_cover.pdf",sep = "")
ggsave(plotname1, p4_2,width = 10*gnum,height = 10,limitsize = FALSE)



# #--网络分析2#--------
# netpath = paste(otupath,"/network3/",sep = "")
# dir.create(netpath)
# data(ps)
# ps
# 
# 
# map = sample_data(ps)
# map$Group = "A"
# sample_data(ps) = map
# 
# 
# result = network.3(ps = ps, 
#                    N = 500,
#                    big = TRUE,
#                    maxnode = 5,
#                    select_layout = TRUE,
#                    layout_net = "model_maptree2",
#                    r.threshold=0.6,
#                    p.threshold=0.05,
#                    label = FALSE,
#                    path = netpath,
#                    keystone = T,
#                    methodkyestone = "zipi",
#                    ncol = gnum,
#                    nrow = 1,
#                    # method = "sparcc",
#                    fill = "Phylum",
#                    envRDA = env1
# )
# 
# # 全部样本的网络比对
# p4_1 = result[[1]] + mytheme1
# # 全部样本网络参数比对
# data = result[[2]]
# plotname1 = paste(netpath,"/network_all.pdf",sep = "")
# ggsave(plotname1, p4_1,width = 6*gnum,height = 6,limitsize = FALSE)
# # plotname1 = paste(netpath,"/network_all.jpg",sep = "")
# # ggsave(plotname1, p4_1,width = 16*gnum,height = 16)
# tablename <- paste(netpath,"/co-occurrence_Grobel_net",".csv",sep = "")
# write.csv(data,tablename)
# # 全部样本的网络比对
# p4_2 = result[[3]] + 
#   scale_fill_brewer(palette = "Paired") +
#   mytheme1
# plotname1 = paste(netpath,"/network_all_cover.pdf",sep = "")
# ggsave(plotname1, p4_2,width = 10*gnum,height = 10,limitsize = FALSE)


#------网络分析3#-----------

netpath = paste(otupath,"/network_igraph/",sep = "")
dir.create(netpath)
# map = sample_data(ps)
# map$Group = "one"
# sample_data(ps16s) = map
library(igraph)
library(ggnewscale)

result = network.i(ps =  ps,
                   N = 500,
                   r.threshold=0.8,
                   big = T,
                   select_layout = T,
                   method = "pearson",
                   scale = FALSE,
                   layout_net = "model_igraph2",
                   p.threshold=0.05,
                   label = FALSE,
                   path = netpath ,
                   ncol = gnum,
                   nrow = 1,
                   zipi = F,
                   order = NULL
)

p1 = result[[1]]

dat = result[[2]]
tablename <- paste(netpath,"/co-occurrence_Grobel_net",".csv",sep = "")
write.csv(dat,tablename)
p = result[[5]]

plotname1 = paste(netpath,"/network_all.pdf",sep = "")
ggsave(plotname1, p1,width = 3*gnum,height = 2.5,limitsize = FALSE)

plotname1 = paste(netpath,"/network_all2.pdf",sep = "")
ggsave(plotname1, p,width = 6*gnum,height = 6,limitsize = FALSE)


#--网络稳定性分析#--------

library(ggClusterNet)
library(phyloseq)
library(tidyverse)
library(igraph)
library(tidyfst)

netpath = paste(otupath,"/network_stab/",sep = "")
dir.create(netpath)

res = module.compare.m(
  ps = ps,
  Top = 200,
  degree = TRUE,
  zipi = FALSE,
  r.threshold= 0.8,
  p.threshold=0.05,
  method = "spearman",
  padj = F,
  n = 3)

p = res[[1]]
p

dat = res[[2]]
head(dat)
dat2 = res[[3]]
head(dat2)
dat2$m1 = dat2$module1 %>% strsplit("model") %>%
  sapply(`[`, 1)

dat2$m2 = dat2$module2 %>% strsplit("model") %>%
  sapply(`[`, 1)
dat2$cross = paste(dat2$m1,dat2$m2,sep = "_Vs_")

head(dat2)

p2 = ggplot(dat2) + geom_bar(aes(x = cross,fill = cross)) +
  labs(x = "",
       y = "numbers.of.similar.modules"
  )+ theme_classic()

p2

#--发现分组1和分组3网络更相似一些
FileName <- paste(netpath,"module.compare.groups.pdf", sep = "")
ggsave(FileName, p, width = 10, height = 10)

FileName <- paste(netpath,"numbers.of.similar.modules.pdf", sep = "")
ggsave(FileName, p2, width = 8, height = 8)

FileName <- paste(netpath,"module.otu.csv", sep = "")
write.csv(dat,FileName, quote = F)

FileName <- paste(netpath,"module.compare.groups.csv", sep = "")
write.csv(dat2,FileName, quote = F)


#--鲁棒性

#--随即取出任意比例节点-网络鲁棒性#---------
res = Robustness.Random.removal(ps = ps,
                                Top = 500,
                                r.threshold= 0.8,
                                p.threshold=0.05,
                                method = "spearman"
)
p = res[[1]]
p
dat = res[[2]]
head(dat)


# dir.create("./Robustness_Random_removal/")
path = paste(netpath,"/Robustness_Random_removal/",sep = "")
fs::dir_create(path)
write.csv(dat,
          paste(path,"random_removal_network.csv",sep = ""))
ggsave(paste(path,"random_removal_network.pdf",sep = ""),  p,width = 8,height = 4)


res= Robustness.Targeted.removal(ps = ps,
                                 Top = 500,
                                 degree = TRUE,
                                 zipi = FALSE,
                                 r.threshold= 0.8,
                                 p.threshold=0.05,
                                 method = "spearman")

p = res[[1]]
p
dat = res[[2]]

path = paste(netpath,"/Robustness_Targeted_removal/",sep = "")
fs::dir_create(path)
write.csv(dat,
          paste(path,"Robustness_Targeted_removal_network.csv",sep = ""))
ggsave(paste(path,"Robustness_Targeted_removal_network.pdf",sep = ""),  p,width = 8,height = 4)


#---网络易损性#------

res = Vulnerability.micro(ps = ps,
                          Top = 500,
                          degree = TRUE,
                          zipi = FALSE,
                          r.threshold= 0.8,
                          p.threshold=0.05,
                          method = "spearman")

p = res[[1]] + theme_bw()
p
dat = res[[2]]

path = paste(netpath,"/Vulnerability/",sep = "")
fs::dir_create(path)

write.csv(dat,
          paste(path,"Vulnerability_network.csv",sep = ""))
ggsave(paste(path,"Vulnerability_network.pdf",sep = ""),  p,width = 4,height = 4)


#--计算负相关的比例#----
res = negative.correlation.ratio(ps = ps,
                                 Top = 500,
                                 degree = TRUE,
                                 zipi = FALSE,
                                 r.threshold= 0.8,
                                 p.threshold=0.05,
                                 method = "spearman")

p = res[[1]]
p
dat = res[[2]]
head(dat)


path = paste(netpath,"/Vulnerability/",sep = "")
fs::dir_create(path)

write.csv(dat,
          paste(path,"Vnegative.correlation.ratio_network.csv",sep = ""))
ggsave(paste(path,"negative.correlation.ratio_network.pdf",sep = ""),  p,width = 4,height = 4)


#-网络抗毁性#-------
# library(tidyfst)
library("pulsar")
# library(ggClusterNet)
# library(phyloseq)
# library(tidyverse)
res = natural.con.microp (
  ps = ps,
  Top = 500,
  r.threshold= 0.6,
  p.threshold=0.05,
  method = "spearman",
  norm = F,
  end = 150,# 小于网络包含的节点数量
  start = 0,
  con.method = "pulsar"
)

p = res[[1]]
p
dat  = res[[2]]

path = paste(netpath,"/Natural_connectivity/",sep = "")
fs::dir_create(path)

write.csv(dat,
          paste(path,"/Natural_connectivity.csv",sep = ""))
ggsave(paste(path,"/Natural_connectivity.pdf",sep = ""),  p,width = 5,height = 4)



#--网络模块化分析#-------
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/alpha-diversity.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/barMainplot.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/net.model.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/net.property_env.R")


path= paste(otupath,"network_model/",sep = "")
fs::dir_create(path)

library(igraph)

#--这个函数功能如下#---------
#1按照分组计算网络-按照模块可视化网络
#2选择模块单独可视化
#3模块微生物组成
#4模块微生物alpha多样性
#5模块丰度

net.model(
  path = path,
  ps = ps,
  j = "Family",
  Top = 500,
  r.threshold= 0.8,
  p.threshold=0.05,
  select.mod = c("model_1","model_2","model_3")
)



#--网络模块化分析2-模块特征向量#-------
# 特点是分组计算模块特征向量并保存
#  其他都一样

id = sample_data(ps)$Group %>% unique()
id
i = 1
netpath = paste(otupath,"/network3_MEs/",sep = "")
dir.create(netpath)
netpath;library(ggClusterNet)
library(igraph)
for (i in 1:length(id)) {
  ps.1 = phyloseq::subset_samples(
    ps,Group %in% c(id[i])
  )
  
  result = network.2(ps = ps.1, N = 500,
                     big = TRUE,
                     select_layout = TRUE,
                     layout_net = "model_maptree",
                     r.threshold=0.8,
                     p.threshold=0.05,
                     label = FALSE,
                     path = netpath,
                     zipi = F,
                     ncol = 1,
                     nrow = 1,
                     # method = "sparcc",
                     fill = "Phylum"
  )
  
  # 节点的模块化信息的输出 group列为模块化信息
  tem <- ggClusterNet::model_maptree(cor =result[[4]],
                                     method = "cluster_fast_greedy",
                                     seed = 12
  )
  node_model = tem[[2]]
  head(node_model)
  tablename <- paste(netpath,"/node_model",".csv",sep = "")
  write.csv(node_model,tablename)
  head(node_model)
  otu = ps.1 %>% 
    filter_OTU_ps(500) %>%
    vegan_otu() %>%
    as.data.frame()
  node_model = node_model[match(colnames(otu),node_model$ID),]
  
  MEList = WGCNA::moduleEigengenes(otu, colors = node_model$group)
  MEs = MEList$eigengenes %>% as.data.frame()
  tablename <- paste(netpath,"/",id[i],"node_characteristic_variables",".csv",sep = "")
  write.csv(MEs,tablename)
  
}

detach("package:sna")
detach("package:igraph")
# detach("package:phyloseq")

#--分组之间距离比较和可视化#-------
#--一下调整必须在map文件中包含两列，
# 一列是时间梯度，使用数字表示，不能带有特殊表述，一列是treat，就是分组信息，这些分组信息必须是有时间梯度的。
if (6 >gnum & gnum > 2) {
  
  group = "Group"
  map = as.data.frame(sample_data(ps))
  alppath = paste(otupath,"/distance/",sep = "")
  dir.create(alppath)
  gro = map[,group] %>% unique()
  colnames(gro) = "group"
  conbgroup = combn(gro$group,2)
  # 计算包括终点均值的所有样品bray距离
  bray_curtis = vegan::vegdist(vegan_otu(ps), method = "bray")
  bray_curtis = as.matrix(bray_curtis)
  
  for (i in 1:dim(conbgroup)[2]) {
    a = conbgroup[,i]
    map = as.data.frame(sample_data(ps))
    head(map)
    
    chose1 = map[as.matrix(map[,group]) %>% as.vector() == a[1],] %>% row.names()
    chose2 = map[as.matrix(map[,group]) %>% as.vector() == a[2],] %>% row.names()
    
    dat = data.frame(group = paste(a[1],a[2],sep = "_VS_"), Distance =bray_curtis[chose1,chose2] %>% as.dist() %>% as.vector() )
    head(dat)
    
    if (i == 1) {
      table = dat
    }
    
    if (i != 1) {
      table = rbind(table,dat)
    }
  }
  
  head(table)
  table$id = 1:dim(table)[1]
  data <- table %>% dplyr::select(id,everything())
  
  
  result = EasyStat::MuiKwWlx(data = data,num = c(3))
  FileName <- paste(alppath,"/distance_label.csv", sep = "")
  write.csv(result,FileName,sep = "")
  FileName <- paste(alppath,"/distance_index.csv", sep = "")
  write.csv(data,FileName,sep = "")
  
  result1 = EasyStat::FacetMuiPlotresultBox(data = data,num = c(3),result = result,sig_show ="abc",ncol = 1 )
  p1_1 = result1[[1]] + 
    # scale_x_discrete(limits = axis_order) + 
    mytheme2 +
    guides(fill = guide_legend(title = NULL)) +
    scale_fill_manual(values = colset4)
  p1_1
  res = EasyStat::FacetMuiPlotresultBar(data = data,num = c(3),result = result,sig_show ="abc",ncol = 1)
  p1_2 = res[[1]]+ 
    # scale_x_discrete(limits = axis_order) + 
    guides(color = FALSE) +
    mytheme2+ 
    guides(fill = guide_legend(title = NULL))+
    scale_fill_manual(values = colset4)
  p1_2
  res = EasyStat::FacetMuiPlotReBoxBar(data = data,num = c(3),result = result,sig_show ="abc",ncol = 1)
  p1_3 = res[[1]]+ 
    # scale_x_discrete(limits = axis_order) + 
    mytheme2 + 
    guides(fill = guide_legend(title = NULL))+
    scale_fill_manual(values = colset4)
  p1_3
  FileName <- paste(alppath,"distance_box", ".pdf", sep = "")
  ggsave(FileName, p1_1, width = ((4+ gnum) ), height =8,limitsize = FALSE)
  
  FileName <- paste(alppath,"distance_bar", ".pdf", sep = "")
  ggsave(FileName, p1_2, width = ((4+gnum) ), height = 8,limitsize = FALSE)
  
  FileName <- paste(alppath,"distance_boxbar", ".pdf", sep = "")
  ggsave(FileName, p1_3, width = ((4+gnum) ), height = 8,limitsize = FALSE)
  
  FileName <- paste(alppath,"distance_box", ".jpg", sep = "")
  ggsave(FileName, p1_1, width = (( 4+gnum) ), height =8,limitsize = FALSE)
  
  FileName <- paste(alppath,"distance_bar", ".jpg", sep = "")
  ggsave(FileName, p1_2, width = ((4+ gnum) ), height = 8,limitsize = FALSE)
  
  FileName <- paste(alppath,"distance_boxbar", ".jpg", sep = "")
  ggsave(FileName, p1_3, width = ((4+ gnum) ), height = 8,limitsize = FALSE)
  
}


#--maptree#-------
maptpath = paste(otupath,"/maptree/",sep = "")
dir.create(maptpath)
#--OTU 水平的maptree
library(ggClusterNet)
library(phyloseq)
library(tidyverse)
library(ggraph)
library(data.tree)
library(igraph)

source("E:\\Shared_Folder\\Function_local\\R_function\\micro/maptree_micro.R")

#--如果数据中存在两个king，则无法运行，所以，这里去掉古菌OTU。
ps.bac <- ps %>% subset_taxa.wt("Kingdom", id) 
ps.bac

p = micro_maptree(ps = ps.bac,
                  Top = 100,
                  labtab =  NULL,
                  seed = 11)
p

FileName <- paste(maptpath,"maptree", ".pdf", sep = "")
ggsave(FileName, p,width = 18,height = 16)


FileName <- paste(maptpath,"maptree", ".png", sep = "")
ggsave(FileName, p,width = 18,height = 16,dpi = 72)



#------FEAST溯源分析#-------
betapath = paste(otupath,"/Feast/",sep = "")
dir.create(betapath)


# source("E:\\Shared_Folder\\Function_local\\R_function\\Liu_project\\amplicon-master\\R\\开发花絮\\FEAST-master\\FEAST_src\\src.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\FEAST.R",encoding = "UTF-8")


result = FEAST(ps = ps,
               group = "Group",
               sinkG = "Group1",
               sourceG = c("Group2","Group3"),
               path = "E:/Shared_Folder/Function_local/R_function/micro/" # 注意按照自己设定的路径进行修改
               )

# result
p <- Plot_FEAST(data = result)
p
p2 = MuiPlot_FEAST(data = result)

FileName <- paste(betapath,"Feast_data.csv", sep = "")
write.csv(result,FileName, quote = F)

FileName <- paste(betapath,"Feast_Group.pdf", sep = "")
ggsave(FileName, p, width = 5, height = 5)

p2 = MuiPlot_FEAST(data = result)
p2
FileName <- paste(betapath,"Feast_sample.pdf", sep = "")
ggsave(FileName, p2, width = 15, height = 15)



#-Tax4Fun2功能预测#------
funcpath = paste(otupath,"/Tax4Fun2/",sep = "")
dir.create(funcpath)

path_to_reference_data = "C:/public/Tax4Fun2/Tax4Fun2_ReferenceData_v2"
otudir = funcpath
#加载
library(Tax4Fun2)
#物种注释
#指定 OTU 代表序列、Tax4Fun2 库的位置、参考数据库版本、序列比对（blastn）线程数等
runRefBlast(path_to_otus = './data/otu.fa', 
            path_to_reference_data = path_to_reference_data, 
            path_to_temp_folder = otudir, database_mode = 'Ref100NR', 
            use_force = TRUE, num_threads = 4)


#预测群落功能
#指定 OTU 丰度表、Tax4Fun2 库的位置、参考数据库版本、上步的物种注释结果路径等
makeFunctionalPrediction(path_to_otu_table = './data/otu.txt',
                         path_to_reference_data = path_to_reference_data, 
                         path_to_temp_folder = otudir, 
                         database_mode = 'Ref100NR', 
                         normalize_by_copy_number = TRUE,
                         min_identity_to_reference = 0.97, 
                         normalize_pathways = FALSE)

library(ggClusterNet)
library(EasyStat)





#----------Funguild#----------
Fungi = F
if (Fungi == T ) {
  # 加载R包
  library(microeco)
  # 加载ggplot2绘图包并设置样式
  library(ggplot2)
  library("WGCNA")
  library(tidyverse)
  library(ggtree)
  library("SpiecEasi")
  library(ggClusterNet)
  library(phyloseq)
  library(magrittr)
  p_list = c("ggplot2", "BiocManager", "devtools","picante", "GUniFrac", "ggalluvial", "rgexf")
  for(p in p_list){if (!requireNamespace(p)){install.packages(p)}
    library(p, character.only = T, quietly = T, warn.conflicts = F)}
  
  # ps = readRDS("./data/dataNEW/ps_ITS.rds")
  # ps = ps
  # 导入内置真菌数据
  # data(sample_info_ITS)
  # data(otu_table_ITS)
  # data(taxonomy_table_ITS)
  
  otu = ps %>% vegan_otu() %>%
    t() %>%
    as.data.frame()
  
  tax = ps %>% vegan_tax() %>%
    as.data.frame()
  # 构建分析对象
  dataset = microtable$new(sample_table = sample_data(ps), otu_table = otu, tax_table = tax)
  # 筛选真菌
  dataset$tax_table %<>% subset(Kingdom == "Fungi")
  
  # 功能预测
  t2 = trans_func$new(dataset)
  # 计算物种的功能
  t2$cal_spe_func()
  data = t2$res_spe_func_raw_funguild
  
  fugipath = paste(otupath,"/funguild",sep = "")
  dir.create(fugipath)
  # dir.create("./result_and_plot/Base_diversity_ITS/OTU_220921//funguild")
  write.csv(data,paste(fugipath ,"/funguild.csv",sep = ""))
}

# 宏基因组测序开票
78*430#33540

10 *150

#----FAPROTAX#---

Bca = T


if (Bca ==T ) {
  # 加载R包
  library(microeco)
  # 加载ggplot2绘图包并设置样式
  library(ggplot2)
  library("WGCNA")
  library(tidyverse)
  library(ggtree)
  library("SpiecEasi")
  library(ggClusterNet)
  library(phyloseq)
  library(magrittr)
  p_list = c("ggplot2", "BiocManager", "devtools","picante", "GUniFrac", 
             "ggalluvial", "rgexf")
  # for(p in p_list){if (!requireNamespace(p)){install.packages(p)}
  #   library(p, character.only = T, quietly = T, warn.conflicts = F)}
  
  # ps = readRDS("./data/dataNEW/ps_16s.rds")
  ps = ps %>%
    filter_OTU_ps(500)
  
  
  otu = ps %>% vegan_otu() %>%
    t() %>%
    as.data.frame()
  
  tax = ps %>% vegan_tax() %>%
    as.data.frame()
  # 构建分析对象
  dataset = microtable$new(sample_table = sample_data(ps), otu_table = otu, tax_table = tax)
  
  t2 = trans_func$new(dataset)
  t2$cal_spe_func()
  t2$res_spe_func[1:5, 1:6]
  
  data = t2$res_spe_func
  
  data = data[rowSums(data)> 0,]
  
  betapath = paste(otupath,"/FAPROTAX/",sep = "")
  dir.create(betapath)
  
  
  # dir.create("./result_and_plot/Base_diversity_16s//OTU_220921//FAPROTAX")
  
  write.csv(data,paste(betapath,"/FAPROTAX.csv",sep = ""))
  # 查看功能 分组列表
  t2$func_group_list
  # 查看某一类
  t2$show_prok_func("methanotrophy")

}




