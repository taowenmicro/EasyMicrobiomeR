
envRDA = env1
env = env1 %>% rownames_to_column("ID")
head(env)



if (is.na(match("Fungi",id))) {
  res1path <- "./result_and_plot/Micro_and_other_index_16s_220922/"

} else if(is.na(match("Bacteria",id))) {
  res1path <- "./result_and_plot/Micro_and_other_index_ITS/"
}


dir.create(res1path)

res1path

#------环境因子差异分析出图#------
envpath = paste(res1path,"/env_difference_plot/",sep = "")
dir.create(envpath)

map = phyloseq::sample_data(ps)

data = map %>% as.tibble()%>% 
  dplyr::select(ID,Group) %>%
  dplyr::inner_join(env) %>%
  as.data.frame() %>%
  dplyr::rename(group = Group)

head(data)


result = EasyStat::MuiaovMcomper2(data = data,num = c(3:ncol(data)))

FileName <- paste(envpath,"/env_different_label.csv", sep = "")
write.csv(result,FileName,sep = "")
FileName <- paste(envpath,"/env_group_index.csv", sep = "")
write.csv(data,FileName,sep = "")

result1 = EasyStat::FacetMuiPlotresultBox(data = data,
                                          num = c(3:6),
                                          result = result,
                                          sig_show ="abc",ncol = 5 )
p1_1 = result1[[1]] + 
  ggplot2::scale_x_discrete(limits = axis_order) + 
  mytheme1 +
  ggplot2::guides(fill = guide_legend(title = NULL)) +
  ggplot2::scale_fill_manual(values = colset1)
p1_1

p1_1 = result1[[2]] %>% ggplot(aes(x=group , y=dd )) + 
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
p1_1

res = EasyStat::FacetMuiPlotresultBar(data = data,num = c(3:ncol(data)),result = result,sig_show ="abc",ncol = 5)
p1_2 = res[[1]]+ scale_x_discrete(limits = axis_order) + guides(color = FALSE) +
  mytheme1+ 
  guides(fill = guide_legend(title = NULL))+
  scale_fill_manual(values = colset1)
p1_2

res = EasyStat::FacetMuiPlotReBoxBar(data = data,num = c(3:ncol(data)),result = result,sig_show ="abc",ncol = 5)
p1_3 = res[[1]]+ scale_x_discrete(limits = axis_order) + 
  mytheme1 + 
  guides(fill = guide_legend(title = NULL))+
  scale_fill_manual(values = colset1)
p1_3


FileName <- paste(envpath,"env_Facet_box", ".pdf", sep = "")
ggsave(FileName, p1_1, width = ((1 + gnum) * 3), height =4*gnum,limitsize = FALSE)

FileName <- paste(envpath,"env_Facet_bar", ".pdf", sep = "")
ggsave(FileName, p1_2, width = ((1 + gnum) * 3), height = 4*gnum,limitsize = FALSE)

FileName <- paste(envpath,"env_Facet_boxbar", ".pdf", sep = "")
ggsave(FileName, p1_3, width = ((1 + gnum) * 3), height = 4*gnum,limitsize = FALSE)

FileName <- paste(envpath,"env_Facet_box", ".jpg", sep = "")
ggsave(FileName, p1_1, width = ((1 + gnum) * 3), height =4*gnum,limitsize = FALSE)

FileName <- paste(envpath,"env_Facet_bar", ".jpg", sep = "")
ggsave(FileName, p1_2, width = ((1 + gnum) * 3), height = 4*gnum,limitsize = FALSE)

FileName <- paste(envpath,"env_Facet_boxbar", ".jpg", sep = "")
ggsave(FileName, p1_3, width = ((1 + gnum) * 3), height = 4*gnum,limitsize = FALSE)



#----------环境因子相关分析#-------------
#---寻找共线性等内容

corpath = paste(res1path,"/env_self_cor/",sep = "")
dir.create(corpath)

ps_env = phyloseq::phyloseq(phyloseq::otu_table(as.matrix(envRDA),taxa_are_rows = F),
                            phyloseq::sample_data(ps)
                            
)



result = ggClusterNet::corMicro (ps = ps_env,
                                 N = 0,
                                 r.threshold=0.9,
                                 p.threshold=0.05
)

cor = result[[1]]
dim(cor)
ps_net = result[[3]]
otu_table = as.data.frame(t(ggClusterNet::vegan_otu(ps_net)))
# tax_table = as.data.frame(ggClusterNet::vegan_tax(ps_net))
library(igraph)
library(network)
library(sna)
netClu  = data.frame(ID = row.names(cor),group = "one")
head(netClu)
netClu$group = "one"
set.seed(12)
result2 = ggClusterNet::PolygonClusterG(cor = cor,node = netClu )
node = result2[[1]]

edge = ggClusterNet::edgeBuild(cor = cor,node =  node)
colnames(edge)[8] = "cor"


p1 <- ggplot() + geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2,color = cor ),
                              data = edge, size = 0.5,alpha = 0.6) +
  geom_point(aes(X1, X2),pch = 21, data = node, size = 3,fill = "#9ACD32") +
  ggrepel::geom_text_repel(aes(X1, X2,label = elements),pch = 21,data = node) +
  # geom_text(aes(X1, X2,label = elements),pch = 21, data = nodes) +
  scale_colour_manual(values = c("#377EB8","#E41A1C")) +
  scale_size(range = c(4, 14)) +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
  theme(panel.background = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.background = element_rect(colour = NA)) +
  theme(panel.background = element_rect(fill = "white",  colour = NA)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
p1


FileName <- paste(corpath,"env_self_cor", ".pdf", sep = "")
ggsave(FileName, p1, width = 10, height =9,limitsize = FALSE)

FileName <- paste(corpath,"env_self_cor", ".png", sep = "")
ggsave(FileName, p1, width = 10, height = 9,limitsize = FALSE)




#--微生物和环境因子共排序RDA/CCA#---------
RDApath = paste(res1path,"/RDA_CCA/",sep = "")
dir.create(RDApath)

source("E:\\Shared_Folder\\Function_local\\R_function\\micro/rda-cca.R")


result = RDA_CCA(ps = ps_Rlefse,
                 env = envRDA,
                 path = RDApath,
                 chose.env = F
                 )
#提取图片
p1 = result[[1]] + mytheme1 + scale_fill_manual(values = colset1)
p1
# 提取作图数据
dataplot = result[[2]]
# 提取带有标记的图片
p2 = result[[3]]+ mytheme1  + scale_fill_manual(values = colset1)
#提取理化提供群落关系的渐检验结果
aov = result[[4]]

# library("Cairo")

##保存
plotnamea = paste(RDApath,"/RDA_envnew.pdf",sep = "")
ggsave(plotnamea, p1, width = 8, height = 6)
plotnamea4 = paste(RDApath,"/RDA_envnew.jpg",sep = "")
ggsave(plotnamea4, p1, width = 8, height = 6)


filenamea = paste(RDApath,"dataplotnew.txt",sep = "")
write.table(dataplot ,file=filenamea,sep="\t",col.names=NA)

filenamea = paste(RDApath,"aovnew.txt",sep = "")
write.table(aov,file=filenamea,sep="\t",col.names=NA)

plotnamea = paste(RDApath,"/RDA_envlabelnew.pdf",sep = "")
ggsave(plotnamea, p2, width = 18, height = 12)#, device = cairo_pdf, family = "Song"
plotnamea4 = paste(RDApath,"/RDA_envlabelnew.png",sep = "")
ggsave(plotnamea4, p2, width = 18, height = 12)# , device = cairo_pdf, family = "Song"


result = RDA_CCA_explain_percent(ps = ps_Rlefse,
                                 env.dat = envRDA)
out = result[[1]]
wxp = result[[2]]

filenamea = paste(RDApath,"each_env_exp_percent.csv",sep = "")
write.csv(out,filenamea)
filenamea = paste(RDApath,"all_index_explain_percent.csv",sep = "")
write.csv(exp,filenamea)



#--层次分割RDA/CCA#---------
library(rdacca.hp)
library(vegan)
library(ggClusterNet)
library(tidyverse)
hpPath = paste(res1path,"/rdacca.hp/",sep = "")
dir.create(hpPath)

source("E:\\Shared_Folder\\Function_local\\R_function\\micro/rdacca.hp.micro.p.R")
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/rdacca.hp.micro.R")
rdacca.hp.micro(OTU = ps %>% filter_OTU_ps(500) %>%vegan_otu() %>% as.data.frame(),
                env = envRDA[,1:5],
                hpPath = hpPath,
                cca = FALSE
                )

rdacca.hp.micro.p(
  OTU = ps %>% filter_OTU_ps(200) %>%vegan_otu() %>% as.data.frame(),
  env = envRDA[,1:5],
  hpPath = hpPath,
  cca = FALSE,
  dbRDA = FALSE
)

#---基于dmm进行非限制性聚类分析-寻找响应群落的因子#---------
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/dmm.Micro.R")
library(dplyr)
library(microbiome)
library(DirichletMultinomial)
library(reshape2)
library(magrittr)
library(ggClusterNet)
ps.1 = ps %>% filter_OTU_ps(200)

dmmPath = paste(res1path,"/dmm/",sep = "")
dir.create(dmmPath)


dmm.Micro (ps.1 = ps.1,
           num = 3,
           path = dmmPath)



#--微生物群落和环境Science组合图表#---------
compath = paste(res1path,"/Conbine_env_plot/",sep = "")
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
                 numpoint = 21,
                 sig = TRUE,
                 siglabel = FALSE,
                 shownum = F,
                 curvature = 0.1,
                 numsymbol = NULL,
                 lacx = "left",
                 lacy = "bottom",
                 p.thur = 0.05,
                 onlysig = T
)
p0

p0 <- p0 + scale_colour_manual(values = c("blue","red")) + 
  scale_fill_distiller(palette="PRGn")
p0

FileName <- paste(compath,"Conbine_envplot", ".pdf", sep = "")
ggsave(FileName, p0,width = 15,height = 10)# , device = cairo_pdf, family = "Song"

FileName <- paste(compath,"Conbine_envplot", ".png", sep = "")
ggsave(FileName, p0,width = 15,height = 10)# , device = cairo_pdf, family = "Song"



#--拆分开来
#--- mantel test
rep = ggClusterNet::MetalTast (env.dat = envRDA, tabOTU = tabOTU1,distance = "bray",method = "metal")
repR = rep[c(-seq(from=1,to=dim(rep)[2],by=2)[-1])]
repP = rep[seq(from=1,to=dim(rep)[2],by=2)]
head( repR)
head( repP)
mat = cbind(repR,repP)
head(mat)

FileName <- paste(compath,"Conbine_envplot_data", ".csv", sep = "")
write.csv(mat,FileName)


result <- ggClusterNet::Miccorplot(data = envRDA,
                     method.cor = "spearman",
                     cor.p = 0.05,
                     x = F,
                     y = F,
                     diag = T,
                     lacx = "left",
                     lacy = "bottom",
                     sig = T,
                     siglabel = F,
                     shownum = F,
                     numpoint = 21,
                     numsymbol = NULL
)

p1 <- result[[1]]
p1 <- p1 + scale_fill_distiller(palette="PRGn")
p1
FileName <- paste(compath,"envCorplot", ".pdf", sep = "")
ggsave(FileName, p1,width = 15,height = 10)
FileName <- paste(compath,"envCorplot", ".png", sep = "")
ggsave(FileName, p1,width = 15,height = 10)




#--随机森林回归最重要的指标#--------
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/ramdom_env.plot.R")


rampath = paste(res1path,"/Random_env/",sep = "")
dir.create(rampath)

result <- ramdom_env.plot(ps  = ps,env = envRDA )
p <- result[[2]]
p
data = result[[1]]
head(data)
hit <- dim(envRDA)[2]
hit
FileName <- paste(rampath,"ranImportant", ".pdf", sep = "")
ggsave(FileName, p,width = 3,height =hit/5)


FileName <- paste(rampath,"ranImportant", ".csv", sep = "")
write.csv(data,FileName)



#--分组多轴随机森林#---------
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/ramdom_env_micro.heatplot.R")

rampath = paste(res1path,"/Random_env/",sep = "")
dir.create(rampath)
# ps = ps0
# map = sample_data(ps)
result <- ramdom_env_micro.heatplot( ps = ps, env = env,seed = 1)
p <- result[[1]] + mytheme2
p

dat = result[[2]]
wid = dim(env)[2]
map = phyloseq::sample_data(ps)
hei = length(unique(map$Group))
hei
FileName <- paste(rampath,"Randomforest_env_micro_heatmap", ".pdf", sep = "")
ggsave(FileName, p,width = 8  ,height = wid/3)

FileName <- paste(rampath,"Randomforest_env_micro_heatmap", ".jpg", sep = "")
ggsave(FileName, p,width = 8 ,height = wid/3)

FileName <- paste(rampath,"Random_env_micro_heatmap", ".csv", sep = "")
write.csv(dat,FileName)


#---高丰度微生物和环境因子的关系探索#----
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/cor_env_ggcorplot.R")

heatpath = paste(res1path,"/cor_env_heapmap_boplot/",sep = "")
dir.create(heatpath)
# library(sna)
# library(igraph)

#---提取门水平
jj = 6
tran = TRUE
Top = 20


for (jj in 2:6) {
  psdata <- ggClusterNet::tax_glom_wt(ps = ps,ranks = jj)
  
  if (tran) {
    psdata = psdata %>%
      phyloseq::transform_sample_counts(function(x) {x/sum(x)})
  }
  
  
  otu = phyloseq::otu_table(psdata)
  tax = phyloseq::tax_table(psdata)
  
  if (dim(otu)[1] < Top) {
    top10 <- otu[names(sort(rowSums(otu), decreasing = TRUE)[1:dim(otu)[1]]),]
    top10 = t(top10)
  } else {
    top10 <- otu[names(sort(rowSums(otu), decreasing = TRUE)[1:Top]),]
    top10 = t(top10)
  }
  head(top10)
  
  
  result = cor_env_ggcorplot(
    env1 = envRDA,
    env2 = top10,
    label =  T,
    col_cluster = T,
    row_cluster = T,
    method = "spearman",
    r.threshold= 0,
    p.threshold= 0
  )
  
  
  
  p1 <- result[[1]] 
  p1
  p2 <- result[[2]]
  p2
  
  hei = dim(env)[2]/2
  wid = Top
  
  filename = paste(heatpath,Top,"_",jj,"_","abundacne_OTU.csv",sep = "")
  write.csv(top10,filename)
  
  filename = paste(heatpath,"/",jj,"ggheatmap.pdf",sep = "")
  ggsave(filename,p1,width = Top/2,height = dim(env)[2]/2)
  filename = paste(heatpath,"/",jj,"ggbubble.pdf",sep = "")
  ggsave(filename,p2,width = Top/2,height = dim(env)[2]/2)
  
  filename = paste(heatpath,"/",jj,"ggheatmap.jpg",sep = "")
  ggsave(filename,p1,width = Top/2,height = dim(env)[2]/2)
  filename = paste(heatpath,"/",jj,"ggbubble.jpg",sep = "")
  ggsave(filename,p2,width = Top/2,height = dim(env)[2]/2)
  
}



#---变化福度大的微生物和环境因子的关系探索#----
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/cor_env_ggcorplot.R")

heatpath = paste(res1path,"/CVmicro_env_hea_boplot/",sep = "")
dir.create(heatpath)
# library(sna)
# library(igraph)

#---提取门水平

tran = TRUE
Top = 40


for (j in 2:6) {
  ps_tem = ps %>% 
    ggClusterNet::scale_micro(method = "rela") %>%
    ggClusterNet::tax_glom_wt(ranks = j) 
  
  rowSD = function(x){
    apply(x,1, sd)
  }
  
  rowCV = function(x){
    rowSD(x)/rowMeans(x)
  }
  
  id <- ps %>% 
    ggClusterNet::scale_micro(method = "rela") %>%
    ggClusterNet::tax_glom_wt(ranks = j) %>%
    # ggClusterNet::filter_OTU_ps(100) %>%
    ggClusterNet::vegan_otu() %>%
    t() %>% as.data.frame() %>% 
    rowCV %>%
    sort(decreasing = TRUE) %>%
    head(Top) %>%
    names()
  
  otu = phyloseq::otu_table(ps_tem)
  tax = phyloseq::tax_table(ps_tem)
  
  head(otu)
  
  data = otu[id,] %>% t() %>%
    as.data.frame()
  
  
  
  result = cor_env_ggcorplot(
    env1 = envRDA,
    env2 = data,
    label =  T,
    col_cluster = T,
    row_cluster = T,
    method = "spearman",
    r.threshold= 0,
    p.threshold= 0
  )
  
  
  
  p1 <- result[[1]] 
  p1
  p2 <- result[[2]]
  p2
  
  hei = dim(env)[2]/5
  wid = Top
  
  filename = paste(heatpath,Top,"_",j,"_","abundacne_OTU.csv",sep = "")
  write.csv(data,filename)
  
  filename = paste(heatpath,"/",j,"ggheatmap.pdf",sep = "")
  ggsave(filename,p1,width = Top/2,height = dim(env)[2]/5)
  filename = paste(heatpath,"/",jj,"ggbubble.pdf",sep = "")
  ggsave(filename,p2,width = Top/2,height = dim(env)[2]/5)
  filename = paste(heatpath,"/",j,"ggheatmap.jpg",sep = "")
  ggsave(filename,p1,width = Top/2,height = dim(env)[2]/5)
  filename = paste(heatpath,"/",j,"ggbubble.jpg",sep = "")
  ggsave(filename,p2,width = Top/2,height = dim(env)[2]/5)

  
}



#---基于网络模块和环境因子的关系分析#------
source("E:\\Shared_Folder\\Function_local\\R_function\\micro/cor_env_ggcorplot.R")

id = sample_data(ps)$Group %>% unique()
id
i = 1

for (i in 1:length(id)) {
  
  netpath = paste(res1path,"/network_env_hub_",id[i],"/",sep = "")
  dir.create(netpath)
  # ps.1 = phyloseq::subset_samples(
  #   ps,Group %in% c(id[i])
  # )
  
  ps.1 = ps %>% scale_micro("TMM") %>%
    subset_samples(
      Group %in% c(id[i])
    )
  
  library(ggClusterNet)
  library(igraph)
  #--计算微生物网络相关矩阵
  result= ggClusterNet::cor_Big_micro(ps = ps.1,
                                      N = 500,
                                      p.threshold = 0.05,
                                      r.threshold = 0.8,
                                      scale = FALSE)
  cor = result[[1]]
  
  #--拟合模块
  tem <- model_maptree(cor =result[[1]],
                       method = "cluster_fast_greedy",
                       seed = 12
  )
  node_model = tem[[2]]
  head(node_model)
  
  
  otu = ps.1 %>%
    phyloseq::subset_taxa(
      row.names(tax_table(ps ))%in%c(row.names(result[[1]]))) %>%
    vegan_otu() %>%
    as.data.frame()
  #-对其
  node_model = node_model[match(colnames(otu),node_model$ID),]
  
  MEList = WGCNA::moduleEigengenes(otu, colors = node_model$group)
  MEs = MEList$eigengenes
  
  tablename <- paste(netpath,"/model_network_feature_value_",id[i],".csv",sep = "")
  write.csv(MEs,tablename)
  
  #--寻找对于某个环境因子作用最大的模块
  source("E:\\Shared_Folder\\Function_local\\R_function\\micro/ramdom_Model_env.plot.R")
  
  env.1 = env %>% filter(ID %in% sample_names(ps.1))
  
  
  result <- ramdom_Model_env.plot(model = MEs,
                                  sink =env.1[,1:2] )
  p <- result[[2]]
  p
  data = result[[1]]
  head(data)
  hit <- dim(MEs)[2]
  hit
  FileName <- paste(netpath,"/ranImportant_Model_order_", id[i],".pdf", sep = "")
  ggsave(FileName, p,width = 6,height =hit/5)
  
  FileName <- paste(netpath,"/ranImportant_Model_order", id[i], ".csv", sep = "")
  write.csv(data,FileName)
  
  nGenes = ncol(otu)
  nSamples = nrow(otu)
  moduleTraitCor = cor(MEs, envRDA[sample_names(ps.1),], use = "p")
  moduleTraitPvalue = WGCNA::corPvalueStudent(moduleTraitCor, nSamples)
  
  #sizeGrWindow(10,6)
  # dim(MEs)[2]/2
  # dim(envRDA)[2]/2
  pdf(file=paste(netpath,"/","Module-env_relationships.pdf",sep = ""),width=dim(envRDA)[2]/2,height=dim(MEs)[2]/2)
  # Will display correlations and their p-values
  textMatrix = paste(signif(moduleTraitCor, 2), "\n(",
                     signif(moduleTraitPvalue, 1), ")", sep = "")
  
  dim(textMatrix) = dim(moduleTraitCor)
  par(mar = c(6, 8.5, 3, 3))
  # Display the correlation values within a heatmap plot
  WGCNA::labeledHeatmap(Matrix = moduleTraitCor,
                        xLabels = names(envRDA),
                        yLabels = names(MEs),
                        ySymbols = names(MEs),
                        colorLabels = FALSE,
                        colors = WGCNA::greenWhiteRed(50),
                        textMatrix = textMatrix,
                        setStdMargins = FALSE,
                        cex.text = 0.5,
                        zlim = c(-1,1),
                        main = paste("Module-trait relationships"))
  dev.off()
  
  #--关键微生物与理化关系#---------
  # NetmodelEnv =  paste(res1path,"/HUB_micro_env/",sep = "")
  # dir.create(NetmodelEnv)
  
  igraph = make_igraph(cor)
  tem = 10
  hub = hub_score(igraph)$vector %>%
    sort(decreasing = TRUE) %>%
    head(tem) %>%
    as.data.frame()
  
  colnames(hub) = "hub_sca"
  
  p = ggplot(hub) +
    geom_bar(aes(x = hub_sca,y = reorder(row.names(hub),hub_sca)),stat = "identity",fill = "#4DAF4A")
  
  p
  FileName <- paste(netpath,"/hub_micro", ".pdf", sep = "")
  ggsave(FileName, p,width = 6,height =tem/2)
  
  FileName <- paste(netpath,"/hub_micro", ".csv", sep = "")
  write.csv(hub,FileName)
  
  id.2 = row.names(hub)
  
  # ps.1 = ps %>% scale_micro("TMM") %>%
  
  otu = phyloseq::otu_table(ps.1)
  tax = phyloseq::tax_table(ps.1)
  
  head(otu)
  
  data = otu[id.2,] %>% t() %>%
    as.data.frame()
  
  
  
  result = cor_env_ggcorplot(
    env1 = envRDA[sample_names(ps.1),],
    env2 = data,
    label =  F,
    col_cluster = F,
    row_cluster = F,
    method = "spearman",
    r.threshold= 0.5,
    p.threshold= 0
  )
  
  
  
  
  p1 <- result[[1]] 
  p1
  p2 <- result[[2]]
  p2
  
  hei = dim(env)[2]/5
  
  # 
  # filename = paste(NetmodelEnv,"hum_env.csv",sep = "")
  # write.csv(top10,filename)
  
  filename = paste(netpath,"hum_env.pdf",sep = "")
  ggsave(filename,p1,width = tem/2,height = dim(env)[2]/5)
  filename = paste(netpath,"hum_env.pdf",sep = "")
  ggsave(filename,p2,width = tem/2,height = dim(env)[2]/5)
  
  filename = paste(netpath,"hum_env.jpg",sep = "")
  ggsave(filename,p1,width = tem/2,height = dim(env)[2]/5)
  filename = paste(netpath,"hum_env.jpg",sep = "")
  ggsave(filename,p2,width = tem/2,height = dim(env)[2]/5)
  
}


#--细菌和环境因子的网络#--------
env = read.csv("./data/dataNEW/env.csv")
head(env)
envRDA = env
head(env)
row.names(envRDA) = env$ID
envRDA$ID = NULL
head(envRDA)

Envnetplot<- paste(res1path,"/Env_network",sep = "")
dir.create(Envnetplot)

ps16s = readRDS("./data/dataNEW/ps_16s.rds")%>% ggClusterNet::scale_micro()
psITS = NULL


library(phyloseq)
#--细菌和真菌ps对象中的map文件要一样
ps.merge <- ggClusterNet::merge16S_ITS(ps16s = ps16s,
                                       psITS= NULL,
                         NITS = 200,
                         N16s = 200)

map =  phyloseq::sample_data(ps.merge)
head(map)

map$Group = "one"

phyloseq::sample_data(ps.merge) <- map

#--环境因子导入
data1 = env

envRDA.s = vegan::decostand(envRDA,"hellinger")
data1[,-1] = envRDA.s

Gru = data.frame(ID = colnames(env)[-1],group = "env" )
head(Gru)


library(sna)
library(ggClusterNet)
library(igraph)

result <- ggClusterNet::corBionetwork(ps = ps.merge,
                        N = 0,
                        r.threshold = 0.6, # 相关阈值
                        p.threshold = 0.05,
                        big = T,
                        group = "Group",
                        env = data1, # 环境指标表格
                        envGroup = Gru,# 环境因子分组文件表格
                        layout_net = "model_maptree2",
                        path = Envnetplot,# 结果文件存储路径
                        fill = "Phylum", # 出图点填充颜色用什么值
                        size = "igraph.degree", # 出图点大小用什么数据
                        scale = TRUE, # 是否要进行相对丰度标准化
                        bio = TRUE, # 是否做二分网络
                        zipi = F, # 是否计算ZIPI
                        step = 100, # 随机网络抽样的次数
                        width = 18,
                        label = TRUE,
                        height = 10
)


p = result[[1]]
p
# 全部样本网络参数比对
data = result[[2]]
plotname1 = paste(Envnetplot,"/network_all.jpg",sep = "")
ggsave(plotname1, p,width = 15,height = 12,dpi = 72)
plotname1 = paste(Envnetplot,"/network_all.png",sep = "")
ggsave(plotname1, p,width = 10,height = 8,dpi = 72)
plotname1 = paste(Envnetplot,"/network_all.pdf",sep = "")
ggsave(plotname1, p,width = 15,height = 12)
tablename <- paste(Envnetplot,"/co-occurrence_Grobel_net",".csv",sep = "")
write.csv(data,tablename)

