library(picante)
library(ape)
library(vegan)
library(FSA)
library(eulerr)
library(grid)
library(gridExtra)
require(minpack.lm)
require(Hmisc)
require(stats4)
library(parallel)

ps = base::readRDS("./data/dataNEW/ps_16s.rds")

# data(ps)

env = read.csv("./data/dataNEW/env.csv")
envRDA = env
head(env)
row.names(envRDA) = env$ID
envRDA$ID = NULL
head(envRDA)


psphy = filter_taxa(ps, function(x) sum(x ) > 1200 , TRUE);psphy



res1path = "./result_and_plot/"
phypath = paste(res1path,"/Phylogenetic_analyse_spacies/",sep = "")
dir.create(phypath)

map = sample_data(ps)
n = map$Group %>% unique() %>%
  length()
n



#--系统发育信号#----
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\phylo_Micro\\phyloSignal_and_phySigplot.R")

sample_data(psphy)
phypath2 = paste(phypath,"/phyloSignal/",sep = "")
dir.create(phypath)
phyloSignal(ps = psphy,
            group  = "Group",
            env = env[,1:2],
            path = phypath2)

result = phySigPlot(ps = ps,group  = "Group",env = env[,1:2],path = phypath2)
#
#提取图片
p2 = result[[1]] + mytheme1
p2
#-提取作图数据
data = result[[2]]
head(data)

FileName <- paste(phypath,"2_phySigPlot", ".pdf", sep = "")
ggsave(FileName, p2,width = 15,height = 6)
FileName <- paste(phypath,"2_phySigPlot", ".csv", sep = "")
write.csv(data,FileName)



#---计算零模型-不用跑#-------
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\phylo_Micro\\nullModel1.R")

result <- nullModel(ps = psphy,
                    group="Group",
                    dist.method =  "bray",
                    gamma.method = "total",
                    transfer = "none",
                    null.model = "ecosphere"
                    )

#--分组零模型运行结果
nullModeltab <- result[[1]]

# 比例
ratiotab <- result[[2]]
#-统计量统计差异
aovtab <- result[[3]]

FileName <- paste(phypath,"3_nullModeltab", ".csv", sep = "")
write.csv(nullModeltab,FileName)

FileName <- paste(phypath,"3_ratiotab", ".csv", sep = "")
write.csv(ratiotab,FileName)


# FileName <- paste(phypath,"3_aovtab", ".csv", sep = "")
# write.csv(aovtab,FileName)


#--BNTI#----
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\phylo_Micro\\bNTICul.R")

result = bNTICul(ps = psphy,group  = "Group",num = 10,thread = 1)
bNTI = result[[1]]
head(bNTI)


filename = paste(phypath,"/4_bNTI.csv",sep = "")
write.csv(bNTI, filename)


#--计算RCbray#-----------
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\phylo_Micro\\RCbary.R")

result = RCbary(ps = psphy ,group  = "Group",num = 10,thread = 1)

RCbary = result[[1]]
head(RCbary)

filename = paste(phypath,"/5_RCb.csv",sep = "")

write.csv(RCbary,filename)


#--BetaNTI和RCbray联合出图#---------
# phypath = "./result_and_plot/16S_env_phylo_processing/Phylogenetic_analyse_spacies/"
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\phylo_Micro\\bNTIRCPlot.R")

bNTI = read.csv(paste(phypath,"/4_bNTI.csv",sep = ""),row.names = 1)
head(bNTI)
# RCbray 数据读入，修改列名
RCb = read.csv(paste(phypath,"/5_RCb.csv",sep = ""),row.names = 1) %>%
  dplyr::mutate(Sample_1 = Site2, Sample_2 = Site1)
head(RCb)

result = bNTIRCPlot(ps = psphy ,RCb  = RCb,bNTI = bNTI,group  = "Group")

#--bNTI出图片
p3 <- result[[1]] + mytheme1
p3

#RCbary可视化
p4 <- result[[2]] + mytheme1
p4

#组合图片BNTI，RCbray
p5 <- result[[3]]
p5
plotdata = result[[4]]
head(plotdata)

dat = result[[5]]
head(dat)


filename = paste(phypath,"/6_bNTI_RCbray.csv",sep = "")
write.csv(plotdata,filename)

FileName <- paste(phypath,"6_bNTI", ".pdf", sep = "")
ggsave(FileName, p3,width =8,height = 6)

FileName <- paste(phypath,"6_RCbary", ".pdf", sep = "")
ggsave(FileName, p4,width = 6,height = 6)

FileName <- paste(phypath,"6_BNTI_RCbray", ".pdf", sep = "")
ggsave(FileName, p5,width = 12,height = 8)

FileName <- paste(phypath,"6_bNTI", ".png", sep = "")
ggsave(FileName, p3,width =8,height = 6)

FileName <- paste(phypath,"6_RCbary", ".png", sep = "")
ggsave(FileName, p4,width = 6,height = 6)

FileName <- paste(phypath,"6_BNTI_RCbray", ".png", sep = "")
ggsave(FileName, p5,width = 12,height = 8)


FileName <- paste(phypath,"6_RCbray.percent.csv", sep = "")
write.csv(dat,FileName, quote = F)



#---环境因子和BetaNTI相关#---------
source("E:\\Shared_Folder\\Function_local\\R_function\\micro\\phylo_Micro\\EnvCorbNTI.R")

#-导入bNTI函数
bNTIRC = read.csv(paste(phypath,"/6_bNTI_RCbray.csv",sep = ""),row.names = 1)
head(bNTIRC)

map = sample_data(psphy)
head(map)
plot = EnvCorbNTI(ps = psphy,
                  bNTIRC = bNTIRC,
                  group  = "Group",
                  env = envRDA
                  )

## 提取相关分析结果，总图
p6 <- plot[[1]]
p6
#提取单个
# plot[[2]][1]

FileName <- paste(phypath,"7_env_corWithBNTI", ".pdf", sep = "")
ggsave(FileName, p6,width = 16,height = 14)

FileName <- paste(phypath,"7_env_corWithBNTI", ".png", sep = "")
ggsave(FileName, p6,width = 16,height = 14)




