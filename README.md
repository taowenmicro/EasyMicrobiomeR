# Using R language in microbiome analysis

Tao Wen2, ‡, Guoqing Niu2, ‡, Qirong Shen2, Jun Yuan2,*, Yong-Xin Liu1,*

![image](https://mmbiz.qpic.cn/sz_mmbiz_jpg/wJzLDA3fpphpCLxj7oiaJ80BbhMosnEGicr8SiaGAWdcWEct5icHwIoMRNzrXSr093POApTb2T8J7AribjAO8WNdD0g/640?wx_fmt=jpeg&wxfrom=5&wx_lazy=1&wx_co=1)


## Abstract

With the gradual maturity of sequencing technology, many microbiome studies have emerged, driving the emergence and advance of related analysis tools. R language is the widely used platform for microbiome data analysis for the powerful functions. However, the tens of thousands of R packages and numerous similar analysis tools have brought major challenges for many researchers to explore microbiome data. How to choose suitable, efficient, convenient, and easy-to-learn tools from the numerous R packages has become a problem for many microbiome researchers. We have organized 322 common R packages for microbiome analysis, classified them according to functional categories (diversity, differential, biomarker, correlation and network analysis, functional prediction, and other), which could help researchers quickly find relevant R packages for microbiome analysis. Furthermore, we systematically sorted the integrated R packages (phyloseq, microbiome, MicrobiomeAnalystR, Animalcules, microeco, and amplicon) for microbiome analysis, and summarized the advantages and limitations, which will help researchers choose the appropriate tools. Finally, we conducted a thorough review of the R packages for microbiome analysis, summarized most of the common analysis content in microbiome, and formed the most suitable pipeline for microbiome analysis. This paper accompanied by detailed code examples, which can help beginners to learn, as well as help analysts compare and test different tools. This paper systematically sorts out the application of R language in microbiome methods, providing an important theoretical basis and practical reference for the development of better microbiome tools in the future. All the code is available at GitHub: <https://github.com/taowenmicro/easy_microbiome>.

## To install all the required R packages for this project


To install most of the required R packages for this project at once using a saved list of R packages.


```

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



```



## Contents


### Before microbiome data analysis

- 1.Before microbiome data analysis/Code1A-2H.Rmd



### Microbial community analysis

- 2.Microbial community analysis/Code3A-8E.Rmd



### Integrated R packages for microbiome

- 3.Integrated R packages for microbiome/Pipeline1-6/01.phyloseq/pipeline.1.phyloseq.Rmd

- 3.Integrated R packages for microbiome/Pipeline1-6/02.microbiome/pipeline.2.microbiome.Rmd

- 3.Integrated R packages for microbiome/Pipeline1-6/03.MicrobiomeAnalystR/pipeline.2.microbiome.Rmd

- 3.Integrated R packages for microbiome/Pipeline1-6/04.animalcules/pipeline.2.microbiome.Rmd

- 3.Integrated R packages for microbiome/Pipeline1-6/05.microeco/pipeline.2.microbiome.Rmd

- 3.Integrated R packages for microbiome/Pipeline1-6/06.EasyAmplicon/pipeline.6.Amplicon.Rmd



### The best practice R pipeline

- 4.The best practice R pipeline/Pipeline.BestPractice.Rmd

![image](https://mmbiz.qpic.cn/sz_mmbiz_jpg/wJzLDA3fpphpCLxj7oiaJ80BbhMosnEGicvibZlWOgsB1tJpZuDkU7yniakR54JUq6hyJk9JA2mpicMH9fOpZJSQeTw/640?wx_fmt=jpeg&wxfrom=5&wx_lazy=1&wx_co=1)

## Other results that can be obtained from running this project can be displayed


![image](https://mmbiz.qpic.cn/sz_mmbiz_jpg/wJzLDA3fpphpCLxj7oiaJ80BbhMosnEGic5tT0skYIpUviaUodh2EfMtFJeUOviaYr8VNEpjceoZUNHkYWl7Cn0LGQ/640?wx_fmt=jpeg&wxfrom=5&wx_lazy=1&wx_co=1)


![image](https://mmbiz.qpic.cn/sz_mmbiz_jpg/wJzLDA3fpphpCLxj7oiaJ80BbhMosnEGicIodzGmJ8TZqhqJKZ5ZrmwJqKEPwrsk3z6ib5wtpAg5JicicSiaHbdiaA9xA/640?wx_fmt=jpeg&wxfrom=5&wx_lazy=1&wx_co=1)


![image](https://mmbiz.qpic.cn/sz_mmbiz_jpg/wJzLDA3fpphpCLxj7oiaJ80BbhMosnEGicUC5zuUkSnCgVMWCkYAKMQacqd1ZW0vO7ewiaZevFREtKZFbU1A0Ug9Q/640?wx_fmt=jpeg&wxfrom=5&wx_lazy=1&wx_co=1)


![image](https://mmbiz.qpic.cn/sz_mmbiz_jpg/wJzLDA3fpphpCLxj7oiaJ80BbhMosnEGicG4dnMXMPPqRlGY4nQXU1CET0qPoEeZLxVfmiaEzBeNmdNA8jIeKnEHQ/640?wx_fmt=jpeg&wxfrom=5&wx_lazy=1&wx_co=1)

![image](https://mmbiz.qpic.cn/sz_mmbiz_jpg/wJzLDA3fpphpCLxj7oiaJ80BbhMosnEGicZF9VZ3BT1Q1exthU4icxRALx7v1MglEVPqmIbgnJY6bpcAahHK11UuQ/640?wx_fmt=jpeg&wxfrom=5&wx_lazy=1&wx_co=1)


## reference

Wen, T., G. Niu, T. Chen, Q. Shen, J. Yuan and Y. X. Liu (2023). "The best practice for microbiome analysis using R." Protein Cell. https://doi.org/10.1093/procel/pwad024.


