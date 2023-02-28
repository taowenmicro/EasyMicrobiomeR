

?SetModuleType
?Read16SAbundData







mbSet<-ApplyAbundanceFilter(mbSet, "prevalence", 4, 0.2);
mbSet<-ApplyVarianceFilter(mbSet, "iqr", 0.1);
smpl.nm.vec <- c("KO1","KO2","KO3","KO4","KO5","KO6","OE1","OE2","OE3","OE4","OE5","OE6","WT1","WT2","WT3","WT4","WT5","WT6")
# mbSet<-UpdateSampleItems(mbSet);



mbSet<-PerformNormalization(mbSet, "none", "colsum", "none");
mbSet<-PlotTaxaAundanceBar(mbSet, "taxa_alpha_0","Phylum","SampleType", "null", "barraw",10, "set3","sum",10, "bottom", "F", "png");
mbSet<-PlotOverallPieGraph(mbSet, "Phylum", 10,"sum", 10, "bottom");
GetSeriesColors()
mbSet<-SavePiechartImg(mbSet, "Phylum","primary_piechart_0","png");
mbSet<-PlotRarefactionCurve(mbSet, "filt","SampleType","SampleType","SampleType","5","rarefaction_curve_0","png");
mbSet<-PlotPhylogeneticTree(mbSet, "SampleType","SampleType","Phylum","rectangular","phylogenetic_tree_0","png");
mbSet<-PlotAlphaData(mbSet, "filt","alpha_diver_0","Chao1","SampleType","OTU", "default", "png");
mbSet<-PlotAlphaBoxData(mbSet, "alpha_diverbox_0","Chao1","SampleType","default", "png");
mbSet<-PerformAlphaDiversityComp(mbSet, "tt","SampleType");
mbSet<-PerformBetaDiversity(mbSet, "beta_diver_0","PCoA","bray","expfac","SampleType","none","OTU","","Chao1", "yes", "png", 72, "default");
mbSet<-PCoA3D.Anal(mbSet, "PCoA","bray","OTU","expfac","SampleType","","Chao1","beta_diver3d_0.json")
mbSet<-PerformCategoryComp(mbSet, "OTU", "adonis","bray","SampleType");
mbSet<-CoreMicrobeAnalysis(mbSet, "core_micro_0",0.2,0.01,"OTU","bwm","overview", "all_samples", "SampleType", "null", "png");
mbSet<-PlotHeatmap(mbSet, "heatmap_0","euclidean","ward.D","bwm","SampleType","OTU","overview","F", "png","T","F");
mbSet<-PlotTreeGraph(mbSet, "plot_tree_0","bray","ward.D","SampleType","OTU", "default", "png");
mbSet<-PrepareCorrExpValues(mbSet, "SampleType", "Phylum", "dbgr", "reingold-tilford", "all", "0.05")
mbSet<-PerformNetworkCorrelation(mbSet,"Phylum", "sparcc", "expr",100, 0.05, 0.3, "mean", "cor_net_0.json")
mbSet<-PerformUnivarTest(mbSet, "SampleType",0.05,"NA","OTU","tt");
mbSet<-PerformRNAseqDE(mbSet, "EdgeR",0.05,"SampleType","NA","OTU");
mbSet<-PerformRNAseqDE(mbSet, "EdgeR",0.05,"SampleType","NA","OTU");
mbSet<-PerformLefseAnal(mbSet,  0.1, "fdr", 2.0,  "SampleType","F","NA","OTU");
mbSet<-PlotLEfSeSummary(mbSet, 15, "dot",  "bar_graph_0","png");
mbSet<-RF.Anal(mbSet, 500,7,1,"SampleType","OTU")
mbSet<-PlotRF.Classify(mbSet, 15, "rf_cls_0","png", width=NA)
mbSet<-PlotRF.VIP(mbSet, 15, "rf_imp_0","png", width=NA)
