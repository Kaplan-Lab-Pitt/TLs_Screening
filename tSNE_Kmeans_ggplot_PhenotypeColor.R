# This is the script for tSNE, kmeans analysis
setwd("/Users/bingbingduan/Documents/lib/Pol2lib/analysis/Phenotype_correlation/classification/tSNEUMAP_2022/frequencyTable_2022/inputs")
dir()
library(ggplot2)
sessionInfo()
#R version 4.0.0 (2020-04-24) -- "Arbor Day"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-pc-linux-gnu (64-bit)

> packageVersion("stats")
#[1] ‘4.0.3’
> packageVersion("Rtsne")
#[1] ‘0.15’
> packageVersion("ggplot2")
#[1] ‘3.3.3’



#1. Plot all data
freqc_tSNE <- read.table(file = "tSNE_viable_P100_kemans_10_phenotype.csv", row.names = 1, sep = ',', header = T, fill = TRUE)

ggplot(freqc_tSNE, aes(x=V1, y=V2, color = freqc_tSNE$LogisticPredicted_class)) +
        geom_point(aes(size=freqc_tSNE$LogisticPredicted_class)) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE") +
        theme_light(base_size=4) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank()) +
        scale_colour_manual(breaks = c("GOF", "LOF", "WT", "Unclassified", "GOFandLOF", "UltraSickToLethal"),values=c("Green4", "blue", "red", "grey", "grey", "grey50")) +
        scale_size_manual(breaks = c("GOF", "LOF", "WT", "Unclassified", "GOFandLOF", "UltraSickToLethal"), values=c(0.5,0.5,0.5,0.5,0.5,0.5))




#plot_cluster=function(data, var_cluster, palette)
#{
        ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
                geom_point(size=0.5) +
                guides(colour=guide_legend(override.aes=list(size=2))) +
                xlab("") + ylab("") +
                ggtitle("") +
                theme_light(base_size=4) +
                theme(axis.text.x=element_blank(),
                      axis.text.y=element_blank(),
                      legend.direction = "horizontal",
                      legend.position = "right",
                      legend.box = "horizontal") +
                scale_colour_brewer(palette = palette)
#}

#plot_cluster(freqc_tSNE, "cl_kmeans", "Paired")
#Error: Continuous value supplied to discrete scale

ggplot(freqc_tSNE, aes(x=V1, y=V2, color = as.factor(cl_kmeans)) +
        geom_point(size=0.5) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_kmeans_10") +
        theme_light(base_size=4) +
        theme(axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
        scale_colour_brewer(palette = "BrBG"))
#Error in aes(x = V1, y = V2, color = as.factor(cl_kmeans)) + geom_point(size = 0.5) +  : 
        #non-numeric argument to binary operator


#Solved the problem by adding factors as an individual line.
freqc_tSNE$cl_kmeans <- as.factor(freqc_tSNE$cl_kmeans)
plot_cluster=function(data, var_cluster, palette)
{
        ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
                geom_point(size=0.5) +
                guides(colour=guide_legend(override.aes=list(size=2))) +
                xlab("") + ylab("") +
                ggtitle("") +
                theme_light(base_size=4) +
                theme(axis.text.x=element_blank(),
                      axis.text.y=element_blank(),
                      legend.direction = "horizontal",
                      legend.position = "bottom",
                      legend.box = "horizontal") +
                scale_colour_brewer(palette = palette)
}
plot_cluster(freqc_tSNE, "cl_kmeans", "Paired")

#Different conditions
ggplot(freqc_tSNE, aes(x=V1, y=V2, color =MPA)) +
        geom_point(size=0.5) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_MPA") +
        theme_light(base_size=4) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank()) +
        scale_colour_gradient2()

ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Mn)) +
        geom_point(size=0.5) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Mn") +
        theme_light(base_size=4) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank()) +
        scale_colour_gradient2()


ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Gal)) +
        geom_point(size=0.5) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Gal") +
        theme_light(base_size=4) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank()) +
        scale_colour_gradient2()

ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Lys)) +
        geom_point(size=0.5) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Lys") +
        theme_light(base_size=4) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank()) +
        scale_colour_gradient2()


ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Form)) +
        geom_point(size=0.5) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Form") +
        theme_light(base_size=4) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank()) +
        scale_colour_gradient2()


#2. Plot all data with grey theme
ggplot(freqc_tSNE, aes(x=V1, y=V2, color = freqc_tSNE$LogisticPredicted_class)) +
        geom_point(aes(size=freqc_tSNE$LogisticPredicted_class)) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE") +
        theme_grey(base_size=4) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank()) +
        scale_colour_manual(breaks = c("GOF", "LOF", "WT", "Unclassified", "GOFandLOF", "UltraSickToLethal"),values=c("Green4", "blue", "red", "grey", "grey", "grey50")) +
        scale_size_manual(breaks = c("GOF", "LOF", "WT", "Unclassified", "GOFandLOF", "UltraSickToLethal"), values=c(2,2,2,2,2,2))


plot_cluster=function(data, var_cluster, palette)
{
        ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
                geom_point(size=2) +
                guides(colour=guide_legend(override.aes=list(size=2))) +
                xlab("tSNE_1") + ylab("tSNE_2") +
                ggtitle("t-SNE") +
                theme_grey(base_size=4) +
                theme(axis.text.x=element_blank(),
                      axis.text.y=element_blank(),
                      legend.direction = "horizontal",
                      legend.position = "bottom",
                      legend.box = "horizontal") +
                scale_colour_brewer(palette = palette)
}
plot_cluster(freqc_tSNE, "cl_kmeans", "Paired")



#Different conditions
ggplot(freqc_tSNE, aes(x=V1, y=V2, color =MPA)) +
        geom_point(size=2) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_MPA") +
        theme_grey(base_size=4) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank()) +
        scale_colour_gradient2()

ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Mn)) +
        geom_point(size=2) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Mn") +
        theme_grey(base_size=4) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank()) +
        scale_colour_gradient2()


ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Gal)) +
        geom_point(size=2) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Gal") +
        theme_grey(base_size=4) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank()) +
        scale_colour_gradient2()

ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Lys)) +
        geom_point(size=2) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Lys") +
        theme_grey(base_size=4) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank()) +
        scale_colour_gradient2()


ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Form)) +
        geom_point(size=2) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Form") +
        theme_grey(base_size=4) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank()) +
        scale_colour_gradient2()





#3. Plot all data with pure grey theme (No grid)

ggplot(freqc_tSNE, aes(x=V1, y=V2, color = LogisticPredicted_class)) +
        geom_point(aes(size=freqc_tSNE$LogisticPredicted_class)) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE") +
        theme(panel.background = element_rect(fill = "gray", colour = "gray"), axis.line = element_line(colour = "grey50")) +
        scale_colour_manual(breaks = c("GOF", "LOF", "WT", "Unclassified", "GOFandLOF", "UltraSickToLethal"),values=c("Green4", "blue", "red", "grey", "grey", "grey50")) +
        scale_size_manual(breaks = c("GOF", "LOF", "WT", "Unclassified", "GOFandLOF", "UltraSickToLethal"), values=c(2,2,2,2,2,2))

ggplot(freqc_tSNE, aes(x=V1, y=V2, color = LogisticPredicted_class)) +
        geom_point(size=2) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE") +
        theme(
                # Hide panel borders and remove grid lines
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                # Change axis line
                axis.line = element_line(colour = "black")
        ) +
        scale_colour_manual(breaks = c("GOF", "LOF", "WT", "Unclassified", "GOFandLOF", "UltraSickToLethal"),values=c("Green4", "blue", "red", "grey", "grey", "grey50"))


freqc_tSNE$cl_kmeans <- as.factor(freqc_tSNE$cl_kmeans)

plot_cluster=function(data, var_cluster, palette)
{
        ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
                geom_point(size=2) +
                guides(colour=guide_legend(override.aes=list(size=2))) +
                xlab("tSNE_1") + ylab("tSNE_2") +
                ggtitle("t-SNE") +
                theme(
                        # Hide panel borders and remove grid lines
                        panel.border = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        # Change axis line
                        axis.line = element_line(colour = "black")
                ) +
                scale_colour_brewer(palette = palette)
}
plot_cluster(freqc_tSNE, "cl_kmeans", "Paired")



#Different conditions
ggplot(freqc_tSNE, aes(x=V1, y=V2, color =MPA)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_MPA") +
        theme(
                # Hide panel borders and remove grid lines
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                # Change axis line
                axis.line = element_line(colour = "black")
        ) +
        scale_colour_gradient2()

ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Mn)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Mn") +
        theme(
                # Hide panel borders and remove grid lines
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                # Change axis line
                axis.line = element_line(colour = "black")
        ) +
        scale_colour_gradient2()


ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Gal)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Gal") +
        theme(
                # Hide panel borders and remove grid lines
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                # Change axis line
                axis.line = element_line(colour = "black")
        ) +
        scale_colour_gradient2()

ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Lys)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Lys") +
        theme(
                # Hide panel borders and remove grid lines
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                # Change axis line
                axis.line = element_line(colour = "black")
        ) +
        scale_colour_gradient2()


ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Form)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Form") +
        theme(
                # Hide panel borders and remove grid lines
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                # Change axis line
                axis.line = element_line(colour = "black")
        ) +
        scale_colour_gradient2()

#3.2 Plot all data with white theme, No grid, size1, scale_colour_gradientn()
ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Form)) +
        geom_point(size=1) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Form") +
        theme_classic() +
        scale_colour_gradientn(colours=c("#3A3A98","grey92","#832424"),limits=c(-7.4,3.6),values=c(0/11,7.4/11,11/11))
#Output size = 4.14x5.85inches

ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Lys)) +
        geom_point(size=1) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Lys") +
        theme_classic() +
        scale_colour_gradientn(colours=c("#3A3A98","grey92","#832424"),limits=c(-6.5,14.4),values=c(0/20.9,6.5/20.9,20.9/20.9))


ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Gal)) +
        geom_point(size=1) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Gal") +
        theme_classic() +
        scale_colour_gradientn(colours=c("#3A3A98","grey92","#832424"),limits=c(-13,17),values=c(0/30,13/30,30/30))

ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Mn)) +
        geom_point(size=1) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Mn") +
        theme_classic() +
        scale_colour_gradientn(colours=c("#3A3A98","grey92","#832424"),limits=c(-4.9,7.8),values=c(0/12.7,4.9/12.7,12.7/12.7))

ggplot(freqc_tSNE, aes(x=V1, y=V2, color =MPA)) +
        geom_point(size=1) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_MPA") +
        theme_classic() +
        scale_colour_gradientn(colours=c("#3A3A98","grey92","#832424"),limits=c(-7.9,6.4),values=c(0/14.3,7.9/14.3,14.3/14.3))

#viable with class color
ggplot(freqc_tSNE, aes(x=V1, y=V2, color = LogisticPredicted_class)) +
        geom_point(size=1) +
        guides(colour=guide_legend(override.aes=list(size=1))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE") +
        theme_classic() +
        scale_colour_manual(breaks = c("GOF", "LOF", "WT", "Unclassified", "GOFandLOF", "UltraSickToLethal"),values=c("Green4", "blue", "red", "grey", "grey", "grey50"))
#Output size = 4.14x6.89 inches


#viable with kmeans color
freqc_tSNE$cl_kmeans <- as.factor(freqc_tSNE$cl_kmeans)

plot_cluster=function(data, var_cluster, palette)
{ ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
  geom_point(size=1) +
  guides(colour=guide_legend(override.aes=list(size=2))) +
  xlab("tSNE_1") + ylab("tSNE_2") +
  ggtitle("t-SNE") +
  theme_classic() +
  scale_colour_brewer(palette = palette)
               }

plot_cluster(freqc_tSNE, "cl_kmeans", "Paired")
#output size = 4.14x6.15 inches


#4. Re-make the figure with all mutants and P50.
Allfreqc_tSNE <- read.table(file = "tSNE_P50_kemans_20.csv", row.names = 1, sep = ',', header = T, fill = TRUE)

Allfreqc_tSNE$cl_kmeans <- as.factor(Allfreqc_tSNE$cl_kmeans)

ggplot(Allfreqc_tSNE, aes(x=V1, y=V2, color = LogisticPredicted_class)) +
        geom_point(size=0.5) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE") +
        theme(
                # Hide panel borders and remove grid lines
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                # Change axis line
                axis.line = element_line(colour = "black")
        ) +
        scale_colour_manual(breaks = c("Unclassified", "GOFandLOF", "UltraSickToLethal", "GOF", "LOF", "WT"),values=c( "grey", "grey", "grey50", "Green4", "blue", "red"))


c25 <- c(
        "dodgerblue2", "#E31A1C", # red
        "green4",
        "#6A3D9A", # purple
        "#FF7F00", # orange
        "black", "gold1",
        "skyblue2", "#FB9A99", # lt pink
        "palegreen2",
        "#CAB2D6", # lt purple
        "#FDBF6F", # lt orange
        "gray70", "khaki2",
        "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
        "darkturquoise", "green1", "yellow4", "yellow3",
        "darkorange4", "brown"
)

plot_cluster=function(data, var_cluster, col)
{
        ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
                geom_point(size=1) +
                guides(colour=guide_legend(override.aes=list(size=2))) +
                xlab("tSNE_1") + ylab("tSNE_2") +
                ggtitle("t-SNE") +
                theme(
                        # Hide panel borders and remove grid lines
                        panel.border = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        # Change axis line
                        axis.line = element_line(colour = "black")
                ) +
                scale_colour_manual(values =  c25)
}

plot_cluster(Allfreqc_tSNE, "cl_kmeans", c25)

#5. Show one color of each.
#Different conditions
#MPA

minimal<- min(freqc_tSNE$MPA)
maximal<- max(freqc_tSNE$MPA)


ggplot(freqc_tSNE, aes(x=V1, y=V2, color =MPA)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_MPA_sensitive") +
        theme_classic() +
        scale_colour_gradient(low = "blue", high = "grey", limits=c(minimal, 0),na.value = "grey")

ggplot(freqc_tSNE, aes(x=V1, y=V2, color =MPA)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_MPA_resistant") +
        theme_classic() +
        scale_colour_gradient(low = "grey", high = "#E31A1C", limits=c(0, maximal),na.value = "grey")




#Mn
minimal<- min(freqc_tSNE$Mn)
maximal<- max(freqc_tSNE$Mn)
ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Mn)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Mn_sensitive") +
        theme_classic() +
        scale_colour_gradient(low = "blue", high = "grey", limits=c(minimal, 0),na.value = "grey")

ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Mn)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Mn_Resistant") +
        theme_classic() +
        scale_colour_gradient(low = "grey", high = "#E31A1C", limits=c(0, maximal),na.value = "grey")

#Gal
minimal<- min(freqc_tSNE$Gal)
maximal<- max(freqc_tSNE$Gal)
ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Gal)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Gal_sensitive") +
        theme_classic() +
        scale_colour_gradient(low = "blue", high = "grey", limits=c(minimal, 0),na.value = "grey")


ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Gal)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Gal_resistant") +
        theme_classic() +
        scale_colour_gradient(low = "grey", high = "#E31A1C", limits=c(0, maximal),na.value = "grey")

#Lys
minimal<- min(freqc_tSNE$Lys)
maximal<- max(freqc_tSNE$Lys)

ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Lys)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Lys") +
        theme_classic() +
        scale_colour_gradient(low = "blue", high = "grey", limits=c(minimal, 0),na.value = "grey")


ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Lys)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Lys") +
        theme_classic() +
        scale_colour_gradient(low = "grey", high = "#E31A1C", limits=c(0, maximal),na.value = "grey")

#Form
minimal<- min(freqc_tSNE$Form)
maximal<- max(freqc_tSNE$Form)

ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Form)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Form") +
        theme_classic() +
        scale_colour_gradient(low = "blue", high = "grey", limits=c(minimal, 0),na.value = "grey")

ggplot(freqc_tSNE, aes(x=V1, y=V2, color =Form)) +
        geom_point(size=2) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE_Form") +
        theme_classic() +
        scale_colour_gradient(low = "grey", high = "#E31A1C", limits=c(0, maximal),na.value = "grey")


ggplot(freqc_tSNE, aes(x=V1, y=V2, color = LogisticPredicted_class)) +
        geom_point(size=2) +
        guides(colour=guide_legend(override.aes=list(size=1))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE") +
        theme_classic() +
        scale_colour_manual(breaks = c("GOF", "LOF", "WT", "Unclassified", "GOFandLOF", "UltraSickToLethal"),values=c("Green4", "blue", "red", "grey", "grey", "grey50"))


freqc_tSNE$cl_kmeans <- as.factor(freqc_tSNE$cl_kmeans)

plot_cluster=function(data, var_cluster, palette)
{
        ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
                geom_point(size=2) +
                guides(colour=guide_legend(override.aes=list(size=2))) +
                xlab("tSNE_1") + ylab("tSNE_2") +
                ggtitle("t-SNE") +
                theme_classic() +
                scale_colour_brewer(palette = palette)
}
plot_cluster(freqc_tSNE, "cl_kmeans", "Paired")


#all frequencies
Allfreqc_tSNE <- read.table(file = "tSNE_P50_kemans_20.csv", row.names = 1, sep = ',', header = T, fill = TRUE)

Allfreqc_tSNE$cl_kmeans <- as.factor(Allfreqc_tSNE$cl_kmeans)

ggplot(Allfreqc_tSNE, aes(x=V1, y=V2, color = LogisticPredicted_class)) +
        geom_point(size=0.5) +
        guides(colour=guide_legend(override.aes=list(size=2))) +
        xlab("tSNE_1") + ylab("tSNE_2") +
        ggtitle("t-SNE") +
        theme_classic() +
        scale_colour_manual(breaks = c("Unclassified", "GOFandLOF", "UltraSickToLethal", "GOF", "LOF", "WT"),values=c( "grey", "grey", "grey50", "Green4", "blue", "red"))


c25 <- c(
        "dodgerblue2", "#E31A1C", # red
        "green4",
        "#6A3D9A", # purple
        "#FF7F00", # orange
        "black", "gold1",
        "skyblue2", "#FB9A99", # lt pink
        "palegreen2",
        "#CAB2D6", # lt purple
        "#FDBF6F", # lt orange
        "gray70", "khaki2",
        "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
        "darkturquoise", "green1", "yellow4", "yellow3",
        "darkorange4", "brown"
)

plot_cluster=function(data, var_cluster, col)
{
        ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
                geom_point(size=1) +
                guides(colour=guide_legend(override.aes=list(size=2))) +
                xlab("tSNE_1") + ylab("tSNE_2") +
                ggtitle("t-SNE") +
                theme_classic() +
                scale_colour_manual(values =  c25)
}

plot_cluster(Allfreqc_tSNE, "cl_kmeans", c25)

