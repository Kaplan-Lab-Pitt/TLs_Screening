#R for PCA of target mutants. 01/23/2023
sessionInfo()
#R version 4.0.0 (2020-04-24) -- "Arbor Day"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-pc-linux-gnu (64-bit)

library(tidyverse)
library(ggplot2)
library(dplyr)
setwd("/Users/bingbingduan/Documents/lib/Pol2lib/analysis/Details_of_each_lib/lib1678910/MedianNormed_fitness/6_PCA")
dir()


#1. DF without missing values.
Deviation_NoNA <- read.csv("./2_inputForPCA/TargetDouMut_deviation_PCA_NoNA.csv", sep=",", header =TRUE)
row.names(Deviation_NoNA) <- Deviation_NoNA[,1]
Deviation_NoNA = Deviation_NoNA[, -1]

#Test is there are any NA values in Deviation_NoNA
test <- as.numeric(unlist(Deviation_NoNA))
head(test)
any(is.na(test))
which(is.na(test))
nonum <- which(is.na(as.numeric(unlist(Deviation_NoNA))))
#The reason is EXP was there. Remove NA value

#Remove Constant value
Deviation_NoNA_NoConstant <- Deviation_NoNA[,sapply(Deviation_NoNA, function(v) var(v, na.rm=TRUE)!=0)]

#PCA
deviation_NoNA_NoConstant_PCA <- prcomp(Deviation_NoNA_NoConstant, scale=TRUE)



## plot pc1 and pc2
plot(deviation_NoNA_NoConstant_PCA$x[,1], deviation_NoNA_NoConstant_PCA$x[,2])

## make a scree plot
pca.var <- deviation_NoNA_NoConstant_PCA$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

## now make a fancy looking plot that shows the PCs and the variation.
pca.data <- data.frame(Sample=rownames(deviation_NoNA_NoConstant_PCA$x),
                       X=deviation_NoNA_NoConstant_PCA$x[,1],
                       Y=deviation_NoNA_NoConstant_PCA$x[,2])
pca.data

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample, color=Sample)) +
        geom_text() +
        xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
        ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
        theme_bw() +
        scale_color_manual(breaks = c("M1079V", "S713P", "E1103G","F1084I", "L1101S","T834P","Y769F", "H1085L", "T834A","N1082S", "H1085Y","Q1078S"),values=c("Green4", "Green4", "Green4","Green4", "Green4","Green4","Green4", "blue", "blue","blue", "blue","blue"))+
        ggtitle("PCA Graph with Deviation_NoNA score")

## get the name of the top 10 measurements (genes) that contribute
## most to pc1.
loading_scores <- deviation_NoNA_NoConstant_PCA$rotation[,1]
gene_scores <- abs(loading_scores) ## get the magnitudes
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
top_10_genes <- names(gene_score_ranked[1:10])

top_10_genes ## show the names of the top 10 genes

deviation_NoNA_NoConstant_PCA$rotation[top_10_genes,1] ## show the scores (and +/- sign)



#Fitness
Fitness_NoNA <- read.csv("./2_inputForPCA/TargetDouMut_fitness_PCA_NoNA.csv", sep=",", header =TRUE)
row.names(Fitness_NoNA) <- Fitness_NoNA[,1]
Fitness_NoNA = Fitness_NoNA[, -1]

fitness_NoNA_NoConstant <- Fitness_NoNA[,sapply(Fitness_NoNA, function(v) var(v, na.rm=TRUE)!=0)]
fitness_NoNA_NoConstant_PCA <- prcomp(Deviation_NoNA_NoConstant, scale=TRUE)

## plot pc1 and pc2
plot(fitness_NoNA_NoConstant_PCA$x[,1], fitness_NoNA_NoConstant_PCA$x[,2])

## make a scree plot
pca.var <- fitness_NoNA_NoConstant_PCA$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

## now make a fancy looking plot that shows the PCs and the variation.
pca.data <- data.frame(Sample=rownames(fitness_NoNA_NoConstant_PCA$x),
                       X=deviation_NoNA_NoConstant_PCA$x[,1],
                       Y=deviation_NoNA_NoConstant_PCA$x[,2])
pca.data

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample, color=Sample)) +
        geom_text() +
        xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
        ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
        theme_bw() +
        scale_color_manual(breaks = c("M1079V", "S713P", "E1103G","F1084I", "L1101S","T834P","Y769F", "H1085L", "T834A","N1082S", "H1085Y","Q1078S"),values=c("Green4", "Green4", "Green4","Green4", "Green4","Green4","Green4", "blue", "blue","blue", "blue","blue"))+
        ggtitle("PCA Graph with fitness_NoNA score")

## get the name of the top 10 measurements (genes) that contribute
## most to pc1.
loading_scores <- deviation_NoNA_NoConstant_PCA$rotation[,1]
gene_scores <- abs(loading_scores) ## get the magnitudes
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
top_10_genes <- names(gene_score_ranked[1:10])

top_10_genes ## show the names of the top 10 genes

fitness_NoNA_NoConstant_PCA$rotation[top_10_genes,1] ## show the scores (and +/- sign)




#2. DF with missing values.
install.packages("missMDA")
library(missMDA)


Deviation <- read.csv("./2_inputForPCA/TargetDouMut_deviation_PCA.csv", sep=",", header =TRUE)
row.names(Deviation) <- Deviation[,1]
Deviation = Deviation[, -1]
#Error in prcomp.default(Deviation_NoNA, scale = TRUE) : cannot rescale a constant/zero column to unit variance
#Remove no variance values
Deviation_NoConstant <- Deviation[,sapply(Deviation, function(v) var(v, na.rm=TRUE)!=0)]
#Imputation to fill missing value. https://www.rdocumentation.org/packages/missMDA/versions/1.18/topics/imputePCA
Deviation_NoConstant_impute <- imputePCA(Deviation_NoConstant, method="Regularized",ncp=2)
# A PCA can be performed on the imputed data
deviation_NoConstant_impute_PCA <- prcomp(Deviation_NoConstant_impute$completeObs,scale=TRUE)

## plot pc1 and pc2
plot(deviation_NoConstant_impute_PCA$x[,1], deviation_NoConstant_impute_PCA$x[,2])

## make a scree plot
pca.var <- deviation_NoConstant_impute_PCA$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

## now make a fancy looking plot that shows the PCs and the variation.
pca.data <- data.frame(Sample=rownames(deviation_NoConstant_impute_PCA$x),
                       X=deviation_NoConstant_impute_PCA$x[,1],
                       Y=deviation_NoConstant_impute_PCA$x[,2])
pca.data

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample, color=Sample)) +
        geom_text() +
        xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
        ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
        theme_bw() +
        scale_color_manual(breaks = c("M1079V", "S713P", "E1103G","F1084I", "L1101S","T834P","Y769F", "H1085L", "T834A","N1082S", "H1085Y","Q1078S"),values=c("Green4", "Green4", "Green4","Green4", "Green4","Green4","Green4", "blue", "blue","blue", "blue","blue"))+
        ggtitle("PCA Graph with deviation_NoConstant_impute_PCA score")

write.csv(Deviation_NoConstant_impute$completeObs, file="Deviation_NoConstant_impute_Regulized.csv")

#Fitness
Fitness <- read.csv("./2_inputForPCA/TargetDouMut_fitness_PCA.csv", sep=",", header =TRUE)
row.names(Fitness) <- Fitness[,1]
Fitness = Fitness[, -1]

fitness_impute <- imputePCA(Fitness, method="EM",ncp=2)
# A PCA can be performed on the imputed data
fitness_impute_PCA <- prcomp(fitness_impute$completeObs,scale=TRUE)

## plot pc1 and pc2
plot(fitness_impute_PCA$x[,1], fitness_impute_PCA$x[,2])

## make a scree plot
pca.var <- fitness_impute_PCA$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

## now make a fancy looking plot that shows the PCs and the variation.
pca.data <- data.frame(Sample=rownames(fitness_impute_PCA$x),
                       X=fitness_impute_PCA$x[,1],
                       Y=fitness_impute_PCA$x[,2])
pca.data

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample, color=Sample)) +
        geom_text() +
        xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
        ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
        theme_bw() +
        scale_color_manual(breaks = c("M1079V", "S713P", "E1103G","F1084I", "L1101S","T834P","Y769F", "H1085L", "T834A","N1082S", "H1085Y","Q1078S"),values=c("Green4", "Green4", "Green4","Green4", "Green4","Green4","Green4", "blue", "blue","blue", "blue","blue"))+
        ggtitle("PCA Graph with fitness_impute")

write.csv(fitness_impute$completeObs, file="fitness_impute_PCA_completeobs_EM.csv")


