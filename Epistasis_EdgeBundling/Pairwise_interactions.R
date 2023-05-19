#2022-09-21
#This is for producing connection network for pairwise interactions in lib2.
setwd("/Users/bingbingduan/Documents/lib/Pol2lib/analysis/Epistasis/Portion/Portion/Edge_Bundling")
dir()
sessionInfo()
#R version 4.0.0 (2020-04-24) -- "Arbor Day"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-pc-linux-gnu (64-bit)

library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)
library(ggforce)
library(concaveman)

#Load tables
TL_edge <- read.csv("./input/pairwise/TL_edges_R_pairwise.csv", header = TRUE)
#TL_vertice <- read.csv("./input/pairwise/TL_vertices_R_pairwise.csv", header = TRUE)
TL_vertice <- read.csv("./input/pairwise/TL_vertices_R_pairwise_wH1085.csv", header = TRUE)

#Adjust angle
TL_vertice$id <- NA
myleaves <- which(is.na( match(TL_vertice$Name, TL_edge$from)))
nleaves <- length(myleaves)
TL_vertice$id[ myleaves ] <- seq(1:nleaves)
TL_vertice$angle <- 90 - 360 * TL_vertice$id / nleaves


# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
TL_vertice$hjust <- ifelse( TL_vertice$angle < -90, 1, 0)
# flip angle BY to make them readable
TL_vertice$angle <- ifelse(TL_vertice$angle < -90, TL_vertice$angle+180, TL_vertice$angle)


TLgraph <- graph_from_data_frame(TL_edge, vertices=TL_vertice)


#Interaction map of each class, positive, negative, synthetic lethal, epistasis and sign epistasis.

#Positive
Positive <- read.csv("./input/pairwise/Pos_pairwise_interactions.csv", header = TRUE)
from  <-  match(Positive$from, TL_vertice$Name)
to  <-  match(Positive$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=TL_vertice$LogisticPredt)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

#Negative
Negative <- read.csv("./input/pairwise/Neg_pairwise_interactions.csv", header = TRUE)
from  <-  match(Negative$from, TL_vertice$Name)
to  <-  match(Negative$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="#5e3c99", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=TL_vertice$LogisticPredt)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))




#Synthetic_lethal
Synleth <- read.csv("./input/pairwise/SynLeth_pairwise_interactions.csv", header = TRUE)
from  <-  match(Synleth$from, TL_vertice$Name)
to  <-  match(Synleth$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="#1a1110", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=TL_vertice$LogisticPredt)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))


#epistasis and sign epistasis
allepst <- read.csv("./input/pairwise/AllEpst_pairwise_interactions.csv", header = TRUE)
from  <-  match(allepst$from, TL_vertice$Name)
to  <-  match(allepst$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="#a19d94", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=TL_vertice$LogisticPredt)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

#epistasis
epst <- read.csv("./input/pairwise/Epistasis_pairwise_interactions.csv", header = TRUE)
from  <-  match(epst$from, TL_vertice$Name)
to  <-  match(epst$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="#ca0020", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=TL_vertice$LogisticPredt)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))



#Sign epistasis
signepst <- read.csv("./input/pairwise/SignEpst_pairwise_interactions.csv", header = TRUE)
from  <-  match(signepst$from, TL_vertice$Name)
to  <-  match(signepst$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="#0571b0", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=TL_vertice$LogisticPredt)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))



#Map the four H1085 substitutions
#H1085Y
H1085Y <- read.csv("./input/pairwise/H1085Y.csv", header = TRUE)
from  <-  match(H1085Y$from, TL_vertice$Name)
to  <-  match(H1085Y$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, aes(colour=H1085Y), tension = 0.5) +
        scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

#H1085L
H1085L <- read.csv("./input/pairwise/H1085L.csv", header = TRUE)
from  <-  match(H1085L$from, TL_vertice$Name)
to  <-  match(H1085L$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, aes(colour=H1085L), tension = 0.5) +
        scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

#H1085Q
H1085Q <- read.csv("./input/pairwise/H1085Q.csv", header = TRUE)
from  <-  match(H1085Q$from, TL_vertice$Name)
to  <-  match(H1085Q$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, aes(colour=H1085Q), tension = 0.5) +
        scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))


#H1085A
H1085A <- read.csv("./input/pairwise/H1085A.csv", header = TRUE)
from  <-  match(H1085A$from, TL_vertice$Name)
to  <-  match(H1085A$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, aes(colour=H1085A), tension = 0.5) +
        scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))




#Map suppression only
#H1085Y
H1085Y <- read.csv("./input/pairwise/H1085Y_positive.csv", header = TRUE)
from  <-  match(H1085Y$from, TL_vertice$Name)
to  <-  match(H1085Y$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

#H1085L
H1085L <- read.csv("./input/pairwise/H1085L_positive.csv", header = TRUE)
from  <-  match(H1085L$from, TL_vertice$Name)
to  <-  match(H1085L$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

#H1085Q
H1085Q <- read.csv("./input/pairwise/H1085Q_positive.csv", header = TRUE)
from  <-  match(H1085Q$from, TL_vertice$Name)
to  <-  match(H1085Q$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
