#2022-09-19
#This is for producing connection network for positive interactions of each target.
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
TL_edge <- read.csv("./input/Pos_interactions/TL_edges_R_all.csv", header = TRUE)
TL_vertice <- read.csv("./input/Pos_interactions/TL_vertices_R_PosIntrct.csv", header = TRUE)

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


#Interaction heatmaps of each single target

#Y769F
Y769F <- read.csv("./input/Pos_interactions/Y769F.csv", header = TRUE)
from  <-  match(Y769F$from, TL_vertice$Name)
to  <-  match(Y769F$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = Y769F_size >= 0.5, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt, size=Y769F_size, alpha=0.2)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

#Note1: I replaced NA value with 50 to avoid that default NA = grey50(which will leave a grey line that I don't want). So I defined any value greater than 11 will be grey100.
#Note2: ggplot2 uses the "value" corresponds to "from" to label the line, this is why I switched from and to. 


#S713P
S713P <- read.csv("./input/Pos_interactions/S713P.csv", header = TRUE)
from  <-  match(S713P$from, TL_vertice$Name)
to  <-  match(S713P$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = S713P_size >= 0.5, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt, size=S713P_size, alpha=0.2)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

#T834P.
T834P <- read.csv("./input/Pos_interactions/T834P.csv", header = TRUE)
from  <-  match(T834P$from, TL_vertice$Name)
to  <-  match(T834P$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = T834P_size >= 0.5, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt, size=T834P_size, alpha=0.2)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))


#T834A
T834A <- read.csv("./input/Pos_interactions/T834A.csv", header = TRUE)
from  <-  match(T834A$from, TL_vertice$Name)
to  <-  match(T834A$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = T834A_size >= 0.5, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt, size=T834A_size, alpha=0.2)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))


#E1103G
E1103G <- read.csv("./input/Pos_interactions/E1103G.csv", header = TRUE)
from  <-  match(E1103G$from, TL_vertice$Name)
to  <-  match(E1103G$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = E1103G_size >= 0.5, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt, size=E1103G_size, alpha=0.2)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))


#L1101S
L1101S <- read.csv("./input/Pos_interactions/L1101S.csv", header = TRUE)
from  <-  match(L1101S$from, TL_vertice$Name)
to  <-  match(L1101S$to, TL_vertice$Name)
ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = L1101S_size >= 0.5, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt, size=L1101S_size, alpha=0.2)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

#H1085Y
H1085Y <- read.csv("./input/Pos_interactions/H1085Y.csv", header = TRUE)
from  <-  match(H1085Y$from, TL_vertice$Name)
to  <-  match(H1085Y$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = H1085Y_size >= 0.5, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt, size=H1085Y_size, alpha=0.2)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

#H1085L
H1085L <- read.csv("./input/Pos_interactions/H1085L.csv", header = TRUE)
from  <-  match(H1085L$from, TL_vertice$Name)
to  <-  match(H1085L$to, TL_vertice$Name)
ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = H1085L_size >= 0.5, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt, size=H1085L_size, alpha=0.2)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

#F1084I
F1084I <- read.csv("./input/Pos_interactions/F1084I.csv", header = TRUE)
from  <-  match(F1084I$from, TL_vertice$Name)
to  <-  match(F1084I$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = F1084I_size >= 0.5, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt, size=F1084I_size, alpha=0.2)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))


#N1082S
N1082S <- read.csv("./input/Pos_interactions/N1082S.csv", header = TRUE)
from  <-  match(N1082S$from, TL_vertice$Name)
to  <-  match(N1082S$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = N1082S_size >= 0.5, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt, size=N1082S_size, alpha=0.2)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

#M1079V
M1079V <- read.csv("./input/Pos_interactions/M1079V.csv", header = TRUE)
from  <-  match(M1079V$from, TL_vertice$Name)
to  <-  match(M1079V$to, TL_vertice$Name)

ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = M1079V_size >= 0.5, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt, size=M1079V_size, alpha=0.2)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

#Q1078S
Q1078S <- read.csv("./input/Pos_interactions/Q1078S.csv", header = TRUE)
from  <-  match(Q1078S$from, TL_vertice$Name)
to  <-  match(Q1078S$to, TL_vertice$Name)
ggraph(TLgraph, layout ='dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = to, to = from), alpha=0.2, colour="#e66101", tension = 0.5) +
        #scale_edge_colour_gradientn(colours=c("#1a1110","#1a1110", "#5e3c99","#5e3c99","#e66101","#e66101", "grey100", "grey100"),limits=c(-21,51),values=c(0/72,11/72,14/72,20/72,22/72,32/72,33/72,72/72)) +
        geom_node_text(aes(x = x*1.15, y=y*1.15, filter = Q1078S_size >= 0.5, label=name, angle = angle, hjust=hjust, colour=TL_vertice$LogisticPredt), size=2, alpha=1) +
        geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=TL_vertice$LogisticPredt, size=Q1078S_size, alpha=0.2)) +
        scale_colour_manual(values=c("Green4", "grey", "blue", "black", "grey"), breaks = c("GOF", "GOFnLOF", "LOF", "Lethal", "Un")) +
        scale_size_continuous(range = c(0.2,4)) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(c(0,0,0,0),"cm"),
        ) +
        expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
