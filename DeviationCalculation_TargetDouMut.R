setwd("/Users/bingbingduan/Documents/lib/Pol2lib/analysis/Details\ of\ each\ lib/lib1678910/MedianNormed_fitness")

dir()
observed_doubles <- read.table("doubleMut_observed_temp1.txt", sep = "\t", header = TRUE, fill = TRUE)

observed_singles <- read.table("singleMut_observed_temp2.txt", sep = "\t", header = TRUE, fill = TRUE)

observed_target <- read.table("Target_singles_normed.txt", sep = "\t", header = TRUE, fill = TRUE)

expected_DouMutFitness = observed_singles + observed_target

write.table(expected_DouMutFitness, "/Users/bingbingduan/Documents/lib/Pol2lib/analysis/Details\ of\ each\ lib/lib1678910/MedianNormed_fitness/expected_DouMutFitness.txt", sep = "\t")

expected_DouMutFitness_temp1 <- read.table("expected_DouMutFitness_temp1.txt", sep = "\t", header = TRUE, fill = TRUE)

DouMutDeviation = observed_doubles - expected_DouMutFitness_temp1

write.table(DouMutDeviation, "/Users/bingbingduan/Documents/lib/Pol2lib/analysis/Details\ of\ each\ lib/lib1678910/MedianNormed_fitness/DouMutDeviation.txt", sep = "\t")
