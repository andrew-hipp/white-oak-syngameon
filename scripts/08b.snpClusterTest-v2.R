## estimate probability of clusters of SNPs on genome
library(openxlsx)
library(magrittr)

nreps = 10000
nrads = 59 # number of RAD loci to simulate

snps.mapped <- read.xlsx('../data/SUPPLEMENT.snp.mappingTable.xlsx', 1, rowNames = TRUE)
snps.mapped$Pseudomolecule.ID <-
  gsub('[a-zA-Z_]', '', snps.mapped$sseqid) %>% as.integer

RAD.mapped <- read.delim('../data/blastN_oaksall_vs_12chromos_percentID80_eval10-5_alnpercent80_besthit_andrew.txt', as.is = T)
RAD.mapped <- RAD.mapped[grep('Chr', RAD.mapped$Oak_chromosome_ID), ]
RAD.mapped$Pseudomolecule.ID <- gsub('[a-zA-Z_]', '', RAD.mapped$Oak_chromosome_ID) %>% as.integer
out <- data.frame(NumChroms = rep(NA, nreps),
                  Dists.Under.10K = rep(NA, nreps),
                  Dists.Under.1M = rep(NA, nreps),
                  meanDist = rep(NA, nreps))
for(i in 1:nreps) {
  temp <- RAD.mapped[sample(1:dim(RAD.mapped)[1], nrads), ]
  temp.split <- split(temp, temp$Pseudomolecule.ID, drop = FALSE)
  tempPos <- lapply(temp.split, function(x) sort(x$aln_start))
  tempIntervals <- lapply(tempPos, diff) %>% unlist
  out[i, 'NumChroms'] <- length(temp.split)
  out[i, 'Dists.Under.10K'] <- sum(tempIntervals < 10000)
  out[i, 'Dists.Under.1M'] <- sum(tempIntervals < 1000000)
  out[i, 'meanDist'] <- mean(tempIntervals)
} # close i
  outP <- c(
    D10Kp = (sum(out$Dists.Under.10K >= sum(as.numeric(snps.mapped$interval) < 10000, na.rm = T))) / nreps,
    D1Mp = (sum(out$Dists.Under.1M >= sum(as.numeric(snps.mapped$interval) < 1000000, na.rm = T))) / nreps,
    mDp = (sum(out$meanDist) <= mean(as.numeric(snps.mapped$interval), na.rm = T)) / nreps
  )
