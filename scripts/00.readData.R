## merge 2017 OAKCODING data with 2018 Sequenom data
## format for analysis
library(openxlsx)
library(Biostrings)
library(magrittr)
library(adegenet)

if(!exists('haversine')) source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/haversine.R')

indsThreshold <- 0.9 # proportion of individuals required for each locus
locThreshold <- 0.7 # proportion of loci required for each individual
includeJacorns <- FALSE
cleanNamesAgain <- FALSE
string.oaks <- c('alba',
                  'macrocarpa',
                  'muehlenbergii',
                  'stellata',
                  'bicolor',
                  'michauxii',
                  'montana',
                  'prinoides')
string.keep <- paste(string.oaks, collapse = '|')

dat.seq <- read.csv('../data/sequenom.dat.cleaned_2018-11-27.csv',
                    as.is = T, row.names = 1)
dat.seq.mapping <- read.xlsx('../data/dat.table.2018-11-25_MH_AH.xlsx', 1)
if(!includeJacorns) dat.seq <- dat.seq[grep('CC', row.names(dat.seq), fixed = T, invert = T), ]
dat.seq.stats <- c(
  samples.orig = dim(dat.seq)[1] - sum(dat.seq.mapping$dropDupe),
  meanMissingLoci.orig = apply(dat.seq, 1, function(x) sum(x == '') / dim(dat.seq)[1]) %>% mean,
  sdMissingLoci.orig = apply(dat.seq, 1, function(x) sum(x == '') / dim(dat.seq)[1]) %>% sd,
  meanMissingInds.orig = apply(dat.seq, 2, function(x) sum(x == '') / dim(dat.seq)[2]) %>% mean,
  sdMissingInds.oSpeciesrig = apply(dat.seq, 2, function(x) sum(x == '') / dim(dat.seq)[2]) %>% sd

  ) # close dat.seq.stats
dat.seq.mapping$reps[is.na(dat.seq.mapping$reps)] <- ''

## prune to sufficiently inclusive data
dat.seq <- dat.seq[ , which(apply(dat.seq, 2, function(x) sum(x != '')) / dim(dat.seq)[1] >= locThreshold)]
dat.seq <- dat.seq[which(apply(dat.seq, 1, function(x) sum(x != '')) / dim(dat.seq)[2] >= indsThreshold), ]
dat.seq <- dat.seq[dat.seq.mapping$codeOrig, ]
#dat.seq.mapping$Species <- factor(dat.seq.mapping$Species)
#levels(dat.seq.mapping$Species) <-
#  c('Quercus macrocarpa',
#     sort(grep('macrocarpa', levels(dat.seq.mapping$Species), value = T, invert = T))
#   )
row.names(dat.seq) <- row.names(dat.seq.mapping) <-
  apply(dat.seq.mapping[c('Species', 'state', 'county', 'specimenCodeUnique')], 1, paste, collapse = '|') %>%
  as.character
checkReps <- lapply(unique(dat.seq.mapping$reps), function(w) {
  a <- dat.seq[dat.seq.mapping$rep == w, ]
  out <- a[, which(!apply(a, 2, function(x) x[1] == x[2])) %>% as.numeric]
  out
  }
) # close lapply
names(checkReps) <- unique(dat.seq.mapping$reps)
checkReps <- checkReps[!names(checkReps) == '']

dat.seq <- dat.seq[!dat.seq.mapping$dropDupe,]
dat.seq.mapping <- dat.seq.mapping[!dat.seq.mapping$dropDupe,]
dat.seq.stats <- c(dat.seq.stats,
  samples.pruned = dim(dat.seq)[1],
  meanMissingLoci.pruned = apply(dat.seq, 1, function(x) sum(x == '') / dim(dat.seq)[1]) %>% mean,
  sdMissingLoci.pruned = apply(dat.seq, 1, function(x) sum(x == '') / dim(dat.seq)[1]) %>% sd,
  meanMissingInds.pruned = apply(dat.seq, 2, function(x) sum(x == '') / dim(dat.seq)[2]) %>% mean,
  sdMissingInds.pruned = apply(dat.seq, 2, function(x) sum(x == '') / dim(dat.seq)[2]) %>% sd
)
dat.seq.mat <- matrix(dat.seq.stats, 5)
row.names(dat.seq.mat) <- gsub('.orig', '', names(dat.seq.stats)[1:5], fixed = T)
colnames(dat.seq.mat) <- c('orig', 'pruned')
## and make a table of genotypes
dat.gen <- df2genind(dat.seq, ncode=1, ploidy = 2)

## Basis of table 2
dat.dists <- sapply(unique(dat.seq.mapping$Species), function(x) {
  haversine(dat.seq.mapping[dat.seq.mapping$Species == x, ],
            lat.long.labels=c('lat', 'long')) %>%
  quantile(probs = c(0.0, 0.5, 1.0), na.rm = T)
  })  %>%
  t
row.names(dat.dists) <- unique(dat.seq.mapping$Species)


## add a function here to find the closest population of each species to
##   some Q. macrocarpa.
dat.mac.minD <- sapply(unique(dat.seq.mapping$Species), function(x) {

  })
