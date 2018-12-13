## process structure run

dat.str.k8 <- read.table('../ZZZ.DISTRUCT/str_2018-12-03/clumpOut.10runs/K8.out', as.is = T)
dat.str.k8[c('V1', 'V2', 'V3', 'V5')] <- NULL
names(dat.str.k8)[1] <- 'sp'
dat.str.pops <- read.table('../ZZZ.DISTRUCT/quercus.pops', as.is = T)
names(dat.str.pops) <- c('label', 'sp')
dat.str.k8$sp <- dat.str.pops$sp[match(dat.str.k8$sp, dat.str.pops$label)]
dat.str.k8$sp[dat.str.k8$sp %in% c('muehlenbergii', 'prinoides')] <- 'muehl-prin'
for(i in c("bicolor", "macrocarpa", "michauxii", "montana", "muehl-prin", "stellata")) {
  cSums <- apply(dat.str.k8[dat.str.k8$sp == i, ], 2, function(x) sum(as.numeric(x)))
  names(dat.str.k8)[which(cSums == max(cSums, na.rm = T))] <- i
  }
names(dat.str.k8)[grep('V[0123456789]', names(dat.str.k8))] <- c('alba.1', 'alba.2')
dat.str.k8$alba <- dat.str.k8$alba.1 + dat.str.k8$alba.2
strSpp <- c("alba", "bicolor", "macrocarpa", "michauxii", "montana", "muehl-prin", "stellata")
dat.str.hybSummary <- list(
  p05 = sapply(strSpp, function(x) {
    apply(dat.str.k8[strSpp][dat.str.k8$sp == x, ], 2, function(y) sum(y >= 0.05))
    }),
  p10 = sapply(strSpp, function(x) {
    apply(dat.str.k8[strSpp][dat.str.k8$sp == x, ], 2, function(y) sum(y >= 0.10))
    }),
  p15 = sapply(strSpp, function(x) {
    apply(dat.str.k8[strSpp][dat.str.k8$sp == x, ], 2, function(y) sum(y >= 0.15))
    }),
  p20 = sapply(strSpp, function(x) {
    apply(dat.str.k8[strSpp][dat.str.k8$sp == x, ], 2, function(y) sum(y >= 0.20))
    })
  )

lapply(names(dat.str.hybSummary), function(x){
  write.csv(dat.str.hybSummary[[x]], paste('SUPP.hybProp', x, 'csv', sep = '.'))
})
