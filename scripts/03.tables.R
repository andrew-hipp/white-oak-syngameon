## tables
library(adegenet)
library(magrittr)
library(ggplot2)
library(reshape2)

roundDigits <- 3
## how many individuals of each sp have each SNP?
dat.gen.tabSum <- tab(dat.gen, NA.method = 'asis') %>%
  aggregate(by = list(dat.seq.mapping$Species), FUN = sum, na.rm = T)
row.names(dat.gen.tabSum) <- dat.gen.tabSum$Group.1
dat.gen.tabSum$Group.1 <- NULL
dat.gen.tabSum <- dat.gen.tabSum[ ,
  colSums(dat.gen.tabSum) /  apply(tab(dat.gen), 2, function(x) sum(!is.na(x)) * 2) != 1
  ] # gets rid of monomorphic sites

dat.gen.tabProp <- (dat.gen.tabSum /
                     as.integer(table(dat.seq.mapping$Species) * 2)) %>%
                     t %>%
                     round(roundDigits)
colnames(dat.gen.tabProp) <- names(table(dat.seq.mapping$Species))
dat.gen.tabProp[1, ] <- table(dat.seq.mapping$Species)
row.names(dat.gen.tabProp)[1] <- 'N.individuals'
dat.gen.tabProp <- dat.gen.tabProp[ , order(dat.gen.tabProp[1, ], decreasing = T)] %>%
  as.data.frame
dat.gen.tabProp$sums <- apply(dat.gen.tabProp, 1, sum)
dat.gen.tabProp <- dat.gen.tabProp[order(dat.gen.tabProp$sums), ]
write.csv(dat.gen.tabProp, '../out/dat.gen.tabProp.csv')
dat.gen.tabDecisiveness <- data.frame(
  max.sp <- 0
)

dat.gen.tabProp.mat <- as.matrix(dat.gen.tabProp[grep('N.individuals', row.names(dat.gen.tabProp), invert=T),
                                  grep("Quercus", names(dat.gen.tabProp))])

loci.def.TF <- rowSums(dat.gen.tabProp.mat) >= 0.8 &
                       rowSums(dat.gen.tabProp.mat) <= 1.8

loci.def.names <- names(loci.def.TF)[loci.def.TF] %>%
                  strsplit('.', fixed = T) %>%
                  sapply(., function(x) x[1]) %>%
                  unique

loci.handPick.names <- c(
  "locus_25236_45",
  "CL_12923_OAKMOR340_48",
  "locus_1378_30",
  "locus_31722_39",
  "locus_8226_51",
  "locus_26885_29",
  "locus_27743_25",
  "locus_30948_43",
  "locus_3999_44",
  "locus_11302_50",
  "locus_8617_30",
  "CL_49943_OAKMOR249_41",
  "locus_7123_50",
  "CL_55087_OAKMOR340_32",
  "CL_54979_OAKMOR204_37",
  "CL_48165_OAKMOR383_56",
  "locus_12538_49",
  "locus_29214_32",
  "CL_35240_OAKMOR383_32",
  "locus_10977_45"
  )

locus.side.colors <- ifelse(loci.def.TF, 'white', 'white')
locus.side.colors[lapply(loci.handPick.names, grep, x = names(loci.def.TF)) %>% unlist] <- 'red'

pdf('../out/locus.heatmap.pdf', 8.5, 11)
heatmap(dat.gen.tabProp.mat, Rowv=NA, Colv=NA,
        #col = cm.colors(256),
        col = gray(256:0 / 256),
        cexRow = 0.5,
        RowSideColors = locus.side.colors,
        margins=c(5,10),
      scale = 'none')
legend("topleft", legend=c("Alleles from 20 handpicked loci"),
        pch=15, pt.cex = 2, col = c('Red'),
      bty = 'n')
dev.off()
