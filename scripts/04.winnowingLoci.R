library(magrittr)
library(adegenet)
library(phangorn)

dat.genList = list(
  dat.gen,
  df2genind(dat.seq[, loci.def.names], ncode=1, ploidy = 2),
  df2genind(dat.seq[, loci.handPick.names], ncode=1, ploidy = 2)
)
names(dat.genList) <- paste('n', c(dim(dat.seq)[2], length(loci.def.names), length(loci.handPick.names)), sep = '')

dat.def.tab <- tab(dat.genList[[1]], NA.method = 'mean')
dat.def.dist <- dist(dat.def.tab)

dat.handPick.tab <- tab(dat.genList[[3]], NA.method = 'mean')
dat.handPick.dist <- dist(dat.handPick.tab)

pdf('../out/Fig3.upgma.pdf', 8.5, 11)
par(mar = rep(0,4))
layout(matrix(1:2, 1))
  plot(upgma(dat.def.dist), cex = 0.3, tip.color = cbbPalette[dat.spp])
  legend('topleft', 'A', bty = 'n')
  plot(upgma(dat.handPick.dist), cex = 0.3, tip.color = cbbPalette[dat.spp])
  legend('topleft', 'B', bty = 'n')
dev.off()
