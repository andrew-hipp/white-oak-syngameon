library(pegas)
library(magrittr)

batchStructure <- function(x, fileName, k = 1:10, reps = 3) {
  if(!is.na(folderName)) dir.create(folderName)
  for(i in k) {
    for(j in seq(reps)) {
      write.structure(x, paste(fileName, '_K', i, '_r', j, '.str', sep = ''))
    } # close j
  } # close i
} # close function

write.structure <- function(x, fileName) {
   # x must be a genind object
   x <- as.loci(x)
   write.loci2(x,
              file = fileName,
              loci.sep ="\t",
              quote = FALSE,
              allele.sep ="\t",
              na ="-9\t-9",
              col.names = FALSE,
              chop = "Quercus ")
            }

write.loci2 <- function (x, file = "", loci.sep = " ", allele.sep = "/|",
                          grabPops = TRUE, snpsToIntegers = TRUE, shortenNames = 5,
                          chop = NA, orderPopsByNum = TRUE,
                          ...)
{
    if(!is.na(chop)) row.names(x) <- gsub(chop, '', row.names(x), fixed = TRUE)
    if (allele.sep != "/|") {
        for (i in attr(x, "locicol")) levels(x[[i]]) <- gsub("[/|]",
            allele.sep, levels(x[[i]]))
    }
    x1 <- x
    if(snpsToIntegers) x <- apply(x, 1:2, function(x) {
      x <- gsub('A', 0, x)
      x <- gsub('G', 1, x)
      x <- gsub('C', 2, x)
      x <- gsub('T', 3, x)
    })
    x2 <- x
    if(grabPops) {
      pops <- sapply(strsplit(row.names(x), "|", fixed = T), function(x) x[1])
      pops <- as.factor(pops)
      popNum <- as.integer(pops)
      popMat <- cbind(popNum = as.integer(unique(pops)),
                      popName = unique(as.character(pops))
                    )
      popMat <- popMat[match(names(sort(table(pops))), popMat[, 2]), ]
      x <- cbind(popNum, x)
    }
    popTable <- table(as.character(pops))
    x3 <- x
    if(!is.na(shortenNames)) row.names(x) <- sapply(strsplit(row.names(x), "|", fixed = T), function(x) x[1]) %>%
                                        substr(start = 1, stop = shortenNames) %>%
                                        make.unique(sep = "_")
    x4 <- x
    write.table(x, file = file, sep = loci.sep, ...)
    if(grabPops) write.table(popMat, file = paste(file, 'pops.txt', sep = '.'), sep = loci.sep, row.names = F,
                              ...)
    out <- list(x1 = x1, x2=x2, x3=x3, x4=x4, popTable = popTable)
    out
}
