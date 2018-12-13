source('../scripts/99.writeStructure.R')

k = 1:12
reps = 1:5
write.structure(dat.genList$n75, '../ZZZ.STRUCTURE/str1/str.str')

## WRITE SHELL SCRIPT TO EXECUTE STRUCTURE FILES
# -L for numloci
# -i for input file
# -o for output file
# currently set up to fork processes using '&'

shScript.structure <- "#!/bin/sh"
for(i in k) {
  for(j in reps) {
    shScript.structure <- c(shScript.structure,
                            paste('structure -i str.str',
                               '-L 75',
                               '-K', i,
                               '-o', paste('../../ZZZ.DISTRUCT/str_2018-12-03/str_K', i, '_r', j,
                               ' &', sep = '')
                             )
                           )
  }
}
shScript.structure <- c(shScript.structure, c('wait', 'echo ALL DONE!!'))
writeLines(shScript.structure, '../ZZZ.STRUCTURE/str1/structureRunEmpirical_1-5.sh')
