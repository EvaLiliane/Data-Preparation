
# Attempt to read my own csv file
library(pbdDEMO)
init.grid()
file.path <- system.file("/home/evaliliane/Documents/PhD/Codes/NewData/CD4Cat_Children2015-04-20.csv") 
dx <- read.csv.ddmatrix(file.path,
                        sep=",", nrows=412921, ncols=50, header=TRUE,
                        bldim=8, num.rdrs=4, ICTXT=0)
comm.print(dx)
comm.print(dx@Data, all.rank=TRUE)
finalize()

# Trying the original default chunk of code 
library(pbdDEMO, quiet = TRUE)
init.grid(nprow=2,npcol=2)
file.path <- system.file("x.csv", package = "pbdDEMO")
dy <- read.csv.ddmatrix(file.path, header=TRUE,
                        sep=",", nrows=10, ncols=10,
                        num.rdrs=2, ICTXT=3)

comm.print(dy)
comm.print(dy@Data, all.rank=TRUE)

finalize()

## In both cases I am getting these errors & warnings:
# Warning messages:
#   1: In read.csv.ddmatrix(file.path, header = TRUE, sep = ",", nrows = 10,  :
#                             Number of readers supplied is less than number requested; defaulting to1readers
#                           2: In file(file, "rt") :
#                             file("") only supports open = "w+" and open = "w+b": using the former
#                           Error in as.matrix(read.csv(file = file, sep = sep)) : 
#                             error in evaluating the argument 'x' in selecting a method for function 'as.matrix': Error in read.table(file = file, header = header, sep = sep, quote = quote,  : 
#                                                                                                                                        no lines available in input

# ==========================================================================================
# Other functions from the same pbdR package that i think would be useful for the current project.
# ==========================================================================================

library(pmclust)
pmclust(X = NULL, K = 2, MU = NULL,
        algorithm = .PMC.CT$algorithm, RndEM.iter = .PMC.CT$RndEM.iter,
        CONTROL = .PMC.CT$CONTROL, method.own.X = .PMC.CT$method.own.X,
        rank.own.X = .SPMD.CT$rank.source, comm = .SPMD.CT$comm)
pkmeans(X = NULL, K = 2, MU = NULL,
        algorithm = c("kmeans", "kmeans.dmat"),
        CONTROL = .PMC.CT$CONTROL, method.own.X = .PMC.CT$method.own.X,
        rank.own.X = .SPMD.CT$rank.source, comm = .SPMD.CT$comm)

# -------------------------------------------------------------------------------------

library(pbdDMPI)
# Parallel Apply and Lapply Functions (seem imilar to 'lapply')
pbdApply(X, MARGIN, FUN, ..., pbd.mode = c("mw", "spmd"),
         rank.source = .SPMD.CT$rank.root, comm = .SPMD.CT$comm)
pbdLapply(X, FUN, ..., pbd.mode = c("mw", "spmd"),
          rank.source = .SPMD.CT$rank.root, comm = .SPMD.CT$comm,
          bcast = FALSE)
pbdSapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE,
          pbd.mode = c("mw", "spmd"), rank.source = .SPMD.CT$rank.root,
          comm = .SPMD.CT$comm, bcast = FALSE)

library(pbdDMAT)
# Simplified Syntax to Distribute Matrix/vector Across Process Grid
as.ddmatrix(x, bldim = .BLDIM, ICTXT = .ICTXT)

# Distribute/Redistribute matrices across the process grid
distribute(x, bldim = .BLDIM, xCTXT = 0, ICTXT = .ICTXT)
redistribute(dx, bldim = dx@bldim, ICTXT = .ICTXT)

## Produces summary for distributed Matrices
summary(dx)

## Row & Columns bind for distributed Matrices
rbind(..., ICTXT = .ICTXT, deparse.level = 1) 
cbind(..., ICTXT = .ICTXT, deparse.level = 1)



