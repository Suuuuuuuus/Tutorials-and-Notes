library("pheatmap")
library("viridis")
library("bandle")
library("pRolocdata")

getStockcol()

setStockcol(paste0(getStockcol(), "80"))

# data(package = "pRolocdata")

data(E14TG2aR)
df1 = E14TG2aR

pData(df1)
exprs(df1)
fData(df1)

heatmap(exprs(df1))

table(fData(df1)$markers)
plot2D(df1, fcol = "markers", grid = FALSE)

tagm_map_res = tagmMapTrain(df1)
df1 = tagmMapPredict(df1, tagm_map_res)

printer_size = expr(fData(df1)$tagm.map.probability) - 1

plot2D(df1, fcol = "tagm.map.allocation", cex = pointer_size, grid = FALSE)



library(viridis)
library(pheatmap)
library(bandle)

data("tan2009r1")
df2 = tan2009r1
plot2D(df2, fcol = "markers")
set.seed(1)
lopitsim = sim_dynamic(object = df2, numRep = 6L, numDyn = 100L)
lopitsim$lopitrep
lopitsim$perm1_names

plot2D(lopitsim$lopitrep[[4]], fcol = "markers")

par(mfrow = c(4,3))
gpParams <- lapply(lopitsim$lopitrep, function(x) fitGPmaternPC(x, hyppar = matrix(c(10, 60, 250), nrow = 1)))
plotGPmatern(lopitsim$lopitrep[[1]], gpParams[[1]])

set.seed(1)

K = length(getMarkerClasses(lopitsim$lopitrep[[1]], fcol = "markers"))
dirPrior = diag(rep(1, K)) + matrix(0.0001, nrow = K, ncol = K)
predDirPrior = prior_pred_dir(object = lopitsim$lopitrep[[1]], dirPrior = dirPrior, q = 15)

predDirPrior$meannotAlloc

control = lopitsim$lopitrep[1:3]
treatment = lopitsim$lopitrep[4:6]

bandleres <- bandle(objectCond1 = control, 
                    objectCond2 = treatment,
                    numIter = 20,
                    burnin = 5L,
                    gpParams = gpParams,
                    numChains = 1,
                    dirPrior = dirPrior)

bandleProcess(bandleres)
