## ----global_options, include = FALSE-------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE, tidy = TRUE, cache = TRUE)

## ------------------------------------------------------------------------
setwd("E:/RLiu_R/sta141/HW3")
load('HW3_Objects.Rda')
digits = read.csv("digitsTrain.csv")
digits$rownum = rownames(digits)
#reorder the columns of digits: move label and rownum to the first two.
digits = digits[, c("label", "rownum", names(digits)[2:(ncol(digits)-1)])]
set.seed(141)
O = sample(nrow(digits))
digits = digits[O,]

## ----eval = FALSE--------------------------------------------------------
## #1
## Sys.time()
## dEuc = dist(digits[, -c(1:2)], method = "euclidean")
## dEuc = as.matrix(dEuc)
## Sys.time()
## #2
## dMan = dist(digits[, -c(1:2)], method = "manhattan")
## dMan = as.matrix(dMan)
## #3
## dMink = dist(digits[, -c(1:2)], method = "minkowski")
## dMink = as.matrix(dMink)

## ------------------------------------------------------------------------
knn = function(data, test, train, k, dist_matrix, err = FALSE){
  #data:original 5000 by 786 dataframe(the 786th colomn is row indice)
  #test/train: row indice of observations in "data" for test/train
  #k: #of nearest neighbors
  #dist_matrix: 5000 by 5000 dist_matrix, be it Euclidean or Manhattan or else.
  #err: logical, if TRUE, return error rate, if FALSE, return predictions
  knnforone = function(onerowdist, k){
    #onerowdist:dist of a single observation to all the rest observations
    #k:#of nearest neighbors
    onerowdist = onerowdist[order(onerowdist)]
    topk = names(onerowdist[1:k])
    trainlabels = data[train, c("label", "rownum")]
    topklabels = trainlabels[trainlabels$rownum %in% topk, "label"]
    table = table(factor(topklabels, levels = unique(topklabels)))
    onepred = names(which.max(table))
    onepred
  }
  subdist = dist_matrix[test, train]
  preds = apply(subdist, 1, knnforone, k = k)
  error = sum(data[test, 1] != as.numeric(preds))/length(test)
  if (err == FALSE)
    return(preds)
  if (err == TRUE)
    return (error)
}

## ------------------------------------------------------------------------
cvEuc = function(data, fold = 5, k, err, ...){
  #data:original 5000 by 786 dataframe(the first two columns are label and row indexes)
  #folds:how many folds for cross validation
  #err: logical, if TRUE, return error rate, if FALSE, return predictions
  data$group = rep(c(1:fold), each = nrow(data)/fold)
  onefold = function(n, err, ...){
      test = as.numeric(data$rownum[data$group == n])
      train = as.numeric(data$rownum[data$group != n])
      knn(data = digits, test = test, train = train, k = k, dist_matrix = dEuc, err = err)
  }
  overallerror = mean(sapply(c(1:5), onefold, err = TRUE))
  allpreds = data.frame()
  for (i in 1:5){
   predi = onefold(n = i, err = FALSE)
    allpreds = rbind(allpreds, data.frame(rownum = names(predi), pred = predi))
  }
  
  if (err == FALSE)
    return(allpreds)
  if (err == TRUE)
    return (overallerror)
}

cvMan = function(data, fold = 5, k, err, ...){
  #data:original 5000 by 786 dataframe(the 786th colomn is row indice)
  #folds:how many folds for cross validation
  #err: logical, if TRUE, return error rate, if FALSE, return predictions
  data$group = rep(c(1:fold), each = nrow(data)/fold)
  onefold = function(n, err, ...){
      test = as.numeric(data$rownum[data$group == n])
      train = as.numeric(data$rownum[data$group != n])
      knn(data = digits, test = test, train = train, k = k, dist_matrix = dMan, err = err)
  }
  overallerror = mean(sapply(c(1:5), onefold, err = TRUE))
  allpreds = data.frame()
  for (i in 1:5){
   predi = onefold(n = i, err = FALSE)
    allpreds = rbind(allpreds, data.frame(rownum = names(predi), pred = predi))
  }
  
  if (err == FALSE)
    return(allpreds)
  if (err == TRUE)
    return (overallerror)
}

cvMink = function(data, fold = 5, k, err, ...){
  #data:original 5000 by 786 dataframe(the 786th colomn is row indice)
  #folds:how many folds for cross validation
  #err: logical, if TRUE, return error rate, if FALSE, return predictions
  data$group = rep(c(1:fold), each = nrow(data)/fold)
  onefold = function(n, err, ...){
      test = as.numeric(data$rownum[data$group == n])
      train = as.numeric(data$rownum[data$group != n])
      knn(data = digits, test = test, train = train, k = k, dist_matrix = dMink, err = err)
  }
  overallerror = mean(sapply(c(1:5), onefold, err = TRUE))
  allpreds = data.frame()
  for (i in 1:5){
   predi = onefold(n = i, err = FALSE)
    allpreds = rbind(allpreds, data.frame(rownum = names(predi), pred = predi))
  }
  
  if (err == FALSE)
    return(allpreds)
  if (err == TRUE)
    return (overallerror)
}

## ------------------------------------------------------------------------
ks = c(1:10)
errEuc = sapply(ks, function(x) cvEuc(k = x, data = digits, fold = 5, err = TRUE))
errMan = sapply(ks, function(x) cvMan(k = x, data = digits, fold = 5, err = TRUE))
(result = data.frame(k = ks, Euc = errEuc, Man = errMan))
which(result == min(result[,-1]), arr.ind = TRUE)

## ------------------------------------------------------------------------
par("mar" = c(2,2,2,2))
matplot(ks, result[,-1], type = "b", pch = c(17, 19), col = c(1,3), ylab = "Error rate", xlab = "k", main = "Error rates of different metrics along ks")
legend("bottomright", inset = .05, legend = c("Euclidean", "Manhattan"), pch = c(17, 19), col = c(1,3))

## ------------------------------------------------------------------------
#confusion table
# allpreds = cvEuc(data = digits, fold = 5, k = 3, err = FALSE)
# allpreds = allpreds[digits$rownum,]#ensure digits and allpreds have same order
(confusion_matrix = table(digits[, 1], allpreds$pred))

## ------------------------------------------------------------------------
#success rates
(success_rate_rounded = round(sort(diag(confusion_matrix)/table(digits$label), decreasing = TRUE), 3))
success_rate = sort(diag(confusion_matrix)/table(digits$label), decreasing = TRUE)
barplot(success_rate, ylim = c(0.8, 1), xpd = FALSE, ylab = "Success rate", xlab = "digits", main = "Success rate of digits")

## ------------------------------------------------------------------------
#topconfusion
(topconfusion = as.data.frame(apply(confusion_matrix, 1, function(x) names(sort(x[x != 0], decreasing = TRUE)[1:10])), row.names = c(paste0("top", c(1:10)))))

## ------------------------------------------------------------------------
# function getImage provided by Duncan: draw an image
getImage =
function(vals)
{
   matrix(as.integer(vals), 28, 28, byrow = TRUE)
}

draw = function(vals, colors = rgb((255:0)/255, (255:0)/255, (255:0)/255), ...)
{
  if(!is.matrix(vals))
     vals = getImage(vals)
  
  m = t(vals)  # transpose the image
  m = m[,nrow(m):1]  # turn up-side-down

  image(m, col = colors, ..., xaxt = "n", yaxt = "n")
}

## ------------------------------------------------------------------------
findmis = function(n){
  misindexes = O[digits[O, "label"] == n & allpreds[O,"pred"] != n]
  return(misindexes)
}
  
plotmis = function(misindexes){
  par(new = TRUE, mfrow = c(6, 10), mar = c(0, 0, 0, 0))
  tmp = lapply(misindexes, function(x) 
    {
    draw(digits[x, -c(1,2)]) 
    text(0.2, 0.9, allpreds[x, "pred"])
    }
    )
}
tmp = lapply(c(0:9), function(x) plotmis(misindexes = findmis(x)))

## ---- echo = FALSE-------------------------------------------------------
d49 = O[digits[O, "label"] == 4 & allpreds[O,"pred"] == 9]
par(new = TRUE, mfrow = c(4,8), mar = c(0, 0, 0, 0))
apply(digits[d49, ], 1, function(x) {
  draw(x[-c(1,2)]) 
  text(0.2, 0.9, "9")
  })

d44 = O[digits[O, "label"] == 4 & allpreds[O,"pred"] == 4][1:32]
par(new = TRUE, mfrow = c(4,8), mar = c(0, 0, 0, 0))
apply(digits[d44, ], 1, function(x) {
  draw(x[-c(1,2)]) 
  text(0.2, 0.9, "4")
  })

## ---- echo = FALSE-------------------------------------------------------
test = digits[1:1000, ]
train = digits[1001:5000, ]
traingroup = split(x = train, f = train$label)
meantrain = lapply(traingroup, function(x) colMeans(x[, -c(1,2)]))
par(new = TRUE, mfrow = c(2,5), mar = c(0, 0, 0, 0))
tmp = lapply(meantrain, draw)

## ------------------------------------------------------------------------
ann = function(test, train, metric, p, ...){
  # test and train are dataframes:the first column is labels and the second column is row indexes.
  traingroup = split(x = train, f = train$label)
  trainmeans = sapply(traingroup, function(x) colMeans(x[,-c(1,2)]))
  trainmeans = t(trainmeans) # make the digits as rows and columns pixels.
  library(flexclust)
  dist2mean = dist2(x = test[, -c(1,2)], y = trainmeans, method = metric, p = p)
  dist2mean = as.data.frame(dist2mean)
  testsize = nrow(test)
  preds = sapply(1:testsize, function(x) names(dist2mean)[which.min(dist2mean[x,])])
  error = sum(preds != test$label)/testsize
  return(error)
}

## ------------------------------------------------------------------------
cv = function(data, fold, metric, ...){
    testsize = nrow(data)/fold
    data$group = rep(c(1:fold), each = testsize)
    totalerror = 0
    for(i in 1:fold){
      test = data[data$group == i, ]
      train = data[data$group != i, ]
      error = ann(test, train, metric, ...)
      totalerror = totalerror + error
    }
    avg_error = totalerror/fold
    return(avg_error)
}

## ------------------------------------------------------------------------
(error_euclidean = cv(data = digits, fold = 5, metric = "euclidean"))
(error_manhattan = cv(data = digits, fold = 5, metric = "manhattan"))

## ------------------------------------------------------------------------
error_minkowski = cv(data = digits, fold = 5, metric = "minkowski", p = 3)
ps = c(0.1, 0.5, 1, 2, 3, 4, 5, 10, 20, 30)
error_minkowski = sapply(ps, function(x) cv(data = digits, fold = 5, metric = "minkowski", p = x))
plot(x = ps, y = error_minkowski, type = "b", xlab = "p", ylab = "error rate", main = "Error rate vs p for Minkowski")

## ---- echo = FALSE-------------------------------------------------------
ps = seq(2, 4, 0.2)
error_minkowski = sapply(ps, function(x) cv(data = digits, fold = 5, metric = "minkowski", p = x))
plot(x = ps, y = error_minkowski, type = "b", xlab = "p", ylab = "error rate", main = "Error rate vs p for Minkowski")
(error_minkowski_min = min(error_minkowski))
(p = ps[which.min(error_minkowski)])

