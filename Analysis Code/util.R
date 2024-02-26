# Data cleaning function
util.cleanFrame = function(x, start) {
  # make working copy
  temp = x
  for (i in start:ncol(temp)) {
    # remove NaN
    temp = subset(temp,!is.nan(temp[,i]))
    # remove outliers
    Q1 = quantile(temp[,i], 0.25)
    Q3 = quantile(temp[,i], 0.75)
    IQR = IQR(temp[,i])
    temp = subset(temp, temp[,i] > (Q1 - 1.5*IQR) & temp[,i] < (Q3 + 1.5*IQR))
  }
  # return result
  temp
}

# Distribution characterization function
util.distStats = function(x) {
  # Mode of a continuous distribution
  continuousMode = function(x) {
    dx = density(x)
    dx$x[which(dx$y == max(dx$y))]
  }
  # print output
  paste("range:[",min(x),",",max(x),"]","mean:",mean(x),"median:",median(x),"mode:",continuousMode(x),"sd:",sd(x))
}

# Distance function using logarithms instead of differences
util.dist = function(...) sqrt(Reduce('+',lapply(list(...),function(x) log(x)^2)))

# Performance function 1
util.perf1 = function(x) {
  temp = c(0)
  for (i in 1:nrow(x)) {
    temp[i] = util.dist(x$Qp[i]/2.979, x$Tp[i]/1560, x$Bw[i]/6.46)
  }
  temp
}

# Performance function 2
util.perf2 = function(x) {
  temp = c(0)
  for (i in 1:nrow(x)) {
    temp[i] = util.dist(x$Tc[i]/840, x$Bwbc[i]/1.28, x$Bwac[i]/1.28)
  }
  temp
}

# Performance function 3
util.perf3 = function(x) {
  temp = c(0)
  for (i in 1:nrow(x)) {
    temp[i] = util.dist(x$Qp[i]/2.979, x$Tp[i]/1560, x$Bw[i]/6.46, x$Tc[i]/840,
                   x$Bwbc[i]/1.28, x$Bwac[i]/1.28)
  }
  temp
}

# Percent error of parameters and metrics
util.percentError = function(x,i, expected) {
  lapply(mapply(function(a,b) abs(b-a)/b*100,expected,x[which(x[,i] == min(x[,i])),]),function(n) formatC(signif(n, digits=3), digits=3, format="fg", flag="#"))
}

# Distribution characterization function for performance measure
util.perfStats = function(x,i) {
  best = min(x[,i])
  worst = max(x[,i])
  x2 = x[which(x[,i] < 0.25*(worst-best)+best),]
  tmp = data.frame(lapply(x, sd), row.names = "OrgSd")
  tmp = rbind(tmp, data.frame(as.list(mapply(function(a,b) a-b, lapply(x, max), lapply(x, min))), row.names = "OrgRange"))
  tmp = rbind(tmp, data.frame(lapply(x2, sd), row.names = "TopSd"))
  tmp = rbind(tmp, data.frame(as.list(mapply(function(a,b) a-b, lapply(x2, max), lapply(x2, min))), row.names = "TopRange"))
  tmp = rbind(tmp, data.frame(as.list(mapply(function(x,y) x/y*100, tmp["TopSd",], tmp["OrgSd",])), row.names = "SdPer"))
  rbind(tmp, data.frame(as.list(mapply(function(x,y) x/y*100, tmp["TopRange",], tmp["OrgRange",])), row.names = "RangePer"))
}