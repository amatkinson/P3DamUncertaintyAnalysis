# Load helper functions
if (!exists("util.cleanFrame", mode="function")) source("util.R")

# Clean data of NaN values and outliers
BBDLBc = cleanFrame(BBDLB,12)


# Graphs
plot(density(BBDLBc$Qp), lwd = 2, col = "red", main = "Peak Flow Rate Density", xlab = "Flow Rate (m^3/s)")
plot(density(BBDLBc$Tp), lwd = 2, col = "red", main = "Time to Peak Flow Density", xlab = "Time (s)")
plot(density(BBDLBc$Bw), lwd = 2, col = "red", main = "Max Breach Width Density", xlab = "Width (m)")
plot(density(BBDLBc$Tc), lwd = 2, col = "red", main = "Time to Collapse Density", xlab = "Time (s)")
plot(density(BBDLBc$Bwbc), lwd = 2, col = "red", main = "Breach Width Before Collapse", xlab = "Width (m)")
plot(density(BBDLBc$Bwac), lwd = 2, col = "red", main = "Breach Width After Collapse", xlab = "Width (m)")


# Metric Distribution Characteristics
util.distStats(BBDLBc$Qp)
util.distStats(BBDLBc$Tp)
util.distStats(BBDLBc$Bw)
util.distStats(BBDLBc$Tc)
util.distStats(BBDLBc$Bwbc)
util.distStats(BBDLBc$Bwac)


# Performance
# Basic functions
BBDLBc$P1 = perf1(BBDLBc)
BBDLBc$P2 = perf2(BBDLBc)
BBDLBc$P3 = perf3(BBDLBc)

# Get the top five results of each performance measure
(BBDLBc[order(BBDLBc$P1), ])[1:5,]
(BBDLBc[order(BBDLBc$P2), ])[1:5,]
(BBDLBc[order(BBDLBc$P3), ])[1:5,]

# Get the percent error of the top result for each performance measure
expectedValues = list("co"=10000, "kd"=33, "tauc"=1.5, "rv"=1557724.8, "ibd"=0.013, "mn"=0.025, "dh"=15.56, "cw"=12.2, "sd"=0.0003, "po"=0.297, "fat"=0.6, "Qp"=3313, "Tp"=3300, "Bw"=96.15, "Tc"=600, "Bwbc"=0.46, "Bwac"=10.0, "P1"=0, "P2"=0, "P3"=0)
data.frame(avgerr(BBDLBc,"P1",expectedValues))
data.frame(avgerr(BBDLBc,"P2",expectedValues))
data.frame(avgerr(BBDLBc,"P3",expectedValues))

# Get the distribution characteristics for the top values of each of the performance measures
util.perfStats(BBDLBc,"P1")
util.perfStats(BBDLBc,"P2")
util.perfStats(BBDLBc,"P3")


# ANOVA
aterm = data.frame(
  "Qp" = anova(aov(Qp ~ co + kd + tauc + sd + po + fat + dh + cw + rv + ibd + mn, data = BBDLBc))$`Pr(>F)`,
  "Tp" = anova(aov(Tp ~ co + kd + tauc + sd + po + fat + dh + cw + rv + ibd + mn, data = BBDLBc))$`Pr(>F)`,
  "Bw" = anova(aov(Bw ~ co + kd + tauc + sd + po + fat + dh + cw + rv + ibd + mn, data = BBDLBc))$`Pr(>F)`,
  "Tc" = anova(aov(Tc ~ co + kd + tauc + sd + po + fat + dh + cw + rv + ibd + mn, data = BBDLBc))$`Pr(>F)`,
  "Bwbc" = anova(aov(Bwbc ~ co + kd + tauc + sd + po + fat + dh + cw + rv + ibd + mn, data = BBDLBc))$`Pr(>F)`,
  "Bwac" = anova(aov(Bwac ~ co + kd + tauc + sd + po + fat + dh + cw + rv + ibd + mn, data = BBDLBc))$`Pr(>F)`,
  row.names = row.names(anova(aov(Qp ~ co + kd + tauc + sd + po + fat + dh + cw + rv + ibd + mn, data = BBDLBc)))
)


# Sensitivity
library(sensemakr)
parameters = c("co","kd","tauc","sd","po","fat","dh","cw","rv","ibd","mn")
metrics = c("Qp","Tp","Bw","Tc","Bwbc","Bwac")
results = ""
for (mtrc in metrics) {
  BBDLB.model = lm(as.formula(paste(mtrc,"~ co + kd + tauc + sd + po + fat + dh + cw + rv + ibd + mn")), data = BBDLBc)
  for (param in parameters) {
    BBDLB.sensitivity = sensemakr(model = BBDLB.model, treatment = param)
    results = paste(results,ovb_minimal_reporting(BBDLB.sensitivity, format = "pure_html"))
  }
}
sink('../Analysis Results/Big Bay/DLBSensitivty.html')
cat(results)
sink()
