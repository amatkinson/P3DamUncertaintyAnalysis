# Load helper functions
if (!exists("util.cleanFrame", mode="function")) source("util.R")

# Clean data of NaN values and outliers
P1DLBc = util.cleanFrame(P1DLB,13)


# Graphs
plot(density(P1DLBc$Qp), lwd = 2, col = "red", main = "Peak Flow Rate Density", xlab = "Flow Rate (m^3/s)")
plot(density(P1DLBc$Tp), lwd = 2, col = "red", main = "Time to Peak Flow Density", xlab = "Time (s)")
plot(density(P1DLBc$Bw), lwd = 2, col = "red", main = "Max Breach Width Density", xlab = "Width (m)")
plot(density(P1DLBc$Tc), lwd = 2, col = "red", main = "Time to Collapse Density", xlab = "Time (s)")
plot(density(P1DLBc$Bwbc), lwd = 2, col = "red", main = "Breach Width Before Collapse", xlab = "Width (m)")
plot(density(P1DLBc$Bwac), lwd = 2, col = "red", main = "Breach Width After Collapse", xlab = "Width (m)")


# Metric Distribution Characteristics
util.distStats(P1DLBc$Qp)
util.distStats(P1DLBc$Tp)
util.distStats(P1DLBc$Bw)
util.distStats(P1DLBc$Tc)
util.distStats(P1DLBc$Bwbc)
util.distStats(P1DLBc$Bwac)


# Performance
# Base functions
P1DLBc$P1 = util.perf1(P1DLBc)
P1DLBc$P2 = util.perf2(P1DLBc)
P1DLBc$P3 = util.perf3(P1DLBc)

# Get the top five results of each performance measure
(P1DLBc[order(P1DLBc$P1), ])[1:5,]
(P1DLBc[order(P1DLBc$P2), ])[1:5,]
(P1DLBc[order(P1DLBc$P3), ])[1:5,]

# Get the percent error of the top result for each performance measure
expectedValues = list("co"=5000, "kd"=120, "tauc"=0.144, "se"=0.912, "ibh"=1.0, "mn"=0.025, "sd"=0.00013, "po"=0.34, "fat"=0.625, "cc"=0.07, "dh"=1.3, "cw"=1.98, "Qp"=2.979, "Tp"=1560, "Bw"=6.46, "Tc"=840, "Bwbc"=1.28, "Bwac"=1.28, "P1"=0, "P2"=0, "P3"=0)
data.frame(util.percentError(P1DLBc,"P1",expectedValues))
data.frame(util.percentError(P1DLBc,"P2",expectedValues))
data.frame(util.percentError(P1DLBc,"P3",expectedValues))

# Get the distribution characteristics for the top values of each of the performance measures
util.perfStats(P1DLBc,"P1")
util.perfStats(P1DLBc,"P2")
util.perfStats(P1DLBc,"P3")


# ANOVA
aterm = data.frame(
  "Qp" = anova(aov(Qp ~ cc + dh + cw + co + kd + tauc + sd + po + fat + se + ibh + mn, data = P1DLBc))$`Pr(>F)`,
  "Tp" = anova(aov(Tp ~ cc + dh + cw + co + kd + tauc + sd + po + fat + se + ibh + mn, data = P1DLBc))$`Pr(>F)`,
  "Bw" = anova(aov(Bw ~ cc + dh + cw + co + kd + tauc + sd + po + fat + se + ibh + mn, data = P1DLBc))$`Pr(>F)`,
  "Tc" = anova(aov(Tc ~ cc + dh + cw + co + kd + tauc + sd + po + fat + se + ibh + mn, data = P1DLBc))$`Pr(>F)`,
  "Bwbc" = anova(aov(Bwbc ~ cc + dh + cw + co + kd + tauc + sd + po + fat + se + ibh + mn, data = P1DLBc))$`Pr(>F)`,
  "Bwac" = anova(aov(Bwac ~ cc + dh + cw + co + kd + tauc + sd + po + fat + se + ibh + mn, data = P1DLBc))$`Pr(>F)`,
  row.names = row.names(anova(aov(Qp ~ cc + dh + cw + co + kd + tauc + sd + po + fat + se + ibh + mn, data = P1DLBc)))
)


# Sensitivity
library(sensemakr)
parameters = c("cc","dh","cw","co","kd","tauc","sd","po","fat","se","ibh","mn")
metrics = c("Qp","Tp","Bw","Tc","Bwbc","Bwac")
results = ""
for (mtrc in metrics) {
  P1DLB.model = lm(as.formula(paste(mtrc,"~ cc + dh + cw + co + kd + tauc + sd + po + fat + se + ibh + mn")), data = P1DLBc)
  for (param in parameters) {
    P1DLB.sensitivity = sensemakr(model = P1DLB.model, treatment = param)
    results = paste(results,ovb_minimal_reporting(P1DLB.sensitivity, format = "pure_html"))
  }
}
sink('../Analysis Results/P1/DLBSensitivty.html')
cat(results)
sink()