# Load helper functions
if (!exists("util.cleanFrame", mode="function")) source("util.R")

# Clean data of NaN values and outliers
BBWDCc = cleanFrame(BBWDC,10)


# Graphs
plot(density(BBWDCc$Qp), lwd = 2, col = "red", main = "Peak Flow Rate Density", xlab = "Flow Rate (m^3/s)")
plot(density(BBWDCc$Tp), lwd = 2, col = "red", main = "Time to Peak Flow Density", xlab = "Time (s)")
plot(density(BBWDCc$Bw), lwd = 2, col = "red", main = "Max Breach Width Density", xlab = "Width (m)")
plot(density(BBWDCc$Tc), lwd = 2, col = "red", main = "Time to Collapse Density", xlab = "Time (s)")
plot(density(BBWDCc$Bwbc), lwd = 2, col = "red", main = "Breach Width Before Collapse", xlab = "Width (m)")
plot(density(BBWDCc$Bwac), lwd = 2, col = "red", main = "Breach Width After Collapse", xlab = "Width (m)")


# Metric Distribution Characteristics
util.distStats(BBWDCc$Qp)
util.distStats(BBWDCc$Tp)
util.distStats(BBWDCc$Bw)
util.distStats(BBWDCc$Tc)
util.distStats(BBWDCc$Bwbc)
util.distStats(BBWDCc$Bwac)


# Performance
# Basic functions
BBWDCc$P1 = perf1(BBWDCc)
BBWDCc$P2 = perf2(BBWDCc)
BBWDCc$P3 = perf3(BBWDCc)

# Get the top five results of each performance measure
(BBWDCc[order(BBWDCc$P1), ])[1:5,]
(BBWDCc[order(BBWDCc$P2), ])[1:5,]
(BBWDCc[order(BBWDCc$P3), ])[1:5,]

# Get the percent error of the top result for each performance measure
expectedValues = list("mn"=0.025, "cw"=12.2, "dh"=15.56, "ibd"=0.013, "ibh"=0.3, "rv"=1557724.8, "tuw"=19, "kd"=33, "tauc"=1.5, "Qp"=3313, "Tp"=3300, "Bw"=96.15, "Bd"=24.7, "Tc"=600, "Bwbc"=0.46, "Bwac"=10.0, "Bdbc"=0.46, "Bdac"=10.0, "P1"=0, "P2"=0, "P3"=0)
data.frame(avgerr(BBWDCc,"P1",expectedValues))
data.frame(avgerr(BBWDCc,"P2",expectedValues))
data.frame(avgerr(BBWDCc,"P3",expectedValues))

# Get the distribution characteristics for the top values of each of the performance measures
util.perfStats(BBWDCc,"P1")
util.perfStats(BBWDCc,"P2")
util.perfStats(BBWDCc,"P3")


# ANOVA
aterm = data.frame(
  "Qp" = anova(aov(Qp ~ mn + cw + dh + ibd + ibh + rv + tuw + kd + tauc, data = BBWDCc))$`Pr(>F)`,
  "Tp" = anova(aov(Tp ~ mn + cw + dh + ibd + ibh + rv + tuw + kd + tauc, data = BBWDCc))$`Pr(>F)`,
  "Bw" = anova(aov(Bw ~ mn + cw + dh + ibd + ibh + rv + tuw + kd + tauc, data = BBWDCc))$`Pr(>F)`,
  "Bd" = anova(aov(Bd ~ mn + cw + dh + ibd + ibh + rv + tuw + kd + tauc, data = BBWDCc))$`Pr(>F)`,
  "Tc" = anova(aov(Tc ~ mn + cw + dh + ibd + ibh + rv + tuw + kd + tauc, data = BBWDCc))$`Pr(>F)`,
  "Bwbc" = anova(aov(Bwbc ~ mn + cw + dh + ibd + ibh + rv + tuw + kd + tauc, data = BBWDCc))$`Pr(>F)`,
  "Bwac" = anova(aov(Bwac ~ mn + cw + dh + ibd + ibh + rv + tuw + kd + tauc, data = BBWDCc))$`Pr(>F)`,
  "Bdbc" = anova(aov(Bdbc ~ mn + cw + dh + ibd + ibh + rv + tuw + kd + tauc, data = BBWDCc))$`Pr(>F)`,
  "Bdac" = anova(aov(Bdac ~ mn + cw + dh + ibd + ibh + rv + tuw + kd + tauc, data = BBWDCc))$`Pr(>F)`,
  row.names = row.names(anova(aov(Qp ~ mn + cw + dh + ibd + ibh + rv + tuw + kd + tauc, data = BBWDCc)))
)


# Sensitivity
library(sensemakr)
parameters = c("mn","cw","dh","ibd","ibh","rv","tuw","kd","tauc")
metrics = c("Qp","Tp","Bw","Bd","Tc","Bwbc","Bwac","Bdbc","Bdac")
results = ""
for (mtrc in metrics) {
  BBWDC.model = lm(as.formula(paste(mtrc,"~ mn + cw + dh + ibd + ibh + rv + tuw + kd + tauc")), data = BBWDCc)
  for (param in parameters) {
    BBWDC.sensitivity = sensemakr(model = BBWDC.model, treatment = param)
    results = paste(results,ovb_minimal_reporting(BBWDC.sensitivity, format = "pure_html"))
  }
}
sink('../Analysis Results/Big Bay/WDCSensitivty.html')
cat(results)
sink()