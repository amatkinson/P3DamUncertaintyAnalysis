# Load helper functions
if (!exists("util.cleanFrame", mode="function")) source("util.R")

# Clean data of NaN values and outliers
P1WDCc = cleanFrame(P1WDC,10)


# Graphs
plot(density(P1WDCc$Qp), lwd = 2, col = "red", main = "Peak Flow Rate Density", xlab = "Flow Rate (m^3/s)")
plot(density(P1WDCc$Tp), lwd = 2, col = "red", main = "Time to Peak Flow Density", xlab = "Time (s)")
plot(density(P1WDCc$Bw), lwd = 2, col = "red", main = "Max Breach Width Density", xlab = "Width (m)")
plot(density(P1WDCc$Tc), lwd = 2, col = "red", main = "Time to Collapse Density", xlab = "Time (s)")
plot(density(P1WDCc$Bwbc), lwd = 2, col = "red", main = "Breach Width Before Collapse", xlab = "Width (m)")
plot(density(P1WDCc$Bwac), lwd = 2, col = "red", main = "Breach Width After Collapse", xlab = "Width (m)")


# Metric Distribution Characteristics
util.distStats(P1WDCc$Qp)
util.distStats(P1WDCc$Tp)
util.distStats(P1WDCc$Bw)
util.distStats(P1WDCc$Tc)
util.distStats(P1WDCc$Bwbc)
util.distStats(P1WDCc$Bwac)


# Performance
# Base functions
P1WDCc$P1 = util.perf1(P1WDCc)
P1WDCc$P2 = util.perf2(P1WDCc)
P1WDCc$P3 = util.perf3(P1WDCc)

# Get the top five results of each performance measure
(P1WDCc[order(P1WDCc$P1), ])[1:5,]
(P1WDCc[order(P1WDCc$P2), ])[1:5,]
(P1WDCc[order(P1WDCc$P3), ])[1:5,]

# Get the percent error of the top result for each performance measure
expectedValues = list("cw"=1.98, "dh"=1.3, "ibh"=1.0, "se"=0.912, "kd"=120, "st"=13020, "tuw"=18.73, "tauc"=0.144, "mn"=0.025, "Qp"=2.979, "Tp"=1560, "Bw"=6.46, "Bd"=1.25, "Tc"=840, "Bwbc"=1.28, "Bwac"=1.28, "Bdbc"=0.61, "Bdac"=1.2, "P1"=0, "P2"=0, "P3"=0)
data.frame(avgerr(P1WDCc,"P1",expectedValues))
data.frame(avgerr(P1WDCc,"P2",expectedValues))
data.frame(avgerr(P1WDCc,"P3",expectedValues))

# Get the distribution characteristics for the top values of each of the performance measures
util.perfStats(P1WDCc,"P1")
util.perfStats(P1WDCc,"P2")
util.perfStats(P1WDCc,"P3")


# ANOVA
aterm = data.frame(
  "Qp" = anova(aov(Qp ~ st + cw + dh + tuw + kd + tauc + ibh + se + mn, data = P1WDCc))$`Pr(>F)`,
  "Tp" = anova(aov(Tp ~ st + cw + dh + tuw + kd + tauc + ibh + se + mn, data = P1WDCc))$`Pr(>F)`,
  "Bw" = anova(aov(Bw ~ st + cw + dh + tuw + kd + tauc + ibh + se + mn, data = P1WDCc))$`Pr(>F)`,
  "Bd" = anova(aov(Bd ~ st + cw + dh + tuw + kd + tauc + ibh + se + mn, data = P1WDCc))$`Pr(>F)`,
  "Tc" = anova(aov(Tc ~ st + cw + dh + tuw + kd + tauc + ibh + se + mn, data = P1WDCc))$`Pr(>F)`,
  "Bwbc" = anova(aov(Bwbc ~ st + cw + dh + tuw + kd + tauc + ibh + se + mn, data = P1WDCc))$`Pr(>F)`,
  "Bwac" = anova(aov(Bwac ~ st + cw + dh + tuw + kd + tauc + ibh + se + mn, data = P1WDCc))$`Pr(>F)`,
  "Bdbc" = anova(aov(Bdbc ~ st + cw + dh + tuw + kd + tauc + ibh + se + mn, data = P1WDCc))$`Pr(>F)`,
  "Bdac" = anova(aov(Bdac ~ st + cw + dh + tuw + kd + tauc + ibh + se + mn, data = P1WDCc))$`Pr(>F)`,
  row.names = row.names(anova(aov(Qp ~ st + cw + dh + tuw + kd + tauc + ibh + se + mn, data = P1WDCc)))
)


# Sensitivity
library(sensemakr)
parameters = c("st","cw","dh","tuw","kd","tauc","ibh","se","mn")
metrics = c("Qp","Tp","Bw","Bd","Tc","Bwbc","Bwac","Bdbc","Bdac")
results = ""
for (mtrc in metrics) {
  P1WDC.model = lm(as.formula(paste(mtrc,"~ st + cw + dh + tuw + kd + tauc + ibh + se + mn")), data = P1WDCc)
  for (param in parameters) {
    P1WDC.sensitivity = sensemakr(model = P1WDC.model, treatment = param)
    results = paste(results,ovb_minimal_reporting(P1WDC.sensitivity, format = "pure_html"))
  }
}
sink('../Analysis Results/P1/WDCSensitivty.html')
cat(results)
sink()