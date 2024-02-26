# Density distributions for output metrics of all setups
densities = list(
  "Qp" = list("BBDLB" = density(BBDLBc$Qp, bw=460), "BBWDC" = density(BBWDCc$Qp, bw=170), "P1DLB" = density(P1DLBc$Qp, bw=0.3), "P1WDC" = density(P1WDCc$Qp, bw=0.12)),
  "Tp" = list("BBDLB" = density(BBDLBc$Tp, bw=150), "BBWDC" = density(BBWDCc$Tp, bw=160), "P1DLB" = density(P1DLBc$Tp), "P1WDC" = density(P1WDCc$Tp, bw=20)),
  "Bw" = list("BBDLB" = density(BBDLBc$Bw, bw=20), "BBWDC" = density(BBWDCc$Bw, bw=1), "P1DLB" = density(P1DLBc$Bw, bw=1), "P1WDC" = density(P1WDCc$Bw, bw=0.1)),
  "Tc" = list("BBDLB" = density(BBDLBc$Tc, bw=60), "BBWDC" = density(BBWDCc$Tc, bw=200), "P1DLB" = density(P1DLBc$Tc), "P1WDC" = density(P1WDCc$Tc)),
  "Bwbc" = list("BBDLB" = density(BBDLBc$Bwbc, bw=0.1), "BBWDC" = density(BBWDCc$Bwbc), "P1DLB" = density(P1DLBc$Bwbc, bw=0.05), "P1WDC" = density(P1WDCc$Bwbc, bw=0.15)),
  "Bwac" = list("BBDLB" = density(BBDLBc$Bwac, bw=1), "BBWDC" = density(BBWDCc$Bwac, bw=1.8), "P1DLB" = density(P1DLBc$Bwac), "P1WDC" = density(P1WDCc$Bwac, bw=0.2))
)

# Peak Flow Rate
plot(densities$Qp$BBDLB, lwd = 2, col = "red",
     main = "Big Bay Peak Flow Rate Probability Density",
     xlab = "Flow Rate (m^3/s)",
     xlim = c(min(densities$Qp$BBDLB$x, densities$Qp$BBWDC$x),max(densities$Qp$BBDLB$x, densities$Qp$BBWDC$x)),
     ylim = c(0,max(densities$Qp$BBDLB$y, densities$Qp$BBWDC$y))
)
lines(densities$Qp$BBWDC, lty = 2, lwd = 2, col = "blue")
abline(v=3313, lwd=2, col="black")

plot(densities$Qp$P1DLB, lwd = 2, col = "red",
     main = "P1 Peak Flow Rate Probability Density",
     xlab = "Flow Rate (m^3/s)",
     xlim = c(min(densities$Qp$P1DLB$x, densities$Qp$P1WDC$x),max(densities$Qp$P1DLB$x, densities$Qp$P1WDC$x)),
     ylim = c(0,max(densities$Qp$P1DLB$y, densities$Qp$P1WDC$y))
)
lines(densities$Qp$P1WDC, lty = 2, lwd = 2, col = "blue")
abline(v=2.979, lwd=2, col="black")

# Time to Peak Flow Rate
plot(densities$Tp$BBDLB, lwd = 2, col = "red",
     main = "Big Bay Time to Peak Flow Probability Density",
     xlab = "Time (s)",
     xlim = c(min(densities$Tp$BBDLB$x, densities$Tp$BBWDC$x),max(densities$Tp$BBDLB$x, densities$Tp$BBWDC$x)),
     ylim = c(0,max(densities$Tp$BBDLB$y, densities$Tp$BBWDC$y))
)
lines(densities$Tp$BBWDC, lty = 2, lwd = 2, col = "blue")
abline(v=3300, lwd=2, col="black")

plot(densities$Tp$P1DLB, lwd = 2, col = "red",
     main = "P1 Time to Peak Flow Probability Density",
     xlab = "Time (s)",
     xlim = c(min(densities$Tp$P1DLB$x, densities$Tp$P1WDC$x),max(densities$Tp$P1DLB$x, densities$Tp$P1WDC$x, 1560)),
     ylim = c(0,max(densities$Tp$P1DLB$y, densities$Tp$P1WDC$y))
)
lines(densities$Tp$P1WDC, lty = 2, lwd = 2, col = "blue")
abline(v=1560, lwd=2, col="black")

# Max Breach Width
plot(densities$Bw$BBDLB, lwd = 2, col = "red",
     main = "Big Bay Max Breach Width Probability Density",
     xlab = "Width (m)",
     xlim = c(min(densities$Bw$BBDLB$x, densities$Bw$BBWDC$x),max(densities$Bw$BBDLB$x, densities$Bw$BBWDC$x)),
     ylim = c(0,max(densities$Bw$BBDLB$y, densities$Bw$BBWDC$y))
)
lines(densities$Bw$BBWDC, lty = 2, lwd = 2, col = "blue")
abline(v=96.15, lwd=2, col="black")

plot(densities$Bw$P1DLB, lwd = 2, col = "red",
     main = "P1 Max Breach Width Probability Density",
     xlab = "Width (m)",
     xlim = c(min(densities$Bw$P1DLB$x, densities$Bw$P1WDC$x),max(densities$Bw$P1DLB$x, densities$Bw$P1WDC$x)),
     ylim = c(0,max(densities$Bw$P1DLB$y, densities$Bw$P1WDC$y))
)
lines(densities$Bw$P1WDC, lty = 2, lwd = 2, col = "blue")
abline(v=6.46, lwd=2, col="black")

# Max Breach Depth
plot(density(BBWDCc$Bd, bw=0.015), lty=2, lwd=2, col="blue",
     main = "Big Bay Max Breach Depth Probability Density",
     xlab="Depth (m)", xlim=c(15.4,25))
abline(v=24.7, lwd=2, col="black")

plot(density(P1WDCc$Bd, bw=0.02), lty=2, lwd=2, col="blue",
     main = "P1 Max Breach Depth Probability Density",
     xlab="Depth (m)")
abline(v=1.25, lwd=2, col="black")

# Time to Collapse
plot(densities$Tc$BBDLB, lwd = 2, col = "red",
     main = "Big Bay Time to Collapse Probability Density",
     xlab = "Time (s)",
     xlim = c(min(densities$Tc$BBDLB$x, densities$Tc$BBWDC$x),max(densities$Tc$BBDLB$x, densities$Tc$BBWDC$x)),
     ylim = c(0,max(densities$Tc$BBDLB$y, densities$Tc$BBWDC$y))
)
lines(densities$Tc$BBWDC, lty = 2, lwd = 2, col = "blue")
abline(v=600, lwd=2, col="black")

plot(densities$Tc$P1DLB, lwd = 2, col = "red",
     main = "P1 Time to Collapse Probability Density",
     xlab = "Time (s)",
     xlim = c(min(densities$Tc$P1DLB$x, densities$Tc$P1WDC$x),max(densities$Tc$P1DLB$x, densities$Tc$P1WDC$x,840)),
     ylim = c(0,max(densities$Tc$P1DLB$y, densities$Tc$P1WDC$y))
)
lines(densities$Tc$P1WDC, lty = 2, lwd = 2, col = "blue")
abline(v=840, lwd=2, col="black")

# Breach Width Before Collapse
plot(densities$Bwbc$BBDLB, lwd = 2, col = "red",
     main = "Big Bay Breach Width Before Collapse\nProbability Density",
     xlab = "Width (m)",
     xlim = c(min(densities$Bwbc$BBDLB$x, densities$Bwbc$BBWDC$x),max(densities$Bwbc$BBDLB$x, densities$Bwbc$BBWDC$x)),
     ylim = c(0,max(densities$Bwbc$BBDLB$y, densities$Bwbc$BBWDC$y))
)
lines(densities$Bwbc$BBWDC, lty = 2, lwd = 2, col = "blue")
abline(v=0.46, lwd=2, col="black")

plot(densities$Bwbc$P1DLB, lwd = 2, col = "red",
     main = "P1 Breach Width Before Collapse\nProbability Density",
     xlab = "Width (m)",
     xlim = c(min(densities$Bwbc$P1DLB$x, densities$Bwbc$P1WDC$x),max(densities$Bwbc$P1DLB$x, densities$Bwbc$P1WDC$x)),
     ylim = c(0,max(densities$Bwbc$P1DLB$y, densities$Bwbc$P1WDC$y))
)
lines(densities$Bwbc$P1WDC, lty = 2, lwd = 2, col = "blue")
abline(v=1.28, lwd=2, col="black")

# Breach Width After Collapse
plot(densities$Bwac$BBDLB, lwd = 2, col = "red",
     main = "Big Bay Breach Width After Collapse\nProbability Density",
     xlab = "Width (m)",
     xlim = c(min(densities$Bwac$BBDLB$x, densities$Bwac$BBWDC$x,10),max(densities$Bwac$BBDLB$x, densities$Bwac$BBWDC$x)),
     ylim = c(0,max(densities$Bwac$BBDLB$y, densities$Bwac$BBWDC$y))
)
lines(densities$Bwac$BBWDC, lty = 2, lwd = 2, col = "blue")
abline(v=10, lwd=2, col="black")

plot(densities$Bwac$P1DLB, lwd = 2, col = "red",
     main = "P1 Breach Width After Collapse\nProbability Density",
     xlab = "Width (m)",
     xlim = c(min(densities$Bwac$P1DLB$x, densities$Bwac$P1WDC$x),max(densities$Bwac$P1DLB$x, densities$Bwac$P1WDC$x)),
     ylim = c(0,max(densities$Bwac$P1DLB$y, densities$Bwac$P1WDC$y))
)
lines(densities$Bwac$P1WDC, lty = 2, lwd = 2, col = "blue")
abline(v=1.28, lwd=2, col="black")

# Breach Depth Before Collapse
plot(density(BBWDCc$Bdbc, bw=0.5), lty=2, lwd=2, col="blue",
     main = "Big Bay Breach Depth Before Collapse\nProbability Density",
     xlab="Depth (m)", xlim=c(0.46,14))
abline(v=0.46, lwd=2, col="black")

plot(density(P1WDCc$Bdbc, bw=0.03), lty=2, lwd=2, col="blue",
     main = "P1 Breach Depth Before Collapse\nProbability Density",
     xlab="Depth (m)")
abline(v=0.61, lwd=2, col="black")

# Breach Depth After Collapse
plot(density(BBWDCc$Bdac, bw=0.01), lty=2, lwd=2, col="blue",
     main = "Big Bay Breach Depth After Collapse\nProbability Density",
     xlab="Depth (m)", xlim=c(10,15.65))
abline(v=10, lwd=2, col="black")

plot(density(P1WDCc$Bdac), lty=2, lwd=2, col="blue",
     main = "P1 Breach Depth After Collapse\nProbability Density",
     xlab="Depth (m)")
abline(v=1.2, lwd=2, col="black")