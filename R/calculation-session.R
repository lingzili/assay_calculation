# Plot calibrator curve
plot(Ins_Cal$Avg, Ins_Cal$Insulin,
  col = "darkblue",
  xlim = c(0, 2.5), ylim = c(0, 7),
  xlab = "Calibrator OD450", ylab = "insulin µg/L"
)

lines(spline(Ins_Cal$Avg, Ins_Cal$Insulin), col = "red", lwd = 3)
