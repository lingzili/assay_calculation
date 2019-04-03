spinach

spinach.model <- drm(SLOPE ~ DOSE, data = spinach, fct = LL.4())

predict(spinach.model, data.frame(dose = 2), se.fit = TRUE)

df_dose <- as.data.frame(2)

predict(spinach.model, df_dose, se.fit = TRUE)

# Demo plot axis
plot(rnorm(99), bty = "n", axes = FALSE, xlab = "", ylab = "")

box(col = "dodgerblue")

axis(1, col = "dodgerblue", col.ticks = "green", col.axis = "orange", cex.axis = 2)
axis(2, col = "red", col.ticks = "red", col.axis = "red", cex.axis = 0.8)


mtext("Index", side = 1, line = 3, col = "red", cex = 1)
mtext("Value", side = 2, line = 3, col = "purple", cex = 0.8)
