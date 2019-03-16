spinach

spinach.model <- drm(SLOPE~DOSE, data = spinach, fct = LL.4())

predict(spinach.model, data.frame(dose=2), se.fit = TRUE)

df_dose <- as.data.frame(2)

predict(spinach.model, df_dose, se.fit = TRUE)
