library(tidyverse)

did_feature <- read_csv(paste0(getwd(), "/data/clean/did_parallel_trends_evidence.csv")) |>
  transmute(feature = name, estimate, value = if_else(p_value<0.05,1,0), model = "DiD", note = "slope") 
scm_feature <- read_csv(paste0(getwd(), "/data/clean/scm_weights.csv")) |> 
  transmute(feature=par, value = coef, model = "SCM", note = "weight")
bsts_feature <- read_csv(paste0(getwd(),  "/data/clean/bsts_importance.csv")) |> 
  transmute(feature = name, positive, value = inclusion, model = "BSTS", note = 'inclusion')
gpr_feature <- read_csv(paste0(getwd(),  "/data/clean/gpr_feature_importance.csv")) |>
  transmute(feature = Feature, value = `Relative Importance`, model = "GPR", note = 'inv_lengthscale')

comb |> 
  bind_rows(
    did_feature,
    scm_feature,
    bsts_feature,
    gpr_feature
  ) |>
  mutate(feature = str_to_lower(feature),
         feature = str_replace_all(feature, " ", "_"),
         feature = if_else(str_sub(feature, 1,2)!="y_" & feature != "x_temp", str_c("y_", feature), feature)
  ) |> 
  group_by(model) |> 
  arrange(value) |> 
  ungroup()

chart_features <- 
comb |>   
  mutate(feature = fct_relevel(feature, comb |> filter(model == "SCM") |> pull(feature))) |> 
  ggplot() + 
  geom_bar(
    aes(x = feature, y = value),
    stat = 'identity'
  ) +
  coord_flip() +
  facet_wrap(~model, scales = 'free_x') +
  scale_y_continuous(NULL) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bw()
