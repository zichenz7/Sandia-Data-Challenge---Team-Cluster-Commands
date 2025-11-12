library(tidyverse)

# 1. 读入数据
df_recycled <- read_csv("AllData_PreEDM_Recycled_RowColIDs.csv")
df_virgin   <- read_csv("AllData_PreEDM_Virgin_RowColIDs.csv")



# 重新给 recycled 数据加 scrap 列
df_recycled <- AllData_PreEDM_Recycled_RowColIDs %>%
  mutate(
    ok_ID    = between(B3_DATUM_B_LOC, 0.415, 0.435),
    ok_OD    = between(B3_REF_OD, 0.445, 0.469),
    ok_floor = between(C1_LOC_INSIDE_PLN, 0.049, 0.069),
    ok_lipH  = between(C4_LOC_TOP_PLN, 0.261, 0.281),
    ok_t1    = between(B3_THICK1_WALL, 0.010, 0.017),
    ok_t2    = between(B3_THICK2_WALL, 0.010, 0.017),
    ok_t3    = between(B3_THICK3_WALL, 0.010, 0.017),
    ok_t4    = between(B3_THICK4_WALL, 0.010, 0.017),
    ok_all   = ok_ID & ok_OD & ok_floor & ok_lipH & ok_t1 & ok_t2 & ok_t3 & ok_t4,
    scrap    = (!ok_all | Nonconformity == TRUE)
  )

# 同理给 virgin 数据加 scrap 列
df_virgin <- AllData_PreEDM_Virgin_RowColIDs %>%
  mutate(
    ok_ID    = between(B3_DATUM_B_LOC, 0.415, 0.435),
    ok_OD    = between(B3_REF_OD, 0.445, 0.469),
    ok_floor = between(C1_LOC_INSIDE_PLN, 0.049, 0.069),
    ok_lipH  = between(C4_LOC_TOP_PLN, 0.261, 0.281),
    ok_t1    = between(B3_THICK1_WALL, 0.010, 0.017),
    ok_t2    = between(B3_THICK2_WALL, 0.010, 0.017),
    ok_t3    = between(B3_THICK3_WALL, 0.010, 0.017),
    ok_t4    = between(B3_THICK4_WALL, 0.010, 0.017),
    ok_all   = ok_ID & ok_OD & ok_floor & ok_lipH & ok_t1 & ok_t2 & ok_t3 & ok_t4,
    scrap    = (!ok_all | Nonconformity == TRUE)
  )

df_6 <- df_virgin %>% filter(str_detect(Layout, "6")) %>% filter(RowID <= 6, ColID <= 6)
df_11 <- bind_rows(
  df_recycled,
  df_virgin %>% filter(str_detect(Layout, "11"))
) %>% filter(RowID <= 11, ColID <= 11)

df_6 %>%
  group_by(RowID, ColID) %>%
  summarise(scrap_rate = mean(scrap, na.rm = TRUE)) %>%
  ggplot(aes(ColID, RowID, fill = scrap_rate)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="#E5F6FF", high="#0077FF") +
  scale_y_reverse() +
  coord_equal() +
  labs(title="Scrap Heatmap (6×6 Build Plates)") +
  theme_minimal(base_size = 14)

df_11 %>%
  group_by(RowID, ColID) %>%
  summarise(scrap_rate = mean(scrap, na.rm = TRUE)) %>%
  ggplot(aes(ColID, RowID, fill = scrap_rate)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="#E5F6FF", high="#0077FF") +
  scale_y_reverse() +
  coord_equal() +
  labs(title="Scrap Heatmap (11×11 Build Plates)") +
  theme_minimal(base_size = 14)
