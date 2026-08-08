m(list=ls())
gc()
graphics.off()
getwd()
library(tidyverse)
library(stringr)
library(readxl)
library(ggplot2)




######################################################################################
######################## DATA MANIPULATION #######################################
########## VALIDATIONS FOR LINE F - PEAK WEEKDAY TRAFFIC ####################################

val <- read.csv(
  "validaciones.csv",
  header = TRUE,
  sep = ";",
  fileEncoding = "Latin1"
)
colnames(val)


val_f <- val %>%
  filter(str_detect(Línea, "Zona F"))

val_f_peak <- val_f %>%
  filter(
    Intervalo >= "06:15", 
    Intervalo <="07:15")
head(val_f_peak)

val_f_day <- val_f_peak %>%
  select(`X05.03.2025`,
         Estación)

val_f_day <- val_f_day %>%
  mutate(`X05.03.2025` = as.numeric(`X05.03.2025`))

val_total <- val_f_day %>%
  group_by(Estación) %>%
  summarise(
    total = sum(`X05.03.2025`, na.rm = TRUE),
    .groups = "drop"
  )


############### EXITS FOR LINE F - PEAK WEEKDAY TRAFFIC ####################################

sal <- read.csv(
  "salidas.csv",
  header = TRUE,
  sep = ";",
  fileEncoding = "Latin1"
)

colnames(sal)

sal_f <- sal %>%
  filter(str_detect(Linea, "Zona F"))

sal_f_peak <- sal_f %>%
  filter(
    Intervalo >= "06:15", 
    Intervalo <="07:15")
head(sal_f_peak)

sal_f_day <- sal_f_peak %>%
  select(X_2025_03_05,
         Estacion)

sal_f_day <- sal_f_day %>%
  mutate(X_2025_03_05 = as.numeric(X_2025_03_05))

sal_total <- sal_f_day %>%
  group_by(Estacion) %>%
  summarise(
    total = sum(X_2025_03_05, na.rm = TRUE),
    .groups = "drop"
  )

#Combining Names
val2 <- val_total %>%
  mutate(key = str_squish(str_replace(`Estación`, "\\)\\s*", ") ")))

sal2 <- sal_total %>%
  mutate(key = str_squish(str_replace(Estacion,  "\\)\\s*", ") ")))

#Mutate Tables
combined <- val2 %>%
  inner_join(sal2, by = "key", suffix = c("_val", "_sal")) %>%
  transmute(
    Estación  = key,
    total_val = total_val,
    total_sal = total_sal
  )

combined <- combined %>%
  mutate(
    total_val = as.integer(total_val),
    total_sal = as.integer(total_sal)
  )

combined <- combined %>%
  mutate(
    ratio_board_alight = if_else(
      total_sal > 0,
      total_val / total_sal,
      NA_real_
    )
  )

view(combined)
#Export
write.csv(combined, "combined_results.csv", row.names = FALSE)



#Visualization
ggplot(combined, aes(x = Estación, y = ratio_board_alight)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    title = "Boarding to Alighting Ratio per Station",
    x = "Station",
    y = "Boarding / Alighting Ratio"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#Graphs
combined_long <- combined %>%
  pivot_longer(
    cols = c(total_val, total_sal),
    names_to = "tipo",
    values_to = "total"
  )

# Plot
combined_long <- combined_long %>%
  mutate(
    tipo = recode(
      tipo,
      total_val = "Entries",
      total_sal = "Exits"
    )
  )


ggplot(combined_long, aes(x = Estación, y = total, fill = tipo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Trunk Line F: Entries & Exits per Station",
    x = "Station",
    y = "Total",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Entries only
p_entries <- combined_long %>%
  filter(tipo == "Entries") %>%
  ggplot(aes(x = Estación, y = total)) +
  geom_col() +
  labs(title = "Entries per Station", x = "Station", y = "Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exits only
p_exits <- combined_long %>%
  filter(tipo == "Exits") %>%
  ggplot(aes(x = Estación, y = total)) +
  geom_col() +
  labs(title = "Exits per Station", x = "Station", y = "Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_entries
p_exits
