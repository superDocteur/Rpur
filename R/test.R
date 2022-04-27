source("R/Rpur.R")

Rpu <- charge_rpu("~/RPU/2021-S1.xml", #TRAITES 2021-S2/63_20220211000007.xml",
                  garde_xml = T, traite_diags = T, traite_actes = T)

rpu   <- Rpu@PASSAGES
diags <- Rpu@DIAGS
actes <- Rpu@ACTES

rpu %>% summarise(DMS = mean(DUREE)) %>% as.numeric() / 60
rpu %>%
 filter(!ORIENT %in% c("PSA", "SCAM", "FUGUE")) %>%
 group_by(MCSORT, ORIENT, GRAVITE) %>%
 summarise(c = n(), DMS = as.numeric(mean(DUREE)) / 60) %>%
 arrange(MCSORT, GRAVITE) %>%
 pivot_wider(names_from = GRAVITE, values_from = c(DMS), values_fill = 0) %>%
 print(n = 1000)
