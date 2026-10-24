


DIR_MODULE = "LongTermImpacts"


ProcScen <- function(.df){
  .df %>% filter(scenario %in% c(Scenario_Storage, Scenario_NoStorage)) |>
    filter(year %in% c(2021,seq(2030, 2100, 10))) |>
    group_by_at(vars(-scenario, -value)) %>%
    mutate(value = value - value[scenario == Scenario_NoStorage]) %>%
    filter(scenario != Scenario_NoStorage)
}


## theme1 ----
theme1 <- theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
                strip.background = element_rect(fill="grey99"),
                strip.text = element_text(size = 12),
                axis.text.x.bottom = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                panel.grid = element_blank(),
                panel.spacing.y = unit(0.5, "lines"),
                panel.spacing.x = unit(0.5, "lines"))

## Land ----
PLand %>%
  ProcScen() |>
  mutate(value = value) %>% #filter(region == "World") |>
  ggplot() +   facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = land), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Million hectare", fill = "Land") +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  #scale_fill_brewer(palette = "Spectral", direction = -1) +
  theme_bw() + theme0 + theme1  -> A1; A1

## Labor ----
Plabor %>%
  # filter(region == "World") %>% filter(year == 2100) |> summarize(value = sum(value))
  ProcScen() %>%
  ggplot + facet_wrap(~region, scale = "free_y", nrow = 1) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Million people", fill = "Sector") +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1 -> A2; A2

## Capital ----
Pcapital %>%
  mutate(value = value) %>%
  ProcScen() %>%
  ggplot + facet_wrap(~region, scale = "free_y", nrow = 1) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Billion 2021$") +
  scale_fill_brewer(palette = "Set2", name = "Sector", direction = 1) +
  theme_bw() + theme0 + theme1 -> A3; A3


## Water ----
Pwater %>%
  ProcScen() |>
  mutate(value = value ) %>%
  ggplot() +   facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Billion cubic meter") +
  scale_fill_brewer(palette = "Set2",
                    name = "Sector", direction = 1) +
  theme_bw() + theme0 + theme1 -> A4; A4

## Fertilizer ----
Pfertilizer %>%
  ProcScen() %>%
  ggplot + facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Million tonne N") +
  scale_fill_brewer(palette = "Set2",
                    name = "Sector", direction = 1) +
  theme_bw() + theme0 + theme1 -> A5; A5

## Ag SUA ----


AgElement_SUA |>
  mutate(DS = if_else(element %in% ElEMSupply, "Supply", "Demand")) %>%
  mutate(value = if_else(!element %in% ElEMSupply, -value, value)) %>%
  mutate(element = factor(element, levels = ELEMLevel, labels = ELEMLabel)) |>
  filter(grepl("Other use", element), region == "World" ) |>
  filter(scenario %in% c(Scenario_Storage, Scenario_NoStorage)) |>
  #filter(year %in% c(2021,seq(2030, 2100, 10))) |>
  group_by_at(vars(-scenario, -value)) %>%
  mutate(value = value - value[scenario == Scenario_NoStorage]) %>%
  filter(scenario != Scenario_NoStorage) |>
  group_by(sector) |> summarize(value = mean(value))


c(brewer.pal(n = length(ELEMAll), name = "Spectral")[1:length(ElEMSupply)],
  brewer.pal(n = length(ELEMAll), name = "Spectral")[length(ElEMSupply)+1:length(ELEMDemand)]
) -> ColUpdate

ss = "Grains"

AgElement_SUA |>
  mutate(DS = if_else(element %in% ElEMSupply, "Supply", "Demand")) %>%
  mutate(value = if_else(!element %in% ElEMSupply, -value, value)) %>%
  mutate(element = factor(element, levels = ELEMLevel, labels = ELEMLabel)) |>
  filter(sector == ss) |> select(-DS) |>
  filter(!grepl("stock", element)) |>
  spread(element, value, fill = 0) |>
  gather(element, value, -scenario:-sector) |>
  ProcScen() %>%
  mutate(value = value) %>%
  ggplot +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = element), stat = "identity", position = "stack",
           color = "black") +
  geom_point(
    data = AgElement_SUA |>
      filter(scenario %in% c(Scenario_Storage, Scenario_NoStorage)) |>
      filter(year %in% c(2021,seq(2030, 2100, 10))) |>
      filter(grepl("stock", element), sector == ss) |>
      spread(element, value) |>
      mutate(`Stock variation` = `Closing stocks` - `Opening stocks`),
    aes(x = year, y = -`Stock variation`), color = "black", size = 1) +
  labs(x = "Year", y = "Million tonne", fill = "Element") +
  scale_fill_manual(values = c("Supply: Opening stocks" = ColUpdate[1],
                               "Supply: Production" = ColUpdate[2],
                               "Supply: Import" = ColUpdate[3],
                               "Demand: Export" = ColUpdate[4],
                               "Demand: Food" = ColUpdate[5],
                               "Demand: Feed" = ColUpdate[6],
                               "Demand: Bioenergy" = ColUpdate[7],
                               "Demand: Other use" = ColUpdate[8],
                               "Demand: Stock loss" = ColUpdate[9],
                               "Demand: Closing stocks" = ColUpdate[10]),
                    breaks = ELEMLabel1) +
  theme_bw() + theme0 + theme1 -> A6p1; A6p1


ss = "Oil crops"

AgElement_SUA |>
  mutate(DS = if_else(element %in% ElEMSupply, "Supply", "Demand")) %>%
  mutate(value = if_else(!element %in% ElEMSupply, -value, value)) %>%
  mutate(element = factor(element, levels = ELEMLevel, labels = ELEMLabel)) |>
  filter(sector == ss) |> select(-DS) |>
  filter(!grepl("stock", element)) |>
  spread(element, value, fill = 0) |>
  gather(element, value, -scenario:-sector) |>
  ProcScen() %>%
  mutate(value = value) %>%
  ggplot +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = element), stat = "identity", position = "stack",
           color = "black") +
  geom_point(
    data = AgElement_SUA |>
      filter(scenario %in% c(Scenario_Storage, Scenario_NoStorage)) |>
      filter(year %in% c(2021,seq(2030, 2100, 10))) |>
      filter(grepl("stock", element), sector == ss) |>
      spread(element, value) |>
      mutate(`Stock variation` = `Closing stocks` - `Opening stocks`),
    aes(x = year, y = -`Stock variation`), color = "black", size = 1) +
  labs(x = "Year", y = "Million tonne", fill = "Element") +
  scale_fill_manual(values = c("Supply: Opening stocks" = ColUpdate[1],
                               "Supply: Production" = ColUpdate[2],
                               "Supply: Import" = ColUpdate[3],
                               "Demand: Export" = ColUpdate[4],
                               "Demand: Food" = ColUpdate[5],
                               "Demand: Feed" = ColUpdate[6],
                               "Demand: Bioenergy" = ColUpdate[7],
                               "Demand: Other use" = ColUpdate[8],
                               "Demand: Stock loss" = ColUpdate[9],
                               "Demand: Closing stocks" = ColUpdate[10]),
                    breaks = ELEMLabel1) +
  theme_bw() + theme0 + theme1 -> A6p2; A6p2

ss = "Sugar crops"

AgElement_SUA |>
  mutate(DS = if_else(element %in% ElEMSupply, "Supply", "Demand")) %>%
  mutate(value = if_else(!element %in% ElEMSupply, -value, value)) %>%
  mutate(element = factor(element, levels = ELEMLevel, labels = ELEMLabel)) |>
  filter(sector == ss) |> select(-DS) |>
  filter(!grepl("stock", element)) |>
  spread(element, value, fill = 0) |>
  gather(element, value, -scenario:-sector) |>
  ProcScen() %>%
  mutate(value = value) %>%
  ggplot +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = element), stat = "identity", position = "stack",
           color = "black") +
  geom_point(
    data = AgElement_SUA |>
      filter(scenario %in% c(Scenario_Storage, Scenario_NoStorage)) |>
      filter(year %in% c(2021,seq(2030, 2100, 10))) |>
      filter(grepl("stock", element), sector == ss) |>
      spread(element, value) |>
      mutate(`Stock variation` = `Closing stocks` - `Opening stocks`),
    aes(x = year, y = -`Stock variation`), color = "black", size = 1) +
  labs(x = "Year", y = "Million tonne", fill = "Element") +
  scale_fill_manual(values = c("Supply: Opening stocks" = ColUpdate[1],
                               "Supply: Production" = ColUpdate[2],
                               "Supply: Import" = ColUpdate[3],
                               "Demand: Export" = ColUpdate[4],
                               "Demand: Food" = ColUpdate[5],
                               "Demand: Feed" = ColUpdate[6],
                               "Demand: Bioenergy" = ColUpdate[7],
                               "Demand: Other use" = ColUpdate[8],
                               "Demand: Stock loss" = ColUpdate[9],
                               "Demand: Closing stocks" = ColUpdate[10]),
                    breaks = ELEMLabel1) +
  theme_bw() + theme0 + theme1 -> A6p3; A6p3

## Ag prices ----

AgElement_SUA_ProdValue |>
  filter(element == "Price") ->
  PAgPrice

PAgPrice %>%
  filter(!sector %in% c("Forest", "Pasture", "Livestock", "Nonstorable crops")) |>
  filter(scenario %in% c(Scenario_Storage, Scenario_NoStorage)) |>
  filter(year %in% c(2021,seq(2030, 2100, 10))) |>
  Proc_Diff(type = "R", -year) |>
  group_by_at(vars(-scenario, -value)) %>%
  mutate(value = value / value[scenario == Scenario_NoStorage]) %>%
  filter(scenario != Scenario_NoStorage) |>
  ggplot() +
  facet_wrap(~region, nrow = 1, scales = "fixed") +
  geom_hline(yintercept = 1) +
  geom_line(aes(x=year, y=value, color=sector), linewidth = 1.4) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  #scale_color_brewer(palette = "Set1", direction = 1) +
  #scale_color_npg() +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  labs(x = "Year", y = "Price ratio (Hidden = 1)", color = "Sector") +
  theme_bw() + theme0 + theme1  -> A8; A8

## Carbon emissions ----

EMs_NCEM_Ag %>%
  mutate(sector = factor(sector, levels = rev(c("CH4_Ag", "N2O_Ag", "LULUCF")))) %>%
  group_by_at(vars(-value, -year)) %>%
  Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2021) %>% ungroup() %>% #filter(year == 2021)
  #mutate(value = value / 1000) %>%
  ProcScen() %>%
  #filter(scenario %in% c(Scenario_Storage)) |>
  ggplot() + facet_wrap(~region, nrow = 1, scales = "free") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector),
           stat = "identity", position = "stack",
           color = "black", linewidth = 0.4) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  #labs(x = "Year", y = expression(paste("Trillion ", tCO[2])), fill = "Emission") +
  labs(x = "Year", y = expression(paste(GtCO[2]-eq)), fill = "Emission") +
  scale_fill_brewer(palette = "BuPu", direction = -1,
                    breaks = c( "CH4_Ag", "N2O_Ag", "LULUCF"),
                    labels = c( "CH4_Ag" = expression(paste(CH[4], " Agriculture")),
                                "N2O_Ag" = expression(paste(N[2], "O Agriculture")),
                                "LULUCF" = expression(paste(CO[2], " LULUCF"))
                    )) +
  theme_bw() + theme0 + theme1 -> A9; A9


(A6p1 + ggtitle("(A) Supply utilization accounts for cereal grains by region") + theme(axis.title.x = element_blank(), legend.position = "right") + labs(fill = "SUA element\n(Panel A-C)"))/
  (A6p2 + ggtitle("(B) Supply utilization accounts for oil crops by region") + theme(axis.title.x = element_blank(), legend.position = "none")) /
  (A6p3 + ggtitle("(C) Supply utilization accounts for sugar crops by region") + theme(axis.title.x = element_blank(), legend.position = "none")) /
  #(A6p4 + ggtitle("(D) Supply utilization accounts for other storable crops by region") + theme(axis.title.x = element_blank(), legend.position = "none")) /
  (A8 + ggtitle("(D) Agricultural prices by sector and region") + theme(axis.title.x = element_blank(),legend.position = "right")+ labs(color = "Sector (Panel D)")) /

  (A1 + ggtitle("(E) Land cover and use by sector and region") + labs(fill = "Land (Panel E)")+theme(axis.title.x = element_blank(), legend.position = "right") )/
  (A2 + ggtitle("(F) Agricultural labor input by sector and region") + theme(axis.title.x = element_blank(), legend.position = "right")  + labs(fill = "Sector (Panels F-I)"))/
  (A3 + ggtitle("(G) Agricultural capital input by sector and region") + theme(axis.title.x = element_blank(), legend.position = "none") )/
  (A4 + ggtitle("(H) Agricultural irrigation water withdrawal by sector and region") +  theme(axis.title.x = element_blank(), legend.position = "none")) /
  (A5 + ggtitle("(I) Fertilizer use by sector and region") + theme(axis.title.x = element_blank(), legend.position = "none")) /
  (A9 + ggtitle("(J) Cumulative agricultural emissions by sector and region") + theme(axis.title.x = element_blank(),legend.position = "right")+ labs(fill = "Sector (Panel J)")) +
  patchwork::plot_layout(guides = "collect", heights = rep(1, 10)) -> pp

pp %>% Write_png(.name = paste0("AgLUDB_Impact_", Scenario_Storage, "_", Scenario_NoStorage), .DIR_MODULE = DIR_MODULE, h = 24, w = 22)


