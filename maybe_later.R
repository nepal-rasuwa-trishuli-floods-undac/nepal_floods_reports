
nuwakot_population_map <- census_working |>
  left_join(wards_joined, 
            by = c("adm2_pcode", "adm3_pcode", "ward")) |>
  filter(!is.na(geometry)) |> 
  filter(district == "Nuwakot") |>
  mutate(ward_label = paste0(ward, "\n", format(population_total, big.mark = ","))) |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(data = adm3, alpha = .25, linewidth = .5, colour = "grey70", fill = NA) +
  geom_sf_text(data = adm3 |> filter(adm2_name == "Rasuwa"), 
               aes(label = adm3_name),
               alpha = .5
  ) +
  # Ward populations and affected wards
  geom_sf(aes(colour = rdna, fill = population_total), linewidth = .5, alpha = .8) +
  # River
  geom_sf(data = river, colour = "#002e6e", fill = "#002e6e", alpha = .6) +
  # Settlements
  geom_sf(data = settlements |> filter(adm2_en == "Nuwakot") |> 
            mutate(fclass = str_to_title(fclass),
                   fclass = fct_relevel(fclass, c("Hamlet", "Village", "Suburb"))), 
          aes(geometry = geometry, shape = fclass), colour = "#ffc800", alpha = .5) +
  geom_text(aes(x = 85.29562119920958, y = 28.111595947460756, label = "Dhunche"), 
            size = 2.5, colour = "#ed1847", alpha = .5) +
  scale_colour_manual(values = c("Affected" = "#e56a54"), 
                      na.value = "grey97", 
                      na.translate = FALSE) +
  scale_fill_continuous(high = "#003425", low = "#e6efd0", labels = comma) +
  scale_shape_manual(values = c("Hamlet" = 20, 
                                "Village" = 16, 
                                "Suburb" = 15)) +
  geom_shadowtext(
    aes(label = ward, 
        geometry = geometry), 
    stat = "sf_coordinates", 
    size = 2.5, color = "black",  bg.color = "white", bg.r = 0.1) +
  coord_sf(xlim = c(85.1, 85.8), ylim = c(27.95, 28.4), expand = TRUE) +
  labs(x = "", y = "", 
       title = "Ward Populations in Nuwakot",
       subtitle = "15 wards affected in Nuwakot. District Headqaurters labelled in red.",
       shape = "Settlement\nType",
       fill = "Ward\nPopulation", 
       colour = "RDNA") + 
  guides(size = guide_legend(override.aes = list(alpha = 1)), 
         shape = guide_legend(override.aes = list(alpha = 1)))

ggsave("./plots/nuwakot_population_map.png", height = 8.27, width = 11.69, units = "in")

buildings_impacted_rasuwa <- municipal_damage_losses |> 
  filter(district == "Rasuwa") |> 
  right_join(adm3 |> filter(adm2_name == "Rasuwa"), 
             by = "adm3_pcode") |> 
  st_as_sf() |>  
  ggplot() + 
  geom_sf(data = adm3, alpha = .25, linewidth = .5, colour = "grey70", fill = NA) +
  geom_sf(aes(fill = impacted_buildings), alpha = .8) +
  geom_sf(data = wards_joined |> filter(adm2_en == "Rasuwa"), aes(colour = rdna), fill = NA, linewidth = .5) +
  geom_sf_text(data = adm3 |> filter(adm2_name == "Rasuwa"), 
               aes(label = adm3_name),
               alpha = .5
  ) +
  scale_fill_continuous(high = "#003425", low = "#e6efd0", labels = comma, na.value = "grey97") +
  # River
  geom_sf(data = river, colour = "#002e6e", fill = "#002e6e") +
  # Settlements
  geom_sf(data = settlements |> filter(adm2_en == "Rasuwa") |> 
            mutate(fclass = str_to_title(fclass),
                   fclass = fct_relevel(fclass, c("Hamlet", "Village", "Suburb"))), 
          aes(geometry = geometry, shape = fclass), colour = "#ffc800", alpha = .5) +
  annotate("text", x = 85.29562119920958, y = 28.111595947460756, label = "Dhunche", 
           size = 2.5, colour = "#ed1847") +
  scale_colour_manual(values = c("Affected" = "#e56a54"), 
                      na.value = "grey97", 
                      na.translate = FALSE) +
  coord_sf(xlim = c(85.1, 85.8), ylim = c(27.95, 28.4), expand = TRUE) +
  labs(x = "", y = "", 
       title = "Impacted buildings in Rasuwa, by Municipality",
       subtitle = "15 wards affected in Rasuwa. District Headqaurters labelled in red.",
       shape = "Settlement\nType",
       fill = "Impacted\nBuildings",
       caption = "Source: NDRRMA RDNA", 
       colour = "RDNA")

ggsave("./plots/buildings_impacted_rasuwa.png", buildings_impacted_rasuwa,
       height = 8.27, width = 11.69, units = "in")

rasuwa_poorest_quintile <- census_working |>
  left_join(wards_joined, 
            by = c("adm2_pcode", "adm3_pcode", "ward")) |>
  filter(!is.na(geometry)) |> 
  filter(district == "Rasuwa") |>
  mutate(ward_label = paste0(ward, "\n", wealth_quintile_poorest_quintile_pc, "%")) |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(data = adm3, alpha = .25, linewidth = .5, colour = "grey70", fill = NA) +
  geom_sf_text(data = adm3 |> filter(adm2_name == "Rasuwa"), 
               aes(label = adm3_name),
               alpha = .5
  ) +
  # Ward populations and affected wards
  geom_sf(aes(colour = rdna, fill = wealth_quintile_poorest_quintile_pc), linewidth = .5, alpha = .8) +
  # River
  geom_sf(data = river, colour = "#002e6e", fill = "#002e6e", alpha = .6) +
  # Settlements
  geom_sf(data = settlements |> filter(adm2_en == "Rasuwa") |> 
            mutate(fclass = str_to_title(fclass),
                   fclass = fct_relevel(fclass, c("Hamlet", "Village", "Suburb"))), 
          aes(geometry = geometry, shape = fclass), colour = "#ffc800", alpha = .5) +
  geom_text(aes(x = 85.29562119920958, y = 28.111595947460756, label = "Dhunche"), 
            size = 2.5, colour = "#ed1847", alpha = .5) +
  scale_colour_manual(values = c("Affected" = "#e56a54"), 
                      na.value = "grey97", 
                      na.translate = FALSE) +
  scale_fill_continuous(high = "#003425", low = "#e6efd0", labels = comma) +
  scale_shape_manual(values = c("Hamlet" = 20, 
                                "Village" = 16, 
                                "Suburb" = 15)) +
  geom_shadowtext(
    aes(label = ward_label, 
        geometry = geometry), 
    stat = "sf_coordinates", 
    size = 1.5, color = "black",  bg.color = "white", bg.r = 0.1) +
  coord_sf(xlim = c(85.1, 85.8), ylim = c(27.95, 28.4), expand = TRUE) +
  labs(x = "", y = "", 
       title = "% of Ward Population in the Poorest Quintile Nationally",
       # subtitle = "15 wards affected in Rasuwa. District Headqaurters labelled in red.",
       shape = "Settlement\nType",
       fill = "% in\nPoorest\nQuintile", 
       caption = "Source: Nepal National Statistics Office, NDRRMA RDNA", 
       colour = "RDNA") + 
  guides(size = guide_legend(override.aes = list(alpha = 1)), 
         shape = guide_legend(override.aes = list(alpha = 1)))

ggsave("./plots/rasuwa_poorest_quintile_map.png", rasuwa_poorest_quintile,
       height = 8.27, width = 11.69, units = "in")

rasuwa_cadastral_affected <- ward_cadastral |>
  right_join(wards_joined, 
             by = c("adm2_pcode", "adm3_pcode", "ward")) |>
  filter(district == "Rasuwa") |>
  mutate(ward_label = ifelse(!is.na(total_affected_area_hectares), 
                             paste0(ward, "\n", total_affected_area_hectares, " ha."), 
                             "")) |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(data = adm3, alpha = .25, linewidth = .5, colour = "grey70", fill = NA) +
  geom_sf_text(data = adm3 |> filter(adm2_name == "Rasuwa"), 
               aes(label = adm3_name),
               alpha = .5
  ) +
  # Ward populations and affected wards
  geom_sf(aes(colour = rdna, fill = total_affected_area_hectares), linewidth = .5, alpha = .8) +
  # River
  geom_sf(data = river, colour = "#002e6e", fill = "#002e6e", alpha = .6) +
  # Settlements
  geom_sf(data = settlements |> filter(adm2_en == "Rasuwa") |> 
            mutate(fclass = str_to_title(fclass),
                   fclass = fct_relevel(fclass, c("Hamlet", "Village", "Suburb"))), 
          aes(geometry = geometry, shape = fclass), colour = "#ffc800", alpha = .5) +
  geom_text(aes(x = 85.29562119920958, y = 28.111595947460756, label = "Dhunche"), 
            size = 2.5, colour = "#ed1847", alpha = .5) +
  scale_colour_manual(values = c("Affected" = "#e56a54"), 
                      na.value = "grey97", 
                      na.translate = FALSE) +
  scale_fill_continuous(high = "#003425", low = "#e6efd0", labels = comma) +
  scale_shape_manual(values = c("Hamlet" = 20, 
                                "Village" = 16, 
                                "Suburb" = 15)) +
  geom_shadowtext(
    aes(label = ward_label, 
        geometry = geometry), 
    stat = "sf_coordinates", 
    size = 2, color = "black",  bg.color = "white", bg.r = 0.1) +
  coord_sf(xlim = c(85.1, 85.8), ylim = c(27.95, 28.4), expand = TRUE) +
  labs(x = "", y = "", 
       title = "Cadastral Land in Rasuwa damaged by the Floods, by Ward",
       subtitle = "15 wards affected in Rasuwa. District Headqaurters labelled in red.",
       shape = "Settlement\nType",
       fill = "Hectares\nAffected", 
       caption = "Source: Nepal National Statistics Office, NDRRMA RDNA", 
       colour = "RDNA") + 
  guides(size = guide_legend(override.aes = list(alpha = 1)), 
         shape = guide_legend(override.aes = list(alpha = 1)))

ggsave("./plots/rasuwa_cadastral_affected.png", rasuwa_cadastral_affected,
       height = 8.27, width = 11.69, units = "in")


rasuwa_reached_map <- consolidated |> 
  filter(response_modality != "Awareness/Advocacy") |> 
  filter(district == "Rasuwa") |>
  group_by(adm3_pcode) |> 
  summarise(
    individuals_reached = max(individuals_reached, na.rm = TRUE), 
    .groups = "drop") |>
  mutate(individuals_reached = ifelse(!is.finite(individuals_reached), 0, individuals_reached)) |> 
  right_join(adm3 |> filter(adm2_name == "Rasuwa"), 
             by = "adm3_pcode") |>
  mutate(municipal_label = paste0(adm3_name, "\n", format(round(individuals_reached), big.mark = ","))) |> 
  st_as_sf() |>  
  ggplot() + 
  geom_sf(data = adm3, alpha = .25, linewidth = .5, colour = "grey70", fill = NA) +
  geom_sf(aes(fill = individuals_reached), alpha = .8) +
  geom_sf(data = wards_joined |> filter(adm2_en == "Rasuwa"), aes(colour = rdna), fill = NA, linewidth = .5) +
  scale_fill_continuous(high = "#004987", low = "#e3edf6", labels = comma, na.value = "grey97") +
  # River
  geom_sf(data = river, colour = "#002e6e", fill = "#002e6e") +
  # Settlements
  geom_sf(data = settlements |> filter(adm2_en == "Rasuwa") |> 
            mutate(fclass = str_to_title(fclass),
                   fclass = fct_relevel(fclass, c("Hamlet", "Village", "Suburb"))), 
          aes(geometry = geometry, shape = fclass), colour = "#ffc800", alpha = .5) +
  geom_text(aes(x = 85.29562119920958, y = 28.111595947460756, label = "Dhunche"), 
            size = 2.5, colour = "#ed1847", alpha = .5) +
  scale_colour_manual(values = c("Affected" = "#e56a54"), 
                      na.value = "grey97", 
                      na.translate = FALSE) +
  geom_shadowtext(
    aes(label = municipal_label, 
        geometry = geometry), 
    stat = "sf_coordinates", 
    size = 2, color = "black",  bg.color = "white", bg.r = 0.1) +
  coord_sf(xlim = c(85.1, 85.8), ylim = c(27.95, 28.4), expand = TRUE) +
  labs(x = "", y = "", 
       title = "Individuals reached in Rasuwa, by Municipality",
       subtitle = "District Headqaurters labelled in red. Beneficiary frequencies are not individuals and include double counting",
       shape = "Settlement\nType",
       fill = "Frequencies\nReached", 
       caption = "Source: 5Ws", 
       colour = "RDNA")

ggsave("./plots/rasuwa_reached_map.png", rasuwa_reached_map,
       height = 8.27, width = 11.69, units = "in")

consolidated |> 
  mutate(sector = ifelse(str_detect(sector, "Protection"), "Protection", sector), 
         sector = ifelse(str_detect(sector, "Health|Nutrition"), "Health and Nutrition", sector)) |> 
  filter(sector %out% c("ETC", "Logistics") & !is.na(sector)) |> 
  filter(province != "Karnali") |>
  group_by(district, adm2_pcode, municipality, adm3_pcode, sector, lead_agency, implementing_partner) |>
  filter(district == "Rasuwa") |> 
  summarise(
    frequencies = sum(individuals_reached, na.rm = TRUE), 
    .groups = "drop"
  ) |> 
  mutate(duplicate = ifelse(
    implementing_partner == lead_agency, 
    1, 0
  )) |> 
  mutate(implementing_partner = ifelse(
    duplicate == 1, NA_character_, implementing_partner
  )) |> 
  pivot_longer(
    lead_agency:implementing_partner, 
    names_to = "role",
    values_to = "agencies"
  ) |> 
  select(-frequencies, -duplicate) |> 
  filter(!is.na(agencies)) |>  
  group_by(district, adm2_pcode, municipality, adm3_pcode, sector) |> 
  summarise(num_agencies = n_distinct(agencies), 
            agencies_text = paste(agencies, collapse = ","), 
            .groups = "drop") |> 
  right_join(
    adm3 |> filter(adm2_name %in% c("Rasuwa")), 
    by = c("adm2_pcode", "adm3_pcode")
  ) |>
  mutate(num_agencies = as.numeric(num_agencies)) |>
  mutate(label = paste0(adm3_name, "\n", num_agencies)) |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(aes(fill = num_agencies), linewidth =.1) +
  geom_sf_text(aes(label = adm3_name), size = 2, colour = "grey40", alpha = .8,
               nudge_x = -0.02, nudge_y = -0.02) +
  geom_sf_text(aes(label = num_agencies), size = 2) +
  scale_fill_continuous(high = "#004987", low = "#e3edf6", labels = comma, na.value = "grey97") +
  # scale_fill_continuous(palette = "Blues", 
  #                       breaks = seq(1, 20, 2), 
  #                       guide = guide_colorbar(barheight = unit(6, "cm"))) +
  # scale_fill_viridis(direction = -1) + 
  facet_wrap(~sector) + 
  labs(title = "Number of agencies by sector, by municipality", 
       subtitle = paste0("As of ", consolidated_date, ", from the 5Ws"), 
       x = "", y = "", 
       caption = "Source: 5Ws", 
       fill = "Number of\nagencies") + 
  theme(strip.background = element_rect(fill = "black"), 
        axis.text.x = element_text(size = 5), 
        axis.text.y = element_text(size = 5), 
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray90"))

ggsave("./plots/rasuwa_sector_agencies_facet_map.png", height = 8.27, width = 11.69, units = "in")

[![](./plots/rasuwa_reached_map.png)](https://raw.githubusercontent.com/nepal-rasuwa-trishuli-floods-undac/nepal_floods_reports/main/plots/rasuwa_reached_map.png)
