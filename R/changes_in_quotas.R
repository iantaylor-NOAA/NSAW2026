library(dplyr)
library(forcats)
library(ggplot2)
library(scales)
library(tidyr)

# values below are from assessment reports and federal register
# https://public-inspection.federalregister.gov/2024-28035.pdf?1734097513
# 2027 values are all projected based on current harvest control rules
# and have not been adopted

quota_changes <- tribble(
    ~species                         , ~year , ~acl    ,
    "Canary rockfish"                ,  2024 ,  1296   ,
    "Canary rockfish"                ,  2025 ,   571   ,
    "Yellowtail rockfish"            ,  2026 ,  6023   ,
    "Yellowtail rockfish"            ,  2027 ,  4723   ,
    "Widow rockfish"                 ,  2026 , 10392   ,
    "Widow rockfish"                 ,  2027 ,  4596   ,
    "Petrale sole"                   ,  2024 ,  3285   ,
    "Petrale sole"                   ,  2025 ,  2354   ,
    "Sablefish"                      ,  2026 , 27238   ,
    "Sablefish"                      ,  2027 , 13964   ,
    "Chilipepper"                    ,  2026 ,  2121   ,
    "Chilipepper"                    ,  2027 ,  3211   ,
    "Quillback rockfish"             ,  2026 ,     1.5 ,
    "Quillback rockfish"             ,  2027 ,    12   ,
    "Rougheye/Blackspotted rockfish" ,  2026 ,   187   ,
    "Rougheye/Blackspotted rockfish" ,  2027 ,   882   ,
    "Shortspine thornyhead"          ,  2024 ,  2030   ,
    "Shortspine thornyhead"          ,  2025 ,   815
)

quota_changes$color <- case_when(
    quota_changes$species == "Widow rockfish" ~ "#000000",
    quota_changes$species == "Yellowtail rockfish" ~ "#009E73",
    quota_changes$species == "Canary rockfish" ~ "#E69F00",
    quota_changes$species == "Chilipepper" ~ "#CC79A7",
    quota_changes$species == "Shortspine thornyhead" ~ "#F0E442",
    quota_changes$species == "Sablefish" ~ "blue4",
    quota_changes$species == "Petrale sole" ~ "purple3",
    quota_changes$species == "Quillback rockfish" ~ "darkorange2",
    quota_changes$species == "Rougheye/Blackspotted rockfish" ~ "brown4",
    TRUE ~ "gray70"
)

# new variable to indicate before vs after quota change
quota_changes <- quota_changes |>
    mutate(
        period = case_when(
            year == 2024 ~ "before",
            year == 2025 ~ "after",
            year == 2026 ~ "before",
            year == 2027 ~ "after",
            TRUE ~ NA
        )
    ) |>
    mutate(
        time = case_when(
            period == "before" ~ 1,
            period == "after" ~ 2,
            TRUE ~ NA
        )
    )


# Reorder by percent change so the most dramatic drops stand out in the legend
quota_changes <- quota_changes |>
    group_by(species) |>
    mutate(
        pct_change = (last(acl) - first(acl)) /
            first(acl)
    ) |>
    ungroup() |>
    mutate(species = fct_reorder(species, pct_change))

# Prepare labels with percent change at the "after" time point
quota_changes <- quota_changes |>
    mutate(
        pct_label = paste0(
            species,
            ": ",
            ifelse(pct_change > 0, "+", ""),
            round(pct_change * 100, 0),
            "%"
        )
    )

quota_change_plot <- function(dat) {
    # Prepare text labels with adjusted vertical position to reduce overlap
    text_dat <- dat |>
        filter(time == 2) # |>
    # arrange(acl) |>
    # mutate(
    #     # Add vertical jitter to separate overlapping labels
    #     y_offset = row_number() * 0.02 * max(acl),
    #     label_y = acl + y_offset
    # )

    dat |>
        ggplot(
            aes(time, acl, color = species, group = species)
        ) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 3) +
        ggrepel::geom_text_repel(
            data = text_dat,
            aes(x = time, y = acl, label = pct_label, color = species),
            #hjust = -1,
            #vjust = 0.5,
            segment.linetype = "dashed",
            #segment.color = "gray70",
            segment.size = 0.25,
            nudge_x = 1.5,
            direction = "y",
            size = 3,
            fontface = "bold"
        ) +
        scale_x_continuous(
            breaks = c(1, 2),
            labels = c("before", "after"),
            expand = expansion(add = c(0.1, 1))
        ) +
        scale_y_continuous(labels = comma, limits = c(0, NA)) +
        scale_color_manual(
            values = setNames(dat$color, dat$species)
        ) +
        labs(
            title = "Potential changes in quotas\nfrom 2023 and 2025 assessments",
            x = "",
            y = "Annual Catch Limit (t)",
            color = "Species"
        ) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")
}

# plot without non-rockfish and without good news
quota_changes |>
    filter(!species %in% c("Sablefish", "Petrale sole")) |>
    filter(
        !species %in%
            c(
                "Chilipepper",
                "Quillback rockfish",
                "Rougheye/Blackspotted rockfish"
            )
    ) |>
    quota_change_plot()
ggsave("figures/quota_changes_bad_news.png", width = 5, height = 5, dpi = 300)

# plot without non-rockfish
quota_changes |>
    filter(!species %in% c("Sablefish", "Petrale sole")) |>
    quota_change_plot()
ggsave("figures/quota_changes_rockfish.png", width = 5, height = 5, dpi = 300)

quota_changes |>
    filter(
        !species %in%
            c(
                "Chilipepper",
                "Quillback rockfish",
                "Rougheye/Blackspotted rockfish"
            )
    ) |>
    quota_change_plot()
ggsave("figures/quota_changes_bad_news2.png", width = 5, height = 5, dpi = 300)

# plot with all species
quota_changes |>
    quota_change_plot()
ggsave("figures/quota_changes.png", width = 5, height = 5, dpi = 300)
