race_bar_chart_animation <- function(
    data_path = './data/stackoverflow.RData',
    tags = c("python", "java", "javascript", "php", "android", "r", "sas", "sql", "rust", "matlab"),
    years = 2008:2020,
    colors_vector = c("#FF2800", "#FF8000", "#008000", "#00A0DE", "#FFC0CB", "#B6995B", "#FFF500", "#008860", "#3E4058", "#00FFFF"),
    output_path = "gganim.gif",
    frames = 300, fps = 10, width = 1500, height = 1000
) {
  # Load the data
  load(data_path)
  stackDT <- data.table::setDT(stackoverflow)

  # Data Wrangling
  stack2 <- stackDT[
    year %in% years & tag %in% tags
  ][
    order(-num_questions, tag)
  ][
    , .SD[1], by = .(tag, year)
  ][
    order(tag, year, -num_questions)
  ]

  stack3 <- stack2 |>
    group_by(year) |>
    mutate(
      rank = rank(-num_questions),
      Value_rel = num_questions / num_questions[rank == 1],
      Value_lbl = paste0("  ", round(num_questions / 1e3), ' K')
    ) |>
    group_by(tag) |>
    filter(rank <= 10) |>
    dplyr::ungroup()

  # Add images and colors
  stack3$images <- paste0('./inst/', stack3$tag, '.png')
  tag_colors <- tibble(tag = tags, colors = colors_vector)
  stack3 <- left_join(stack3, tag_colors, by = "tag")

  # Plotting
  staticplot2 <- ggplot(stack3, aes(rank, num_questions)) +
    geom_bar(stat = 'identity', aes(fill = colors)) +
    scale_fill_identity() +
    coord_flip(clip = "off", expand = TRUE, ylim = c(0, 2600000)) +
    geom_text(aes(rank, y = 0, label = paste(as.factor(tag), "   ")), hjust = 1, size = 8) +
    scale_x_reverse() +
    guides(color = 'none', fill = "none") +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none",
      panel.background = element_blank(),
      plot.background = element_blank(),
      plot.margin = margin(2, 2, 2, 4, "cm"),
      plot.title=element_text(size=30, hjust=0.5, face="bold", colour="black", vjust=-1),
      plot.subtitle=element_text(size=23, hjust=0.5, face="italic", color="black"),
      plot.caption =element_text(size=18, hjust=0.5, face="italic", color="black")
    ) +
    geom_image(aes(image = images), size = 0.11, asp = 0.5) +
    geom_text(aes(y = num_questions, label = Value_lbl, hjust = -0.6), size = 6)

  # Animation
  anim <- staticplot2 +
    gganimate::transition_states(year, transition_length = 4, state_length = 1) +
    gganimate::view_follow(fixed_x = TRUE) +
    gganimate::enter_fade() +
    gganimate::exit_fade() +
    gganimate::ease_aes("quadratic-in-out") +
    labs(
      title = 'Popularity of Tags Per Year : {closest_state}',
      subtitle = "Top 10 Programming Languages",
      caption = "Data Source: Stack Overflow"
    )

  # Save animation
  save_anim <- gganimate::animate(
    anim, nframes = frames, fps = fps, width = width, height = height,
    renderer = gganimate::gifski_renderer(output_path)
  )

  # Return the final data frame and animation object
  list(final_data = stack3, animation = anim, save_anim=save_anim)
}

# Example usage:
result <- race_bar_chart_animation(
  data_path = './data/stackoverflow.RData',
  tags = c("python", "java", "javascript", "php", "android", "r", "sas", "sql", "rust", "matlab"),
  years = 2008:2020,
  output_path = "gganim.gif"
)


