# devtools::load_all()

load('./data/stackoverflow.RData')

colors_vector <- c("#FF2800", "#FF8000", "#008000", "#00A0DE", "#FFC0CB", "#B6995B", "#FFF500", "#008860", "#3E4058", "#00FFFF")

# stack2 <- stackoverflow |>
#   filter(year %in% c(2008:2020)) |> arrange(-num_questions, tag) |>
#   group_by(tag,year) |>
#   slice_head(n=1) |> arrange(tag, year, -num_questions) |>
#   filter(tag %in% c('python','java','javascript','php','android','r','sas','sql','rust','matlab'))

stackDT <- data.table::setDT(stackoverflow)

stack2 <- stackDT[
  year %in% 2008:2020 & tag %in% c("python", "java", "javascript", "php", "android", "r", "sas", "sql",'rust','matlab') # Filter years and tags
][
  order(-num_questions, tag) # Arrange by num_questions (descending) and tag
][
  , .SD[1], by = .(tag, year) # Group by tag and year, and select the first row in each group
][
  order(tag, year, -num_questions) # Final ordering by tag, year, and descending num_questions
]

stack3 <- stack2 |>
  group_by(year) |> # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-num_questions),
         Value_rel = num_questions/num_questions[rank==1],
         Value_lbl = paste0("  ",round(num_questions/1e3),' K')) |>
  group_by(tag) |>
  filter(rank <=10) |>
  dplyr::ungroup()

stack3$images <- paste0('./inst/',stack3$tag,'.png')

tags <- stack3$tag |> unique() |> as_tibble_col(column_name = "tag")
tags$colors <- colors_vector

stack3 <- reduce(list(stack3,tags), left_join, join_by(tag))

# staticplot = ggplot(stack3, aes(rank, y = as.factor(tag),
#                                 fill = colors )) +
#   geom_image(aes(image=images), size=0.11, asp=0.5) +
#   geom_tile(aes(y = num_questions/2,
#                 height = num_questions,
#                 width = 0.9), alpha = 0.8, color = NA) +
#   geom_text(aes(y = 0, label = paste(as.factor(tag), "   ")), vjust = 0.2, hjust = 1, size = 8) +
#   geom_text(aes(y=num_questions,label = Value_lbl, hjust=0), size = 7) +
#   coord_flip(clip = "off", expand = FALSE) +
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_reverse() +
#   guides(color = 'none', fill = "none") +
#   theme_classic() +
#   theme(axis.line=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         legend.position="none",
#         panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         plot.title=element_text(size=30, hjust=0.5, face="bold", colour="black", vjust=-1),
#         plot.subtitle=element_text(size=20, hjust=0.5, face="italic", color="black"),
#         plot.caption =element_text(size=15, hjust=0.5, face="italic", color="black"),
#         plot.background=element_blank(),
#         plot.margin = margin(2,2, 2, 4, "cm"))
#



staticplot2 = ggplot(stack3, aes(rank, num_questions)) +
  geom_bar(stat = 'identity', aes(fill=colors)) +
  scale_fill_identity() +
  coord_flip(clip = "off", expand = TRUE, ylim = c(0, 2600000)) +
  # geom_tile(aes(y = num_questions/2,
  #               height = num_questions,
  #               width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(rank, y = 0, label = paste(as.factor(tag), "   ")), hjust = 1, size = 8) +
  # scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = 'none', fill = "none") +
  theme_classic() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title=element_text(size=30, hjust=0.5, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=23, hjust=0.5, face="italic", color="black"),
        plot.caption =element_text(size=18, hjust=0.5, face="italic", color="black"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  geom_image(aes(image=images), size=0.11, asp=0.5) +
  geom_text(aes(y=num_questions,label = Value_lbl, hjust=-0.6), size = 6)



anim = staticplot2 + gganimate::transition_states(year, transition_length = 4, state_length = 1) +
  gganimate::view_follow(fixed_x = TRUE)  +
  gganimate::enter_fade() +
  gganimate::exit_fade() +
  gganimate::ease_aes("quadratic-in-out") +
  labs(title = 'Popularity of Tags Per Year : {closest_state}',
       subtitle  =  "Top 10 Programming Languages",
       caption  = "Data Source: Stack Overflow")


gganimate::animate(anim, 300, fps = 10,  width = 1500, height = 1000, rewind = FALSE,
        renderer = gganimate::gifski_renderer("gganim.gif"))
