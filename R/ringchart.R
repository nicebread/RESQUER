# # see https://www.pipinghotdata.com/posts/2021-06-01-custom-interactive-sunbursts-with-ggplot-in-r/
#
# df <- data.frame(
#   prereg=c("NA", "NA", "no", "no", "no", "yes", "yes", "yes", "yes", "RR", "RR", "RR", "RR"),
#   replic=c("NA", "NA", "NA", "NA", "NA", "no", "no", "yes", "yes", "no", "yes", "yes", "yes")
# )
# df$prereg <- factor(df$prereg, levels=c("NA", "no", "yes", "RR"))
#
# df <- applicant$pubs %>% select(prereg=P_Preregistration2, replic=P_PreregisteredReplication)
#
#
# df_inner <- df %>%
#   group_by(prereg) %>%
#   count() %>%
#   ungroup() %>%
#   mutate(
#     xmax=cumsum(n),
#     xmin=xmax-n,
#     xmid = (xmax -xmin) / 2 + xmin,
#   )
#
# df_outer <- df %>%
#   group_by(prereg, replic) %>%
#   count() %>%
#   ungroup() %>%
#   mutate(
#     xmax=cumsum(n),
#     xmin=xmax-n,
#     xmid = (xmax -xmin) / 2 + xmin,
#   )
#
#
# ggplot(df_outer) +
#   geom_rect(
#     aes(
#       ymin = 20,
#       # fix rectangle height here
#       ymax = 26,
#       xmin = xmin,
#       xmax = xmax,
#       fill = replic
#     ),
#     show.legend = FALSE
#   ) +
#   geom_text(aes(label=replic, x=xmid, y=23))+
#   ylim(c(0, 26)) +
#   coord_polar() +
#   geom_rect(data = df_inner,
#             aes(
#               ymin = 10,
#               # fix rectangle height here
#               ymax = 20,
#               xmin = xmin,
#               xmax = xmax,
#               fill = prereg
#             ),
#             show.legend = FALSE
#   ) +
#   geom_text(data=df_outer, aes(label=prereg, x=xmid, y=15)) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     panel.grid = element_blank(),
#     legend.position = "none",
#   ) + xlab("") + ylab("")
