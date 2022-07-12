theme_enem <- function(variables) {
    ggplot2::theme(                                                      # customização do tema
        panel.background = ggplot2::element_rect(fill = "#EBF9FF"),
        plot.background = ggplot2::element_rect(fill = "#EBF9FF"),
        plot.margin = ggplot2::unit(c(1, 2, 1, 1), "cm"),                              # título
        plot.title = ggplot2::element_text(
            size = 20,
            face = "bold",
            family = "",
            hjust = 0,
            margin = ggplot2::unit(c(0, 0, 0.5, 0.5), "cm")
        ),
        plot.subtitle = ggplot2::element_text(                                      # subtítulo
            size = 15,
            family = "",
            hjust = 0,
        ),
        text = ggplot2::element_text(                              # textos config. geral texto
            family = "",
            color = "#000E14",
            size = 14,
            hjust = 0,
            face = "bold"
        ),
        axis.title = ggplot2::element_text(                         # texto do título eixo x/y
            face = "bold",
            size = 16,
            hjust = 0.5,
        ),
        axis.text.x = ggplot2::element_text(                                  # texto do eixo x
            color = "#000E14",
            size = 10,
            face = "plain",
            margin = ggplot2::unit(c(0.3, 0, 0.5, 0), "cm")
        ),
        axis.text.y = ggplot2::element_text(                                  # texto do eixo y
            color = "#000E14",
            size = 10,
            face = "plain",
            family = "",
            margin = ggplot2::unit(c(0, 0.5, 0, 0.5), "cm")
        ),
        axis.ticks.x = ggplot2::element_line(color = "#1F1F1F"),              # ticks do eixo x
        axis.line.x = ggplot2::element_line(color = "#1F1F1F"),        # cor da linha do eixo x
        axis.ticks.y = ggplot2::element_line(color = "#1F1F1F"),     # padrões dos ticks eixo x
        axis.line.y = ggplot2::element_line(color = "#1F1F1F"),        # cor da linha do eixo y
    )

}


theme_enem_fundo_branco <- function(variables) {
    ggplot2::theme(                                                      # customização do tema
        panel.background = ggplot2::element_rect(fill = "#FFFFFF"),
        plot.background = ggplot2::element_rect(fill = "#FFFFFF"),
        plot.margin = ggplot2::unit(c(1, 2, 1, 1), "cm"),                              # título
        plot.title = ggplot2::element_text(
            size = 20,
            face = "bold",
            family = "",
            hjust = 0,
            margin = ggplot2::unit(c(0, 0, 0.5, 0.5), "cm")
        ),
        plot.subtitle = ggplot2::element_text(                                      # subtítulo
            size = 12,
            family = "",
            hjust = 0,
        ),
        text = ggplot2::element_text(                              # textos config. geral texto
            family = "",
            color = "#000E14",
            size = 14,
            hjust = 0,
            face = "bold"
        ),
        axis.title = ggplot2::element_text(                         # texto do título eixo x/y
            face = "bold",
            size = 16,
            hjust = 0.5,
        ),
        axis.text.x = ggplot2::element_text(                                  # texto do eixo x
            color = "#000E14",
            size = 10,
            face = "plain",
            margin = ggplot2::unit(c(0.3, 0, 0.5, 0), "cm")
        ),
        axis.text.y = ggplot2::element_text(                                  # texto do eixo y
            color = "#000E14",
            size = 10,
            face = "plain",
            family = "",
            margin = ggplot2::unit(c(0, 0.5, 0, 0.5), "cm")
        ),
        axis.ticks.x = ggplot2::element_line(color = "#1F1F1F"),              # ticks do eixo x
        axis.line.x = ggplot2::element_line(color = "#1F1F1F"),        # cor da linha do eixo x
        axis.ticks.y = ggplot2::element_line(color = "#1F1F1F"),     # padrões dos ticks eixo x
        axis.line.y = ggplot2::element_line(color = "#1F1F1F"),        # cor da linha do eixo y
    )

}
