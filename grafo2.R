
library(ggraph)
library(tidygraph)
library(tidyverse)

paleta_epe <- c(
    "#001F66",
    "#FF8000",
    "#780116",
    "#034732",
    "#AF9164"    
)



bds <- tibble(
    nome = str_glue("BD{1:4}"),
    x_pos = seq(from = 0.1, to = 0.9, length.out = 4 ),
    y_pos = 0.1,
    tipo = "BD"
)

analises <- tibble(
    nome = str_glue("A{1:4}"),
    x_pos = seq(from = 0.1, to = 0.9, length.out = 4 ),
    y_pos = 0.9,
    tipo = "Analise"
)


etls_bd <- crossing(bds %>% select(bd = nome)) %>% 
    mutate(
        nome = str_glue("{bd}->BD"),
        tipo = "Código"
    ) %>% 
    mutate(
        x_pos = seq(from = 0, to = 1, length.out = 4 ),
        y_pos = 0.35
    )


bd <- tibble(
    tibble(
        nome = "BD",
        tipo = "BD",
        x_pos = 0.5,
        y_pos = 0.65
    )
)
    

bd_to_code <- crossing(
    bds %>% select(from = nome),
    etls_bd %>% select(to = nome),
 ) %>% 
    filter(
        str_detect(to, from),
    ) %>% 
    select(
        from, to
    ) %>% 
    mutate(
        cor = "sim"
    )


code_to_bd <- bd_to_code %>% select(from = to) %>% 
    mutate(
        to = "BD"
    ) %>% 
    mutate(
        cor = "sim"
    )

    

code_to_analise <- crossing(
    analises %>% select(to = nome),
) %>% 
    mutate(
        from = "BD"
    ) %>% 
    select(
        from, to
    ) %>% 
    mutate(
        cor = "sim"
    )



grafo_com_nos <- create_empty(n = 0, directed = TRUE) %>% 
    activate("nodes") %>% 
    bind_nodes(
        bds,
        etls_bd %>% select(-bd),
        bd ,
        analises
    ) %>% 
    bind_edges(
        bd_to_code
    ) %>% 
    bind_edges(
        bd_to_code %>% mutate(cor = NA),
        code_to_bd,
        code_to_analise
    )



ggraph(
    grafo_com_nos,
    x = x_pos,
    y = y_pos
) +
    geom_edge_fan(
        arrow = arrow(length = unit(3, 'mm')),
        end_cap = rectangle(height =  1, width = 2, width_unit = "cm", height_unit = "cm"),
        start_cap = rectangle(height = 0.5, width = 1, width_unit = "cm", height_unit = "cm"),
        edge_width = 1.25,
        aes(
            color = factor(cor)
        ),
        show.legend = FALSE
    ) +
    geom_node_label(
        aes(
            label = nome,
            fill = tipo
        ),
        color = "white",
        size = 4,
        show.legend = FALSE,
        label.padding =  unit(0.35, "lines")
    ) +
    theme_graph(
    ) +
    theme(
        aspect.ratio = 0.8
    ) +
    coord_flip() +
    scale_x_reverse() +
    scale_color_discrete(
        type = paleta_epe
    ) +
    scale_edge_color_manual(
        values = "darkgray",
        na.value = "white"
    ) +
    scale_fill_discrete(
        type = paleta_epe
    )






