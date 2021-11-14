
library(ggraph)
library(tidygraph)
library(tidyverse)



bds <- tibble(
    nome = str_glue("BD{1:3}"),
    x_pos = seq(from = 0.25, to = 0.75, by = 0.25 ),
    y_pos = 0.25,
    tipo = "BD"
)

analises <- tibble(
    nome = str_glue("A{1:3}"),
    x_pos = seq(from = 0.25, to = 0.75, by = 0.25 ),
    y_pos = 0.75,
    tipo = "Analise"
)


etls <- crossing(bds %>% select(bd = nome), analises %>% select(analises = nome)) %>% 
    mutate(
        nome = str_glue("{bd}->{analises}"),
        tipo = "Código"
    ) %>% 
    mutate(
        x_pos = seq(from = 0, to = 1, length.out = 9 ),
        y_pos = 0.5
    )


bd_to_code <- crossing(
    bds %>% select(from = nome),
    etls %>% select(to = nome),
) %>% 
    filter(
        str_detect(to, from),
    ) %>% 
    select(
        from, to
    )

code_to_analise <- crossing(
    etls %>% select(from = nome),
    analises %>% select(to = nome),
) %>% 
    filter(
        str_detect(from, to),
    ) %>% 
    select(
        from, to
    )





grafo_com_nos <- create_empty(n = 0, directed = TRUE) %>% 
    activate("nodes") %>% 
    bind_nodes(
        bds,
        etls,
        analises
    ) %>% 
    bind_edges(
        bd_to_code
    ) %>% 
    bind_edges(
        code_to_analise
    )



ggraph(
    grafo_com_nos,
    x = x_pos,
    y = y_pos
) +
    geom_node_label(
        aes(
            label = nome,
            color = tipo,
            fill = tipo
        ),
        alpha = 0.1,
        size = 4,
        show.legend = FALSE
    ) +
    geom_edge_fan(
        arrow = arrow(length = unit(4, 'mm')),
        end_cap = circle(10, 'mm'),
        start_cap = circle(10, 'mm'),
        edge_width = 1,
        color = "gray"
    ) +
    theme_graph() +
    coord_flip() +
    scale_x_reverse()





