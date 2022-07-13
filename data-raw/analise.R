#                                ANÁLISE DO COMÉRCIO EXTERIOR
#                          ÁREA DE CONTROLE INTEGRADO DE SANTA HELENA/PR
#                    ALFÂNDEGA DA RECEITA FEDERAL DO BRASIL EM FOZ DO IGUAÇU/PR
#                    INSPETORIA DA RECEITA FEDERAL DO BRASIL EM SANTA HELENA/PR
###############################################################################################

# 

# Carregamento das bases e funções  ------------------------------------------------------

library(tidyverse)
library(readxl)
library(forcats)
library(ggrepel)
library(scales)
library(patchwork)
library(plotly)
library(gganimate)
library(gifski)
library(stringr)
source("R/funcao_tema_graficos.R")

# Carregamento das paletas de cores

# paleta com quatro cores
paleta_4_cores_impo_vol <- c("#1714FB", "#A007B9", "#7ECDCA", "#CF3476")
paleta_4_cores_expo_val <- c("#B68F88", "#DCADA5", "#E2E2E2", "#6A6A6A")

# paleta com cinco cores
paleta_5_cores_impo_vol <- c("#AE2030", "#8B4095", "#6A5E68", "#F2A519", "#28B46D")


################################## IMPORTAÇÃO #################################################

# Carregamento da base de dados de importação --------------------------------------------

impo <- read_xlsx("importacao/importacao_acumulada.xlsx")


# gráfico volumes de importação por ano -----------------------------------------------
# base
peso_vol_impo <- impo |> 
   select(
      "produto",
      "ano",
      "peso"
      ) |> 
   group_by(ano) |> 
   summarise(volumes = sum(peso)) |> 
   mutate(ano = as.factor(ano))
  
# gráfico
plot1 <- peso_vol_impo |> 
   ggplot()+
   aes(x = ano, 
       y = volumes,
       fill = ano,
       label = volumes)+
   geom_col()+
   geom_label(
      vjust = -0.5,
      colour = "#000000",
      alpha = 0
   )+
   theme(
      legend.position = "none"
   )+
   theme_enem_fundo_branco()+
   labs(
      title = "Volume de Importações x ano",
      x = "Ano",
      y = "Peso (ton)"
   )+
   scale_y_continuous(labels = comma_format(
      big.mark = ".",
      decimal.mark = ","
   ))+
   scale_fill_manual(values = paleta_4_cores_impo_vol)

plot1

# gráfico valores de importação por ano-----------------------------------------------
# base
peso_val_impo <- impo |> 
   select(
      "produto",
      "ano",
      "valor"
   ) |> 
   group_by(ano) |> 
   summarise(valores = sum(valor)) |> 
   mutate(ano = as.factor(ano))

# gráfico
plot2 <- peso_val_impo |> 
   ggplot()+
   aes(x = ano, 
       y = valores,
       fill = ano,
       label = valores)+
   geom_col()+
   geom_label(
      vjust = -0.5,
      colour = "#000000",
      alpha = 0
   )+
   theme(
      legend.position = "none"
   )+
   theme_enem_fundo_branco()+
   labs(
      title = "Valores de Importações x ano",
      x = "Ano",
      y = "Peso (ton)"
   )+
   scale_y_continuous(labels = comma_format(
      big.mark = ".",
      decimal.mark = ","
   ))+
   scale_fill_manual(values = paleta_4_cores_expo_val)

plot2

# gráfico de volume de mercadorias por ano na importação (anos de 2019, 2020, 2021, 2022)
# base
mercadoria_vol_per <- impo |>
  select(produto, peso, ano) |>
  group_by(produto, ano) |>
  summarise(volume = sum(peso)) |>
  mutate(volume = volume) |> 
  ungroup() |> 
  group_by(ano) |> 
  summarise(produto,ano, percentual = round((volume/sum(volume))*100,2)) |> 
  mutate(
    produto = as.factor(produto),
    ano = as.factor(ano)
  )
   
# gráfico
plot2_a <- mercadoria_vol_per |> 
   ggplot()+
   aes(
      x = produto,
      y = percentual,
      fill = ano,
      label = percentual
      )+
   geom_col(
      position = "dodge",
      color = "#000000")+
   geom_label(
      aes(label = percentual),
      vjust = -0.6,
      alpha = 0.5,
      position = position_dodge(width = 1),
      size = 3.5,
      color = "#000000"
      
   )+
   theme_enem_fundo_branco()+
   labs(
      title = "Volumes percentuais por tipo de produto e ano",
      x = "Produto",
      y = "Percentuais"
   )+
   guides(
      fill = guide_legend(
         title = "Ano",
         override.aes = aes(label = "")
      )
   )+
   scale_y_continuous(labels = comma_format(
      big.mark = ".",
      decimal.mark = ","
   ))+
   scale_fill_manual(values = paleta_4_cores_impo_vol)
   
plot2_a



# gráfico de volume de mercadorias por ano na importação (anos de 2019, 2020, 2021, 2022)
# base
mercadoria_vol <- impo |>
   select(produto, peso, ano) |>
   group_by(produto, ano) |>
   summarise(percentual = sum(peso)) |>
   mutate(
      produto = fct_reorder(
         produto,
         volume,
         .desc = TRUE
      ),
      ano = as.factor(ano)
   )

# gráfico
plot2 <- mercadoria_vol |> 
   ggplot()+
   aes(
      x = produto,
      y = volume,
      fill = ano,
      label = volume
   )+
   geom_col(
      position = "dodge",
      color = "#000000")+
   geom_label(
      aes(label = round(volume,0)),
      vjust = -0.6,
      alpha = 0.5,
      position = position_dodge(width = 1),
      size = 3.5,
      color = "#000000"
      
   )+
   theme_enem_fundo_branco()+
   labs(
      title = "Volumes por tipo de produto e ano",
      x = "Produto",
      y = "Volumes"
   )+
   guides(
      fill = guide_legend(
         title = "Ano",
         override.aes = aes(label = "")
      )
   )+
   scale_y_continuous(labels = comma_format(
      big.mark = ".",
      decimal.mark = ","
   ))+
   scale_fill_manual(values = paleta_4_cores_impo_vol)

plot2



# gráfico volume importacao por mês no ano

vol_imp_mes_ano <- impo |> 
   select(mes, ano, peso) |> 
   group_by(mes, ano) |>
   summarise(volume = sum(peso)) |> 
   mutate(ano = as.factor(ano))

plot3 <- vol_imp_mes_ano |> 
   ggplot()+
   aes(x = mes, y = volume, group = ano, color = ano)+
   geom_line(size = 1)+
   geom_point(size = 3)+
   scale_x_continuous(
      breaks = seq(0, 12, 1)
   )+
   theme_enem_fundo_branco()+
   labs(
      title = "Volume de importação por mês",
      x = "Mês",
      y = "Volumes (ton)"
   )+
   scale_color_manual(values = paleta_4_cores_impo_vol)

plot3 |> 
   ggplotly()

# gráfico volume importação por produtos nos anos 2019, 2020, 2021 e 2022

paleta_5_cores_impo_vol <- c("#AE2030", "#8B4095", "#6A5E68", "#F2A519", "#28B46D")
paleta_5_cores_impo_vol2 <- c("#8B4095", "#6A5E68", "#F2A519", "#28B46D", "#AE2030")

vol_impo_mes_produto_2019 <- impo |> 
   filter(ano == "2019") |> 
   select(produto, peso, mes) |> 
   group_by(mes, produto) |> 
   summarise(volume = sum(peso)) |> 
   mutate(produto = as.factor(produto))

plot4_1 <- vol_impo_mes_produto_2019 |> 
   ggplot()+
   aes(
      x = mes,
      y = volume,
      label = volume,
      color = produto,
      group = produto
   )+
   geom_line()+
   geom_point(size = 3)+
   scale_x_continuous(
      breaks = seq(0, 12, 1)
   )+
   theme_enem_fundo_branco()+
   labs(
      title = "Volume de Importação por produto em 2019",
      x = "Mês",
      y = "Volumes(ton)"
   )+
   scale_color_manual(values = paleta_5_cores_impo_vol)

plot4_1

# gráfico volume importação por mes e produto em 2020
vol_impo_mes_produto_2020 <- impo |> 
   filter(ano == "2020") |> 
   select(produto, peso, mes) |> 
   group_by(mes, produto) |> 
   summarise(volume = sum(peso)) |> 
   mutate(produto = as.factor(produto))


plot4_2 <- vol_impo_mes_produto_2020 |> 
   ggplot()+
   aes(
      x = mes,
      y = volume,
      label = volume,
      color = produto,
      group = produto
   )+
   geom_line()+
   geom_point(size = 3)+
   scale_x_continuous(
      breaks = seq(0, 12, 1)
   )+
   theme_enem_fundo_branco()+
   labs(
      title = "Volume de Importação por produto em 2020",
      x = "Mês",
      y = "Volumes(ton)"
   )+
   scale_color_manual(values = paleta_5_cores_impo_vol)

plot4_2


# gráfico volume importação por mes e produto em 2021
vol_impo_mes_produto_2021 <- impo |> 
   filter(ano == "2021") |> 
   select(produto, peso, mes) |> 
   group_by(mes, produto) |> 
   summarise(volume = sum(peso)) |> 
   mutate(produto = as.factor(produto))


plot4_3 <- vol_impo_mes_produto_2021 |> 
   ggplot()+
   aes(
      x = mes,
      y = volume,
      label = volume,
      color = produto,
      group = produto
   )+
   geom_line()+
   geom_point(size = 3)+
   scale_x_continuous(
      breaks = seq(0, 12, 1)
   )+
   theme_enem_fundo_branco()+
   labs(
      title = "Volume de Importação por produto em 2021",
      x = "Mês",
      y = "Volumes(ton)"
   )+
   scale_color_manual(values = paleta_5_cores_impo_vol2)

plot4_3

# gráfico volume importação por mes e produto em 2022
vol_impo_mes_produto_2022 <- impo |> 
   filter(ano == "2022") |> 
   select(produto, peso, mes) |> 
   group_by(mes, produto) |> 
   summarise(volume = sum(peso)) |> 
   mutate(produto = as.factor(produto))


plot4_4 <- vol_impo_mes_produto_2022 |> 
   ggplot()+
   aes(
      x = mes,
      y = volume,
      label = volume,
      color = produto,
      group = produto
   )+
   geom_line()+
   geom_point(size = 3)+
   scale_x_continuous(
      breaks = seq(0, 12, 1)
   )+
   theme_enem_fundo_branco()+
   labs(
      title = "Volume de Importação por produto em 2022",
      subtitle = "Primeiro semestre",
      x = "Mês",
      y = "Volumes(ton)"
   )+
   scale_color_manual(values = paleta_5_cores_impo_vol2)

plot4_4

library(patchwork)
plot4 <-  (plot4_1 + plot4_2)/(plot4_3 + plot4_4)

plot4

# gráfico dinâmico do fluxo do porto de santa helena
anim_impo_mes <- impo |> 
   select(mes, peso, ano) |> 
   group_by(mes, ano) |> 
   summarise(volume = sum(peso)) |> 
   ggplot()+
   aes(x = mes, y = volume)+
   geom_line(color = "#3185FC",
             size = 0.5,
             lineend = "round")+
   geom_point(color = "red")+
   facet_wrap(~ ano)+
   scale_x_continuous(
      breaks = seq(0, 12, 1)
   )+
   labs(
      x = "Meses de janeiro a dezembro",
      y = "Volume em toneladas",
    )+
   transition_reveal(mes)+
   theme_enem_fundo_branco()+
   theme_linedraw()


animate(
   anim_impo_mes,
   nframes = 40,
   duration = 12,
   start_pause = 2,
   end_pause = 8,
   width = 800,
   height = 400
)

# Quantidades de processos de importação por ano
num_di <- read_xlsx("data-raw/numero_dis.xlsx")

num_di <- num_di |> 
   group_by(ano) |> 
   summarise(qte_di_ano = sum(qte_di)) |> 
   mutate(ano = as.factor(ano))

plot6_i <- num_di |> 
   ggplot()+
   aes(x = ano, 
       y = qte_di_ano,
       fill = ano,
       label = qte_di_ano)+
   geom_col()+
   geom_label(
      vjust = -0.5,
      colour = "#000000",
      alpha = 0
   )+
   theme(
      legend.position = "none"
   )+
   theme_enem_fundo_branco()+
   labs(
      title = "Quantidade de DI's x ano",
      x = "Ano",
      y = "Quantidade"
   )+
   scale_fill_manual(values = paleta_4_cores_impo_vol)

plot6_i



############################################EXPORTACAO#########################################

# consolidar todos os arquivos 

# criando um vetor com todos os caminhos usando o list.files()


# o mesmo resultado usando o pacote fs


arquivos_com_fs_1 <- fs::dir_ls(path = "data-raw", glob = '*xlsx')

arquivos_com_fs |> 
   purrr::map(readxl::read_xlsx) |> 
   dplyr::bind_rows()


################################# união das planilhas xlsx ####################################

# arquivos sem problema na importação em se tratando de tipos
arquivos_1 <- list.files(path = "data-raw",
                       pattern = '.xlsx$',
                       full.names = TRUE
)

arq1 <- arquivos_1 |> 
   purrr::map(readxl::read_xlsx) |> 
   dplyr::bind_rows()

arq1

# arquivos com problema na importação mas tipo de erro idêntico
arquivos_2 <- list.files(path = "data-raw/arquivos_arrumar/",
                         pattern = '.xlsx$',
                         full.names = TRUE
)



arq2 <- arquivos_2 |> 
   purrr::map(readxl::read_xlsx) |> 
   dplyr::bind_rows()


arq2$ano_desembaraco <- as.character(arq2$ano_desembaraco)

arq2

# tratamento de arquivos com erros únicos
arq3 <- readxl::read_xlsx("data-raw/arquivos_arrumar/arquivos_arruma2/outubro_2020.xlsx")

arq3$mes_desembaraco <- as.character(arq3$mes_desembaraco)

# tratamento de arquivos com erros únicos
arq4 <- readxl::read_xlsx("data-raw/arquivos_arrumar/arquivos_arruma2/dezembro_2021.xlsx")

arq4$mes_desembaraco <- as.character(arq4$mes_desembaraco)
arq4$ano_desembaraco <- as.character(arq4$ano_desembaraco)

# inclusão dos dados de 2019
arquivos_5 <- list.files(path = "data-raw/arquivos_arrumar/2019/",
                         pattern = '.xlsx$',
                         full.names = TRUE
)



arq5 <- arquivos_5 |> 
   purrr::map(readxl::read_xlsx) |> 
   dplyr::bind_rows()


# unir os quatro arquivos (arq1 + arq2 + arq3 + arq4)
expo <- dplyr::bind_rows(arq1, arq2, arq3, arq4, arq5)



################################## ANÁLISE DOS DADOS ##########################################

# excluir a coluna gerada 06
expo$`06` <- NULL

write_csv(expo,"data/expo.csv")

# Volumes na exportação

expo <- read_csv("data/expo.csv")

volumes_expo_ton <- expo |> 
   select(ano_desembaraco, peso_liquido_expo_kg) |> 
   group_by(ano_desembaraco) |> 
   summarise(volumes_kg = sum(peso_liquido_expo_kg), volumes_ton = volumes_kg/1000) |> 
   mutate(ano_desembaraco = as.factor(ano_desembaraco))


# gráfico
plot1_e <- volumes_expo_ton |> 
   ggplot()+
   aes(x = ano_desembaraco, 
       y = volumes_ton,
       fill = ano_desembaraco,
       label = volumes_ton)+
   geom_col()+
   geom_label(
      vjust = -0.5,
      colour = "#000000",
      alpha = 0
   )+
   theme_enem_fundo_branco()+
   labs(
      title = "Toneladas x ano",
      x = "Ano",
      y = "Volumes"
   )+
   scale_y_continuous(
      labels = comma_format(
         big.mark = ".",
         decimal.mark = ","
      ),
      limits = c(0, 50000),
      breaks = seq(0, 50000, 5000))+
   scale_fill_manual(values = paleta_4_cores_impo_vol) +
   theme(
      legend.position = "none",
      plot.title = element_text(
         size = 16,
         face = "bold",
         family = "",
         hjust = 0,
         margin = unit(c(0, 0, 0.5, 0.5), "cm")
      ))

plot1_e


# valores na exportação

valores_expo_ton <- expo |> 
   select(ano_desembaraco, vmle_dolar_expo) |> 
   group_by(ano_desembaraco) |> 
   summarise(valores = sum(vmle_dolar_expo)) |> 
   mutate(ano_desembaraco = as.factor(ano_desembaraco))

# gráfico
plot2_e <- valores_expo_ton |> 
   ggplot()+
   aes(x = ano_desembaraco, 
       y = valores,
       fill = ano_desembaraco,
       label = valores)+
   geom_col()+
   geom_label(
      vjust = -0.5,
      colour = "#000000",
      alpha = 0
   )+
   theme(
      legend.position = "none"
   )+
   theme_enem_fundo_branco()+
   labs(
      title = "Dólares x ano",
      x = "Ano",
      y = "Dólares"
   )+
   scale_y_continuous(labels = comma_format(
      big.mark = ".",
      decimal.mark = ","
   ),
   limits = c(0, 18000000),
   breaks = seq(0, 18000000, 5000000))+
   scale_fill_manual(values = paleta_4_cores_expo_val)+
   theme(
      legend.position = "none",
      plot.title = element_text(
         size = 16,
         face = "bold",
         family = "",
         hjust = 0,
         margin = unit(c(0, 0, 0.5, 0.5), "cm")))

plot2_e


# Fluxo da expo ao longo do ano

volumes_expo_ton_mes <- expo |> 
   select(ano_desembaraco, peso_liquido_expo_kg, mes_desembaraco) |> 
   group_by(ano_desembaraco, mes_desembaraco) |> 
   summarise(volumes_kg = sum(peso_liquido_expo_kg), volumes_ton = volumes_kg/1000) |> 
   mutate(ano_desembaraco = as.factor(ano_desembaraco), mes_desembaraco = as.numeric(mes_desembaraco))


# gráfico dinâmico do fluxo do porto de santa helena
anim_expo_mes <- volumes_expo_ton_mes |> 
   select(mes_desembaraco, volumes_ton, ano_desembaraco) |> 
   group_by(mes_desembaraco, ano_desembaraco) |> 
   summarise(volume = sum(volumes_ton)) |> 
   ggplot()+
   aes(x = mes_desembaraco, y = volume)+
   geom_line(color = "#3185FC",
             size = 0.5,
             lineend = "round")+
   geom_point(color = "red")+
   facet_wrap(~ ano_desembaraco)+
   labs(
      x = "Meses de janeiro a dezembro",
      y = "Volume em toneladas",
   )+
   transition_reveal(mes_desembaraco)+
   theme_enem_fundo_branco()+
   theme_linedraw()

animo_expo_mes

# Quantidade de dues por ano

num_due <- expo |> 
   group_by(ano_desembaraco) |> 
   summarise(qte_due_ano = n()) |> 
   mutate(ano_desembaraco = as.factor(ano_desembaraco))

plot6_e <- num_due |> 
   ggplot()+
   aes(x = ano_desembaraco, 
       y = qte_due_ano,
       fill = ano_desembaraco,
       label = qte_due_ano)+
   geom_col()+
   geom_label(
      vjust = -0.5,
      colour = "#000000",
      alpha = 0
   )+
   theme(
      legend.position = "none"
   )+
   theme_enem_fundo_branco()+
   labs(
      title = "Quantidade de Due's x ano",
      x = "Ano",
      y = "Quantidade"
   )+
   scale_fill_manual(values = paleta_4_cores_impo_vol)

plot6_e



# Quantidade de dues por ano e mes

paleta_cores_canal <- c("#FF9B42", "#4A7856", "#EF233C")
num_due_mes <- expo |> 
   mutate(canal = stringr::str_to_lower(canal)) |>  
   select(ano_desembaraco, canal, mes_desembaraco) |> 
   group_by(ano_desembaraco, canal) |> 
   summarise(qte_canal = n())

plot11 <- num_due_mes |> 
   ggplot()+
   aes(x = canal, y = qte_canal, label = qte_canal, fill = canal)+
   geom_col()+ 
   facet_wrap(~ano_desembaraco)+
   scale_fill_manual(values = paleta_cores)+
   geom_label(vjust = -0.3, show.legend = FALSE)+
   theme_enem_fundo_branco()+
   labs(
      title = "Canais de parametrização",
      subtitle = "Número de processos por canal de conferência",
      x = "",
      y = ""
   )+
   scale_y_continuous(
      limits = c(0,900)
   )
   

# Quantidade de dues por ano e mes

paleta_cores_canal <- c("#FF9B42", "#4A7856", "#EF233C")
num_due_mes <- expo |> 
   mutate(canal = stringr::str_to_lower(canal)) |>  
   select(ano_desembaraco, canal, mes_desembaraco) |> 
   group_by(ano_desembaraco, canal) |> 
   summarise(qte_canal = n())

plot11 <- num_due_mes |> 
   ggplot()+
   aes(x = canal, y = qte_canal, label = qte_canal, fill = canal)+
   geom_col()+ 
   facet_wrap(~ano_desembaraco)+
   scale_fill_manual(values = paleta_cores_canal)+
   geom_label(vjust = -0.3, show.legend = FALSE)+
   theme_enem_fundo_branco()+
   labs(
      title = "Canais de parametrização",
      subtitle = "Número de processos por canal de conferência",
      x = "",
      y = ""
   )+
   scale_y_continuous(
      limits = c(0,900)
   )


plot11


############################################ BAGAGEM ##########################################

# análise do total de pessoas que cruzam a fronteira
qte_pessoas <- read_xlsx("data-raw/quantidade_pessoas.xlsx")

qte_pessoas |> 
   group_by(ano) |> 
   summarize(saida = sum(qte_saida),
             entrada = sum(qte_entrada)
             ) |> 
   mutate(ano = as.numeric(ano)) |> 
   pivot_longer(cols = c(saida, entrada),
                names_to = "Fluxo",
                values_to = "movimento") |> 
   ggplot()+
   aes(x = ano, y = movimento, fill = Fluxo, label = movimento)+
   geom_line()+
   geom_point(size = 4)+
   geom_label_repel(show.legend = NA)+
   annotate("text",
            x = 2020.8,
            y = 7500, 
            label = "Fronteira fechada entre abril/2020 a abril/2021 ")+
   labs(
      title = "Entradas e saídas de pessoas",
      x = "",
      y = ""
      )+
   theme_enem_fundo_branco()

# quantidade de veículos de passeio que cruzam a fronteira

qte_veiculos <- read_xlsx("data-raw/quantidade_veiculos.xlsx")

qte_veiculos |> 
   group_by(ano) |> 
   summarize(saida = sum(qte_saida),
             entrada = sum(qte_entrada)
   ) |> 
   mutate(ano = as.numeric(ano)) |> 
   pivot_longer(cols = c(saida, entrada),
                names_to = "Fluxo_veiculos",
                values_to = "movimento") |> 
   ggplot()+
   aes(x = ano, y = movimento, fill = Fluxo_veiculos, label = movimento)+
   geom_line()+
   geom_point(size = 4)+
   geom_label_repel(show.legend = NA)+
   annotate("text",
            x = 2020.8,
            y = 1500, 
            label = "Fronteira fechada entre abril/2020 a abril/2021 ")+
   labs(
      title = "Entradas e saídas de veículos",
      x = "",
      y = ""
   )+
   theme_enem_fundo_branco()


# quantidade de veículos de carga que cruzam a fronteira

qte_veiculos_carga <- read_xlsx("data-raw/quantidade_veiculos_carga.xlsx")

qte_veiculos_carga |> 
   group_by(ano) |> 
   summarize(saida = sum(qte_saida),
             entrada = sum(qte_entrada)
   ) |> 
   mutate(ano = as.numeric(ano)) |> 
   pivot_longer(cols = c(saida, entrada),
                names_to = "Fluxo_veiculos",
                values_to = "movimento") |> 
   ggplot()+
   aes(x = ano, y = movimento, fill = Fluxo_veiculos, label = movimento)+
   geom_line()+
   geom_point(size = 4)+
   geom_label_repel(show.legend = NA)+
   annotate("text",
            x = 2019.5,
            y = 22000, 
            label = "Obs: durante a pandemia as atividades de importação e exportação não foram interrompidas ")+
   labs(
      title = "Entradas e saídas de veículos de carga",
      x = "",
      y = ""
   )+
   theme_enem_fundo_branco()+
   scale_y_continuous(limits = c(0, 22000))

