temas <- read_excel("dados/Temas_inicial.xlsx", sheet = 1)
anos <- read_excel("dados/Anos.xlsx")

anos$ibge <- substr(anos$ibge,1,6)
temas$ano <- as.factor(temas$ano)
anos$populacao_acompanhada <- round(anos$populacao_acompanhada)

temas_atualizado <- left_join(temas, anos[c(1,3,7)], by = c("ibge", "ano"))

temas_atualizado <- temas_atualizado %>% 
  mutate(Regiao = case_when(
    uf %in% c("AP", "AM", "RR", "PA", "TO", "RO", "AC") ~ "Norte",
    uf %in% c("MA", "PI", "CE", "RN", "PE", "PB", "SE", "AL", "BA") ~ "Nordeste",
    uf %in% c("DF", "MT", "MS", "GO") ~ "Centro-oeste",
    uf %in% c("SP", "RJ", "ES", "MG") ~ "Sudeste",
    uf %in% c("SC", "RS", "PR") ~ "Sul"
  ))
# Indicadores

banco <- temas_atualizado %>% 
  mutate_at(c(5:20), as.numeric)  

colnames(banco)[8] <- "Ações de combate ao Aedes aegy"
colnames(banco)[20] <- "Populacao"


# De acordo com os dados disponíveis, calcularemos os 15 indicadores para cada observação do banco (`r count(banco)` observações), totalizando então `r 15*count(banco)` indicadores.

banco %>% 
  mutate(
    agravo_indicador = `Agravos negligenciados`/Populacao,
    alimentacao_indicador = `Alimentação saudável`/Populacao,
    autocuidado_indicador = `Autocuidado de pessoas com doe`/Populacao,
    acoes_indicador = `Ações de combate ao Aedes aegy`/Populacao,
    cidadania_indicador = `Cidadania e direitos humanos`/Populacao,
    dependencia_indicador = `Dependência química / tabaco /`/Populacao,
    envelhecimento_indicador = `Envelhecimento / Climatério /`/Populacao,
    plantas_indicador = `Plantas medicinais / fitoterap`/Populacao,
    prevencao_indicador = `Prevenção da violência e promo`/Populacao,
    ambiental_indicador = `Saúde ambiental`/Populacao,
    bucal_indicador = `Saúde bucal`/Populacao,
    trabalhador_indicador = `Saúde do trabalhador`/Populacao,
    mental_indicador = `Saúde mental`/Populacao,
    sexual_indicador = `Saúde sexual e reprodutiva`/Populacao,
    semana_saude_indicador = `Semana saúde na escola`/Populacao
    
  ) -> banco

banco <- banco %>% mutate_at(c(22:36), ~ .x * 100000)
banco[banco == "Inf"] <- 0
banco[banco == "NaN"] <- 0
banco <- banco %>% 
  mutate_at(c(22:36), ~(round(.,4)))
indicadores <- select(banco, c(1:4, 21:36))

abas <- list("sheet1" = temas_atualizado, "sheet2" = indicadores)
write.xlsx (abas, "dados/temas_para_saude.xlsx")

write_rds(banco, "dados/temas.rds")
write_rds(indicadores, "dados/indicadores.rds")


