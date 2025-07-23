# Suas bibliotecas originais
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(readxl)
library(quantmod)
library(dplyr)
library(lubridate)
library(openxlsx) # Adicionado aqui para garantir que a função o encontre

################################################################################
#####    Sua função para escrever a tabela (com uma pequena correção)   ########
################################################################################
escrever_res = function(df, fpath, extension, fname){
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  fil_nam <- paste0(fname, '_', timestamp, extension)
  full_path <- paste0(fpath, fil_nam)
  if (extension == '.xlsx'){
    write.xlsx(df, file = full_path)
  } else if (extension == '.csv'){
    write.csv(df, file = full_path, row.names = FALSE)
  } else {
    stop("Extensão de arquivo não suportada. Use '.csv' ou '.xlsx'.")
  }
  return(full_path)
}

#função para removendo valores infinitos

rmv_inf_values_row <- function(df) {
  rows_with_inf <- apply(df, 1, function(row) any(is.infinite(row)))
  df <- df[!rows_with_inf, ]
  return(df)
}

################################################################################
###  Função otimizada para calcular e retornar as variações dos preços      ###
################################################################################
# Argumentos:
#   df_trimestre: O tibble de UM trimestre, vindo da sua lista 'crit_tri'.
#   df_day_completo: O data frame com TODAS as cotações diárias.
#   dias_variacao: O número de dias no futuro para calcular a variação.

calcular_variacao_preco <- function(df_trimestre, df_day_completo, dias_variacao) {
  
  # 1. Prepara os dados iniciais, calculando a data final alvo para cada ação
  df_precos_iniciais <- df_trimestre %>%
    mutate(
      data_inicial = as.Date(data_cotacao),
      data_final_alvo = data_inicial + days(dias_variacao),
      preco_inicial = `Preço da Ação`
    ) %>%
    select(codigo, data_inicial, data_final_alvo, preco_inicial)
  
  # 2. Otimização: Para cada ação, encontra a PRIMEIRA cotação disponível
  #    APÓS a data final alvo. Isso é feito com um join condicional.
  df_precos_finais <- df_precos_iniciais %>%
    left_join(
      df_day_completo %>% select(codigo, data_cotacao, preco_fechamento),
      by = "codigo",
      relationship = "many-to-many"
    ) %>%
    filter(data_cotacao.y >= data_final_alvo) %>% # A data da cotação final deve ser >= à data alvo
    group_by(codigo) %>%
    slice_min(order_by = data_cotacao.y, n = 1, with_ties = FALSE) %>% # Pega a mais próxima
    ungroup() %>%
    select(codigo, preco_final = preco_fechamento, data_final_real = data_cotacao.y)
  
  # 3. Junta os preços iniciais e finais e calcula a variação
  df_resultado_final <- df_precos_iniciais %>%
    left_join(df_precos_finais, by = "codigo") %>%
    # Calcula a variação apenas para as linhas onde um preço final foi encontrado
    mutate(
      variacao_preco = ifelse(
        !is.na(preco_final), 
        (preco_final - preco_inicial) / preco_inicial, 
        NA_real_
      )
    ) %>%
    # Remove casos onde não foi possível calcular a variação (preço final não encontrado)
    filter(!is.na(variacao_preco)) %>%
    select(
      codigo, 
      preco_inicial, 
      preco_final, 
      data_inicial, 
      data_final_real,
      variacao_preco = variacao_preco
    )
  
  # 4. Junta o resultado da variação com a tabela original
  # para "mostrar ele junto com a variação de preços"
  df_trimestre_com_variacao <- df_trimestre %>%
    inner_join(df_resultado_final, by = "codigo")
  
  return(df_trimestre_com_variacao)
}

############################################################################
###############    função para retirar valores extremos    #################
############################################################################

# remover valores extremos
rmv_wild = function(df, fator_multiplicativo) {
  
  exib_wild = function(dados, fator_multiplicativo){
    # Obter estatísticas do boxplot
    stats <- boxplot.stats(dados)
    
    # Calcular limites personalizados
    limite_inferior <- stats$stats[2] - fator_multiplicativo * IQR(dados, na.rm = T)
    limite_superior <- stats$stats[4] + fator_multiplicativo * IQR(dados, na.rm = T)
    
    # Identificar outliers
    outliers <- dados[dados < limite_inferior | dados > limite_superior]
    
    # Exibir os valores dos outliers
    return(outliers)  
  }
  
  wild_row = c('so', 'para','length', 'nao', 'ser', 'zero')
  
  n_iter = 0
  while (length(wild_row) != 0) {
    wild_row = c()
    for (c in 1:ncol(df)) {
      if (length(exib_wild(df[,c], fator_multiplicativo)) != 0){
        for (i in exib_wild(df[,c], fator_multiplicativo)) {
          #numero das linhas
          j = match(i, df[,c])
          df[j,c] = NA
          if (row.names(df)[j] %in% wild_row){} else {
            wild_row = c(wild_row, row.names(df)[j])  
          }
        }
      }
    }
    rmv_na_val <- function(df) {
      incomplete_rows <- !complete.cases(df)
      
      if (any(incomplete_rows)) {
        df <- df[complete.cases(df), ]
      }
      
      return(df)
    }
    
    print(paste('empresas removidas na iteração', n_iter, ':'))
    print(wild_row)
    n_iter = n_iter+1
    
    
    df = rmv_na_val(df)  
  }
  
  return(df)
}


# Seu diretório de trabalho original
setwd("C:/files/programacao/python/acoes_data")

# Seu carregamento de dados original
df_tri = read.csv("data/acoesb3.csv")

df_day = read.csv("data/df_day_2025-07-09_09-55-20.csv")
'emp_codes = unique(df_tri$codigo)
emp_codes_sa = paste0(emp_codes, ".SA") # Adiciona .SA para a busca

# Suas datas originais
data_inicial = min(as.Date(df_tri$ultBal))
data_final = max(as.Date(df_tri$ultInsert))

# Seu dataframe final original
df_day <- data.frame()

# Seu loop e sua lógica originais
for (ticker in emp_codes_sa) { # Usamos a variável com .SA para a busca
  tryCatch({
    # Baixar cotação diária
    dados <- getSymbols(ticker, from = data_inicial, to = data_final, auto.assign = FALSE)
    dados <- data.frame(date = index(dados), coredata(dados))
    colnames(dados) <- c("date", "open", "high", "low", "close", "volume", "adjusted")
    dados$codigo <- ticker # Aqui o código ainda tem .SA
    
    # Baixar dividendos
    dividendos_xts <- getDividends(ticker, from = data_inicial - years(1), to = data_final, auto.assign = FALSE)
    
    dividendos <- data.frame(date = index(dividendos_xts), dividend_val = coredata(dividendos_xts))
    colnames(dividendos) <- c("date", "dividend")
    
    # Sua lógica de cálculo original, linha por linha (rowwise)
    dados <- dados %>%
      rowwise() %>%
      mutate(dividendo_ttm = sum(dividendos$dividend[dividendos$date > (date - 365) & dividendos$date <= date], na.rm = TRUE)) %>%
      ungroup()
    
    # Seu cálculo de Dividend Yield original
    dados <- dados %>%
      mutate(dividend_yield = ifelse(adjusted > 0, dividendo_ttm / adjusted, NA))
    
    # Adicionar ao dataframe final
    df_day <- bind_rows(df_day, dados)
    
    cat("Sucesso:", ticker, "\n")
    
  }, error = function(e) {
    cat("Erro em", ticker, ":", conditionMessage(e), "\n")
  })
}


# --- FINALIZAÇÃO E SALVAMENTO USANDO SUA FUNÇÃO ---
cat("\n--- Processamento Concluído ---\n")

# --- ALTERAÇÃO AQUI ---
# Limpeza final: remove o sufixo .SA da coluna de códigos de uma só vez
cat("Limpando sufixo .SA da coluna de códigos...\n")
df_day$codigo <- gsub(".SA", "", df_day$codigo)
# ----------------------

# Chamando sua função para salvar o arquivo com os códigos já limpos
arquivo_salvo <- escrever_res(
  df = df_day, 
  fpath = "data/", 
  extension = ".csv", 
  fname = "df_day"
)

cat("\nArquivo salvo com sucesso em:", arquivo_salvo, "\n")'

# Carregando os dados
df_tri_raw <- read.csv("data/acoesb3.csv")
df_day_raw <- read.csv("data/df_day_2025-07-09_09-55-20.csv") # Usando o arquivo que o script anterior gerou

# --- PREPARAÇÃO E LIMPEZA INICIAL DOS DADOS ---

# 1. Converter colunas de data para o tipo Date (essencial para joins e filtros)
df_tri <- df_tri_raw %>%
  mutate(
    ultBal = as.Date(ultBal),
    ultInsert = as.Date(ultInsert),
    # Adicionando a coluna nAcoes que é necessária para os cálculos.
    # !! CERTIFIQUE-SE DE QUE ESTA COLUNA EXISTE NO SEU CSV 'acoesb3.csv' !!
    # Se o nome for outro, ajuste aqui. Ex: nAcoes = sua_coluna_de_acoes
    # nAcoes = nAcoes 
  )

df_day <- df_day_raw %>%
  mutate(date = as.Date(date)) %>%
  # Renomeando colunas para clareza e para facilitar o join
  rename(data_cotacao = date, codigo = codigo, preco_fechamento = close, dy_decimal = dividend_yield)

# 2. Filtrar apenas as datas de balanço trimestrais padrão (muito mais rápido que um loop)
valid_endings <- c("03-31", "06-30", "09-30", "12-31")
df_tri_filtrado <- df_tri %>%
  filter(format(ultBal, "%m-%d") %in% valid_endings)

# --- NOVO: Excluindo períodos específicos ---
datas_para_remover <- as.Date(c("2022-03-31", "2022-06-30"))

df_tri_filtrado <- df_tri_filtrado %>%
  filter(!ultBal %in% datas_para_remover)

cat("Info: Períodos", paste(datas_para_remover, collapse = " e "), "removidos da análise.\n")
# -------------------------------------------

# --- O CORAÇÃO DA OTIMIZAÇÃO: JUNÇÃO DE DADOS (SUBSTITUIÇÃO DOS LOOPS) ---

# O objetivo é: para cada linha em df_tri_filtrado, encontrar a primeira cotação em df_day
# que seja igual ou posterior à data de inserção do balanço (ultInsert).

# Usamos um "join por condição" para encontrar todos os pares válidos
df_completo <- df_tri_filtrado %>%
  left_join(
    df_day,
    by = "codigo",
    # A condição de join: data da cotação deve ser posterior à data de inserção do balanço
    relationship = "many-to-many"
  ) %>%
  filter(data_cotacao >= ultInsert) %>%
  # Agora, para cada balanço, selecionamos apenas a cotação mais antiga que satisfaz a condição
  group_by(codigo, ultBal) %>%
  slice_min(order_by = data_cotacao, n = 1, with_ties = FALSE) %>%
  ungroup()

# --- CÁLCULOS DOS INDICADORES (SUBSTITUIÇÃO DO SEGUNDO GRANDE LOOP) ---
# Todos os cálculos são feitos de uma vez, de forma vetorizada, usando mutate()

# Supondo que você tenha as funções de limpeza `rmv_inf_values_row` e `rmv_na_val` carregadas no seu ambiente
# crit = rmv_inf_values_row(crit)
# crit <- rmv_na_val(crit)

crit_df <- df_completo %>%
  # Renomeia colunas para corresponder aos seus cálculos
  rename(
    cotAtual_t = preco_fechamento,
    divY_t = dy_decimal,
    n_ac_t = nAcoes # Certifique-se que df_tri tem a coluna 'nAcoes'
  ) %>%
  # Calcula todos os indicadores de uma vez
  mutate(
    lpa = round(LucLiq12m / n_ac_t, 3),
    lpa3 = round(as.numeric(LucLiq3m) / n_ac_t, 3),
    vpa = round(patLiq / n_ac_t, 3),
    cx_a = round(disponib / n_ac_t, 3),
    ativc_a = round(ativCirc / n_ac_t, 3),
    ativ_a = round(ativos / n_ac_t, 3),
    divb_a = round(divBruta / n_ac_t, 3),
    ebit_a = round(ebit12m / n_ac_t, 3),
    ebit_a3 = round(as.numeric(ebit3m) / n_ac_t, 3),
    dividendos = round(divY_t * cotAtual_t, 3), # DY já é decimal, não precisa dividir por 100
    rec_a = round(recLiq12m / n_ac_t, 3),
    rec_a3 = round(as.numeric(recLiq3m) / n_ac_t, 3),
    preco_acao = cotAtual_t
  ) %>%
  # Seleciona e renomeia as colunas finais para o seu formato desejado
  select(
    ultBal,
    codigo,
    data_cotacao,
    "LPA" = lpa,
    "LPA (tri)" = lpa3,
    "VPA" = vpa,
    "Caixa/Ação" = cx_a,
    "Ativos Circulantes/Ação" = ativc_a,
    "Ativos/Ação" = ativ_a,
    "Dív Bruta/Ação" = divb_a,
    "EBIT/Ação" = ebit_a,
    "EBIT/Ação (tri)" = ebit_a3,
    "Dividendos" = dividendos,
    "Receita/ Ação" = rec_a,
    "Receita/Ação (tri)" = rec_a3,
    "Preço da Ação" = preco_acao
  )

# --- SEPARAÇÃO FINAL POR TRIMESTRE (SUBSTITUIÇÃO DO ÚLTIMO LOOP) ---

# A função group_split() cria a lista de data frames, um para cada trimestre.
# O resultado é idêntico à sua lista 'crit_tri'
crit_tri <- crit_df %>%
  group_by(ultBal) %>%
  group_split()

# Para nomear a lista como antes (opcional, mas bom para consistência)
nomes_trimestres <- crit_df %>%
  arrange(ultBal) %>%
  pull(ultBal) %>%
  unique()

names(crit_tri) <- nomes_trimestres

# --- Verificação do Resultado ---
print("Processo concluído!")
print(paste(length(crit_tri), "data frames trimestrais foram criados na lista 'crit_tri'."))

# Para ver o cabeçalho do primeiro data frame da lista
print(head(crit_tri[[1]]))

# Para ver o cabeçalho do data frame de um trimestre específico
# print(head(crit_tri$`2024-03-31`))


#escolhendo o periodo que sera analisado
per_choice = crit_tri[[4]]

# verificando NA values
colSums(is.na(per_choice))

boxplot(scale(per_choice), range = 3)

per_choice=rmv_wild(per_choice, 3)

boxplot(scale(per_choice), range = 3)


################################################################################
#############         Escolhendo os Índices           ##########################
################################################################################

##############                 regressão                    ####################

# Realize a regressão linear
modelo_regressao <- lm(per_choice$`Preço da Ação`~per_choice$LPA+
                         per_choice$`LPA (tri)`+per_choice$VPA+
                         per_choice$`Caixa/Ação`+
                         per_choice$`Ativos Circulantes/Ação`+
                         per_choice$`Ativos/Ação`+per_choice$`Dív Bruta/Ação`+
                         per_choice$`EBIT/Ação`+per_choice$`EBIT/Ação (tri)`+
                         per_choice$Dividendos+per_choice$`Receita/ Ação`+
                         per_choice$`Receita/Ação (tri)`)

# Exiba o sumário do modelo
summary(modelo_regressao)

# vizualizando a matriz de correlação
library(ggcorrplot)
corr_matrix <- data.frame(cor(scale(per_choice)))
ggcorrplot(corr_matrix)

##    Argumentos
# 1 - object
# 2 - file path
# 3 - extensão do arquivo (.alguma_coisa)
# 4 - nome do arquivo
escrever_res (corr_matrix,
    'C:/files/projects/programacao/python/acoes_data/logs/correlation_matrix/',
    '.xlsx', 'corr_per_choice')

# regressão com algumas variáveis retiradas (valor mais alto das corr altas)
model_fit = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                 per_choice$`LPA (tri)`+per_choice$VPA+
                 per_choice$`Caixa/Ação`+
                 per_choice$`Ativos Circulantes/Ação`+
                 per_choice$`Ativos/Ação`+per_choice$`Dív Bruta/Ação`+
                 per_choice$`EBIT/Ação`+per_choice$`EBIT/Ação (tri)`+
                 per_choice$Dividendos+
                 per_choice$`Receita/Ação (tri)`)

summary(model_fit)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit2 = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                  per_choice$`LPA (tri)`+per_choice$VPA+
                  per_choice$`Caixa/Ação`+
                  per_choice$`Ativos Circulantes/Ação`+
                  per_choice$`Ativos/Ação`+per_choice$`Dív Bruta/Ação`+
                  per_choice$`EBIT/Ação`+
                  per_choice$Dividendos+
                  per_choice$`Receita/Ação (tri)`)

summary(model_fit2)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit3 = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                  per_choice$`LPA (tri)`+per_choice$VPA+
                  per_choice$`Caixa/Ação`+
                  per_choice$`Ativos Circulantes/Ação`+
                  per_choice$`Dív Bruta/Ação`+
                  per_choice$`EBIT/Ação`+
                  per_choice$Dividendos+
                  per_choice$`Receita/Ação (tri)`)

summary(model_fit3)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit4 = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                  per_choice$`LPA (tri)`+per_choice$VPA+
                  per_choice$`Caixa/Ação`+
                  per_choice$`Ativos Circulantes/Ação`+
                  per_choice$`EBIT/Ação`+
                  per_choice$Dividendos+
                  per_choice$`Receita/Ação (tri)`)

summary(model_fit4)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit5 = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                  per_choice$`LPA (tri)`+per_choice$VPA+
                  per_choice$`Caixa/Ação`+
                  per_choice$`Ativos Circulantes/Ação`+
                  per_choice$`EBIT/Ação`+
                  per_choice$`Receita/Ação (tri)`)

summary(model_fit5)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit6 = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                  per_choice$VPA+
                  per_choice$`Caixa/Ação`+
                  per_choice$`Ativos Circulantes/Ação`+
                  per_choice$`EBIT/Ação`+
                  per_choice$`Receita/Ação (tri)`)

summary(model_fit6)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit7 = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                  per_choice$VPA+
                  per_choice$`Caixa/Ação`+
                  per_choice$`Ativos Circulantes/Ação`+
                  per_choice$`EBIT/Ação`)
summary(model_fit7)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit8 = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                  per_choice$VPA+
                  per_choice$`Caixa/Ação`+
                  per_choice$`Ativos Circulantes/Ação`+
                  per_choice$`EBIT/Ação`)

summary(model_fit8)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit9 = lm(per_choice$`Preço da Ação`~
                  per_choice$`LPA (tri)`+per_choice$VPA+
                  per_choice$Dividendos)

summary(model_fit9)

# função para a equação da reta de regressão
#
#reta = function (lpa, vpa, cxa, atca) {
#  y = 3.1065 + 3.2643*lpa + 0.4131*vpa + 1.3144*cxa + -0.2865*atca
#  return(round(y, 2))
#}
#
## vetor para conter os valores
#p_jus = c()
#for (p in 1:nrow(per_choice)) {
#  p_jus[p] = reta(per_choice[p,"LPA"], per_choice[p,"VPA"],
#                  per_choice[p, "Caixa/Ação"],
#                  per_choice[p, "Ativos Circulantes/Ação"])
#}
#

# função para a equação da reta de regressão

reta = function (lpa_t, vpa, divid) {
  y = 2.3702 + 6.7774*lpa_t + 0.4300*vpa + 6.1210*divid
  return(round(y, 2))
}

# vetor para conter os valores
p_jus = c()
for (p in 1:nrow(per_choice)) {
  p_jus[p] = reta(per_choice[p,"LPA (tri)"], per_choice[p,"VPA"],
                  per_choice[p, "Dividendos"])
}

#adicionando ao db

per_choice$preco_jus = p_jus

# analisando a discrepancia

disc = (per_choice$preco_jus - per_choice$`Preço da Ação`)/
  per_choice$`Preço da Ação`

#adicionando ao db

per_choice$discrepancia = disc
