# Lendo o arquivo de trabalho

library(readxl)
base_ipea_copia_ <- read_excel("base_ipea(copia).xlsx")
View(base_ipea_copia_)

# Instalando pacotes que serão usados
install.packages("tidyverse")
install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot")
install.packages("esquisse")
# Mudando a categoria das variáveis

library(dplyr)
library(magrittr)

base_ipea <- base_ipea_copia_ %>%
  mutate(regiao_onde_foi_realizada_a_entrevista = as.factor(base_ipea_copia_$regiao_onde_foi_realizada_a_entrevista),
         idade = as.integer(base_ipea_copia_$idade),
         area = as.factor(base_ipea_copia_$area),
         sexo = as.factor(base_ipea_copia_$sexo),
         cor_ou_raca = as.factor(base_ipea_copia_$cor_ou_raca),
         ultima_serie_escolar_que_concluiu_com_aprovacao = as.factor(base_ipea_copia_$ultima_serie_escolar_que_concluiu_com_aprovacao),
         ate_que_serie_o_seu_pai_estudou = as.factor(base_ipea_copia_$ate_que_serie_o_seu_pai_estudou),
         ate_que_serie_a_sua_mae_estudou = as.factor(base_ipea_copia_$ate_que_serie_a_sua_mae_estudou),
         renda_total_do_chefe_da_família_no_ultimo_mes = as.numeric(base_ipea_copia_$renda_total_do_chefe_da_família_no_ultimo_mes),
         renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes = as.numeric(base_ipea_copia_$renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes),
         numero_de_moradores_no_domicilio_parentes_e_agregados = as.integer(base_ipea_copia_$numero_de_moradores_no_domicilio_parentes_e_agregados),
         no_ultimo_mes_alguma_pessoa_deste_domicilio_recebeu_rendimentos_do_bolsa_familia = as.factor(base_ipea_copia_$no_ultimo_mes_alguma_pessoa_deste_domicilio_recebeu_rendimentos_do_bolsa_familia),
         religiao = as.factor(base_ipea_copia_$religiao),
         mulheres_que_usam_roupas_que_mostram_o_corpo_merecem_ser_atacadas = as.factor(base_ipea_copia_$mulheres_que_usam_roupas_que_mostram_o_corpo_merecem_ser_atacadas),
         as_mulatas_sao_mais_fogosas_do_que_as_mulheres_brancas = as.factor(base_ipea_copia_$as_mulatas_sao_mais_fogosas_do_que_as_mulheres_brancas),
         da_para_entender_que_um_homem_que_cresceu_em_uma_familia_violenta_agrida_sua_mulher = as.factor(base_ipea_copia_$da_para_entender_que_um_homem_que_cresceu_em_uma_familia_violenta_agrida_sua_mulher),
         os_homens_devem_ser_a_cabeca_do_lar = as.factor(base_ipea_copia_$os_homens_devem_ser_a_cabeca_do_lar),
         casos_de_violencia_dentro_de_casa_devem_ser_discutidos_somente_entre_os_membros = as.factor(base_ipea_copia_$casos_de_violencia_dentro_de_casa_devem_ser_discutidos_somente_entre_os_membros),
         incomoda_ver_dois_homens_ou_duas_mulheres_se_beijando_na_boca_em_publico = as.factor(base_ipea_copia_$incomoda_ver_dois_homens_ou_duas_mulheres_se_beijando_na_boca_em_publico),
         se_as_mulheres_soubessem_como_se_comportar_haveria_menos_estupros = as.factor(base_ipea_copia_$se_as_mulheres_soubessem_como_se_comportar_haveria_menos_estupros),
         homem_que_bate_na_esposa_tem_que_ir_para_a_cadeia = as.factor(base_ipea_copia_$homem_que_bate_na_esposa_tem_que_ir_para_a_cadeia),
         e_violencia_falar_mentiras_sobre_uma_mulher_para_os_outros = as.factor(base_ipea_copia_$e_violencia_falar_mentiras_sobre_uma_mulher_para_os_outros),
         toda_mulher_sonha_em_se_casar = as.factor(base_ipea_copia_$toda_mulher_sonha_em_se_casar),
         o_que_acontece_com_o_casal_em_casa_nao_interessa_aos_outros = as.factor(base_ipea_copia_$o_que_acontece_com_o_casal_em_casa_nao_interessa_aos_outros),
         quando_ha_violencia_os_casais_devem_se_separar = as.factor(base_ipea_copia_$quando_ha_violencia_os_casais_devem_se_separar),
         casais_de_pessoas_do_mesmo_sexo_devem_ter_os_mesmos_direitos_dos_outros_casais = as.factor(base_ipea_copia_$casais_de_pessoas_do_mesmo_sexo_devem_ter_os_mesmos_direitos_dos_outros_casais),
         da_pra_entender_que_um_homem_rasgue_ou_quebre_as_coisas_da_mulher_se_ficou_nervoso = as.factor(base_ipea_copia_$da_pra_entender_que_um_homem_rasgue_ou_quebre_as_coisas_da_mulher_se_ficou_nervoso),
         um_homem_pode_xingar_e_gritar_com_sua_propria_mulher = as.factor(base_ipea_copia_$um_homem_pode_xingar_e_gritar_com_sua_propria_mulher),
         e_da_natureza_do_homem_ser_violento = as.factor(base_ipea_copia_$e_da_natureza_do_homem_ser_violento),
         em_briga_de_marido_e_mulher_nao_se_mete_a_colher = as.factor(base_ipea_copia_$em_briga_de_marido_e_mulher_nao_se_mete_a_colher),
         a_roupa_suja_deve_ser_lavada_em_casa = as.factor(base_ipea_copia_$a_roupa_suja_deve_ser_lavada_em_casa),
         uma_mulher_so_se_sente_realizada_quando_tem_filhos = as.factor(base_ipea_copia_$uma_mulher_so_se_sente_realizada_quando_tem_filhos),
         a_mulher_casada_deve_satisfazer_o_marido_na_cama_mesmo_quando_nao_tem_vontade = as.factor(base_ipea_copia_$a_mulher_casada_deve_satisfazer_o_marido_na_cama_mesmo_quando_nao_tem_vontade),
         piada_de_preto_e_so_brincadeira_nao_e_racismo = as.factor(base_ipea_copia_$piada_de_preto_e_so_brincadeira_nao_e_racismo),
         um_casal_de_dois_homens_vive_um_amor_tao_bonito_quanto_entre_um_homem_e_uma_mulher = as.factor(base_ipea_copia_$um_casal_de_dois_homens_vive_um_amor_tao_bonito_quanto_entre_um_homem_e_uma_mulher),
         mulher_que_e_agredida_e_continua_com_o_parceiro_gosta_de_apanhar = as.factor(base_ipea_copia_$mulher_que_e_agredida_e_continua_com_o_parceiro_gosta_de_apanhar),
         casamento_de_homem_com_homem_ou_de_mulher_com_mulher_deve_ser_proibido = as.factor(base_ipea_copia_$casamento_de_homem_com_homem_ou_de_mulher_com_mulher_deve_ser_proibido),
         a_mulher_que_apanha_em_casa_deve_ficar_quieta_para_nao_prejudicar_os_filhos = as.factor(base_ipea_copia_$a_mulher_que_apanha_em_casa_deve_ficar_quieta_para_nao_prejudicar_os_filhos),
         a_questao_da_violencia_contra_as_mulheres_recebe_mais_importancia_do_que_merece = as.factor(base_ipea_copia_$a_questao_da_violencia_contra_as_mulheres_recebe_mais_importancia_do_que_merece),
         tem_mulher_que_e_pra_casar_tem_mulher_que_e_pra_cama = as.factor(base_ipea_copia_$tem_mulher_que_e_pra_casar_tem_mulher_que_e_pra_cama))

# Resumo dos dados e estatísticas descritivas básicas
summary(base_ipea)

# a)  Defina o seu diretório de trabalho para o local onde se encontra a base de dados. 

getwd()

# b)  Importe a base de dados base_ipea.csv para o software escolhido. 

# R foi o software escolhido

# c) A base é composta por quantas linhas e colunas? 

nrow(base_ipea)
ncol(base_ipea)
# A Base é composta por 3810 linhas e 40 colunas.

# d) Calcule a frequência de pessoas em cada região do Brasil. 

# Usando a função summary como consulta, podemos facilmente calcular as frequencias e apresenta-los em um pequeno gráfico com as respectivas porcentagens

summary(base_ipea$regiao_onde_foi_realizada_a_entrevista)
frequenciaPorRegiao <- tibble(Regioes = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul"),
                              Frequencias = c(((285/3810)*100),((1065/3810)*100),((300/3810)*100),((1605/3810)*100),((555/3810)*100)))
frequenciaPorRegiao  

library(ggplot2)

ggplot(frequenciaPorRegiao) +
 aes(x = Regioes, weight = Frequencias) +
 geom_bar(fill = "#d8576b") +
 labs(y = "%", title = "Frequência por Região") +
 theme_minimal()

# e) Qual é a região mais frequente (moda)? 
# A Região Sudeste é a mais frequente. E pode ser visualizada no gráfico anterior.

# f) Qual é a idade da pessoa mais nova nessa amostra? E da mais velha? 
summary(base_ipea$idade)
# Novamente com o auxílio da função summary podemos ver que a pessoa mais nova possui 16 anos, e a mais velha possui 88.

# g) Calcule a média, a mediana e a moda para a variável idade. A partir disso, o que você pode dizer
# sobre a distribuição dessa variável (assimétrica positiva, assimétrica negativa ou simétrica)? 
mean(base_ipea$idade)
median(base_ipea$idade)

# Criando uma função que calcule a moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(base_ipea$idade)

# A simetria dessa variavel pode ser visualizada no gráfico da sua densidade

library(ggplot2)

ggplot(base_ipea) +
 aes(x = idade) +
 geom_density(adjust = 0.7, fill = "#d8576b") +
 labs(y = "Idade") +
 theme_minimal()

# O gráfico densidade da variável Idade nos mostra que essa variável possui assimetria à direita. Logo, assimetria positiva.

# h) Classifique as idades de acordo com as faixas etárias a seguir. (Crie uma nova coluna no data frame para essa classificação). A amostra é composta de mais Jovens, Adultos ou Idosos?

# x<= 29 - Jovens
# 30 <= x <= 59  - Adultos
# x >= 60 - Idosos

#criando nova coluna

base_ipea <- base_ipea %>%
  mutate(Faixa_Etaria = case_when(idade <= 29 ~ "Jovens",
                                  idade >= 30 & idade <= 59 ~ "Adultos",
                                  idade >= 60 ~ "Idosos"))

esquisser()

library(ggplot2)

ggplot(base_ipea) +
 aes(x = Faixa_Etaria) +
 geom_bar(fill = "#d8576b") +
 labs(x = "Faixa Etária", y = "Contagem") +
 theme_minimal()

# A amostra é composta de mais adultos

# i) Calcule a média, a mediana, o primeiro quartil, o terceiro quartil e os valores máximo e mínimo para a variável “renda total de todos os moradores, parentes e agregados no último mês”. Comente os resultados. 

summary(base_ipea$renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes)

# O valor mínimo da variável em questão é 150. Isso que significa que, pelomenos um lar, declarou uma renda total, com todos os seus integrantes somados de apenas R$150,00 no ultimo mês. O que indica uma situação de grave vulnerabilidade socioeconomica.

# O primeiro quartil possui o valor 800. Isso significa que 25% das observações possuem renda total familiar inferior a R$800,00.

# A mediana possui o valor de 1400 e representa o nosso valor central. 50% das observações declararam uma renda inferior a R$1400,00 e 50% declarou ter recebido uma renda superior a R$1400,00 no ultimo mês.

# As observações declararam ter recebido, em média, uma renda familiar total igual a R$ 1801 no último mês.

# O nosso terceiro quartil possui o valor de 2150, e significa que 75% das observações declararam ter recebido uma quantia menor que R$2150,00 no ultimo mês.

# O valor maximo declarado foi R$19000, todos os outros valores declarados em nosso banco de dados está abaixo desse valor.

# O valor 527 NA's, indica que há 527 dados faltantes.

# j) Interprete o primeiro e o terceiro quartis encontrados no item anterior. 

# O valor 800 representa o primeiro quartil, esse resultado nos diz que 25% das nossa observações declararam renda
# familiar total inferior a R$ 800,00 no último mês. Logo, 75% declararam renda superior a R$ 800 no ultimo mês.
# O valor 2150 representa o terceiro quartil, esse resultado nos diz que 75% das nossas observações declararam 
# renda familiar total inferior a R$ 2150,00 no ultimo mês. Ou seja, apenas 25% das nossa observações declararam renda 
# superior a R$ 2150,00 no ultimo mês.

# k) Crie uma função que calcule o coeficiente de variação.

coeficienteVar <- function(x){
  coeficiente <- sd(x, na.rm = T)/mean(x, na.rm = T)*100
  return(coeficiente)}

# l) Calcule o coeficiente de variação para a variável idade e renda. Compare os dois coeficientes de variação.

coeficienteVar(base_ipea$idade) #cv Idade
coeficienteVar(base_ipea$renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes) # Renda Total
coeficienteVar(base_ipea$renda_total_do_chefe_da_família_no_ultimo_mes) # Renda do chefe de família

# Os coeficientes de variação observados possuem valor maior que 30%, isso significa que os dados estão
# dispersos em torno da média, ou seja, são dados heterogeneos. A variável Idade possui o menor dos valores
# observados: 39,97%. Ainda assim, excede a tolerância para que a amostra seja considerada homogenea.

# m) Calcule o desvio-padrão para a renda de acordo com cada região do Brasil. Qual é a região que possui um comportamento mais homogêneo em relação à renda?
# como a renda não foi especificada, usarei a renda total como objeto
library(dplyr)
library(magrittr)

# CENTRO-OESTE 
CO <- base_ipea %>%
  select(renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes, regiao_onde_foi_realizada_a_entrevista) %>%
  filter(regiao_onde_foi_realizada_a_entrevista == "CENTRO-OESTE")

desvCO <- sd(CO$renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes, na.rm = T)

# NORTE 
N <- base_ipea %>%
  select(renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes, regiao_onde_foi_realizada_a_entrevista) %>%
  filter(regiao_onde_foi_realizada_a_entrevista == "NORTE")

desvN <- sd(N$renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes, na.rm = T)

# NORDESTE 
NO <- base_ipea %>%
  select(renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes, regiao_onde_foi_realizada_a_entrevista) %>%
  filter(regiao_onde_foi_realizada_a_entrevista == "NORDESTE")

desvNO <- sd(NO$renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes, na.rm = T)

# SUL
SUL <- base_ipea %>%
  select(renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes, regiao_onde_foi_realizada_a_entrevista) %>%
  filter(regiao_onde_foi_realizada_a_entrevista == "SUL")

desvSUL <- sd(SUL$renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes, na.rm = T)

# SUDESTE 
SUD<- base_ipea %>%
  select(renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes, regiao_onde_foi_realizada_a_entrevista) %>%
  filter(regiao_onde_foi_realizada_a_entrevista == "SUDESTE")

desvSUD <- sd(SUD$renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes, na.rm = T)

desviosPorRegiao <- tibble(Regioes = c("Norte", "Sudeste","Centro-Oeste", "Sul", "Nordeste"),
                           desvios = c(desvN, desvSUD, desvCO, desvSUL, desvNO))

# Representação gráfica

library(ggplot2)

ggplot(desviosPorRegiao) +
 aes(x = Regioes, weight = desvios) +
 geom_bar(fill = "#d8576b") +
 labs(x = "Regiões", y = "Desvios") +
 theme_minimal()

# A Região nordeste possui o menor desvio, consequentemente um comportamento mais homogeneo em relação a renda.
