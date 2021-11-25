library("tidyverse")
library("readxl") # carrega funções para carregar dataset *.xlxs
library("writexl") # carrega funções para exportar o dataset do R para *.xlxs

# importar o dataset

clientes <- read_excel("cgdis_clientes.xlsx")
nomes_clientes <- read_excel("cgdis_nomes_clientes.xlsx")
  
# analisar a estrutura do dataset

glimpse(clientes)
glimpse(nomes_clientes)

# excluir as variáveis indesejadas do dataset: x e y

clientes_v1 <- clientes %>% select(everything(),-c(x,y))
glimpse(clientes_v1)

# trazer os nomes dos clientes do dataset cgdis_nomes_clientes para o dataset clientes_v1

clientes_v1 <- left_join(clientes_v1,nomes_clientes, by = "id") %>% 
  relocate(nome,sobrenome,.after = individuos)

view(clientes_v1)

# criar os vetores faixa_renda e consumo_medio e inclui-los no dataset clientes_v1

faixa_renda <- NA
consumo_medio <- NA

clientes_v1 <- mutate(clientes_v1, 
                      faixa_renda,
                      consumo_medio=renda*0.3) %>% 
               relocate(faixa_renda,consumo_medio,.after = renda)
               
view(clientes_v1)

# analisar as variáveis qualitativas para verificar possíveis inconsistências no preenchimento das categorias

table(clientes_v1$sexo)
table(clientes_v1$escolaridade)
table(clientes_v1$nacionalidade)
table(clientes_v1$cidade)
table(clientes_v1$tipo_logradouro)

# filtrar as observações com inconsistências nos valores da variável cidade

cidade_NA <- filter(clientes_v1,
                            cidade =="-"|cidade=="."|cidade=="..."|cidade=="A")

# substituir os valores errados por NA

clientes_v1 <- mutate(clientes_v1,
                      cidade = replace(cidade, cidade=="A", NA),
                      cidade = replace(cidade, cidade==".", NA),
                      cidade = replace(cidade, cidade=="...", NA),
                      cidade = replace(cidade, cidade=="-", NA))

table(clientes_v1$cidade)

# filtrar as observações com inconsistências novas valores da variável tipo_logradouro

logradouro_erros <- filter(clientes_v1,
                    tipo_logradouro =="AV"|tipo_logradouro =="Av."|tipo_logradouro =="AVENIDA"|
                    tipo_logradouro =="Est"|tipo_logradouro =="Est."|tipo_logradouro =="Estr"|
                    tipo_logradouro =="r"|tipo_logradouro =="R"|tipo_logradouro =="R."|tipo_logradouro =="RUA")

# substituir os valores errados pelos corretos

clientes_v1 <- mutate(clientes_v1,
                      tipo_logradouro = replace(tipo_logradouro, tipo_logradouro=="AV", "Avenida"),
                      tipo_logradouro = replace(tipo_logradouro, tipo_logradouro=="Av.", "Avenida"),
                      tipo_logradouro = replace(tipo_logradouro, tipo_logradouro=="AVENIDA", "Avenida"),
                      tipo_logradouro = replace(tipo_logradouro, tipo_logradouro=="Est", "Estrada"),
                      tipo_logradouro = replace(tipo_logradouro, tipo_logradouro=="Est.", "Estrada"),
                      tipo_logradouro = replace(tipo_logradouro, tipo_logradouro=="Estr", "Estrada"),
                      tipo_logradouro = replace(tipo_logradouro, tipo_logradouro=="r", "Rua"),
                      tipo_logradouro = replace(tipo_logradouro, tipo_logradouro=="R", "Rua"),
                      tipo_logradouro = replace(tipo_logradouro, tipo_logradouro=="R.", "Rua"),
                      tipo_logradouro = replace(tipo_logradouro, tipo_logradouro=="RUA", "Rua"))

table(clientes_v1$tipo_logradouro)

# calcular a variavel faixa_renda a partir da variavel renda

clientes_v1 <- mutate(clientes_v1,
                      faixa_renda = replace(faixa_renda, renda < 1001, "Renda até R$ 1000"),
                      faixa_renda = replace(faixa_renda, renda > 1000 & renda < 2001, "Renda de R$ 1001 a R$ 2000"),
                      faixa_renda = replace(faixa_renda, renda > 2000 & renda < 3001, "Renda de R$ 2001 a R$ 3000"),
                      faixa_renda = replace(faixa_renda, renda > 3000 & renda < 4001, "Renda de R$ 3001 a R$ 4000"),
                      faixa_renda = replace(faixa_renda, renda > 4000 & renda < 5001, "Renda de R$ 4001 a R$ 5000"),
                      faixa_renda = replace(faixa_renda, renda > 5001, "Renda maior que R$ 5001"))
                      
print(clientes_v1$faixa_renda)
view(clientes_v1)
                      
# criando o dataset v2

nome_completo <- NA
endereco_completo <- NA
cod_localizador <- NA
facebook <- NA
instagram <- NA
email <- NA
chat <- NA
telefone <- NA
whatsapp <- NA


clientes_v2 <- clientes_v1 %>% select(everything(),-c(individuos)) 
                           

clientes_v2 <- mutate(clientes_v2,
                      nome_completo,
                      endereco_completo,
                      cod_localizador,
                      facebook,
                      instagram,
                      email,
                      chat,
                      telefone,
                      whatsapp) %>% 
               relocate(nome_completo,.after = sobrenome) %>% 
               relocate(endereco_completo,.after = num_logradouro) %>% 
               relocate(cod_localizador,.before = contatos)

view(clientes_v2)


# filtrar as observações da variável contatos que tenham o valor "Facebook"

filter_3 <- filter(clientes_v2, grepl("Facebook",contatos))

x <- grep("Facebook",clientes_v2$contatos)
print(x)

y <- grepl("Facebook",clientes_v2$contatos)
print(y)


# gerar variáveis dummy




# renomear as variaveis
  
clientes_v2<- rename(clientes_v2,
                      ID=1,
                      Nome=2,
                      Sobrenome=3,
                      Cliente=4,
                      Sexo=5,
                      Idade=6,
                      Escolaridade=7,
                      "Renda Mensal"=8,
                      "Faixa de Renda"=9,
                      "Consumo Médio"=10,
                      Nacionalidade=11,
                      Cidade=12,
                      "Tipo de logradouro"=13,
                      Logradouro=14,
                      Número=15,
                      "Endereço completo"=16,
                      Contatos=17,
                      Facebook=18,
                      Instagram=19,
                      "E-mail"=20,
                      Chat=21,
                      Telefone=22,
                      WhattsApp=23)

view(clientes_v2)

# exportar o dataset para formato Excel (*.xlsx)

write_xlsx(clientes_v2,"bd_resultado_final.xlsx")

