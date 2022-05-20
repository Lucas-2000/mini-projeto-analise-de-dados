install.packages('ggplot2')
library(ggplot2)
# Salvando a base numa variável
arq <- read.csv('dados_turma.csv');arq

# 2. Mostre um gráfico apropriado para os seguintes itens

# 2.1 Percentual por I.

faixa_tot <- arq[arq$Faixa.etÃ.ria..I., ]; faixa_tot
faixa1 <- arq[arq$Faixa.etÃ.ria..I. == 'De 18 a 22 anos', ]; faixa1
faixa2 <- arq[arq$Faixa.etÃ.ria..I. == 'De 23 a 27 anos', ]; faixa2
faixa3 <- arq[arq$Faixa.etÃ.ria..I. == 'De 28 a 32 anos', ]; faixa3

p1 <- (nrow(faixa1)/nrow(faixa_tot)) * 100; p1
p2 <- (nrow(faixa2)/nrow(faixa_tot)) * 100; p2
p3 <- (nrow(faixa3)/nrow(faixa_tot)) * 100; p3

p_tot <- c(p1,p2,p3); p_tot

barplot(p_tot, main='Percentual por I', xlab='Faixas',
        ylab='Percentual', col=c('green', 'blue', 'red'),
        legend.text = c('De 18 a 22 anos', 'De 23 a 27 anos', 'De 28 a 32 anos'))

# 2.2 Percentual por T
t_tot <- arq[arq$Trabalha.na.Ã.rea.desejada...T., ]; t_tot
t1 <- arq[arq$Trabalha.na.Ã.rea.desejada...T. == 'Sim', ]; t1
t2 <- arq[arq$Trabalha.na.Ã.rea.desejada...T. == 'NÃ£o', ]; t2

pt1 <- (nrow(t1)/nrow(t_tot)) * 100; pt1
pt2 <- (nrow(t2)/nrow(t_tot)) * 100; pt2

pt_tot <- c(pt1,pt2); pt_tot

barplot(pt_tot, main='Percentual por T', xlab='Sim ou Não',
        ylab='Percentual', col=c('green', 'blue'),
        legend.text = c('Sim', 'Não'))

# 2.3 DP por DIST.
ggplot(data = arq) +
  geom_col(mapping = aes(x = DistÃ.ncia.de.sua.casa.Ã..universidade..arredonde.para.responder...DIST., y = Quantas.DPs.jÃ..pegou.ao.longo.do.curso...DP.)) +
  labs(title = "DP por DIST", x = "Distancia",
       y = "DP'S")

# 2.4 DP por ENT
ggplot(data = arq) +
  geom_col(mapping = aes(x = Tempo.mÃ.dio.diÃ.rio.gasto.em.redes.sociais.para.entretenimento..arredonde.para.responder...ENT., y = Quantas.DPs.jÃ..pegou.ao.longo.do.curso...DP.)) +
  labs(title = "DP por ENT", x = "Horas em entretenimento",
       y = "DP'S")

# 2.5 I por ENT
ggplot(data = arq) +
  geom_col(mapping = aes(x = Tempo.mÃ.dio.diÃ.rio.gasto.em.redes.sociais.para.entretenimento..arredonde.para.responder...ENT., y = Faixa.etÃ.ria..I.)) +
  labs(title = "I por ENT", x = "Horas em entretenimento",
       y = "Faixa Etária")


# 2.6 SEM por SAL
ggplot(data = arq) +
  geom_line(mapping = aes(x = Faixa.salarial..arredonde.para.responder...SAL., y = Quantos.semestres.jÃ..concluiu...SEM.)) +
  labs(title = "SEM por SAL", x = "Salário",
       y = "Quantidade de Semestre")

# 2.7 T por SAL
ggplot(data = arq) +
  geom_col(mapping = aes(x = Faixa.salarial..arredonde.para.responder...SAL., y = Trabalha.na.Ã.rea.desejada...T.)) +
  labs(title = "T por SAL", x = "Salário",
       y = "Trabalha na área")

# 3. Mostre evidências (gráficas, numéricas etc) para as seguintes perguntas
# 3.1 É verdade que as pessoas maiores de idade tendem a ter menos DPs?
f1 <- arq[arq$Faixa.etÃ.ria..I. == 'De 18 a 22 anos', 'Quantas.DPs.jÃ..pegou.ao.longo.do.curso...DP.'];f1
f2 <- arq[arq$Faixa.etÃ.ria..I. == 'De 23 a 27 anos', 'Quantas.DPs.jÃ..pegou.ao.longo.do.curso...DP.'];f2
f3 <- arq[arq$Faixa.etÃ.ria..I. == 'De 28 a 32 anos', 'Quantas.DPs.jÃ..pegou.ao.longo.do.curso...DP.'];f3
f1 > f3
f2 > f3

# 3.2 É verdade que as pessoas que trabalham na área desejada tendem a passar menos
# tempo nas redes sociais?
s1 <- arq[arq$Trabalha.na.Ã.rea.desejada...T. == "Sim","Tempo.mÃ.dio.diÃ.rio.gasto.em.redes.sociais.para.entretenimento..arredonde.para.responder...ENT."];t1
s2 <- arq[arq$Trabalha.na.Ã.rea.desejada...T. == "NÃ£o","Tempo.mÃ.dio.diÃ.rio.gasto.em.redes.sociais.para.entretenimento..arredonde.para.responder...ENT."];t2
s1 < s2

# 3.3 É verdade que as pessoas que moram mais longe da faculdade tendem a ter mais
# DPs?
d1 <- arq[arq$DistÃ.ncia.de.sua.casa.Ã..universidade..arredonde.para.responder...DIST. == 'De 1 a 3 KM', 'Quantas.DPs.jÃ..pegou.ao.longo.do.curso...DP.'];d1
d2 <- arq[arq$DistÃ.ncia.de.sua.casa.Ã..universidade..arredonde.para.responder...DIST. == 'De 4 a 6 KM', 'Quantas.DPs.jÃ..pegou.ao.longo.do.curso...DP.'];d2
d3 <- arq[arq$DistÃ.ncia.de.sua.casa.Ã..universidade..arredonde.para.responder...DIST. == 'Mais de 6 KM', 'Quantas.DPs.jÃ..pegou.ao.longo.do.curso...DP.'];d3
d3 > d1

# 3.4 É verdade que as pessoas que trabalham na área desejada tendem a ter os maiores
# salários?
ds1 <- arq[arq$Trabalha.na.Ã.rea.desejada...T. == "Sim","Faixa.salarial..arredonde.para.responder...SAL."];ds1
ds2 <- arq[arq$Trabalha.na.Ã.rea.desejada...T. == "NÃ£o","Faixa.salarial..arredonde.para.responder...SAL."];ds2
ds1 < ds2
