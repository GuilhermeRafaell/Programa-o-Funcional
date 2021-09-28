type Dia = Int
type Mes = Int
type Ano = Int
type Data = (Dia,Mes,Ano)
--Epecificacao de tipos Inteiros para Dia,Mes,Ano e Data.

bissexto :: Ano -> Bool
bissexto ano
 |mod ano 4 == 0 || mod ano 400 == 0  = True
 |otherwise    = False
--casos que determinam se um ano sera bissexto ou nao e saber se o resto da divisao do ano por 4 ou 400 sera 0, caso seja o ano sera bissexto.

numDeDiasEmCadaMesDeUmAno :: Ano -> [Int]
numDeDiasEmCadaMesDeUmAno ano = [31,feb,31,30,31,30,31,31,30,31,30,31]
  where feb = if bissexto ano then 29 else 28
--funcao identifica se o ano sera bissexto e devolve os dias corretos de todos os meses daquele determinado ano em formato de lista.
   

numDeDias :: Data -> Int
numDeDias (dia,mes,ano) = dia + sum (take (mes-1) (numDeDiasEmCadaMesDeUmAno ano)) + (ano-2001)*365 + (ano-2001)`div`4
--"sum" realiza a soma dos dias decorrentes dos meses passados naquele ano ,ja funcao "take" retira um mes da data especificada no imput.

nomeDoDia :: Int -> String
nomeDoDia 0 = "Domingo"
nomeDoDia 1 = "Segunda-Feira"
nomeDoDia 2 = "Terca-Feira"
nomeDoDia 3 = "Quarta-Feira"
nomeDoDia 4 = "Quinta-Feira"
nomeDoDia 5 = "Sexta-Feira"
nomeDoDia 6 = "Sabado"
--vaores atribuidos para dias da semana

diaDaSemana :: Data -> String
diaDaSemana (dia,mes,ano) = nomeDoDia (numDeDias (dia,mes,ano) `mod` 7)
--para achar o numero especifico da semana calcule o resto da divisao do numero de dias de uma Data e 7, e o resultante disso sera avaliado pela funcao nomeDoDia





 
