--Declaracao de tipos 
type Nome   = String
type Preco  = Int
type CodBar = Int
type BaseDeDados = [ (CodBar,Nome,Preco) ] 
type ListaDeCodigos = [CodBar]
type Recibo = [ (Nome,Preco) ]

--Computa todos os produtos disponiveis 
listaDeProdutos :: BaseDeDados
listaDeProdutos = [
 (1234, "Oleo DoBom, 1L",195),
 (4756, "Chocolate Cazzeiro, 250g",1010),
 (3216, "Arroz DoBom, 5Kg",213),
 (5823, "Balas Pedregulho, 1Kg",379),
 (4719, "Queijo Mineirim, 1Kg",449),
 (6832, "Iogurte Maravilha, 1Kg",499),
 (1112, "Rapadura QuebraDente, 1Kg",80),
 (1111, "Sal Donorte, 1Kg",221),
 (1113, "Cafe  DoBom, 1Kg",285),
 (1115, "Biscoito Bibi, 1Kg",80),
 (3814, "Sorvete QGelo, 1l",695)]

--Define o tamanho da lista
tamanhoLinha :: Int
tamanhoLinha = 30

--Transforma valor Int em formato de Dinheiro                  
formataCentavos :: Preco -> String
formataCentavos n =  (show (div n 100)) ++ "." ++ (show (rem n 100))

--Resulta no preco final da compra
geraTotal :: Recibo -> Preco
geraTotal itens = sum (map snd itens) 

--Recebe a lista dos itens comprados e entrega uma string que representa a conta do supermercado
formataTotal :: Preco -> String
formataTotal preco = "\nTotal" ++ (replicate qtpontos '.') ++ "$" ++ precoStr
 where
 precoStr = formataCentavos preco
 qtpontos = tamanhoLinha - length "Total" - length precoStr

  
--Recebe um par formado pelo nome e preco da mercadoria e entrega um String que representa uma linha da conta do Supermercado    
formataLinha :: (Nome,Preco) -> String
formataLinha (nome,preco) =  nome ++ (replicate (tamanhoLinha - (length nome + length (formataCentavos preco))) '.') ++  (formataCentavos preco) ++ "\n"


--Recebe uma lista de pares formados pelos nomes doas mercadorias e seus respectivos precos e devolve um string que corresponde ao corpo da conta do Supermercado
formataLinhas :: [(Nome,Preco)] -> String
formataLinhas [] = ""
formataLinhas (item:resto) = formataLinha item ++ formataLinhas resto

--Resulta na lista dos pares (Nome,Preco) para as mercadorias compradas
achaItem :: CodBar -> (Nome,Preco)
achaItem codigo = acha listaDeProdutos codigo 

--Recebe os nomes e precos das mercadorias disponiveis no Supermercado e o codigo de barras
--Devolve o preco da mercadoria . Se o codigo de barras nao constar no banco de dados , o resultado sera "Item desconhecido"
acha :: BaseDeDados -> CodBar -> (Nome,Preco)
acha [] _ = ("Item desconhecido",0)
acha ((codigo,nome,preco):resto) codigo2
 | codigo == codigo2 = (nome,preco)
 | otherwise = acha resto codigo2

--Recebe um codigo de barras e devolve um string com nome e preco
fazRecibo :: ListaDeCodigos -> Recibo
fazRecibo codigos = map achaItem codigos

--Formata conta para um string
formataRecibo :: Recibo -> String
formataRecibo itens =  "Supermercado QLegal\n" ++ "\n" ++ formataLinhas itens ++ formataTotal (geraTotal itens) 

--Gera o recibo da compra
geraRecibo :: ListaDeCodigos -> String
geraRecibo codigos = formataRecibo (fazRecibo codigos)


main :: IO ()

main = do 
   x <- getLine
   putStrLn ((geraRecibo (read x :: ListaDeCodigos)))
   y <- getLine
   putStrLn ("End")

