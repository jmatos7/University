data Hora = H Int Int
          deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

v :: Viagem 
v = [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]

type Horas = (Int, Int)
horaValida :: (Int,Int) -> Bool
horaValida (a,b) = if a < 23 && b < 60 
                   then True
                   else False

etapaValida :: Etapa -> Bool
etapaValida ( H hi mi , H hf mf) | horaValida (hi,mi) && horaValida (hf,mf) && hi < hf = True
                                 | hi==hf && mi < mf = True           
                                 | otherwise = False

horaValida' :: Hora -> Bool
horaValida' (H h m) = h >= 0 && h<24 && m > 0 

horaMaior :: Hora -> Hora -> Bool
horaMaior (H h1 m1) (H h2 m2) | h1 > h2 = True 
                              | h1 == h2 && m1 > m2 = True 
                              | otherwise = False

etapaValida' :: Etapa -> Bool
etapaValida' (hc,hp) = horaValida' hp && horaValida' hc && horaMaior hp hc

viagemValida :: Viagem -> Bool
viagemValida [e1]         = etapaValida' e1
viagemValida (e1:e2:es) = etapaValida' e1 
                        && horaMaior (fst e2) (snd e1)
                        && viagemValida (e2:es)

partidaChegada :: Viagem -> Etapa
partidaChegada v = (fst (head v), snd (last v))

horaEmMinutos :: Hora -> Int
horaEmMinutos (H h m) = h*60 + m 

tempoEtapa :: Etapa -> Int
tempoEtapa (hi,hf) = horaEmMinutos hf - horaEmMinutos hi 

tempoViagem :: Viagem -> Int
tempoViagem [] = 0
tempoViagem (e:es) = tempoEtapa e + tempoViagem es 

tempoEmEspera :: Viagem -> Int
tempoEmEspera [] = 0
tempoEmEspera [e] = 0
tempoEmEspera (e1:e2:es) = horaEmMinutos (fst e2) - horaEmMinutos (snd e1)
                         + tempoEmEspera (e2:es)

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
                deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

agendaTP2 :: Agenda 
agendaTP2 = [("Joao Saraiva",[Tlm 964200918, Email "saraiva@di.uminho.pt"])
,("Ana",[Tlm 969876513])
,("To",[Email "toto@gmail.com",casa 253222222])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n e [] = [(n, [Email e])]
acrescEmail n e (c:cs) 