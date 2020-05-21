module InfosReponses where 

import Data.Array(uncons, length, filter, head  , (:) , sortBy, concat, index, cons, groupBy)
import Data.Foldable(foldr)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Array.NonEmpty (toArray)
import Data.Maybe (Maybe(..), maybe) 
import Data.String (joinWith)
import Prelude
import Data.Int(odd)
import Data.HeytingAlgebra(not)
data Reponse = Reponse1
              | Reponse2
              | Reponse3
              | Reponse4
              | Reponse5
              | Reponse6
              | Reponse7
              | Reponse8
          

derive instance eqReponses :: Eq Reponse

type EnregistreReponse = { numQ :: Int, reponse :: Reponse }

type Details = {
                  reponse :: Reponse 
                  ,nom    :: String
                  ,id     :: Int
                  ,correct :: Boolean }

type Resume = {numeroQuestion :: Int
              ,listeDesReponses ::Array Details}


data Question = Question1  | Question2 

derive instance eqQuestion :: Eq Question

showQuestion :: Question -> String 
showQuestion question = 
  case question of 
    Question1 ->  "Cochez la case si l'animal est un mamifère" 
    Question2 ->  "Cochez la case si l'animal mesure plus de 2 mètres"

showReponse :: EnregistreReponse -> Resume -> Int ->  String
showReponse reponseCherchee reponses numero =
    case  head (filter(\record -> record.reponse == reponseCherchee.reponse && reponseCherchee.numQ == numero) reponses.listeDesReponses) of 
                                Just tete -> tete.nom
                                Nothing -> ""

indexUnsafe ::  Array Resume   -> Int -> Resume
indexUnsafe elt tab = 
  case index elt tab of 
    Just valeur -> valeur 
    Nothing -> reponsesQ1

indexUnsafeQ ::  Array Question   -> Int -> Question
indexUnsafeQ elt tab = 
  case index elt tab of 
    Just valeur -> valeur 
    Nothing -> Question1


showReponses :: Array EnregistreReponse -> Array Resume -> String
showReponses reponsesEnregistrees reponsesFormulaire = 
  joinWith " " (map (\pl ->  
      showReponse pl (indexUnsafe reponsesFormulaire (pl.numQ - 1) ) pl.numQ ) reponsesEnregistrees  )


associe :: Array Resume -> Reponse -> Int -> Int
associe reponsesDuFormulaire reponseCherchee numQuestion=
   case  head (filter(\record -> record.reponse == reponseCherchee)
              (concat $ map 
                  (\rep -> rep.listeDesReponses)
                  ( filter(\rep -> rep.numeroQuestion == numQuestion) reponsesDuFormulaire )))  of 
      Just tete ->  tete.id 
      Nothing -> -1


occurences :: forall a. Eq a => a -> Array a -> Int
occurences reponse listeReponse = length (filter(\p -> p == reponse) listeReponse)

enleveDoublons :: forall a. Eq a => Array a -> Array a
enleveDoublons l = 
  let 
    aux l2 acc =
      case uncons l2 of 
        Just{head , tail} -> 
              
                            if occurences  head acc == 0
                            then aux tail ( head : acc) 
                            else aux tail acc
        Nothing -> acc 
    
  in aux l []


enleveDeclic :: Array EnregistreReponse -> Array EnregistreReponse 
enleveDeclic arr =
  sortBy(\rec1 rec2 -> compare rec1.numQ rec2.numQ) (enleveDoublons (filter(\rec -> odd $ occurences rec arr) arr))
  
reponsesCorrectes :: Array Resume ->  Array  {numQ :: Int, rep ::  Array Int}
reponsesCorrectes reponses =
 map(\record -> 
        {
          numQ : record.numeroQuestion
          ,rep : map
                  (\detail -> detail.id)
                  (filter(\rec ->  rec.correct == true)record.listeDesReponses)
        }
    ) reponses
        



distribue ::  Array  {numQ :: Int, rep ::  Array Int} ->  Array  {numQ :: Int, rep :: Int}
distribue arr = concat $ map(\rec -> map(\eltTab -> {numQ : rec.numQ, rep : eltTab}) rec.rep) arr

bonneReponseDonnee :: EnregistreReponse -> Array Resume -> Boolean
bonneReponseDonnee reponseDonnee reponsesFormulaires = 
  occurences
    {numQ : reponseDonnee.numQ, rep:  associe reponsesFormulaires reponseDonnee.reponse reponseDonnee.numQ }  
    (distribue  (reponsesCorrectes reponsesFormulaires)) == 1

score :: Array EnregistreReponse ->  Array Resume ->   Int 
score reponsesDonnees reponsesFormulaire = 
  foldr (\repf  accScore  -> 
    if bonneReponseDonnee repf reponsesFormulaire
    then  accScore + 1
    else  accScore -   1 
    )
    0
    reponsesDonnees

correctionQ reponsesDonnees reponsesFormulaire = 
  foldr (\repf  accScore  -> 
    if bonneReponseDonnee repf reponsesFormulaire
    then  cons (HH.p [HP.id_ "repCorrecte"] [HH.text ((showReponses [repf] reponsesFormulaire ) <>" : Vrai ")]) accScore
    else  cons (HH.p [HP.id_ "repFausse"] [HH.text ((showReponses [repf] reponsesFormulaire ) <>" : Faux")]) accScore
    )
    []
    reponsesDonnees

correction  reponsesDonnees reponsesFormulaire = 
  map
    (\repF ->
      HH.div_[
        HH.p_ [HH.text (showQuestion (indexUnsafeQ 
                            listQuestions
                            ( maybe 0 (\tete -> tete.numQ) (head repF))))
              ]
        ,HH.div_(correctionQ repF reponsesFormulaire) ])
    (map toArray (groupBy(\rec1 rec2  ->  rec1.numQ == rec2.numQ ) reponsesDonnees))

reponsesQ1 :: Resume
reponsesQ1 = 
  {numeroQuestion : 1
  ,listeDesReponses :[
    {reponse : Reponse1 , nom : "Chien",  id: 0, correct : true},
    {reponse : Reponse2  , nom : "Lezard" , id: 1, correct : false},
    {reponse  : Reponse3 , nom : "Chat", id: 2, correct : true},
    {reponse  : Reponse4 , nom : "Zebre", id: 3, correct : true},
    {reponse : Reponse5 , nom : "Poisson", id: 4, correct : false}
    ]
  }


reponsesQ2 :: Resume
reponsesQ2 = 
  {numeroQuestion : 2
  ,listeDesReponses : [
  
  {reponse : Reponse1  , nom : "Cheval" , id: 0, correct : false}
  ,{reponse : Reponse2 , nom : "Autruche",  id: 1, correct : true}
  ,{reponse : Reponse3  , nom : "Elephant" , id: 2, correct : true}
  ,{reponse : Reponse4  , nom : "Chien" , id: 3, correct : false}]
  }

listReponses = [reponsesQ1, reponsesQ2]

listQuestions :: Array Question
listQuestions = [Question1, Question2]