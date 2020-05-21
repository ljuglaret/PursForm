module Form where

import Prelude 
import Data.Functor(map)
import Data.Array(cons)
import Data.Foldable(foldr)
import Data.Const (Const)
import Data.Maybe (Maybe(..)) 
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import InfosReponses (EnregistreReponse, Reponse, correction, enleveDeclic, indexUnsafe, indexUnsafeQ, listQuestions, listReponses, score, showQuestion, showReponse)

type AskReponseStage =  {reponse :: Maybe Reponse   }
type ListeDesScores = Array Int 

data Stage =  Formulaire  (Array EnregistreReponse)(Array Int)
            |ResultatUnePartie (Array EnregistreReponse) (Array Int)
            |ScoreFinal  (Array Int)


type State = { stage :: Stage }


data Msg
  =   StockeReponse   Reponse  (Array EnregistreReponse) Int ListeDesScores
  |EnvoiReponsesFormulaire (Array EnregistreReponse) Int ListeDesScores
  | Recommencer  (Array EnregistreReponse)ListeDesScores
  | EnvoiScore ListeDesScores

page ∷ forall m. H.Component HH.HTML (Const Void) Unit Void m 
page = 
    H.mkComponent { initialState: const initialState
                  , render
                  , eval: H.mkEval $ H.defaultEval
                    { handleAction = update}
                  }


initialState ∷ State
initialState = { stage : Formulaire  [][] }


update :: forall m. Msg -> H.HalogenM State Msg () Void m Unit

update(EnvoiScore scoreEnvoye ) = 
  H.modify_ _{ stage = 
      ScoreFinal scoreEnvoye   }


update(Recommencer  listeDesReponse scores) = 
  H.modify_ _{ stage = 
      Formulaire   listeDesReponse scores  }
    
update(EnvoiReponsesFormulaire  listeRecordRepNum score scores ) =
    H.modify_ _{stage = ResultatUnePartie listeRecordRepNum  (cons score scores)} 


update(StockeReponse  newReponse listeDesReponse numQuestion scores) =
    H.modify_ _{ stage = 
      Formulaire    (enleveDeclic(cons { numQ : numQuestion, reponse : newReponse }  listeDesReponse)) scores }
    
    
renderNextButton :: forall m. Maybe Msg -> String -> H.ComponentHTML Msg () m
renderNextButton action texteBouton =
   HH.button
  ( case action of
      Nothing -> [ HP.disabled true ]
      Just action' -> [ HE.onClick <<< const $ Just action' ] )
  [ HH.text texteBouton ]

render :: forall m. State -> H.ComponentHTML Msg () m


render { stage: Formulaire   arr scores} =
  HH.div[][
    HH.div[] (
      map
         (\indice ->
            let 
              question  = indexUnsafeQ listQuestions (indice - 1 )
              reponse   = indexUnsafe  listReponses (indice - 1 )
            in
              HH.div_[
                HH.p_[HH.text $ showQuestion  question ]
                ,HH.p_(map 
                    (\record -> 
                        makeButton record.reponse reponse indice )
                        reponse.listeDesReponses)
              ]
        )
        [1,2]
    )    
    ,renderNextButton (Just (EnvoiReponsesFormulaire  arr (score arr listReponses ) scores ))  "Score"]

    where
      makeButton reponseDonnee numRep numQuestion =
        HH.div_  [
          HH.input [
            HP.type_ HP.InputCheckbox
            , HE.onChecked (\b ->  Just $ StockeReponse   reponseDonnee  arr numQuestion scores)
            ]
          , HH.label_ [ HH.text $ (showReponse {numQ : numQuestion , reponse : reponseDonnee } numRep numQuestion) ]
         ]
          

render {stage : ResultatUnePartie l scores} = 
  HH.div[HP.id_ "body"] [
    HH.div[HP.id_ "haut"][
        HH.p_[HH.text ("Votre score est de : " <> (show (score l listReponses )))]
        ,HH.p_[HH.text "Voici la correction "]
        ,HH.div_ (correction l listReponses )
    ]
    ,HH.div[HP.id_ "bas"][
      renderNextButton (Just (Recommencer [] scores)) "Recommencer"
      ,renderNextButton (Just (EnvoiScore scores)) "Score Final"
    ]
  ]

render {stage : ScoreFinal listeDesScores } = 
  HH.div_[
    HH.p_ [HH.text("Votre score final est de : ")]
  ,HH.div_ [HH.text(show (foldr (+) 0  listeDesScores))]
  ,renderNextButton (Just (Recommencer [] [])) "Recommencer"
  ]
   
