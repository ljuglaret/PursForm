module Form where

import Prelude 
import Data.Array(cons , length)
import Data.Foldable(foldr)
import Data.Const (Const)
import Data.Maybe (Maybe(..)) 
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import InfosReponses (EnregistreReponse, Reponse, Resume, Question, correction, enleveDeclic, indexUnsafe, indexUnsafeQ, listQuestions, listReponses, score, showQuestion, showReponse)
import Data.Int (toNumber)


type AskReponseStage =  {reponse :: Maybe Reponse   }
type ListeDesScores = Array Number

data Stage =  
            Presentation
            |Formulaire        (Array EnregistreReponse) ListeDesScores  NombreDessais 
            |ResultatUnePartie  (Array EnregistreReponse) ListeDesScores  NombreDessais 
            |ScoreFinal         ListeDesScores


type State = { stage :: Stage }

type NombreDessais = Int

data Msg
  =   FormulaireVide
  |StockeReponse  Reponse     (Array EnregistreReponse) Int ListeDesScores NombreDessais
  | EnvoiReponsesFormulaire      (Array EnregistreReponse) Number ListeDesScores NombreDessais
  | RecommencerUnePartie          ListeDesScores NombreDessais
  | EnvoiScore                    ListeDesScores

page ∷ forall m. H.Component HH.HTML (Const Void) Unit Void m 
page = 
    H.mkComponent { initialState: const initialState
                  , render
                  , eval: H.mkEval $ H.defaultEval
                    { handleAction = update}
                  }


initialState ∷ State
initialState = { stage : Presentation}


update :: forall m. Msg -> H.HalogenM State Msg () Void m Unit
update(StockeReponse  newReponse listeDesReponse numQuestion scores nbEssais) =
    H.modify_ _{ stage = 
      Formulaire
        (enleveDeclic(cons { numQ : numQuestion, reponse : newReponse }  listeDesReponse))
        scores 
        nbEssais}

update(EnvoiReponsesFormulaire  reponsesPartieActuelle scorePartieActuelle scores nbEssais ) =
    if nbEssais < 3 
    then 
       H.modify_ _{ stage = 
        ResultatUnePartie
          reponsesPartieActuelle 
          (cons scorePartieActuelle scores)
          nbEssais} 
    else 
      H.modify_ _{ stage = 
          ScoreFinal scores}
   


update (RecommencerUnePartie scores nbEssais)   = 
    H.modify_ _{ stage = 
        Formulaire    [] scores (nbEssais + 1 )}

      

update(EnvoiScore scoreEnvoye ) = 
  H.modify_ _{ stage = 
      ScoreFinal scoreEnvoye }

update FormulaireVide   = 
  H.modify_ _{ stage = 
      Formulaire   [] [] 1}


renderNextButton :: forall m. Maybe Msg -> String -> H.ComponentHTML Msg () m
renderNextButton action texteBouton =
   HH.button
  ( case action of
      Nothing -> [ HP.disabled true ]
      Just action' -> [ HE.onClick <<< const $ Just action' ] )
  [ HH.text texteBouton ]


render :: forall m. State -> H.ComponentHTML Msg () m

render {stage : Presentation } = 
  HH.div_[
    HH.h2_[HH.text "Les règles sont les suivantes  : "]
    ,HH.h3_[HH.text "+ 1 point pour une bonne réponses"]
    , HH.h3_[HH.text "- 0.5 point pour une mauvaise réponse"]
    ,HH.h3_[HH.text "3 essais sont autorisés"]
    ,renderNextButton (Just FormulaireVide) "Commencer"
  ]

  

render { stage  : Formulaire   arr scores nbEssais} =
  HH.div[][
    
    HH.p_[HH.text $ "Ceci est votre essai numero " <>(show nbEssais)]    
    ,HH.div[] (
      map
         (\indice ->
            let 
              intituleQuestion :: Question
              intituleQuestion  = indexUnsafeQ listQuestions (indice - 1 )
              
              intituleReponse :: Resume
              intituleReponse   = indexUnsafe  listReponses  (indice - 1 )
            in
              HH.div_[
                HH.p_[HH.text $ showQuestion  intituleQuestion ]
                ,HH.p_(map 
                    (\record -> 
                        makeButton record.reponse intituleReponse indice )
                        intituleReponse.listeDesReponses)
              ]
        )
        [1,2]
    )
    ,renderNextButton (Just (EnvoiReponsesFormulaire  arr (score arr listReponses ) scores nbEssais) )   "Score"
     ]

    where
      makeButton reponseDonnee numRep numQuestion =
        HH.div_  [
          HH.input [
            HP.type_ HP.InputCheckbox
            , HE.onChecked (\b ->  Just $ StockeReponse   reponseDonnee  arr numQuestion scores nbEssais)
            ]
          , HH.label_ [ HH.text $ (showReponse {numQ : numQuestion , reponse : reponseDonnee } numRep numQuestion) ]
         ]
          

render {stage : ResultatUnePartie l scores nbEssais} = 
  HH.div[HP.id_ "body"] [
    HH.div[HP.id_ "haut"][
        HH.p_[HH.text ("Votre score est de : " <> (show (score l listReponses )))]
        ,HH.p_[HH.text "Voici la correction "]
        ,HH.div_ (correction l listReponses )
    ]
    ,HH.div[HP.id_ "bas"][
      renderNextButton (Just (RecommencerUnePartie  scores nbEssais )) "Recommencer"
      ,renderNextButton (Just (EnvoiScore scores)) "Score Final"
    ]
  ]

render {stage : ScoreFinal listeDesScores } = 
  HH.div_[
    HH.p_ [HH.text("Votre moyenne est de : ")]
  ,HH.div_ [HH.text(show ((foldr (+) 0.0  listeDesScores) / (toNumber (length listeDesScores))))]
  ,renderNextButton (Just FormulaireVide) "Recommencer"
  ]
   
