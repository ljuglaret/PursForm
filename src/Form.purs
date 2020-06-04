module Form where

import Prelude 
import Data.Array(cons , length, last,sort,head)
import Data.Foldable(foldr)
import Data.Const (Const)
import Data.Maybe (Maybe(..)) 
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import InfosReponses (EnregistreReponse, Reponse, Resume, Question, correction, enleveDeclic, indexUnsafe, indexUnsafeQ, listQuestions, listReponses, score, showQuestion, showReponse)
import Data.Int (toNumber)


type ListeDesScores = Array Number
type NumeroQuestion = Int 

data Stage =  
            Presentation
            |Formulaire        (Array EnregistreReponse)  ListeDesScores 
            |ResultatUnePartie  (Array EnregistreReponse) ListeDesScores  
            |ScoreFinal         ListeDesScores


type State = { stage :: Stage }

data Msg
  =   FormulaireVide                -- Model  =   Presentation ou ScoreFinal
  |StockeReponse  Reponse  NumeroQuestion -- Model  =   Formulaire
  | DemandeScore         -- Model  =   Formulaire ou ResultatUnePartie
  | RecommencerUnePartie            -- Model  =   ResultatUnePartie

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

update(StockeReponse  newReponse  numQuestion) =
  H.modify_ (\state -> 
    case state of 
      {stage : Formulaire listeDesReponses listeDesScores } -> 
        { stage : 
          Formulaire
            (enleveDeclic(cons { numQ : numQuestion, reponse : newReponse }  listeDesReponses))
            listeDesScores
            }
      _ -> state)


update DemandeScore =
    H.modify_ (\state -> 
      case state of 
        {stage : Formulaire reponsesPartieActuelle  listeDesScores} -> 
          { stage : 
            ResultatUnePartie
              reponsesPartieActuelle 
              (cons (score reponsesPartieActuelle listReponses) listeDesScores)
              }
        {stage : ResultatUnePartie _ listeDesScores} -> 
          {
            stage : 
              ScoreFinal listeDesScores
          }

        _ -> state) 
  

update RecommencerUnePartie   = 
  H.modify_ (\state -> 
    case state of 
      {stage : ResultatUnePartie _ listeDesScores } -> 
          if (length listeDesScores) < 3 
          then
            { stage : Formulaire [] listeDesScores  }
          else
            {stage : ScoreFinal listeDesScores}
      _ -> state)




update FormulaireVide   = 
  H.modify_ _{ stage = 
      Formulaire  [] [] }


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

  

render { stage  : Formulaire    arr  scores   } =
  HH.div[][
    
    HH.p_[HH.text $ "Ceci est votre essai numero " <>(show ((length scores) + 1)) ]    
    ,HH.div[] (
      map
         (\indice ->
            let 
              intituleQuestion :: Question
              intituleQuestion  = indexUnsafeQ listQuestions indice
              
              intituleReponse :: Resume
              intituleReponse   = indexUnsafe  listReponses  indice
            in
              HH.div_[
                HH.p_[HH.text $ showQuestion  intituleQuestion ]
                ,HH.p_(map 
                    (\record -> 
                        makeButton record.reponse intituleReponse indice )
                        intituleReponse.listeDesReponses)
              ]
        )
        [0,1]
    )
    ,renderNextButton (Just DemandeScore )   "Score"
     ]

    where
      makeButton reponseDonnee numRep numQuestion =
        HH.div_  [
          HH.input [
            HP.type_ HP.InputCheckbox
            , HE.onChecked (\b ->  Just $ StockeReponse   reponseDonnee   numQuestion)
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
      renderNextButton (Just RecommencerUnePartie ) "Recommencer"
      ,renderNextButton (Just DemandeScore) "Score Final"
    ]
  ]

render {stage : ScoreFinal listeDesScores } = 
  let 
    scoresTries = sort listeDesScores
    scoreMoinsBon = head scoresTries
    scoreMeilleur = last scoresTries
    moyenne = (foldr (+) 0.0  listeDesScores) / (toNumber (length listeDesScores))
  in
    HH.div_[
      HH.p_ [HH.text(("Votre meilleur score est :  ") <> (show  scoreMeilleur)) ]
    , HH.p_ [HH.text(("Votre moins bon score est :  ") <> (show scoreMoinsBon)) ]
    ,HH.p_ [HH.text(("Votre moyenne est de : ") <> (show moyenne))]
    ,renderNextButton (Just FormulaireVide) "Recommencer"
  ]
   
