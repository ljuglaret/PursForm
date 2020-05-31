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


type AskReponseStage =  {reponse :: Maybe Reponse   }
type ListeDesScores = Array Number

data Stage =  
            Presentation
            |Formulaire        (Array EnregistreReponse)  ListeDesScores  NombreDessais 
            |ResultatUnePartie  (Array EnregistreReponse) ListeDesScores  NombreDessais 
            |ScoreFinal         ListeDesScores


type State = { stage :: Stage }

type NombreDessais = Int

data Msg
  =   FormulaireVide                -- Model  =   Presentation
  |StockeReponse  Reponse      Int  -- Model  =   Formulaire
  | EnvoiReponsesFormulaire         -- Model  =   Formulaire
  | RecommencerUnePartie            -- Model  =   ResultatUnePartie
  | EnvoiScore   ListeDesScores     -- Model  =   ScoreFinal

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
      {stage : Formulaire listeDesReponses listeDesScores nbEssais} -> 
        { stage : 
          Formulaire
            (enleveDeclic(cons { numQ : numQuestion, reponse : newReponse }  listeDesReponses))
            listeDesScores
            nbEssais}
      _ -> state)


update EnvoiReponsesFormulaire=

    H.modify_ (\state -> 
      case state of 
        {stage : Formulaire reponsesPartieActuelle  listeDesScores nbEssais} -> 
          { stage : 
            ResultatUnePartie
              reponsesPartieActuelle 
              (cons (score reponsesPartieActuelle listReponses) listeDesScores)
              nbEssais}
        _ -> state) 
  

update RecommencerUnePartie   = 
  H.modify_ (\state -> 
    case state of 
      {stage : ResultatUnePartie listeDesReponses listeDesScores nbEssais} -> 
          if nbEssais < 3 
          then
            { stage : Formulaire [] listeDesScores (nbEssais + 1 ) }
          else
            {stage : ScoreFinal listeDesScores}
      _ -> state)


update(EnvoiScore scoreEnvoye ) = 
  H.modify_ _{ stage = 
      ScoreFinal scoreEnvoye }

update FormulaireVide   = 
  H.modify_ _{ stage = 
      Formulaire  [] [] 1}


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

  

render { stage  : Formulaire    arr  scores   nbEssais} =
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
    ,renderNextButton (Just EnvoiReponsesFormulaire )   "Score"
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
          

render {stage : ResultatUnePartie l scores nbEssais} = 
  HH.div[HP.id_ "body"] [
    HH.div[HP.id_ "haut"][
        HH.p_[HH.text ("Votre score est de : " <> (show (score l listReponses )))]
        ,HH.p_[HH.text "Voici la correction "]
        ,HH.div_ (correction l listReponses )
    ]
    ,HH.div[HP.id_ "bas"][
      renderNextButton (Just RecommencerUnePartie ) "Recommencer"
      ,renderNextButton (Just (EnvoiScore scores)) "Score Final"
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
   
