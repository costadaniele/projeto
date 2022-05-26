{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Auxiliar where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core
import Data.Map.Strict
import Common.Api
import Common.Route
import Control.Monad.Fix

import Text.Read
import Data.Maybe


revText :: T.Text -> T.Text
-- revText t = T.pack (reverse (T.unpack t))
revText t = T.pack (T.unpack t)

countClick :: DomBuilder t m => m (Event t Int)
countClick = do
    (ev, _) <- el' "button" (text "+")
    return ((const 1) <$> domEvent Click ev)
    
pagClick :: (MonadHold t m, PostBuild t m, DomBuilder t m, MonadFix m) => m ()
pagClick = do
    evt <- countClick
    st <- accumDyn (+) 0 evt
    el "div" (dynText (fmap (T.pack . show) st))

somaClick :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Event t Double)
somaClick = do
    el "label" (text "Digite suas moedas: ")
    el "label" (text "M1 ")
    n1 <- numberInput
    el "label" (text "M2 ")
    n2 <- numberInput
    (e,_) <- el' "button" (text "Somar")
    let soma = zipDynWith (+) n1 n2
    return $ attachPromptlyDynWith const soma (domEvent Click e)
    
somaEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
somaEvt = do
    resEvt <- somaClick
    hl <- holdDyn 0 resEvt
    el "div" (dynText (fmap (T.pack . show) hl))

-- constroi o evento de clique
buttonClick :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Event t T.Text)
buttonClick = do
    el "label" (text "Digite seu nome: ")
    t <- inputElement def
    (e,_) <- el' "button" (text "OK")
    el "label" (text "Olá: ")
    return $ attachPromptlyDynWith const (fmap revText (_inputElement_value t)) (domEvent Click e)

-- consome o evento de clique
bttnEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
bttnEvt = do
    evt <- buttonClick
    hl <- holdDyn "" evt -- Event -> Dynamic
    el "div" (dynText hl)
    
numberInput :: (DomBuilder t m, Num a, Read a) => m (Dynamic t a)
numberInput = do
    -- Lenses (getters e setters funcionais)
    n <- inputElement $ def
            & inputElementConfig_initialValue .~ "0"
            & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number" <> "class" =: "myClass")
    return $ fmap (fromMaybe 0 . readMaybe . T.unpack) (_inputElement_value n)

-- somar :: (PostBuild t m, DomBuilder t m) => m ()
-- somar = do
--    el "label" (text "N1: ")
--    n1 <- numberInput -- Dynamic t Int
--    el "label" (text "N2: ")
--    n2 <- numberInput -- Dynamic t Int
--    text " "
--    dynText (fmap (T.pack . show) (zipDynWith (+) n1 n2))
    
    
-- caixas :: (PostBuild t m, DomBuilder t m) => m ()
-- caixas = do
  -- t <- inputElement def -- Dynamic t Text
  -- s <- inputElement def -- Dynamic t Text 
  --  text " "
  --  dynText (zipDynWith (<>) (_inputElement_value t) (_inputElement_value s))

-- listaAtributos :: Map T.Text T.Text
-- listaAtributos = "id" =: "li2" <> "class" =: "class1"

txt :: DomBuilder t m => m () 
txt = do
    el "h2" $ text $ T.pack commonStuff
    el "p" $ text "Uma criptomoeda ou cibermoeda é um meio de troca, podendo ser centralizado ou descentralizado que se utiliza da tecnologia de blockchain e da criptografia para assegurar a validade das transações e a criação de novas unidades da moeda."
    el "p" $ text "O Bitcoin, a primeira criptomoeda descentralizada, foi criado em 2009 por um usuário que usou o pseudônimo Satoshi Nakamoto."  
    el "p" $ text "Desde então, muitas outras criptomoedas foram criadas."  
    el "h6" $ text "fonte: wikipédia"
