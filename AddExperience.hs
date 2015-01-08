{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : AddExperience
Description : Extension to Aerts et al. 2005b paper; demonstrates how new experiences/observations can be added
Copyright   : 
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at

Small example demonstrating the experiences of myself. I want to have lunch and dependent of the company I will change my selection.
-}
module AddExperience where

import SCOPList

data LunchContext 
   = UnitContext -- ^ represents the basic/unit context of the concept lunch: "I'm going for lunch"
   | C1 -- ^ represents the context :"I'm going for lunch with collegues"
   | C2 -- ^ represents the context : "I'm going for lunch with my professor"
   | C3 -- ^ represents the context : "I'm going for lunch with my professor and my collegues"
   deriving (Enum,Eq)

instance Show LunchContext where
 show UnitContext = "I'm going for lunch"
 show C1 = "I'm going for lunch with collegues"
 show C2 = "I'm going for lunch with my professor"
 show C3 = "I'm going for lunch with my professor and my collegues"

data LunchInstances 
   = Spar -- ^ reprents the instance spar of the concept lunch
   | Bok -- ^ reprents the instance bok of the concept lunch
   | TofuChilli -- ^ reprents the instance "Tofu and Chilli" of the concept lunch
   | Burger -- ^ reprents the instance "Burger restaurant" of the concept lunch
   | Uber -- ^ reprents the instance "Gasthaus Uber" of the concept lunch
   | Wirtshaus -- ^ reprents the instance "Das Wirtshaus" of the concept lunch
   deriving (Enum,Eq,Show)

data LunchProperties
   = Expensive -- ^ represents the property: "is an expensive place to buy lunch"
   | Moderate -- ^ represents the property: "the price is moderate"
   | Cheap -- ^ represents the property: "the price is cheap"
   deriving (Enum,Eq,Show)

instance SCOP LunchContext LunchInstances

-- * establishing experiences with contexts

-- | represents my own experience
-- I took lunch 76 times in unit context @   spar,
--              22 times                 @   bok,
--               8 times                 @   TofuChilli,
--                 ...                         ...
o_1 = [76,22,8,2,2,2] 
-- | represents my experience influenced by context C1
-- I took lunch with my collugues :
--              76 times @ Spar
--              22 times @ bok
--                   ...
e_1 = [76,22,8,2,0,1]
e_2 = [0,0,0,0,0,1]
e_3 = [0,0,0,0,2,0]

experience = (generateExperiences [o_1,e_1,e_2,e_3] ):: [(LunchContext,LunchInstances)]

-- * establishing properties with contexts
-- TODO
op_1 = []
p_1 = []
p_2 = []
p_3 = []

-- | probability to go to Spar Alone
--
-- >>> sparAlone = 0.6785714
sparAlone = mu UnitContext Spar experience

-- | probability to go to Uber Alone
--
-- >>> uberAlone =  1.7857144e-2
uberAlone = mu UnitContext Uber experience

-- | probability to go to Spar with my professor and the collegues
--
-- >>> sparProfandCollegues = 0.0
sparProfandCollegues = mu C3 Spar experience

-- | probability to go to Uber with my professor and the collegues
--
-- >>> uberProfandCollegues = 1.0
uberProfandCollegues = mu C3 Uber experience

-- | probability to go to Spar with Professor
--
-- >>> sparProf = 0.0
sparProf= mu C2 Spar experience


-- | probability to go to Wirtshaus with my professor
--
-- >>> wirtshausProf = 1.0
wirtshausProf = mu C2 Wirtshaus experience

-- * add a new experience
-- Professor joines me to have lunch at Bok restaurant
addedExperience = addObservation (C2,Bok) experience

-- | probability to go to Wirtshaus with my professor
--
-- >>> wirtshausProfAdded = 0.5
wirtshausProfAdded = mu C2 Wirtshaus addedExperience

-- | probability to go to Bok with my professor
--
-- >>> wirtshausProf = 0.5
bokProfAdded = mu C2 Bok addedExperience

-- * added a new experience
-- professor joines me to have lunch at Bok again
added2Experience = addObservation (C2,Bok) addedExperience

-- | probability to go to Wirtshaus with my professor
--
-- >>> wirtshausProfAdded = 0.3333
wirtshausProfAdded2 = mu C2 Wirtshaus added2Experience

-- | probability to go to Bok with my professor, it is more probable now to have lunch at Bok compared to previous states 
-- 
-- >>> wirtshausProf = 0.6666
bokProfAdded2 = mu C2 Bok added2Experience
