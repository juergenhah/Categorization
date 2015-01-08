{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Pet
Description : implementation of the Aerts et al. 2005b Paper, with lists
Copyright   : 
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at

Formalizing the concept pet of the Aerts et al. 2005b Paper
-}
module Pet where

import SCOPList
-- * formalizing the concept pet

data PetContext
   = UnitContext -- ^ represents the basic/unit context of the concept pet: "The pet is just a pet"
   | C1 -- ^ represents the context: "The pet is chewing a bone"
   | C2 -- ^ represents the context: "The pet is being taught"
   | C3 -- ^ represents the context: "The pet runs through the garden"
   | C4 -- ^ represents the context: "Did you see the type of pet he has? This explains that he is a weird person"
   | C5 -- ^ represents the context: "The pet is being taught to talk"
   | C6 -- ^ represents the context: "The pet is a fish"
   deriving (Enum,Eq)

instance Show PetContext where
 show UnitContext = "The pet is just a pet"
 show C1 = "The pet is chewing a bone"
 show C2 = "The pet is being taught"
 show C3 = "The pet runs through the garden"
 show C4 = "Did you see the type of pet he has? This explains that he is a weird person"
 show C5 =  "The pet is being taught to talk"
 show C6 = "The pet is a fish"

data PetInstances 
   = Rabbit -- ^ represents the instance rabbit of concept pet
   | Cat -- ^ represents the instance cat of concept pet
   | Mouse -- ^ represents the instance mouse of concept pet
   | Bird -- ^ represents the instance bird of concept pet
   | Parrot -- ^ represents the instance parrot of concept pet
   | Goldfish -- ^ represents the instance goldfish of concept pet
   | Hamster -- ^ represents the instance hamster of concept pet
   | Canary -- ^ represents the instance canary of concept pet
   | Guppy -- ^ represents the instance guppy of concept pet
   | Snake -- ^ represents the instance snake of concept pet
   | Spider -- ^ represents the instance spider of concept pet
   | Dog -- ^ represents the instance dog of concept pet
   | Hedgehog -- ^ represents the instance hedgehog of concept pet
   | GuineaPig -- ^ represents the instance guineapig of concept pet
    deriving (Show,Enum,Eq)

instance SCOP PetContext PetInstances 

-- | enlarged space, values are picked from Table II paper Aerts et al. 2005b
-- observations for unit/context for all instances:
-- 98 times rabbit for UnitContex, 168 times cat for UnitContext, ...
o_1 = [98,168,70,112,98,140,98,112,126,42,28,168,42,98]

-- | obervations influenced by context C1
-- 12 times rabbit influenced by C1, ...
e_1 = [12,75,9,6,6,3,12,3,3,6,3,150,6,9]
e_2 = [35,65,30,40,80,10,35,35,10,10,5,95,10,35]
e_3 = [75,110,40,10,5,0,30,5,0,5,15,120,40,45]
e_4 = [5,3,11,4,4,2,4,2,2,22,23,3,12,4]
e_5 = [2,6,2,34,126,0,2,14,0,0,0,12,0,2]
e_6 = [0,1,0,1,1,48,0,1,46,1,0,1,0,0]


-- | relative frequencies, values are picked from Table 2 paper Aerts et al. 2005a
of_1 = [7,12,5,8,7,10,7,8,9,3,2,12,3,7]
ef_1 = [4,25,3,2,2,1,4,1,1,2,1,50,2,3]
ef_2 = [7,13,6,8,16,2,7,7,2,2,1,19,2,7]
ef_3 = [15,22,8,2,1,0,6,1,0,1,3,24,8,9]
ef_4 = [5,3,11,4,4,2,4,2,2,22,23,3,12,4]
ef_5 = [1,3,1,17,63,0,1,7,0,0,0,6,0,1]
ef_6 = [0,1,0,1,1,48,0,1,46,1,0,0,0,0]

-- | which enlargement factor is used here?
-- for what does it stand for?
teste = o_1 == map (*14) of_1

-- | create SCOP model with enlarged values
petExperiences = (generateExperiences [o_1 ,e_1,e_2,e_3,e_4,e_5,e_6]) :: [(PetContext,PetInstances)]  

-- | create SCOP model with relative values
petFExp = (generateExperiences [of_1,ef_1,ef_2,ef_3,ef_4,ef_5,ef_6]):: [(PetContext,PetInstances)]  


-- | equation 35 from Aerts et al. 2005b
--
-- >>> 0.24752475
xpu = mu C1 Cat petExperiences
xpfu = mu C1 Cat petFExp

isEqual1 = xpu== xpfu

-- | equation 37 from Aerts et al. 2005b
--
-- >>> 0.12
xph = mu UnitContext Cat petExperiences
xpfh = mu UnitContext Cat petFExp

isEqual2 = xph == xpfh

-- | equation 43 from Aerts et al. 2005b
--
-- >>> 0.0
xpi =mu C6 Hedgehog petExperiences
xpfi  = mu C6 Hedgehog petFExp

isEqual3 = xpi == xpfi

-- | equation 47 from Aerts et al. 2005b
--
-- >>> 0.48
xpj = mu C6 Goldfish petExperiences
xpfj = mu C6 Goldfish petFExp

isEqual4 = xpj == xpfj -- accuracy issue
 -- StateMonad?
