
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Fish
Description : implementation of the Aerts et al. 2005b Paper, with lists
Copyright   : 
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at

Formalizing the concept fish of the Aerts et al. 2005b Paper
-}
module Fish where
 
import SCOPList

data FishContext 
   = UnitContext -- ^ represents the basic/unit context of the concept fish : "The fish is just a fish"
   | C30         -- ^ represents the context: "The fish is a pet"
    deriving (Eq,Enum)

instance Show FishContext where
 show UnitContext = "The fish is just a fish"
 show C30 = "The fish is a pet"

data FishInstances 
   = Trout -- ^ represents the instance trout of concept fish
   | Shark -- ^ represents the instance shark of concept fish
   | Whale -- ^ represents the instance whale of concept fish
   | Dolphin -- ^ represents the instance dolphin of concept fish
   | Pike -- ^ represents the instance pike of concept fish
   | Goldfish -- ^ represents the instance goldfish of concept fish
   | Ray -- ^ represents the instance ray of concept fish
   | Tuna -- ^ represents the instance tuna of concept fish
   | Barracuda -- ^ represents the instance barracuda of concept fish
   | Mackerel -- ^ represents the instance mackerel of concept fish
   | Herring -- ^ represents the instance herring of concept fish
   | Guppy -- ^ represents the instance guppy of concept fish
   | Plaice -- ^ represents the instance plaice of concept fish 
   | Carp -- ^ represents the instance carp of concept fish
   deriving (Show,Eq,Enum)

instance SCOP FishContext FishInstances

-- | scaled observation values taken from table V Aerts et al. 2005b
o_1 = [36,36,28,28,20,40,24,36,12,28,36,32,28,24]
e_30= [2,2,1,4,1,40,1,1,1,1,1,39,1,5]


fishExperiences = generateExperiences [o_1,e_30] :: [(FishContext,FishInstances)]

-- | equations from the paper Aerts et al. 2005b
-- 
-- >>> 0.05 in paper, result here = 4.901961e-2
pikeinFish = mu UnitContext Pike fishExperiences

-- | equations from the paper Aerts et al. 2005b
-- 
-- >>> 0.07 in paper, result here = 6.8627454e-2
dolphinFish = mu UnitContext Dolphin fishExperiences

-- | equations from the paper Aerts et al. 2005b
-- 
-- >>> 0.08 in paper, result here = 7.8431375e-2  
guppyFish = mu UnitContext Guppy fishExperiences
