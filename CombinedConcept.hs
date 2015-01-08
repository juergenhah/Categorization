module CombinedConcept  where

import Pet
import Fish
import SCOPList

-- | compare to equation 88 Aerts paper 2005b
-- experience list for the concept pet influenced by context : "the pet is a fish"
s_pet = filterContext C6 petExperiences

-- experience list for the concept fish influenced by context : "the fish is a pet"
s_fish = filterContext C30 fishExperiences

-- * combination
-- only goldfish and guppy are instances of both concepts all others will result in a probability value of 0

combinedGoldfish = (mu C6 Pet.Goldfish petExperiences )* (mu C30 Fish.Goldfish fishExperiences)

combinedGuppy =  (mu C6 Pet.Guppy petExperiences )* (mu C30 Fish.Guppy fishExperiences)


