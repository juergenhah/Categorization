
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : SCOPList
Description : implementation of the Aerts et al. 2005b Paper, with lists
Copyright   : nobody
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at

TODO: data structure for context should be a lattice (suggested by the papers), 
      wrap it into a state monad with bind only possible if the context is in the lattice


partial implementation of State COntext Property model, properties are not implemented yet. The idea is extracted from the following papers:
"A theory of concepts and their combinations I: A Hilbert space representation" - Aerts et al. 2005a
"A theory of concepts and their combinations II: A Hilbert space representation" - Aerts et al. 2005b
The proposed Hilbert space representation is not used here. We use lists and are able reproduce all equations and results included in the two papers.
-}
module SCOPList where

-- * The SCOP model is based on three Sets:
--   set of states, set of contexts, set of properties
--   including two functions mu and vi.
-- So far we implemented the function mu and do not use the set of properties with function vi, because these are not mentioned in the second paper anyway.
-- The list elements building the experience list consists of (context,instance) pairs.

-- | The list implementation requires the Enum and Eq typeclasses for the contexts and instances types, what is accountable as ecological rational
class (Enum c,Eq c,Enum i,Eq i) => SCOP c i where -- TODO add set of properties

--  functions needed to fill the sets

 -- |concats a bunch of experiences
 addObservations :: [[(c,i)]] -- ^ list of observations each list element is for one context
                 -> [(c,i)]   -- ^ resulting list of experiences
 addObservations = concat

 -- |adds a single experience (context,instance) tupel to experiences included so far
 addObservation :: (c,i)   -- ^ one experience (context,instance) pair
                -> [(c,i)] -- ^ previous experience list
                -> [(c,i)] -- ^ new experience list including the additional experience
 addObservation = (:)

  -- | generates a list including all experiences (experience list), replicates an experience as often as in the [[Int]] given the [[Int] list must have a defined order, all contexts and instances are iterated through given the order from the Enum
 generateExperiences :: (Enum c, Enum i)=>  [[Int]] -- ^ amount of observations for context and instances
                     -> [(c,i)] -- ^ generated experience list
 generateExperiences  obs = addObservations $ zipWith (\l e -> replicate l e) osAmount ctxIns
  where  ctxIns = [ (i,j) | i <-enumFrom (toEnum 0), j <- enumFrom (toEnum 0)] 
         osAmount = foldl1 (++) obs

--  functions that also exist in the papers but with other terminology

 -- | counts the elements in the experience list,
 --   compareable to a sum over the Hilbert space
 count :: [(c,i)] -- ^ experience list
       -> Float   -- ^ length of the experience list converted to Float type
 count = fromIntegral.length
 
 -- | extracts only these experiences from the experience list matching the selected context,
 --   compareable with a projector in the Hilbert space
 filterContext ::Eq c => c -- ^ context that acts as filter
               -> [(c,i)]  -- ^ experience list
               -> [(c,i)]  -- ^ experience list containing only the experiences having the context c
 filterContext c = filter (\y -> c== fst y)
 
 -- | extracts only these experiences from the given experience list matching the selected instance
 --   compareable with a projector in the Hilbert space
 filterInstance :: Eq i => i -- ^ instance that acts as filter
                 -> [(c,i)]  -- ^ experience list
                 -> [(c,i)]  -- ^ experience list containing only the experiences having the instance i
 filterInstance i = filter (\y -> i== snd y)

 -- | combines the functions filterContext and filterInstance
 --   compareable with a projector in the Hilbert space
 filterContextInstance :: (Eq c, Eq i) =>c -- ^ context that acts as filer
                       -> i         -- ^ instance that acts as filter
                       -> [(c,i)]   -- ^ experience list
                       -> [(c,i)]   -- ^ experience list containing only the experiences with context c and instance i
 filterContextInstance c i =filterContext c. filterInstance i

 --  | represents the function mu from the papers 
 -- calculates the probability of an instance in a context
 mu :: (Eq c, Eq i) => c -- ^ context 
    -> i       -- ^ instance
    -> [(c,i)] -- ^ experience list in this state
    -> Float   -- ^ probability/likelihood of the instance i for the context c for this experience
 mu c i obs= (count $ filterContextInstance c i obs) / (count $ filterContext c obs )

