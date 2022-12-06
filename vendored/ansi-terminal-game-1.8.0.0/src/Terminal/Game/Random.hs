module Terminal.Game.Random ( R.StdGen,
                              R.UniformRange,
                              R.getStdGen,
                              R.mkStdGen,
                              getRandom,
                              pickRandom )
            where

import System.Random as R


-- | Simple, pure pseudo-random generator.
getRandom :: UniformRange a => (a, a) -> StdGen -> (a, StdGen)
getRandom bs sg = uniformR bs sg

-- | Picks at random from list.
pickRandom :: [a] -> StdGen -> (a, StdGen)
pickRandom as sg = let l = length as
                       (a, sg') = getRandom (0, l-1) sg
                   in (as !! a, sg')
