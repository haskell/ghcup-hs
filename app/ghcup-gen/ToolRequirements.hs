{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ToolRequirements where

import           GHCup.Types
import           GHCup.Utils.String.QQ

import qualified Data.Map                      as M



-- | Currently 'GHC' is used for both GHC and cabal to simplify
-- this, until we need actual separation.
toolRequirements :: ToolRequirements
toolRequirements = M.fromList
  [ ( GHC
    , M.fromList
      [ ( Nothing
        , M.fromList
          [ ( Linux UnknownLinux
            , M.fromList
              [ ( Nothing
                , Requirements
                  []
                  [s|You need the following packages: curl g++ gcc gmp make ncurses realpath xz-utils. Consult your distro documentation on the exact names of those packages.|]
                )
              ]
            )
          , ( Linux Alpine
            , M.fromList
              [ ( Nothing
                , Requirements
                  [ "curl"
                  , "gcc"
                  , "g++"
                  , "gmp-dev"
                  , "ncurses-dev"
                  , "libffi-dev"
                  , "make"
                  , "xz"
                  , "tar"
                  , "perl"
                  ]
                  ""
                )
              ]
            )
          , ( Linux Ubuntu
            , M.fromList
              [ ( Nothing
                , Requirements
                  [ "build-essential"
                  , "curl"
                  , "libgmp-dev"
                  , "libffi-dev"
                  , "libncurses-dev"
                  , "libtinfo5"
                  ]
                  ""
                )
              ]
            )
          , ( Darwin
            , M.fromList
              [ ( Nothing
                , Requirements
                  []
                  "On OS X, in the course of running ghcup you will be given a dialog box to install the command line tools. Accept and the requirements will be installed for you. You will then need to run the command again."
                )
              ]
            )
          , ( FreeBSD
            , M.fromList
              [ ( Nothing
                , Requirements
                  [ "curl"
                  , "gcc"
                  , "gmp"
                  , "gmake"
                  , "ncurses"
                  , "perl5"
                  , "libffi"
                  , "libiconv"
                  ]
                  ""
                )
              ]
            )
          ]
        )
      ]
    )
  ]
