module Test where

import IntList
import Laws

main = checkLaws (Proxy :: Proxy IntList)
