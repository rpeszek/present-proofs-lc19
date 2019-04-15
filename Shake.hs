
-- for simple use: 
-- > alias rshake="runhaskell Shake"
-- 
-- > rshake                 #same as rshake build (requires liquid, stack, and idris to be installed)
-- > rshake <target>-build  #e.g. rshake stack-build
-- > rshake clean          
-- > rshake <target>-clean  

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Data.Semigroup ((<>))


opts = shakeOptions { shakeFiles = ".shake/" }        

(<->) pref surf = pref <> "-" <> surf
task = flip (<->)

proj = "present-proofs-lc19"
proj_idr = proj <.> "ipkg"
inSrc = Cwd "src"

idris = "idris"
stack = "stack"
liquid = "liquid"

targets = [liquid, idris, stack] 

clean = "clean"
build = "build"

main :: IO ()
main = shakeArgs opts $ do
    want [build]

    clean ~> do
       need $ map (task clean) targets
       pure ()

    build ~> do
       need $ map (task build) targets
       pure ()

    "shake-clean" ~> removeFilesAfter ".shake" ["//*"]
    idris <-> clean ~> cmd_ [idris] "--clean" [proj_idr]   
    stack <-> clean ~> cmd_ [stack] "clean"
    liquid <-> clean ~> removeFilesAfter "src/Motivation/.liquid" ["//*"] 

    idris <-> build ~> cmd_ [idris] "--build" [proj_idr]
    stack <-> build ~> cmd_ [stack] "build" 
    liquid <-> build ~> cmd_ [liquid] "src/Motivation/Liquid.hs"

    idris ~> cmd_ inSrc idris
    