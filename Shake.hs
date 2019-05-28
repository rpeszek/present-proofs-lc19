
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

(<->) pref suf = pref <> "-" <> suf
task = flip (<->)

proj = "present-proofs-lc19"
proj_idr = proj <.> "ipkg"
inSrc = Cwd "src"

idris = "idris"
stack = "stack"
liquid = "liquid"
ohi = "ohi"  -- local editor compilation

targets = [liquid, idris, stack, ohi] 

clean = "clean"
build = "build"

repl = "repl"

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
    ohi <-> clean ~> (removeFilesAfter "src" ["//*.o"] >> 
                     removeFilesAfter "src" ["//*.hi"])

    idris <-> build ~> cmd_ [idris] "--build" [proj_idr]
    stack <-> build ~> cmd_ [stack] "build" 
    liquid <-> build ~> cmd_ [liquid] "src/Motivation/Liquid.hs"
    ohi <-> build ~> pure ()

    repl <-> idris ~> cmd_ inSrc idris

    repl <-> "maybeb" ~> cmd "stack ghci src/Present/MaybeB.hs"    
    repl <-> "boolalg" ~> cmd "stack ghci src/Present/ProofsBoolAlg.hs"  
    repl <-> "perf" ~> cmd "stack ghci src/Present/ProofsNatAlg.hs"   
    repl <-> "dec" ~> cmd "stack ghci src/Present/ProofsDecidable.hs"
    repl <-> "typelits" ~> cmd "stack ghci src/Present/WorkingWithTypeLits.hs"

    