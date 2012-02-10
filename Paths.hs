module Paths where

libPath = "/home/rotsor/agda/lib/src"
pastesPath = "/home/rotsor/agda/pastes"
htmlPath = "/home/rotsor/agda/html"
childPath dir file = dir ++ "/" ++ file
agdaCmdLine id = 
  ["--html"] 
  ++ concat [["-i",path] | path <- [pastesPath, libPath]] 
  ++ ["--html-dir="++htmlPath]
  ++ [submissionPath id]
submissionFileName submissionId = "S" ++ show submissionId  ++".agda"
submissionPath :: Int -> String
submissionPath = childPath pastesPath . submissionFileName

