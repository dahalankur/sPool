module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

let translate (SProgram(statements)) = raise (TODO "translate")