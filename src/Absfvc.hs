module Absfvc where

-- Haskell module generated by the BNF converter
import AbsLatte

type FProgram = [FourProgram]

data CodeFlow =  Flow CodeBlock [CodeFlow] | Leaf deriving (Show, Eq, Ord)
type Label = String

data Iden = Iden Type String | LocId Type String | DecId Type String | Emmm deriving (Eq, Ord)
instance Show Iden where
  show (Iden _ str) = str
  show (LocId _ str) = str
  show (DecId _ str) = str
  show Emmm = ""
data FourProgram =
   FourProg Type Iden [DataDef] CodeFlow | Emm 
    deriving (Eq,Ord,Show)

idenType :: Iden -> Type 
idenType (Iden type_ _) = type_
idenType (LocId type_ _) = type_
idenType (DecId type_ _) = type_

data DataDef =
   IntDataDef Iden
 | StrDataDef Iden
 | BoolDataDef Iden
 | ArrDataDef Iden Integer Integer
  deriving (Eq,Ord)

dataType :: DataDef -> Type
dataType d = case d of
 (IntDataDef id) -> Int
 (StrDataDef id) -> Str
 (BoolDataDef id) -> Int
instance Show DataDef where
  show (IntDataDef id) = show id
  show (StrDataDef iden) = show iden 
  show (BoolDataDef iden) = show iden
  show (ArrDataDef iden int1 int2) = show iden
  
data CodeBlock =
   CodeBlock Label [NonJmpStmt] JmpStmt
  deriving (Eq,Ord,Show)

data NonJmpStmt =
   StmtBinOp Iden Atom ArithOp Atom
 | StmtNegOp Iden Atom
 | StmtCall Iden [Atom]
 | StmtCallRet Iden Iden [Atom] 
 | StmtEmpty 
 | StmtNoOp Iden Atom
 | StmtArrLoad Iden Iden Atom
 | StmtArrStore Iden Atom Atom
  deriving (Eq,Ord,Show)

data JmpStmt =
   StmtGoTo Label
 | StmtCondJmp Atom RelOp_ Atom Label Label
 | StmtRet Atom
 | StmtVRet
  deriving (Eq,Ord,Show)

data Atom =
   AtomVar Iden
 | AtomConst Integer
 | AtomStr String
  deriving (Eq,Ord,Show)

atomType :: Atom -> Type
atomType (AtomVar id) = idenType id
atomType (AtomConst _) = Int
atomType (AtomStr _) = Str

data ArithOp =
   AddArithOp
 | SubArithOp
 | MulArithOp
 | DivArithOp
 | ModArithOp
 | ConcatArithOp
  deriving (Eq,Ord)

instance Show ArithOp where
  show AddArithOp = "addl"
  show SubArithOp = "subl"
  show MulArithOp = "imull"
  show DivArithOp = "divl"
  show ModArithOp = "divl"
  show ConcatArithOp = "concat"

data RelOp_ =
   EqRelOp
 | NeRelOp
 | GtRelOp
 | GeRelOp
 | LtRelOp
 | LeRelOp
  deriving (Eq,Ord)

instance Show RelOp_ where
  show EqRelOp = "je"
  show NeRelOp = "jne"
  show GtRelOp = "jg"
  show GeRelOp = "jge"
  show LtRelOp = "jl"
  show LeRelOp = "jle"

