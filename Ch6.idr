import Data.Vect

%default total

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True  = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False  = "Ninety four"
getStringOrInt True   = 94

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False x = trim x
valToString True x  = cast x

AdderType : (numargs : Nat) -> Type
AdderType Z = Int
AdderType (S k) = (next : Int) -> AdderType k

adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)

data Format =
    Number Format
  | Str Format
  | Chr Format
  | Flt Format
  | Lit String Format
  | End

PrintfType : Format -> Type
PrintfType (Number fmt)   = (i : Int) -> PrintfType fmt
PrintfType (Str fmt)      = (str : String) -> PrintfType fmt
PrintfType (Chr fmt)      = (ch : Char) -> PrintfType fmt
PrintfType (Flt fmt)      = (f : Double) -> PrintfType fmt
PrintfType (Lit str fmt)  = PrintfType fmt
PrintfType End            = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc    = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc       = \str => printfFmt fmt (acc ++ str)
printfFmt (Chr fmt) acc       = \c => printfFmt fmt (acc ++ show c)
printfFmt (Flt fmt) acc       = \f => printfFmt fmt (acc ++ show f)
printfFmt (Lit lit fmt) acc   = printfFmt fmt (acc ++ lit)
printfFmt End acc             = acc

toFormat : (xs : List Char) -> Format
toFormat []                     = End
toFormat ('%' :: 'd' :: chars)  = Number (toFormat chars)
toFormat ('%' :: 's' :: chars)  = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars)  = Chr (toFormat chars)
toFormat ('%' :: 'f' :: chars)  = Flt (toFormat chars)
toFormat ('%' :: chars)         = Lit "%" (toFormat chars)
toFormat (c :: chars)           =
  case toFormat chars of
    Lit lit chars' => Lit (strCons c lit) chars'
    fmt            => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Int)

TupleVect : Nat -> Type -> Type
TupleVect Z _       = ()
TupleVect (S k) ty  = (ty, TupleVect k ty)
