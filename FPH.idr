module FPH

import Data.List
import Data.String

square : Int -> Int
square x = x * x


hword : String -> Bool
hword xs = hwAcc (unpack xs)  where
  hwAcc : List Char -> Bool
  hwAcc Nil       = False
  hwAcc (x :: xs) = (x == 'h') || hwAcc xs


gen : Int -> String
gen 0 = "Sentence go on"
gen n = gen (n-1) ++ " and on"
genS : Int -> String
genS n = gen n ++ "."
genS2 : Int -> String
genS2 n    = genrec n ++ "." where
  genrec : Int -> String
  genrec 0 = "Sentence go on"
  genrec n = genrec (n-1) ++ " and on"

story : Int -> String
story n    = (storec n) where
  storec : Int -> String
  storec 0 = "Let's cook and eat that final missionary, and off to bed."
  storec k = "The night was pitch dark, mysterious and deep.\n"
             ++ "Ten cannibals were seated around a boiling cauldron.\n"
             ++ "Their leader got up and addressed them like this:\n'"
             ++ story (k-1) ++ "'"

reversal : String -> String
reversal xs = reversAcc (unpack xs) "" where
  reversAcc : List Char -> String ->String
  reversAcc Nil       ys = ys
  reversAcc (x :: xs) ys = reversAcc xs (strCons x ys)

sonnet18 : String
sonnet18 =
 "Shall I compare thee to a summer's day? \n"
 ++ "Thou art more lovely and more temperate: \n"
 ++ "Rough winds do shake the darling buds of May, \n"
 ++ "And summer's lease hath all too short a date: \n"
 ++ "Sometime too hot the eye of heaven shines, \n"
 ++ "And often is his gold complexion dimm'd; \n"
 ++ "And every fair from fair sometime declines, \n"
 ++ "By chance or nature's changing course untrimm'd; \n"
 ++ "But thy eternal summer shall not fade \n"
 ++ "Nor lose possession of that fair thou owest; \n"
 ++ "Nor shall Death brag thou wander'st in his shade, \n"
 ++ "When in eternal lines to time thou growest: \n"
 ++ "  So long as men can breathe or eyes can see, \n"
 ++ "  So long lives this and this gives life to thee."

sonnet73 : String
sonnet73 =
 "That time of year thou mayst in me behold\n"
 ++ "When yellow leaves, or none, or few, do hang\n"
 ++ "Upon those boughs which shake against the cold,\n"
 ++ "Bare ruin'd choirs, where late the sweet birds sang.\n"
 ++ "In me thou seest the twilight of such day\n"
 ++ "As after sunset fadeth in the west,\n"
 ++ "Which by and by black night doth take away,\n"
 ++ "Death's second self, that seals up all in rest.\n"
 ++ "In me thou see'st the glowing of such fire\n"
 ++ "That on the ashes of his youth doth lie,\n"
 ++ "As the death-bed whereon it must expire\n"
 ++ "Consumed with that which it was nourish'd by.\n"
 ++ "This thou perceivest, which makes thy love more strong,\n"
 ++ "To love that well which thou must leave ere long."

count : Eq a => a -> List a -> Int
count x []                  = 0
count x (y::ys) = if x == y then 1 + (count x ys)
                  else count x ys

average : List Int -> Maybe Double
average [] = Nothing
average xs = Just (cast (sum xs) / cast (length xs))


prefix' : Eq a => List a -> List a -> Bool
prefix' []      ys      = True
prefix' (x::xs) []      = False
prefix' (x::xs) (y::ys) = (x==y) && prefix' xs ys

prefixString : String -> String -> Bool
prefixString xs ys = prefixStrM (strM xs) (strM ys) where
  prefixStrM : StrM _ -> StrM _ -> Bool
  prefixStrM StrNil _ = True
  prefixStrM _ StrNil = False
  prefixStrM (StrCons x xs) (StrCons y ys) = x==y && prefixString xs ys


mutual
  even : Nat -> Bool
  even Z = True
  even (S k) = odd k

  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k



notElem : Eq a => a -> List a -> Bool
notElem x xs = not (elem x xs)


puncfilter : List Char -> List Char
puncfilter = filter (\x => notElem x ['?', ';', ':', ',', '.'])


preprocess : String -> String
preprocess = pack . map toLower . puncfilter . unpack

process : String -> List String
process = sort . nub . words



export cnt : String -> List (String, Int)
cnt sonnet = [ (x,n)| x <- (process . preprocess) sonnet,
                 n <- [count x (words (preprocess sonnet))],
                 n > 1
             ]

 
ront : Char -> Char
front s = case s of 
          'a' => 'ä'
          'o' => 'ö'
          'u' => 'y'
          _ => s


back : Char -> Char
back s = case s of 
          'ä' => 'a'
          'ö' => 'o'
          'y' => 'u'
          _ => s


appendSuffixF : String -> String -> String
appendSuffixF stem suffix = stem ++ pack (mapImpl vh (unpack suffix))
  where
    vh : Char -> Char 
    vh = if isCons [ p | p <- (unpack stem), elem p ['a', 'o', 'u']] 
          then back 
          else
          if isCons [ p | p <- (unpack stem), elem p ['ä', 'ö', 'y']]
             then front
             else id
           

