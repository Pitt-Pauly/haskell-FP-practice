-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!


import System
import IO
import List( nub )
import Char
import Test.QuickCheck

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

tutorialURL = "http://www.inf.ed.ac.uk/admin/itodb/mgroups/tuts/inf1-fp.html"
groupURL    = "http://www.inf.ed.ac.uk/admin/itodb/mgroups/stus/inf1-fp.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:w.b.heijltjes@sms.ed.ac.uk\">Willem Heijltjes</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br><b>TA:</b> "
            , "mailto:w.b.heijltjes@sms.ed.ac.uk\">Willem Heijltjes</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Philip Wadler","wadler@inf.ed.ac.uk")
               , ("Willem Heijltjes","w.b.heijltjes@sms.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url =
    do username <- getEnv "LOGNAME"
       system ("wget -O /tmp/fp-tmp-" ++ username ++ ".html --quiet " ++ url)
       readFile ("/tmp/fp-tmp-" ++ username ++ ".html")

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <predefined functions>

-- substrIndex gives the length of the string before the substring occurs;
--  if it doesn't occur, it returns the length of the whole string
substrIndex :: String -> String -> Int
substrIndex xs ys  =  head ([ i | i <- [0..n-1], prefix xs (drop i ys) ] ++ [n])
    where
      n = length ys

-- takeUntil takes the part of `ys' before `xs' occurs as a substring;
--  if it doesn't occur, the whole string `ys' is returned
takeUntil :: String -> String -> String
takeUntil xs ys  =  take (substrIndex xs ys) ys

-- dropUntil takes the remainder of a string `ys' after the first
--  occurrence of `xs'; if `xs' is not a substring, it returns the empty string
dropUntil :: String -> String -> String
dropUntil xs ys  =  drop (substrIndex xs ys + length xs) ys

-- </predefined functions>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString str1 str2  =  toLowers str1 == toLowers str2
    where
      toLowers str  =  [toLower c | c <- str]

prop_sameString :: String -> Bool
prop_sameString str  = 
    [toLower c | c <- str] `sameString`  [toUpper c | c <- str]

-- 2.
prefix :: String -> String -> Bool
prefix substr str  =  (substr `sameString` take (length substr) str)

prop_prefix :: String -> Int -> Bool
prop_prefix str n  =  prefix substr [toLower c | c <- str] &&
		      prefix substr [toUpper c | c <- str]
                          where
                            substr  =  take n str


-- 3.
contains :: String -> String -> Bool
contains str substr  =
    [i | i <- [0..length str], prefix substr (drop i str)] /= []

prop_contains :: String -> Int -> Int -> Bool
prop_contains str n m  =  [toLower c | c <- str] `contains` substr &&
                          [toUpper c | c <- str] `contains` substr
                              where
                                substr  =  take n (drop m str)

-- An alternative solution that (arguably) creates better test data,
--  but occasionally produces 'arguments exhausted':
prop_contains2 :: String -> Int -> Int -> Property
prop_contains2 str n m  =  (0 <= n && 0 <= m && n + m <= length str) ==>
                           ([toLower c | c <- str] `contains` substr &&
                            [toUpper c | c <- str] `contains` substr )
                              where
                                substr  =  take n (drop m str)


-- 4.
split :: String -> String -> [String]
split "" str   =  error "Can't split on an empty string"
split sep str  
    | contains str sep  =  takeUntil sep str : split sep (dropUntil sep str)
    | otherwise         =  [str]

reconstruct :: String -> [String] -> String
reconstruct _ []            =  error "cannot reconstruct from empty list"
reconstruct _ [str]         =  str
reconstruct sep (str:strs)  =  str ++ sep ++ reconstruct sep strs

prop_split :: String -> String -> Property
prop_split sep str  =  sep /= [] ==> reconstruct sep (split sep str) `sameString` str

-- 5.
linksFromHTML :: HTML -> [Link]
linksFromHTML doc  =  tail (split "<a href=\"" doc)

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks

-- 6.
takeEmails :: [Link] -> [Link]
takeEmails links  =  [link | link <- links, prefix "mailto:" link]

-- 7.
link2pair :: Link -> (Name, Email)
link2pair link  =  (name, email)
    where email  =  takeUntil "\">"  (dropUntil "mailto:" link)
          name   =  takeUntil "</a>" (dropUntil "\">" link)

-- 8.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML html  =  
    nub [link2pair link | link <- takeEmails (linksFromHTML html)]

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook

-- 9.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail name addrs = [(n, e) | (n, e) <- addrs, n `contains` name]


-- 10.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html name  = 
    findEmail name (emailsFromHTML html)

-- 11.
--ppAddrBook :: [(Name, Email)] -> String
--ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]

ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addrBook  =
    unlines [ formatName name 
              ++ replicate (2 + maxLen - length name) ' ' 
              ++ email 
              | (name,email) <- addrBook ]
        where maxLen  =  maximum [length (formatName name) | (name, email) <- addrBook]

formatName name  
    | name `contains` ","  =  name
    | otherwise  =  reconstruct " " (tail splitNames) ++ ", " ++ head splitNames 
    where splitNames  =  split " " name
