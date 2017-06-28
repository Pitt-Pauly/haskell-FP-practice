-- Informatics 1 - Functional Programming 
-- Tutorial 4
-- (c) Korte, Wadler
--
-- Week 6 - Due: 30/31 Oct.


import System
import IO
import List( nub )
import Char
import Test.QuickCheck

import Data.List

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
strToLower :: String -> String
strToLower str = [ toLower c | c <- str ]

strToUpper :: String  -> String
strToUpper str = [ toUpper c | c <- str ]


sameString :: String -> String -> Bool
sameString str1 str2 = if length str1 == length str2  
                         then and [ c1 == c2 | (c1,c2) <- zip (strToLower str1) (strToLower str2)]
                         else False  

prop_sameString :: String -> Bool
prop_sameString str  = 
    [toLower c | c <- str] `sameString`  [toUpper c | c <- str]


-- 2.
prefix :: String -> String -> Bool
prefix l s = strToLower l `isPrefixOf` strToLower s


prop_prefix :: String -> Int -> Bool
prop_prefix str n  =  prefix substr [toLower c | c <- str] &&
		      prefix substr [toUpper c | c <- str]
                          where
                            substr  =  take n str

-- 3.

contains :: String -> String -> Bool
contains [] [] = True
contains long short = or [ (strToLower short) `isPrefixOf` (drop i (strToLower long)) | i <- [0..(length long)-1] ]

prop_contains :: String -> Int -> Int -> Bool
prop_contains str n i = contains (strToUpper str) substr && 
                        contains (strToLower str) substr
                            where 
                              substr = take n (drop i str)
                        

-- 4.
split :: String -> String -> [String]
split [] sep = error "Seperator is empty!"
split sep [] = []
split sep str = p : split sep (drop (length p + length sep) str)
      where p = takeUntil sep str 

reconstruct :: String -> [String] -> String
reconstruct sep []     = []
reconstruct sep [x]    =  x 
reconstruct sep (x:xs) =  x ++ sep ++ reconstruct sep xs

prop_split :: String -> String -> Property
prop_split sep str  =  
    sep /= [] ==> reconstruct sep (split sep str) `sameString` str


-- 5.
linksFromHTML :: HTML -> [Link]
linksFromHTML html  = split sep (dropUntil sep html)
         where sep = "<a href=\""

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 6.
takeEmails :: [Link] -> [Link]
takeEmails ls =  [ e | e <- ls, contains e "mailto:" ]


-- 7.
link2pair :: Link -> (Name, Email)
link2pair l =  (getName l , getEmail l)
  where 
    getName l = takeUntil "</a>" ( dropUntil "\">" l )
    getEmail l = takeUntil "\">" ( dropUntil "mailto:" l ) 


-- 8.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML html = nub  [ link2pair l | l <- takeEmails (linksFromHTML html) ]  

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 9.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail p book = [(n,e) | (n,e) <- book, contains n p]  


-- 10.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML  html name =  findEmail name (emailsFromHTML html)  


-- 11. (optional)
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ takeUntil " " name 
                                    ++ ", " 
                                    ++ dropUntil " " name 
                                    ++ replicate (30 - length name) ' '
                                    ++ email 
                                        | (name,email) <- addr ]
