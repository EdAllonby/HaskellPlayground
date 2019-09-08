module FileSystem where

import           Data.List (break)

type Name = String

type Data = String

data FSItem = File Name Data | Folder Name [FSItem]
    deriving (Show)

myDisk :: FSItem
myDisk =
    Folder "root"
           [ File "goat_yelling_like_man.wmv" "baaaaaa"
           , File "pope_time.avi" "god bless"
           , Folder "pics"
                    [ File "ape_throwing_up.jpg" "bleargh"
                    , File "watermelon_smash.gif" "smash!!"
                    , File "skull_man(scary).bmp" "Yikes!"
                    ]
           , File "dijon_poupon.doc" "best mustard"
           , Folder "programs"
                    [ File "fartwizard.exe" "10gotofart"
                    , File "owl_bandit.dmg" "mov eax, h00t"
                    , File "not_a_virus.exe" "really not a virus"
                    , Folder "source code"
                             [File "best_hs_prog.hs" "main = print (fix error)", File "random.hs" "main = print 4"]
                    ]
           ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem]
    deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs : bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item : rs) = break (nameIs name) items in (item, FSCrumb folderName ls rs : bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _)     = name == fileName

(-:) :: a -> (a -> b) -> b
x -: f = f x

newFocus :: FSZipper
newFocus = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"

newFocus2 :: FSZipper
newFocus2 = newFocus -: fsUp -: fsTo "watermelon_smash.gif"

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder _ items, bs) = (Folder newName items, bs)
fsRename newName (File _ dat, bs)     = (File newName dat, bs)

newFocus3 :: FSZipper
newFocus3 = (myDisk, []) -: fsTo "pics" -: fsRename "cspi" -: fsUp

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = (Folder folderName (item : items), bs)

newFocus4 :: FSZipper
newFocus4 = (myDisk, []) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp