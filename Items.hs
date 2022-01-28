module Items (
    isEmpty,
    getItem,
    isEmptyAnd,
    getItemOr,
    isOnlyDirty,
    isOnlyDirty1,
    isOnlyDirty2,
    Item(..),
) where

data Item = Item
    {
        dirty  :: Bool,
        yard   :: Bool,
        boy    :: Bool,
        robot  :: Bool,
        object :: Bool,
        load   :: Bool
    }

instance Show Item where
    show (Item d y b r o l) = 
        if     d && not y && not r &&     b && not o && not l then "XB " else
        if     d && not y && not r && not b && not o && not l then " X " else
        if     d && not y &&     r &&     b && not o &&     l then "XbR" else
        if     d && not y &&     r &&     b && not o && not l then "XBR" else
        if     d && not y &&     r && not b && not o && not l then "XR " else
        if not d && not y &&     r &&     b && not o &&     l then "bR " else
        if not d && not y &&     r &&     b && not o && not l then "BR " else
        if not d && not y &&     r && not b && not o && not l then " R " else
        if not d && not y && not r &&     b && not o && not l then " B " else
        if not d && not y && not r && not b &&     o && not l then " O " else
        if not d &&     y && not r &&     b && not o && not l then "[B]" else
        if not d &&     y && not r && not b && not o && not l then "[ ]" else
        if not d &&     y &&     r &&     b && not o && not l then "[Z]" else
        if not d &&     y &&     r && not b && not o && not l then "[R]" else
        if not d &&     y &&     r &&     b && not o &&     l then "[r]" else " _ "

isEmpty :: Item -> Bool
isEmpty (Item x0 x1 x2 x3 x4 x5) = not (x0 || x1 || x2 || x3 || x4 || x5)

isEmptyAnd :: Item -> Item -> Bool
isEmptyAnd x y = (isEmpty x) && (isEmpty y)

getItem :: Item -> String -> Bool
getItem (Item x0 x1 x2 x3 x4 x5) _type
    | _type == "D" = x0
    | _type == "Y" = x1
    | _type == "B" = x2
    | _type == "R" = x3
    | _type == "O" = x4
    | _type == "L" = x5
    | otherwise = False

getItemOr :: Item -> Item -> String -> Bool
getItemOr x y _type = (getItem x _type) || (getItem y _type)

isOnlyDirty :: Item -> Bool
isOnlyDirty (Item x0 x1 x2 x3 x4 x5) = x0 && not x1 && not x2 && not x3 && not x4 && not x5

isOnlyDirty1 :: Item -> Bool
isOnlyDirty1(Item x0 x1 x2 x3 x4 x5) = x0 && not x1 && not x2 && x3 && not x4 && not x5

isOnlyDirty2 :: Item -> Bool
isOnlyDirty2(Item x0 x1 x2 x3 x4 x5) = x0 && not x1 && x2 && x3 && not x4 && x5