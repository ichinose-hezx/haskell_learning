type Picture = [String]

flipH :: Picture -> Picture
flipH pic = [reverse line|line<-pic]

--flipV :: Picture -> Picture
--flipV pic = reverse pic

above :: Picture -> Picture -> Picture
above pica picb = pica++picb

sideBySide :: Picture -> Picture -> Picture
sideBySide pica picb = [aline++bline|(aline,bline)<-zip pica picb]


black :: Picture
black = ["###","###"]
white :: Picture
white = ["   ","   "]

quartet :: Picture
quartet = two `above` two
    where 
        two = check `sideBySide` check
            where 
                check = wb `above` flipH wb
                    where 
                        wb = white`sideBySide` black

board :: Picture
board = four `above` four
    where four = quartet `sideBySide` quartet

main :: IO()
main = do
    putStr (unlines $ board)