titleCol=DarkOrange3
keyCol=green2

OPTS='-bg grey5 -fg grey80 -x 600 -y 0 -l 30 -ta l -w 500 -p -e onstart=uncollapse,scrollhome;button5=scrolldown;key_Down=scrolldown;button4=scrollup;key_Up=scrollup;key_Page_Down=scrolldown:30;key_Page_Up=scrollup:30;key_Escape=exit;button3=exit;entertitle=grabkeys;enterslave=grabkeys;leaveslave=ungrabkeys'

(
echo "     ^fg($titleCol)----------- keys -----------^fg()";
sed -n \
 -e 's/^.*"\(M-[^"]*\)".*--[ \t]*\(.*\)$/^fg('$keyCol')\1^fg() \2/p' \
 -e 's/^[ \t]*$//p' \
 ~/.xmonad/xmonad.hs
) | cat -s \
| dzen2  $OPTS

