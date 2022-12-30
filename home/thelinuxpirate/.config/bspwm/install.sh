echo "This Script WILL Replace ANY BSPWM Configurations if they are already on your system!"

echo "Generating BSPWM Config"
rm -r ~/.config/bspwm/
mkdir ~/.config/bspwm/
mv ~/dotfiles/home/thelinuxpirate/.config/bspwm/bspwmrc ~/.config/bspwm/
chmod +x ~/.config/bspwm/bspwmrc
echo "DONE, & made executable"

echo "Generating SXHKD Config"
rm -r ~/.config/sxhkd/
mkdir ~/.config/sxhkd/
mv ~/dotfiles/home/thelinuxpirate/.config/bspwm/sxhkd/sxhkdrc ~/.config/bspwm/sxhkd/
echo "DONE"

echo "Generating POLYBAR Config (OLD-Piratebar)"
rm -r ~/.config/polybar/
mkdir ~/.config/polybar/
mv ~/dotfiles/home/thelinuxpirate/bspwm/polybar/config.ini ~/.config/polybar/
mv ~/dotfiles/home/thelinuxpirate/bspwm/polybar/launch.sh ~/.config/polybar/
chmod +x ~/.config/polyabr/launch.sh
echo "DONE"

echo "Generating PICOM Config"
rm -r ~/.config/picom/
mkdir ~/.config/picom/
mv ~/dotfiles/home/thelinuxpirate/.config/picom/picom.conf ~/.config/picom/
chmod +x ~/.config/picom/picom.conf
echo "DONE"
