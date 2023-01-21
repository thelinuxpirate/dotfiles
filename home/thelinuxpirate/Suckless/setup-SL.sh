echo "Turned slstatus config into a temp directory..."
mv slstatus temp/

echo "Cloning Slstatus:"
git clone https://git.suckless.org/slstatus
echo "Complete"

echo "Transfering Configurations"
cd slstatus && rm config.h
mv ~/Suckless/temp/config.h ~/Suckless/slstatus/

echo "Cleaning Up"
rm -r ~/Suckless/temp
