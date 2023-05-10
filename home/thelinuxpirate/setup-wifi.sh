# Wifi Driver for "TP-Link AC1300 // Archer T3U Plus"
echo "Remember, this installation will need ROOT Perms"

echo "| Cloning Driver |"
cd ~/
mkdir System && mkdir System/wifi
cd System/wifi
git clone https://github.com/RinCat/RTL88x2BU-Linux-Driver

echo "| Installing Driver |"
cd RTL88x2BU-Linux-Driver
make clean && make
sudo make install
sudo modprobe 88x2bu

# Shoutouts to: https://github.com/RinCat/RTL88x2BU-Linux-Driver
