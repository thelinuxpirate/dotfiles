  echo "Cloning XMonad-Base files & XMonad-Contrib files for Installation"
  git clone https://github.com/xmonad/xmonad && git clone https://github.com/xmonad/xmonad-contrib
  echo "Getting ready to build!"
  
  echo "Generating init file..."
  stack init

  echo "Installing: XMonad + Contrib"
  stack install
  echo "DONE!"

  export PATH=$HOME/.local/bin:$PATH
  echo "Exported ~/.local/bin to your $PATH"

  echo "Proof of installation:"
  which xmonad
