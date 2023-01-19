echo "Generating init file..."
stack init

echo "Installing: XMonad + Contrib"
stack install
echo "DONE!"

export PATH=$HOME/.local/bin:$PATH
echo "Exported ~/.local/bin to your $PATH"

echo "Proof of installation:"
which xmonad
