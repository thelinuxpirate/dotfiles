export PATH="$HOME/bin:/usr/local/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/System/Applications/eww/target/release:$PATH"
export PATH="$HOME/System/Applications/smblevelworkshop2/build/ws2editor/launch/:$PATH"
export ZSH="$HOME/.oh-my-zsh"

#neofetch --ascii "$(cat ~/.local/neofetch/charmander)"
pokemon-colorscripts -r --no-title -b
#pokemon-colorscripts -n dragonite -b -s --no-title

ZSH_THEME="bira"
CASE_SENSITIVE="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh
export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export ARCHFLAGS="-arch x86_64"

if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='nvim'
 else
   export EDITOR='helix'
 fi

alias d="doas"
alias t="tree"
alias c="clear"
alias vi="nvim"
alias hx="helix"
alias nv="env -u WAYLAND_DISPLAY neovide"

alias dnfi="doas dnf install"
alias dnfr="doas dnf remove"
alias dnfs="doas dnf search"
alias dnfu="doas dnf update"
alias dnfU="doas dnf upgrade"
alias dnfuU="doas dnf update && doas dnf upgrade"

alias godot="cd && ./System/Applications/Godot4/Godot_v4.0.2-stable_mono_linux.x86_64 && cd -"
alias melee="cd && ./System/Applications/Slippi/Slippi-Launcher.AppImage && cd -"
alias tuss=" cd && ./System/Code/thetuss/thetuss && cd -"

alias Cfetch="neofetch --ascii '$(cat ~/.local/neofetch/charmander)' "

export DEVKITPRO=/opt/devkitpro
export DEVKITARM=${DEVKITPRO}/devkitARM
export DEVKITPPC=${DEVKITPRO}/devkitPPC

export PATH=${DEVKITPRO}/tools/bin:$PATH

# ScreenShot Location
GRIM_DEFAULT_DIR=~/Pictures/Screenshits/

#source /etc/profile.d/devkit-env.sh

# SOURCES
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
export PATH=$PATH:/home/thelinuxpirate/.spicetify