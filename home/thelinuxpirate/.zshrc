export PATH="$HOME/bin:/usr/local/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/System/Applications/eww/target/release:$PATH"
export PATH="$HOME/System/Applications/smblevelworkshop2/build/ws2editor/launch/:$PATH"
export ZSH="$HOME/.oh-my-zsh"

pokemon-colorscripts -r --no-title -b
#pokemon-colorscripts -n dragonite -b -s --no-title

ZSH_THEME="robbyrussell"
CASE_SENSITIVE="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh
export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export ARCHFLAGS="-arch x86_64"

if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='helix'
 else
   export EDITOR='emacs'
 fi

alias d="doas"
alias t="tree"
alias c="clear"
alias vim="nvim"
alias hx="helix"

alias paci="doas pacman -S"
alias pacs="doas pacman -Ss"
alias pacy="doas pacman -Sy"
alias pacyu="doas pacman -Syu"
alias pacu="doas pacman -Su"
alias pacr="doas pacman -R"

alias godot="cd && ./System/Applications/Godot4/Godot_v4.0.2-stable_mono_linux.x86_64 && cd -"
alias slippi="cd &&./System/Applications/Slippi/Slippi-Launcher.AppImage && cd -"

export DEVKITPRO=/opt/devkitpro
export DEVKITARM=${DEVKITPRO}/devkitARM
export DEVKITPPC=${DEVKITPRO}/devkitPPC

export PATH=${DEVKITPRO}/tools/bin:$PATH

# ScreenShot Location
GRIM_DEFAULT_DIR=~/Pictures/Screenshits/

#source /etc/profile.d/devkit-env.sh

# SOURCES
source ~/System/Zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
