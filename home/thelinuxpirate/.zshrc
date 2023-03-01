export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH="$HOME/.emacs.d/bin:$PATH"
export ZSH="$HOME/.oh-my-zsh"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"

colorscript -r

ZSH_THEME="robbyrussell"

CASE_SENSITIVE="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh
export LANG=en_US.UTF-8

 if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='emacs'
 else
   export EDITOR='nvim'
 fi

export ARCHFLAGS="-arch x86_64"

 alias d="doas"
 alias paci="doas pacman -S"
 alias pacs="doas pacman -Ss"
 alias pacy="doas pacman -Sy"
 alias pacyu="doas pacman -Syu"
 alias pacu="doas pacman -Su"
 alias pacr="doas pacman -R"
 alias vim="nvim"
 alias t="tree"
 alias c="clear"

# DEVKIT VARIABLES
export DEVKITPRO=/opt/devkitpro
export DEVKITARM=${DEVKITPRO}/devkitARM
export DEVKITPPC=${DEVKITPRO}/devkitPPC

export PATH=${DEVKITPRO}/tools/bin:$PATH

# ScreenShot Location
GRIM_DEFAULT_DIR=~/Pictures/Screenshots/

source /etc/profile.d/devkit-env.sh

# SOURCES
source ~/System/Zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
