neofetch

export ZSH="$HOME/.oh-my-zsh"

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
 alias aptu="doas nala update"
 alias aptU="doas nala upgrade"
 alias apti="doas nala install"
 alias apts="doas nala search"
 alias aptr="doas nala remove"
 alias aptuU="doas nala update && doas nala upgrade"
 alias t="tree"
 alias c="clear"

 alias startX="startx ~/.startx/xmonad"
 alias startD="startx ~/.startx/dwm"

source "$HOME/System/Applications/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
source "$HOME/.ghcup/env"
source "$HOME/.cargo/env"
