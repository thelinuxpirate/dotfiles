export PATH="$HOME/.nimble/bin:$PATH"
export PATH="$BUN_INSTALL/bin:$PATH"

krabby random -i

plugins=(
  git,
  zsh-autosuggestions
)

ZSH_THEME="afowler"
CASE_SENSITIVE="true"

export ARCHFLAGS="-arch x86_64"
export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8

export HISTFILE=~/.zsh_history
export HISTSIZE=1000000
export SAVEHIST=1000000
setopt appendhistory

export DEVKITPRO=/opt/devkitpro
export DEVKITARM=/opt/devkitpro/devkitARM
export DEVKITPPC=/opt/devkitpro/devkitPPC

export BUN_INSTALL="$HOME/.bun"

if [ "$WAYLAND_DISPLAY" = "0" ]; then
    xset r rate 200 60
else
    export DISPLAY=":0"
    export WAYLAND_DISPLAY="wayland-1"
    export QT_QPA_PLATFORM="wayland-egl"
    export MOZ_ENABLE_WAYLAND="1"
    export _JAVA_AWT_WM_NONREPARENTING="1"
    export XDG_SESSION_TYPE="wayland"
fi

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
else
  export EDITOR='nano'
fi

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

alias d='doas'
alias s='sudo'
alias c='clear'
alias t='tree'

alias ls='ls --color=auto'
alias vi='nvim'
alias hx='helix'
alias discordBot='cmd'

alias paci='doas pacman -S'
alias pacr='doas pacman -R'
alias pacs='pacman -Ss'
alias pacu='doas pacman -Sy'
alias pacdup='doas pacman -Syu'

alias auri='paru -S'
alias aurr='paru -R'
alias aurs='paru -Ss'
alias aurdup='paru'


[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"
source $HOME/.nix-profile/etc/profile.d/nix.sh
eval "$(starship init zsh)"
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
