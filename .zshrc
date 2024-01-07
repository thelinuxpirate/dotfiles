export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="$HOME/.nimble/bin:$PATH"
export PATH="$BUN_INSTALL/bin:$PATH"

xset r rate 200 60
krabby random -i

plugins=(git)
ZSH_THEME="afowler"
CASE_SENSITIVE="true"

export ARCHFLAGS="-arch x86_64"
export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8

export DEVKITPRO=/opt/devkitpro
export DEVKITARM=/opt/devkitpro/devkitARM
export DEVKITPPC=/opt/devkitpro/devkitPPC

export BUN_INSTALL="$HOME/.bun"

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='emacs'
else
  export EDITOR='helix'
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
alias discordBot='cd && ./System/Code/wiggler/target/release/wiggler && cd -'

alias zypi='sudo zypper in'
alias zypr='sudo zypper rm'
alias zyps='zypper se'
alias zypu='sudo zypper up'
alias zypdup='sudo zypper dup'
alias zypar='sudo zypper ar'
alias zyprf='sudo zypper refresh'

[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"
source $HOME/.nix-profile/etc/profile.d/nix.sh
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
eval "$(starship init zsh)"
