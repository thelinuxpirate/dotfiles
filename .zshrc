export PATH="$HOME/.config/emacs/bin:$PATH"

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

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='emacs'
else
  export EDITOR='nvim'
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
alias discordBot='cd && ./System/Code/wiggler/target/release/wiggler && cd -'

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
eval "$(starship init zsh)"
export PATH=$PATH:/home/pingu/.spicetify

# bun completions
[ -s "/home/pingu/.bun/_bun" ] && source "/home/pingu/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
