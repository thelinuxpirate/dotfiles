[[ -r $HOME/.repos/znap/znap.zsh ]] ||
    git clone --depth 1 -- \
        https://github.com/marlonrichert/zsh-snap.git $HOME/.repos/znap
source $HOME/.repos/znap/znap.zsh  

source $HOME/.nix-profile/etc/profile.d/nix.sh
export DENO_INSTALL="$HOME/.deno"
export PATH="$HOME/.nimble/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$DENO_INSTALL/bin:$PATH"
export PATH="$HOME/.bun/bin:$PATH"

znap source zsh-users/zsh-syntax-highlighting
znap source zsh-users/zsh-autosuggestions
znap source zsh-users/zaw

krabby random -i

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

[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"
eval "$(starship init zsh)"

# bun completions
[ -s "/home/trong/.bun/_bun" ] && source "/home/trong/.bun/_bun"
[ -f "/home/trong/.ghcup/env" ] && source "/home/trong/.ghcup/env" # ghcup-env
