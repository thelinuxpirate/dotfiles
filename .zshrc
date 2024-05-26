[[ -r $HOME/.repos/znap/znap.zsh ]] ||
    git clone --depth 1 -- \
        https://github.com/marlonrichert/zsh-snap.git $HOME/.repos/znap
source $HOME/.repos/znap/znap.zsh  
source $HOME/.nix-profile/etc/profile.d/nix.sh
source /etc/profile.d/devkit-env.sh

export DENO_INSTALL="$HOME/.deno"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="$HOME/.nimble/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$DENO_INSTALL/bin:$PATH"
export PATH="$HOME/.bun/bin:$PATH"
export PATH="$HOME/.platformio/penv/bin:$PATH"

znap source zsh-users/zsh-syntax-highlighting
znap source zsh-users/zsh-autosuggestions
znap source zsh-users/zaw

xset r rate 200 60
krabby random -i

ZSH_THEME="afowler"
CASE_SENSITIVE="true"

export ARCHFLAGS="-arch x86_64"
export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8

export HISTFILE=$HOME/.zsh_history
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

# ffmpeg -i <input> -pix_fmt bgr8  yo%03d.bmp <remember me!>

alias d='doas'
alias s='sudo'
alias c='clear'
alias t='tree -A -C'

alias ls='ls --color=auto'
alias vi='nvim'
alias hx='helix'
alias cleannix='nix-collect-garbage --delete-old'

alias paci='doas pacman -S'
alias pacr='doas pacman -R'
alias pacs='pacman -Ss'
alias pacu='doas pacman -Sy'
alias pacup='doas pacman -Syu'
alias pacdup='paru'

alias auri='paru -S'
alias aurr='paru -R'
alias aurs='paru -Ss'

alias rebuildthesucc='cd $HOME/.config/sleepy-dwm/ && doas make clean install && cd slstatus/ && doas make clean install && cd .. && cd dmenu/ && doas make clean install && cd'
alias rebuildthewm='cd $HOME/.config/sleepy-dwm/ && doas make clean install && cd'
alias rebuildthebar='cd $HOME/.config/sleepy-dwm/slstatus/ && doas make clean install && cd'
alias rebuildthemenu='cd $HOME/.config/sleepy-dwm/dmenu/ && doas make clean install && cd'


[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"
eval "$(starship init zsh)"

# bun completions
[ -s "/home/trong/.bun/_bun" ] && source "/home/trong/.bun/_bun"
