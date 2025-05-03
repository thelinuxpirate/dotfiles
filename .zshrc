[[ -r $HOME/.repos/znap/znap.zsh ]] ||
    git clone --depth 1 -- \
        https://github.com/marlonrichert/zsh-snap.git $HOME/.repos/znap
source $HOME/.repos/znap/znap.zsh  
#source $HOME/.nix-profile/etc/profile.d/nix.sh
#source /etc/profile.d/devkit-env.sh

ZSH_THEME="flazz"
CASE_SENSITIVE="true"

export BROWSER="brave"
export DENO_INSTALL="$HOME/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="$HOME/.bun/bin:$PATH"
export PATH="$HOME/.platformio/penv/bin:$PATH"

znap source ohmyzsh/ohmyzsh
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

alias s='sudo'
alias d='doas'
alias t='tree'

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

alias vencordmanager='sh -c "$(curl -sS https://raw.githubusercontent.com/Vendicated/VencordInstaller/main/install.sh)"'
#eval "$(starship init zsh)"
