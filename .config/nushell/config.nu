## Aliases

# Single Char
alias d = doas
alias s = sudo
alias c = clear
alias t = tree -A -C

# Misc
alias ls     = ^ls --color=auto
alias vi     = nvim
alias vim    = nvim
alias hx     = helix

# Package Management
alias paci   = doas pacman -S
alias pacr   = doas pacman -Rns
alias pacs   = pacman -Ss
alias pacu   = doas pacman -Sy
alias pacup  = doas pacman -Syu
alias pacdup = paru
alias auri   = paru -S
alias aurr   = paru -Rns
alias aurs   = paru -Ss

# Building Utils
def rebuildthewm [] {
    cd $nu.env.HOME/.config/chadwm/chadwm
    doas make clean install
    cd -
}

def rebuildthemenu [] {
    cd $nu.env.HOME/.config/chadwm/dmenu
    doas make clean install
    cd -
}

# IDK Yet
alias vencordmanager = sh -c $"(curl -sS https://raw.githubusercontent.com/Vendicated/VencordInstaller/main/install.sh)"

$env.config.show_banner = false

# Commands ran upon EVERY startup
xset r rate 200 60
krabby random -i
