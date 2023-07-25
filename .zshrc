export PATH="$HOME/bin:/usr/local/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.spicetify:$PATH"
export PATH="$HOME/.nimble/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/System/Applications/eww/target/release:$PATH"
export PATH="$HOME/System/Applications/smblevelworkshop2/build/ws2editor/launch/:$PATH"

export ZSH="$HOME/.oh-my-zsh"
export GUIX_PROFILE="/home/tlp/.guix-profile"
export OPENAI_API_KEY="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"

pokemon-colorscripts -r -b --no-title
#pokemon-colorscripts -n dragonite -b -s --no-title

ZSH_THEME="afowler"
CASE_SENSITIVE="true"

plugins=(git)

export STARSHIP_CONFIG=~/.config/starship.toml
export STARSHIP_CACHE=~/.starship/cache
export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export ARCHFLAGS="-arch x86_64"

# Statements
if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='emacs'
 else
   export EDITOR='helix'
 fi

if [ -n "$GUIX_ENVIRONMENT" ]; then
    if [[ $PS1 =~ (.*)"\\$" ]]; then
        PS1="${BASH_REMATCH[1]} [env]\\\$ "
    fi
fi

alias d="doas"
alias t="tree"
alias c="clear"
alias vi="nvim"
alias hx="helix"

alias dnfi="doas dnf install"
alias dnfr="doas dnf remove"
alias dnfs="doas dnf search"
alias dnfu="doas dnf update"
alias dnfU="doas dnf upgrade"
alias dnfuU="doas dnf update && doas dnf upgrade"

alias godot="cd && ./System/Applications/Godot4/Godot_v4.0.2-stable_mono_linux.x86_64 && cd -"
alias melee="cd && ./System/Applications/Slippi/Slippi-Launcher.AppImage && cd -"
alias tuss=" cd && ./System/Code/thetuss/thetuss && cd -"

export DEVKITPRO=/opt/devkitpro
export DEVKITARM=${DEVKITPRO}/devkitARM
export DEVKITPPC=${DEVKITPRO}/devkitPPC

export PATH=${DEVKITPRO}/tools/bin:$PATH

# ScreenShot Location
GRIM_DEFAULT_DIR=~/Pictures/Screenshits/


# SOURCES
source $ZSH/oh-my-zsh.sh
source $GUIX_PROFILE/etc/profile
source /etc/profile.d/devkit-env.sh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
