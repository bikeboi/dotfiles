# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'l:|=* r:|=*'
zstyle :compinstall filename '/home/bikeboi/.zsh/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh/.zshfiles/histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd extendedglob nomatch notify
unsetopt appendhistory beep
bindkey -e
# End of lines configured by zsh-newuser-install

# Plugins
plugins=(
    cabal
    dirhistory
)

# Important stuff
source /etc/profile.d/nix.sh
source /etc/profile.d/nix-daemon.sh

echo "  |-- Welcome $USER --|\n"
