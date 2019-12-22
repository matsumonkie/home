# * settings

autoload -Uz promptinit
promptinit
prompt adam1

setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*' menu select

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# Editor
export EDITOR='emacsclient -nw -t'

# Theme
ZSH_THEME="ys"
DEFAULT_USER="$USER"

# No duplicate
setopt hist_ignore_all_dups

# No autocorrect
unsetopt correct_all
unsetopt correct
setopt nocorrectall
DISABLE_CORRECTION="true"

# * Alias & Function

alias -g G='| grep'
alias -g L='| less'
alias -g B='&& notify-send -u critical "\(^o^)/" --expire-time=1000 || notify-send "(╯°□°）╯︵ ┻━┻"'

alias l='ls -1'
alias rm='trash'
alias copy='xclip -selection clipboard'
alias paste='xclip -o'

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias -- -="cd -"

# man pages with color
man() {
  env LESS_TERMCAP_mb=$'\E[01;31m' \
  LESS_TERMCAP_md=$'\E[01;38;5;74m' \
  LESS_TERMCAP_me=$'\E[0m' \
  LESS_TERMCAP_se=$'\E[0m' \
  LESS_TERMCAP_so=$'\E[1;44;33m' \
  LESS_TERMCAP_ue=$'\E[0m' \
  LESS_TERMCAP_us=$'\E[04;38;5;146m' \
  man "$@"
}

function f {
    find . -iname "*$1*"
}

# Emacs

alias e='emacsclient -nw -t'

# Git

function current_branch {
  git symbolic-ref HEAD | cut -d'/' -f3;
}

alias g='git'
alias gco='git checkout'
alias gd='git diff'
alias grhh='git reset --hard HEAD'
alias gds='git diff --staged'
alias ga='git add'
alias ggpush='git push origin $(current_branch)'
alias ggpull='git pull origin $(current_branch)'
alias gs='git status -s'
alias gg="git log --pretty=oneline -n 20 --graph --abbrev-commit"
alias gga="git log --graph --pretty=format:'%Cred%h%Creset %an: %s - %Creset %C(yellow)%d%Creset %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gc='git commit'
alias gcam='git commit --amend -C HEAD'
alias ggg='git log --pretty=oneline --graph --abbrev-commit'
function fix {
  git commit -m "fixup! $*"
}

alias fax='echo "fixup! $*"'

# Keyboard
alias bd='~/.kbd/kbd'
alias fr='setxkbmap fr'

# Ruby
source /usr/local/share/chruby/chruby.sh
chruby ruby-2.6.5

alias b='bundle'
alias be='bundle exec'
alias bl='bundle list'

alias p='cd $(git rev-parse --show-toplevel)'

function take () {
  mkdir -p "$@" && cd "$@"
}

alias nixe="nix-env"
alias nixc="nix-channel"
alias nixs="nix-store"
