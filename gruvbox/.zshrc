# Enable colors and change prompt:
autoload -U colors && colors	# Load colors
PS1="%B%{$fg[red]%}[%{$fg[green]%}%~%{$fg[red]%}] "
setopt autocd		# Automatically cd into typed directory.
stty stop undef		# Disable ctrl-s to freeze terminal.
setopt interactive_comments

# History in cache directory:
HISTSIZE=10000000
SAVEHIST=10000000
HISTFILE=~/.cache/zsh/history

autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)

# Load aliases and shortcuts if existent.

alias l="ls --color -ls"
alias ls="ls --color"
alias sl="ls --color"
#alias mocp="padsp mocp"
alias tsm="transmission-remote"
alias nnnp="tmux new-session -s nnnp -n nnn nnn"
alias vim="nvim"
alias spotifyd="spotifyd --no-daemon & disown"
alias mkin="make && sudo make install"


# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.
## case-insensitive (all) completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Use lf to switch directories and bind it to ctrl-o
lfcd () {
    tmp="$(mktemp)"
    lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp" >/dev/null
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}

bindkey -s '^o' 'lfcd\n'

bindkey -s '^a' 'bc -lq\n'

bindkey -s '^f' 'cd "$(dirname "$(fzf)")"\n'

bindkey ';5C': forward-word

bindkey ';5D': backward-word

# Load syntax highlighting; should be last.
source /home/basqs/.config/zsh/fast-syntas-highlighting/fast-syntax-highlighting.plugin.zsh
