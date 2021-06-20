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

alias l="ls --color -lgh"
alias ls="ls --color"
alias sl="ls --color"
alias vimb="tabbed -c vimb -e"
alias mocp="padsp mocp"
alias temacs="emacsclient -nw"
#alias vim="emacsclient -nw"
alias vim="nvim"
alias spotifyd="spotifyd --no-daemon & disown"
alias nnn="zsh & tmux -2 new-session -n nnn nnn"
alias steam="prime-run steam"
alias mkin="make && sudo make install"

#export NNN_BMS="d:$HOME/Documents;D:$HOME/Downloads/"
#export LC_COLLATE="C"
#export NNN_FIFO=/tmp/nnn.fifo
#export NNN_PLUG='o:fzopen;m:nmount;.:preview-tui-ext;x:_chmod +x $nnn'
#export NNN_ARCHIVE="\\.(7z|bz2|gz|tar|tgz|zip)$"
#BLK="0B" CHR="0B" DIR="02" EXE="06" REG="00" HARDLINK="06" SYMLINK="06" MISSING="00" ORPHAN="09" FIFO="06" SOCK="0B" OTHER="06"
#export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"

nnn-preview ()
{
    # Block nesting of nnn in subshells
    if [ -n "$NNNLVL" ] && [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, remove the "export" as in:
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # This will create a fifo where all nnn selections will be written to
    NNN_FIFO="$(mktemp --suffix=-nnn -u)"
    export NNN_FIFO
    (umask 077; mkfifo "$NNN_FIFO")

    # Preview command
    preview_cmd="/path/to/preview_cmd.sh"

    # Use `tmux` split as preview
    if [ -e "${TMUX%%,*}" ]; then
        tmux split-window -e "NNN_FIFO=$NNN_FIFO" -dh "$preview_cmd"

    # Use `xterm` as a preview window
    elif (which xterm &> /dev/null); then
        xterm -e "$preview_cmd" &

    # Unable to find a program to use as a preview window
    else
        echo "unable to open preview, please install tmux or xterm"
    fi

    nnn "$@"

    rm -f "$NNN_FIFO"
}
nnn_cd()
{
    if ! [ -z "$NNN_PIPE" ]; then
        printf "%s\0" "0c${PWD}" > "${NNN_PIPE}" !&
    fi
}

trap nnn_cd EXIT


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

#if command -v tmux>/dev/null; then
# [[ ! $TERM =~ screen ]] && [ -z $TMUX ] && tmux -2
#fi

bindkey -s '^o' 'lfcd\n'

bindkey -s '^a' 'bc -lq\n'

bindkey -s '^f' 'cd "$(dirname "$(fzf)")"\n'

bindkey ';5C': forward-word

bindkey ';5D': backward-word

# Load syntax highlighting; should be last.
source /home/basqs/.config/zsh/fast-syntas-highlighting/fast-syntax-highlighting.plugin.zsh
