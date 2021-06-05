export PATH=/home/basqs/.cabal/bin:$PATH
export PATH=/home/basqs/.local/share/cargo/bin:$PATH
export PATH=/home/basqs/.local/bin:$PATH

export EDITOR="nvim "
export TERMINAL="urxvt"
export BROWSER="firefox"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export CARGO_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/cargo"
export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}/go"
export WEECHAT_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/weechat"
export GTK2_RC_FILES="${XDG_CONFIG_HOME:-$HOME/.config}/gtk-2.0/gtkrc-2.0"

export NNN_BMS="d:$HOME/Documents;D:$HOME/Downloads/"
export LC_COLLATE="C"
export NNN_FIFO=/tmp/nnn.fifo
export NNN_PLUG='f:fzopen;o:fzcd;m:nmount;.:preview-tui-ext;x:_chmod +x $nnn'
export NNN_ARCHIVE="\\.(7z|bz2|gz|tar|tgz|zip)$"
BLK="0B" CHR="0B" DIR="04" EXE="06" REG="00" HARDLINK="06" SYMLINK="06" MISSING="00" ORPHAN="09" FIFO="06" SOCK="0B" OTHER="06"
export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"

if [ "$(tty)" = "/dev/tty1" ]; then
	startx
fi
