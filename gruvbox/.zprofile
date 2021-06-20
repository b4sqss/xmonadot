export PATH=/home/basqs/.cabal/bin:$PATH
export PATH=/home/basqs/.local/share/cargo/bin:$PATH
export PATH=/home/basqs/.local/bin:$PATH

export EDITOR="nvim "
export TERMINAL="urxvt"
export BROWSER="firefox"

#change config directories

export NIX_REMOTE=daemon
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export CARGO_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/cargo"
export R_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/R"
export CABALPATH="${XDG_DATA_HOME:-$HOME/.local/share}/cabal"
export GNUPG_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/gnupg"
export DBUSSESSION="${XDG_DATA_HOME:-$HOME/.local/share}/dbus"
export GHCPATH="${XDG_CONFIG_HOME:-$HOME/.local/share}/ghc"
export MOCPATH="${XDG_CONFIG_HOME:-$HOME/.config}/moc"
export ICEWMPATH="${XDG_CONFIG_HOME:-$HOME/.config}/icewm"
export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}/go"
export WEECHAT_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/weechat"
export MOZILLA_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/mozilla"
export URXVT_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/urxvt"
export XMONAD_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/xmonad"
export WORDGRINDER_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/wordgrinder"
export THUNDERBIRD_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/thunderbird"
export GTK2_RC_FILES="${XDG_CONFIG_HOME:-$HOME/.config}/gtk-2.0/gtkrc-2.0"
export EMACS_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/emacs"
#export XINITRC="${XDG_CONFIG_HOME:-$HOME/.config}/x11/xinitrc"
#export MBSYNCRC="${XDG_CONFIG_HOME:-$HOME/.config}/mbsync/config"
export QT_QPA_PLATFORMTHEME="gtk2"
export _JAVA_AWT_WM_NONREPARENTING=1


#nnn file manager

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
