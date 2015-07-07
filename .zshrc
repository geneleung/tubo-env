# colors
[ -e /etc/zsh/zprofile ] && source /etc/zsh/zprofile
[ -e /opt/etc/zsh/zprofile ] && source /opt/etc/zsh/zprofile
which dircolors >> /dev/null && eval `dircolors $HOME/.zsh/colors`

bindkey -e

autoload -U zutil

# Resource files
for file in $HOME/.zsh/rc/*.zsh; do
	source $file
done
