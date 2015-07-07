# for have colors
autoload -U colors
colors

# define your colors here (i hate white background)
host_color="green"
path_color="blue"

host="%{$fg[$host_color]%}%n@%m"
cpath="%{$fg[$path_color]%}%1d"
end="%{$reset_color%}% > "

PS1="$host $cpath $end"
