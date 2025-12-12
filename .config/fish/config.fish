if status is-interactive
    # Commands to run in interactive sessions can go here
    starship init fish | source
    atuin init fish | source
end

# set --erase fish_greeting


zoxide init fish | source

alias ls='eza -lah --total-size --icons=auto --group-directories-first'
alias lt='eza --tree --group-directories-first --icons --git-ignore'
alias ll='eza -la --group-directories-first --icons'

alias yy='yazi'
alias ff='fastfetch --raw /home/rep/Pictures/image.bin --logo-width 15'

# alias pbpaste='xclip -selection clipboard -o'

# alias wgup="sudo wg-quick up wg0"
# alias wgdn="sudo wg-quick down wg0"

alias gdriv='rclone copy "/home/rep/Documents/DATA_TEMP/other/сбор данных/pwd.kdbx" gdrive:backup'
alias dropb='rclone copy "/home/rep/Documents/DATA_TEMP/other/сбор данных/pwd.kdbx" remote:backup'




function fish_greeting
    # random choice "Hello!" "Hi" "G'day" "Howdy"
    ff
end
