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

alias myip="curl 'https://api.ipify.org?format=json'"
alias mygeo="curl 'https://ipinfo.io/json'"

alias rclone_all='func_rclone $buku_db_path $pwd_path $zoxide_db'

alias wea='curl wttr.in/Санкт-Петербург?lang=ru&format=3'

# alias pbpaste='xclip -selection clipboard -o'
# 

# alias wgup="sudo wg-quick up wg0"
# alias wgdn="sudo wg-quick down wg0"


set zoxide_db "/home/rep/.local/share/zoxide/db.zo"
set buku_db_path "/home/rep/.local/share/buku/bookmarks.db"
set pwd_path "/home/rep/Documents/pwd/pwd.kdbx"

function func_rclone -d "Backup files to dropbox and gdirve"
    for p in $argv
      echo "copy $p to GDRIVE"
      # rclone copy $p gdrive:backup -v
      rclone copy $p gdrive:backup
      echo "copy $p to DROPBOX"
      rclone copy $p dropbox:backup
    end
end

if [ "$INSIDE_EMACS" = vterm ]

    function clear
        vterm_printf "51;Evterm-clear-scrollback"
        tput clear
    end

    function fish_greeting
        random choice "Hello!" "Hi" "G'day" "Howdy"
    end
else
    function fish_greeting
        ff
    end

end

# alias gdriv='rclone copy $pwd_path gdrive:backup'
# alias dropb='rclone copy $pwd_path dropbox:backup'

