function fish_right_prompt
	set cmd_status_ $status
	if test $cmd_status_ -ne 0
		echo -n (set_color red) "[$cmd_status_] "
	end
	echo (set_color white) $CMD_DURATION\ms
	echo -n (set_color cyan) (git rev-parse --abbrev-ref HEAD 2> /dev/null)(set_color normal)
end

function cd
	builtin cd $argv
	ls
end

alias em 'emacsclient -t'
alias emacs 'emacsclient -t'
alias su 'su -'

bind \cd delete-char

function fish_greeting
end
