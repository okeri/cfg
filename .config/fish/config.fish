set -gx LIBSEAT_BACKEND logind
set -gx EDITOR "emacst"
set -x ANDROID_SDK_ROOT /opt/android-sdk
set -x MOZ_USE_XINPUT2 1
set -x MOZ_ENABLE_WAYLAND 1
set -x MPLBACKEND GTK3Agg
set -x CALIB_ROOT ~/proj/calibrator

if not contains /opt/android-ndk $PATH
	set -x PATH /opt/bin ~/.cargo/bin /opt/android-ndk /opt/mb/bin $PATH
end

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

bind "[27;5;13~" execute
bind "[27;2;13~" execute
bind \cd delete-char

function fish_greeting
end
