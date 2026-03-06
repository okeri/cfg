set -gx LIBSEAT_BACKEND logind
set -gx EDITOR "emacst"
set -x ANDROID_SDK_ROOT /opt/android-sdk
set -x MOZ_USE_XINPUT2 1
set -x MOZ_ENABLE_WAYLAND 1
set -x MPLBACKEND GTK3Agg
set -x CALIB_ROOT ~/proj/calibrator
set -x XDG_CURRENT_DESKTOP sway
set -x XDG_SESSION_TYPE wayland

if not contains /opt/bin $PATH
	set -x PATH /opt/bin ~/.cargo/bin /opt/android-ndk /opt/mb/bin ~/.local/bin $PATH
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

bind \cd delete-char

function fish_greeting
end
