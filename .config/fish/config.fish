set -x ANDROID_SDK_ROOT /opt/android-sdk
set -x XDG_RUNTIME_DIR /run/user/17
set -x EDITOR "emacsclient -t"
set -x LANG en_US.UTF-8

if not contains /opt/android-ndk $PATH
	set -x PATH $PATH /opt/bin $ANDROID_SDK_ROOT/tools $ANDROID_SDK_ROOT/platform-tools $ANDROID_SDK_ROOT/build-tools/25.0.1/ /opt/android-ndk /opt/cuda/bin
end


function fish_right_prompt
	set cmd_status_ $status
	if test $cmd_status_  = 0
		set cmd_status_ ''
	else
		echo -n (set_color red)
		set cmd_status_ "[$cmd_status_]"
	end
	echo -n -s (set_color cyan)(git branch ^/dev/null | grep \* | sed 's/* //')(set_color normal)
end

alias em 'emacsclient -t'
alias su 'su -'
alias ll 'ls -la'

if [ ! "$DISPLAY" ]
	set -x XKB_DEFAULT_LAYOUT us
	set -x XKB_DEFAULT_OPTIONS grp:toggle
	set -x GDK_BACKEND wayland
	sway
end
