set -x ANDROID_SDK_ROOT /opt/android-sdk
set -x NDK_ROOT /opt/android-ndk
set -x XDG_RUNTIME_DIR /run/user/17
set -x EDITOR "emacsclient -t"
set -x LANG en_US.UTF-8

if [ ! "$DISPLAY" ]
	set -x PATH $PATH $ANDROID_SDK_ROOT/tools $ANDROID_SDK_ROOT/platform-tools $ANDROID_SDK_ROOT/build-tools/20.0.0/ $NDK_ROOT /opt/bin /opt/cuda/bin
	set -x XKB_DEFAULT_LAYOUT us
	set -x XKB_DEFAULT_OPTIONS grp:toggle
	set -x GDK_BACKEND wayland
	sway
end

function fish_right_prompt
	set cmd_status_ $status
	if test $cmd_status_  = 0
		set cmd_status_ ''
	else
		echo -n (set_color red)
		set cmd_status_ "[$cmd_status_]"
	end
	echo -n -s $cmd_status_ (set_color yellow) " $CMD_DURATION" (set_color normal) 'ms'
end

alias em 'emacsclient -t'
alias su 'su -'
alias ll 'ls -la'
