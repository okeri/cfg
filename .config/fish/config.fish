set -x ANDROID_SDK_ROOT /opt/android-sdk
set -x XDG_RUNTIME_DIR /run/user/17
set -x EDITOR "emacsclient -t"
set -x LANG en_US.UTF-8
#set -x SWAYGRAB_FFMPEG_OPTS_DECODER '-c:v h264_nvenc -preset slow'
set -x CUDA_DIR /opt/cuda
set -x LEELOO_TESTDATA /home/okeri/proj/pkg-leeloo/testdata
set -x LEELOO_MAIN_QML /home/okeri/proj/pkg-leeloo/modules/ui/source/qml/Main.qml
set -x ARCH_AARCH64_COMPONENTS_ROOT /opt/aarch64-cross

#$ANDROID_SDK_ROOT/tools $ANDROID_SDK_ROOT/platform-tools $ANDROID_SDK_ROOT/build-tools/25.0.1/ /opt/android-ndk
if not contains /opt/android-ndk $PATH
	set -x PATH $PATH /opt/bin $CUDA_DIR/bin
end

function fish_right_prompt
	set cmd_status_ $status
	if test $cmd_status_ -ne 0
		echo -n (set_color red) "[$cmd_status_] "
	end
	echo -n -s (set_color cyan)(git rev-parse --abbrev-ref HEAD ^ /dev/null)(set_color normal)
end

function mc -d "Midnight Commander"
	set old_shell $SHELL
	set SHELL /bin/bash
	command /bin/mc
	set SHELL $old_shell
end

function cd
	builtin cd $argv
	ls
end

alias em 'emacsclient -t'
alias emacs 'emacsclient -t'
alias su 'su -'

if [ ! "$DISPLAY" ]
	env XKB_DEFAULT_LAYOUT=us XKB_DEFAULT_MODEL=pc98 XKB_DEFAULT_OPTIONS=grp:toggle sway
end

function fish_greeting
end
