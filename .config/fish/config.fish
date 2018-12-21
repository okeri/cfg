set -x ANDROID_SDK_ROOT /opt/android-sdk
set -x XDG_RUNTIME_DIR /run/user/17
set -x EDITOR "emacsclient -t"
set -x LANG en_US.UTF-8
set -x MOZ_USE_XINPUT2 1
set -x GDK_BACKEND wayland

#$ANDROID_SDK_ROOT/tools $ANDROID_SDK_ROOT/platform-tools $ANDROID_SDK_ROOT/build-tools/25.0.1/ /opt/android-ndk
if not contains /opt/android-ndk $PATH
	set -x PATH /opt/bin ~/.cargo/bin $PATH
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

function mail_kernel_patch
	set -l patch $argv[1]
	set -e argv[1]
	./scripts/checkpatch.pl $patch
	if [ $status -eq 0 ]
		git send-email --cc-cmd="./scripts/get_maintainer.pl --norolestats $patch" $argv $patch
		return 0
	else
		echo patch contains errors. Please fix it first
		return 1
	end
end
