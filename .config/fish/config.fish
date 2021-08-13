set -gx XDG_RUNTIME_DIR /run/user/17
set -gx LIBSEAT_BACKEND logind
set -gx EDITOR "emacst"
set -gx LANG en_US.UTF-8
set -x ANDROID_SDK_ROOT /opt/android-sdk
set -x MOZ_USE_XINPUT2 1
set -x MOZ_ENABLE_WAYLAND 1
set -x CALIB_ROOT ~/proj/calibrator

#$ANDROID_SDK_ROOT/tools $ANDROID_SDK_ROOT/platform-tools $ANDROID_SDK_ROOT/build-tools/25.0.1/ 
if not contains /opt/android-ndk $PATH
	set -x PATH /opt/bin ~/.cargo/bin /opt/android-ndk /opt/xilinx/Vivado/2019.1/bin $PATH
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

if test -z "$SWAYSOCK" -a /dev/tty1 = (tty)
    sway
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
