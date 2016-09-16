set -x ANDROID_SDK_ROOT /opt/android-sdk
set -x NDK_ROOT /opt/android-ndk
set -x PATH $PATH $ANDROID_SDK_ROOT/tools $ANDROID_SDK_ROOT/platform-tools $ANDROID_SDK_ROOT/build-tools/20.0.0/ $NDK_ROOT /opt/bin /opt/cuda/bin
set -x XDG_RUNTIME_DIR /run/user/17
set -x EDITOR "emacsclient -t"
set -x LANG en_US.UTF-8

if [ ! "$DISPLAY" ]
   set -x XKB_DEFAULT_LAYOUT us
   set -x XKB_DEFAULT_OPTIONS grp:toggle
   set -x GDK_BACKEND wayland
   sway
end

alias em 'emacsclient -t'
alias su 'su -'
alias ll 'ls -la'
