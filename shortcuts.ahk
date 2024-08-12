#Requires AutoHotkey v2.0.2
#UseHook
#SingleInstance force

SendMode("Input")

; make the escape key send the grave/tilde keys instead
; this is for small keyboards that have an ESC instead of tilde, since this
; script uses capslock for ESC instead
;*Esc::Send("{Blind}{`` Down}")
;*Esc Up::Send("{Blind}{`` Up}")

; Ctrl F9 - restarts all windows explorer.
^F9:: {
    RunWait("TASKKILL /F /IM explorer.exe")
    Run("explorer.exe")
}


Hyper(Keys) {
    Global HyperShortcutPressed := true
    Send(Keys)
}

; if capslock is pressed on its own quickly, treat it as escape
~Capslock:: {
    Global HyperShortcutPressed := false
    Global HyperKeyTime := A_TickCount

    KeyWait("Capslock")

    TimeElapsed := A_TickCount - HyperKeyTime
    if !HyperShortcutPressed and TimeElapsed < 300 {
        Send("{Esc down}")

        if WinActive("ahk_class Valheim") {
            Sleep(80)
        }

        Send("{Esc up}")
    }

    SetCapsLockState("Off")
    SetCapsLockState("AlwaysOff")
}

SetCapsLockState("AlwaysOff")

; Capslock + jkli (left, down, up, right)
~Capslock & h::Hyper("{Blind}{Left DownTemp}")
~Capslock & h up::Hyper("{Blind}{Left Up}")

~Capslock & j::Hyper("{Blind}{Down DownTemp}")
~Capslock & j up::Hyper("{Blind}{Down Up}")

~Capslock & k::Hyper("{Blind}{Up DownTemp}")
~Capslock & k up::Hyper("{Blind}{Up Up}")

~Capslock & l::Hyper("{Blind}{Right DownTemp}")
~Capslock & l up::Hyper("{Blind}{Right Up}")

; Capslock + uobn (pgdown, pgup, home, end)
~Capslock & b::Hyper("{Blind}{Home Down}")
~Capslock & b up::Hyper("{Blind}{Home Up}")

~Capslock & n::Hyper("{Blind}{End Down}")
~Capslock & n up::Hyper("{Blind}{End Up}")

~Capslock & u::Hyper("{Blind}{PgUp Down}")
~Capslock & u up::Hyper("{Blind}{PgUp Up}")

~Capslock & o::Hyper("{Blind}{PgDn Down}")
~Capslock & o up::Hyper("{Blind}{PgDn Up}")

; Capslock + number keys equal function keys (F1-F10)
~Capslock & 1::Hyper("{Blind}{F1}")
~Capslock & 2::Hyper("{Blind}{F2}")
~Capslock & 3::Hyper("{Blind}{F3}")
~Capslock & 4::Hyper("{Blind}{F4}")
~Capslock & 5::Hyper("{Blind}{F5}")
~Capslock & 6::Hyper("{Blind}{F6}")
~Capslock & 7::Hyper("{Blind}{F7}")
~Capslock & 8::Hyper("{Blind}{F8}")
~Capslock & 9::Hyper("{Blind}{F9}")
~Capslock & 0::Hyper("{Blind}{F10}")
~Capslock & -::Hyper("{Blind}{F11}")
~Capslock & =::Hyper("{Blind}{F12}")

; Make ~Capslock & Backspace equivalent to delete
~Capslock & BS::Hyper("{Del Down}")
~Capslock & BS up::Hyper("{Del Up}")

; Make ~Capslock & ' equivalent to delete
~Capslock & '::Hyper("{Del Down}")
~Capslock & ' up::Hyper("{Del Up}")
~Capslock & `;::Hyper("{Ins Down}")
~Capslock & `; up::Hyper("{Ins Up}")

; backup escape keys for weird situations
~Capslock & `::Hyper("{Esc Down}")
~Capslock & ` up::Hyper("{Esc Up}")
~Capslock & Esc::Hyper("{Esc Down}")
~Capslock & Esc up::Hyper("{Esc Up}")

; Komorebic(cmd) {
;     RunWait(Format("komorebic.exe {}", cmd), , "Hide")
; }
;
; !q::Komorebic("close")
; !m::Komorebic("minimize")
;
; ; Focus windows
; !h::Komorebic("focus left")
; !j::Komorebic("focus down")
; !k::Komorebic("focus up")
; !l::Komorebic("focus right")
;
; !+[::Komorebic("cycle-focus previous")
; !+]::Komorebic("cycle-focus next")
;
; ; Move windows
; !+h::Komorebic("move left")
; !+j::Komorebic("move down")
; !+k::Komorebic("move up")
; !+l::Komorebic("move right")
;
; ; Stack windows
; !Left::Komorebic("stack left")
; !Down::Komorebic("stack down")
; !Up::Komorebic("stack up")
; !Right::Komorebic("stack right")
; !;::Komorebic("unstack")
; ![::Komorebic("cycle-stack previous")
; !]::Komorebic("cycle-stack next")
;
; ; Resize
; !=::Komorebic("resize-axis horizontal increase")
; !-::Komorebic("resize-axis horizontal decrease")
; !+=::Komorebic("resize-axis vertical increase")
; !+_::Komorebic("resize-axis vertical decrease")
;
; ; Manipulate windows
; !t::Komorebic("toggle-float")
; !f::Komorebic("toggle-monocle")
;
; ; Window manager options
; !+r::Komorebic("retile")
; !p::Komorebic("toggle-pause")
;
; ; Layouts
; !x::Komorebic("flip-layout horizontal")
; !y::Komorebic("flip-layout vertical")
;
; ; Workspaces
; !1::Komorebic("focus-workspace 0")
; !2::Komorebic("focus-workspace 1")
; !3::Komorebic("focus-workspace 2")
; !4::Komorebic("focus-workspace 3")
; !5::Komorebic("focus-workspace 4")
; !6::Komorebic("focus-workspace 5")
; !7::Komorebic("focus-workspace 6")
; !8::Komorebic("focus-workspace 7")
;
; ; Move windows across workspaces
; !+1::Komorebic("move-to-workspace 0")
; !+2::Komorebic("move-to-workspace 1")
; !+3::Komorebic("move-to-workspace 2")
; !+4::Komorebic("move-to-workspace 3")
; !+5::Komorebic("move-to-workspace 4")
; !+6::Komorebic("move-to-workspace 5")
; !+7::Komorebic("move-to-workspace 6")
; !+8::Komorebic("move-to-workspace 7")
