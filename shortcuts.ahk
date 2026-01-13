#Requires AutoHotkey v2.0.2
#UseHook
#SingleInstance force

if not A_IsAdmin
    Run '*RunAs "' A_ScriptFullPath '"'

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
; remap mouse button to \ to turn off discord mic
#HotIf WinActive("ahk_class DayZ")
XButton1::\


; Autorun - Press X
#HotIf WinActive("ahk_class DayZ")
~*'::
{
    if GetKeyState("w") {
        Send('{shift Up}')
        Send('{w Up}')
    } else {
        Send('{shift Down}')
        Send('{w Down}')
    }
}
