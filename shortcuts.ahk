#NoEnv ; recommended for performance and compatibility with future autohotkey releases.
#UseHook
#InstallKeybdHook
#SingleInstance force

SendMode Input


; make the escape key send the grave/tilde keys instead
; this is for small keyboards that have an ESC instead of tilde, since this
; script uses capslock for ESC instead
*Esc::Send {Blind}{`` Down}
*Esc Up::Send {Blind}{`` Up}

; Ctrl F9 - restarts all windows explorer.
^F9::
    Runwait TASKKILL /F /IM explorer.exe
    Run explorer.exe
return

; if capslock is pressed on its own, treat it as escape
~Capslock::
    KeyWait, Capslock
    if (A_PriorKey = "Capslock") {
        Send {Esc}
    }

    SetCapsLockState, Off
    SetCapsLockState, AlwaysOff
return


; =========================
; Pok3r Layout Marcos
; =========================


#Persistent
SetCapsLockState, AlwaysOff


; Capslock + jkli (left, down, up, right)
~Capslock & h::Send {Blind}{Left DownTemp}
~Capslock & h up::Send {Blind}{Left Up}

~Capslock & j::Send {Blind}{Down DownTemp}
~Capslock & j up::Send {Blind}{Down Up}

~Capslock & k::Send {Blind}{Up DownTemp}
~Capslock & k up::Send {Blind}{Up Up}

~Capslock & l::Send {Blind}{Right DownTemp}
~Capslock & l up::Send {Blind}{Right Up}


; Capslock + uobn (pgdown, pgup, home, end)
~Capslock & b::SendInput {Blind}{Home Down}
~Capslock & b up::SendInput {Blind}{Home Up}

~Capslock & n::SendInput {Blind}{End Down}
~Capslock & n up::SendInput {Blind}{End Up}

~Capslock & u::SendInput {Blind}{PgUp Down}
~Capslock & u up::SendInput {Blind}{PgUp Up}

~Capslock & o::SendInput {Blind}{PgDn Down}
~Capslock & o up::SendInput {Blind}{PgDn Up}


; Capslock + number keys equal function keys (F1-F10)
~Capslock & 1::SendInput {Blind}{F1}
~Capslock & 2::SendInput {Blind}{F2}
~Capslock & 3::SendInput {Blind}{F3}
~Capslock & 4::SendInput {Blind}{F4}
~Capslock & 5::SendInput {Blind}{F5}
~Capslock & 6::SendInput {Blind}{F6}
~Capslock & 7::SendInput {Blind}{F7}
~Capslock & 8::SendInput {Blind}{F8}
~Capslock & 9::SendInput {Blind}{F9}
~Capslock & 0::SendInput {Blind}{F10}
~Capslock & -::SendInput {Blind}{F11}
~Capslock & =::SendInput {Blind}{F12}

; Make ~Capslock & Backspace equivalent to delete
~Capslock & BS::SendInput {Del Down}
~Capslock & BS up::SendInput {Del Up}

; Make ~Capslock & ' equivalent to delete
~Capslock & '::SendInput {Del Down}
~Capslock & ' up::SendInput {Del Up}

; backup escape keys for weird situations
~Capslock & `::Send {Esc Down}
~Capslock & ` up::Send {Esc Up}
~Capslock & Esc::Send {Esc Down}
~Capslock & Esc up::Send {Esc Up}