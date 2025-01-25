type t = ANSITerminal.style list

let fg_black = ANSITerminal.[ Foreground Black ]
let fg_white = ANSITerminal.[ Foreground White ]
let green = ANSITerminal.[ green ]
let repo = ANSITerminal.[ Bold; blue ]
let selected = ANSITerminal.[ Bold; green ]
let directory = ANSITerminal.[ Bold; magenta ]
let chosen = ANSITerminal.[ Bold; magenta ]
let secondary = ANSITerminal.[ cyan ]
let bold = ANSITerminal.[ Bold ]
let pr_closed = ANSITerminal.[ Bold; red ]
let pr_open = ANSITerminal.[ Bold; green ]
let pr_merged = ANSITerminal.[ Bold; magenta ]
