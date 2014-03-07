" saves all the visible windows if needed/possible
function functions#AutoSave()
  let this_window = winnr()

  windo call functions#SmartUpdate()

  execute this_window . 'wincmd w'

endfunction

function functions#SmartUpdate()
  if &buftype != "nofile" && expand('%') != '' && &modified
    write

  endif

endfunction
