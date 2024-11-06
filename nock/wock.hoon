=>
|%
++  wock-app
  |$  [model event]
  $_  ^?
  |%
  ++  init  *model
  ++  view  |~  model  *manx
  ++  move  |~  [model event]  *model
  --
++  app
  |*  [model=mold event=mold]
  |=  app=(wock-app model event)
  |=  [cmd=@tas args=*] 
  ?+  cmd  !!
    %init
      init.app
    %view
      (view.app (model args))
    %move
      =/  [=model event-tape=tape payload=*]  (,[model tape *] args)
      (move.app [model (event [(crip event-tape) payload])])
  ==
--
=>
|%
+$  event
  $%
    [%inc ~]
    [%dec ~]
  ==
--
%-  (app @ud event)
|%
++  view
  |=  count=@ud
  ^-  manx
  ;div
    ;h1: {<count>}
    ;button(on-click "inc"): +
    ;button(on-click "dec"): -
  ==
++  init  8
++  move
  |=  [model=@ud step=event]
  ^-  @ud
  ?-  -.step
    %inc  +(model)
    %dec  ?:  =(model 0)  0  (dec model)
  ==
--
