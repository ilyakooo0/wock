=>
|%
++  hop
  |$  step
  $%
    [%fetch request:http]
  ==
++  wock-app
  |$  [model step]
  $_  ^?
  |%
  ++  init  *model
  ++  view  |~  model  *manx
  ++  move  |~  [model step]  *model
  --
++  app
  |*  [model=mold step=mold]
  |=  app=(wock-app model step)
  |=  [cmd=@tas args=*] 
  ?+  cmd  !!
    %init
      init.app
    %view
      (view.app (model args))
    %move
      =/  [=model step-tape=tape payload=*]  (,[model tape *] args)
      (move.app [model (step [(crip step-tape) payload])])
  ==
--
=>
|%
+$  step
  $%
    [%inc ~]
    [%dec ~]
  ==
--
%-  (app @ud step)
|%
++  view
  |=  count=@ud
  ^-  manx
  ;div
    ;h1: {<count>}
    ;button(on-click "inc"): +
    ;button(on-click "dec"): -
  ==
++  init  11
++  move
  |=  [model=@ud step=step]
  ^-  @ud
  ?-  -.step
    %inc  +(model)
    %dec  ?:  =(model 0)  0  (dec model)
  ==
--
