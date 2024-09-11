|.
=/  hoon=[type nock]  (ride *type .^(@t %cx /~zod/base/1/sys/hoon/hoon))
|=  [tag=@tas args=*]
?+  tag  !!
  %ride  (ride (,[type @] args))
  %comb  (comb (,[nock nock] args))
  %hoon  hoon
  %wash  (crip (of-wall:format (wash (,[[@ @] tank] args))))
==
