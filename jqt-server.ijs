load 'socket'
coinsert 'jsocket'

PORT=: 8017
sdcleanup''
server0 =: 0 { > sdcheck sdsocket ''
sdcheck sdbind server0;AF_INET_jsocket_;'';PORT
sdcheck sdlisten server0,1

responseFor=: 4 : 0
  try. ". y
       'got ',y
  catch. 'request to evaluate ',LF,y,LF,'failed' end.
)

repl=: 3 : 0
echo 'in repl'
while. 1 do.
  echo 'in loop'
  while. server0 e. ready =. >{. sdcheck sdselect (sdcheck sdgetsockets ''),'';'';<1e3
  do. sdcheck sdaccept server0 end.
  for_socket. ready do.
   request=: ; sdcheck sdrecv socket, 65536 0
   sdcheck (socket responseFor request) sdsend socket, 0
   sdcheck sdclose socket
  end.
end.
)


echo 'hiho!'
repl''