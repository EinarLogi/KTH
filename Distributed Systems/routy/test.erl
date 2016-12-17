
routy:start(r1, stockholm).
routy:start(r2, malmo).
routy:start(r3, vasteras).
routy:start(r4,lund).
routy:start(r5, uppsala).

r1 ! {add, malmo, {r2, 'sweden@130.229.189.211'}}.
r1 ! {add, vasteras, {r3, 'sweden@130.229.189.211'}}.
r2 ! {add, vasteras,{r3, 'sweden@130.229.189.211'}}.
r2 ! {add, lund, {r4,'sweden@130.229.189.211'}}.
r3 ! {add, stockholm,{r1,'sweden@130.229.189.211'}}.
r4 ! {add, uppsala,{r5,'sweden@130.229.189.211'}}.
r5 ! {add, stockholm, {r1, 'sweden@130.229.189.211'}}.


r1 ! broadcast.
r2 ! broadcast.
r3 ! broadcast.
r4 ! broadcast.
r5 ! broadcast.

r1 ! update.
r2 ! update.
r3 ! update.
r4 ! update.
r5 ! update

r2 ! {send, stockholm, 'jallajalla'}.
routy:stop(r3).
r1 ! broadcast.
r2 ! broadcast.
r3 ! broadcast.
r4 ! broadcast.
r5 ! broadcast.

r1 ! update.
r2 ! update.
r3 ! update.
r4 ! update.
r5 ! update
r2 ! {send, stockholm, 'jallajalla'}.