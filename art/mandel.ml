

                                                            ;;
                                                         let(q)=
                                                         (08192+
                                                          0)let
                                                          (++)(x
                                               )y    =x+(y)let(--)x(y
                                               )=x-(y)let(//)x(y)=(x*q+ y/2
                                               )/(y)let(%%)x(y)=(x*y+q/2)/(
                                              q)let(!)x=x*(q)let(xmin)=(!(
                                             -2))let(xmax)=(!1//(!2));;let(
                                          deltax)=(xmax--xmin)//(!(2*40))let(
                                           ymin)=(!(-1))let(ymax)=(!1)(*1597
                           *) let( (     deltay))=(ymax--ymin)//(!40)let(*8*)
                           rec(**)iter   (n)(a)(b)(xn)(yn)=n=100||(let(xn2)=(
                         xn%%xn)in(let(  yn2)=(yn%%yn)in(xn2++yn2<=(!4)&&iter
                         (n+1)a(b)(((xn2 --yn2))++a)(!2%%xn%%yn++b))))(*42*)
                     let(**)inside(x)(y)=iter(0)x(y)0(0)let(**)rec(**)row(j
       )(   y      )=if(j<2*40)then(let(x)=xmin++(!j%%deltax)in(let(c)=if
                     inside(x)(y)then('0')else('.')in(print_char((c));row(j
                         +1)y)))let(*5*) rec(*.*)m(i)=if(i<40)then(let(y)=((
                         ymin++(!i)%%((  deltay))))in(row(0)y;print_newline()
                            ;m(i+(1)))   )(*3.1415926535897932384626433832795
                           02 8 84        1971*)let(**)rec(**)t(a)b(c)(ct)=if
                          (               a>00)then(let(**)rec(**)loop(e)(ct
                                           )=if(e>0)then(let(d)=(e)land(-e)in
                                             loop(e-d)(t(a-d)((b+d)*2)((c+d
                                            ) /2)ct))else(ct)in(loop((((a))
                                               land(lnot(b))land(lnot(c))))
                                               ct))else(ct+1)let(q8)=t( 255
                                               )0  0 (0)let(dt)=0x7e1
                                                        ,12 , 19
                                                          let((
                                                         au))="J
                                                         CF";;m(
                                                            0)
